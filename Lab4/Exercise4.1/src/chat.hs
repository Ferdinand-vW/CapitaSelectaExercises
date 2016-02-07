{-
   Adapted from haskell-chat-sever-example which is
      Copyright (c) 2012, Joseph Adams

   Modifications (c) 2012, Simon Marlow
-}

{-# LANGUAGE RecordWildCards #-}
module Main where

import ConcurrentUtils

import Control.Concurrent hiding (forkFinally,race)
import Control.Concurrent.STM
import Control.Concurrent.Async hiding(race)
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import System.Timeout
import Control.Exception
import Network
import Control.Monad
import Text.Printf
import Data.Time.Clock

{-
Notes

- protocol:
    Server: "Name?"
    Client: <string>
    -- if <string> is already in use, ask for another name
    -- Commands:
    --   /tell <string> message...  (single-user tell)
    --   /quit                      (exit)
    --   /kick <string>             (kick another user)
    --   message...                 (broadcast to all connected users)

- a client needs to both listen for commands from the socket and
  listen for activity from other clients.  Therefore we're going to
  need at least two threads per client (for listening to multiple
  things).  Easiest is to use STM for in-process communication, and to
  have a receiving thread that listens on the socket and forwards to a
  TChan.

- Handle all errors properly, be async-exception safe

- Consistency:
  - if two clients simultaneously kick a third client, only one will be
    successful

See doc/lab-exercises.tex for some ideas for enhancements that you
could try.

-}

-- <<main
main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle server) (\_ -> hClose handle)

port :: Int
port = 44444
-- >>


-- ---------------------------------------------------------------------------
-- Data structures and initialisation

-- <<Client
type ClientName = String
type ClientId = Int

data Client = Client
  { clientId       :: ClientId
  , clientName     :: TVar ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  , clientBroadCastChan :: TChan Message
  }
-- >>

-- <<newClient
newClient :: TChan Message -> TVar Int -> ClientName -> Handle -> STM Client
newClient bchan id name handle = do
  c <- newTChan
  k <- newTVar Nothing
  n <- newTVar name
  dchan <- dupTChan bchan
  clientid <- do i <- readTVar id
                 writeTVar id (i + 1)
                 return i
  return Client { clientId            = clientid
                , clientName          = n
                , clientHandle        = handle
                , clientSendChan      = c
                , clientKicked        = k
                , clientBroadCastChan = dchan
                }
-- >>

-- <<Server
data Server = Server
  { clients :: TVar (Map ClientId Client),
    serverChan :: TChan Message, --Channel used for broadcasting to all clients
    nextId :: TVar Int
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  chan <- atomically newBroadcastTChan
  id <- newTVarIO 0
  return Server { clients = c, serverChan = chan, nextId = id }
-- >>

-- <<Message
data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String
             deriving Show
-- >>

-- -----------------------------------------------------------------------------
-- Basic operations

-- <<broadcast
broadcast :: TChan Message -> Message -> STM ()
broadcast bchan msg = do
    writeTChan bchan msg --Broadcast
-- >>

-- <<sendMessage
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg
-- >>

getClientFromName :: Server -> ClientName -> STM (Maybe Client)
getClientFromName Server{..} name = do
    clientmap <- readTVar clients
    let clients = Map.elems clientmap
    foldr (\c x -> do  
                    cname <- readTVar $ clientName c
                    if cname == name
                    then return $ Just c
                    else x) (return Nothing) clients

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} Client{..} who msg = do
  mclient <- atomically $ getClientFromName server who
  case mclient of
    Nothing -> hPutStrLn clientHandle (who ++ " is not connected.")
    Just client -> do
      atomically $ do
        name <- readTVar clientName
        sendMessage client (Tell name msg)

kick :: Server -> ClientName -> Client -> STM ()
kick server@Server{..} who client = do
  mvictim <- getClientFromName server who
  case mvictim of
    Nothing ->
      void $ sendMessage client (Notice $ who ++ " is not connected")
    Just victim -> do
        name <- readTVar $ clientName client
        writeTVar (clientKicked victim) $ Just ("by " ++ name)
        void $ sendMessage client (Notice $ "you kicked " ++ who)
        
clientNames :: Map ClientId Client -> STM [ClientName]
clientNames clientmap =
    sequence $ map (\c -> readTVar $ clientName c) $ Map.elems clientmap  
      
listNames :: Server -> Handle -> IO ()
listNames server@Server{..} handle = do
    clientmap <- atomically $ readTVar clients
    names <- atomically $ clientNames clientmap --Get all names from the clientMap
    hPutStrLn handle "Currently connected clients:"
    mapM_ (\name -> hPutStrLn handle $ "  " ++ name) names --Print the names
    
changeName :: Server -> Client -> ClientName -> IO ()
changeName Server{..} client name = do
    ok <- atomically $ do
        clientmap <- readTVar clients
        names <- clientNames clientmap --First we get the names of all clients
        if elem name names
            then return False --Name must be unique
            else do
                oldName <- readTVar $ clientName client
                writeTVar (clientName client) name --update the name
                broadcast serverChan $ Notice $ oldName ++ " has changed his name to " ++ name
                return True
                
    if ok
        then return ()
        else hPutStrLn (clientHandle client) "Name already exists!"
                        
                                                

-- -----------------------------------------------------------------------------
-- The main server

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  readName
 where
-- <<readName
  readName = do
    hPutStrLn handle "What is your name?"
    mname <- timeout 20000000 $ hGetLine handle --We wait 20s before timing out
    case mname of
        Nothing -> do --If we didnt receive anything, then we close the handle.
            hPutStrLn handle "20 seconds have passed. You have timed-out."
            hClose handle
        Just name ->
            if null name
              then readName
              else mask $ \restore -> do        -- <1>
                     ok <- checkAddClient server name handle
                     case ok of
                       Nothing -> restore $ do  -- <2>
                          hPrintf handle
                             "The name %s is in use, please choose another\n" name
                          readName
                       Just client ->
                          restore (runClient server client) -- <3>
                              `finally` removeClient server client
-- >>

-- <<checkAddClient
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  mclient <- getClientFromName server name
  case mclient of
    Just _ -> return Nothing
    Nothing -> do
        client <- newClient serverChan nextId name handle
        writeTVar clients $ Map.insert (clientId client) client clientmap
        broadcast serverChan $ Notice (name ++ " has connected")
        return (Just client)
-- >>

-- <<removeClient
removeClient :: Server -> Client -> IO ()
removeClient server@Server{..} Client{..} = atomically $ do
  modifyTVar' clients $ Map.delete clientId
  name <- readTVar clientName
  broadcast serverChan $ Notice (name ++ " has disconnected")
-- >>

-- <<runClient
runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  forkIO $ fillClientChan
  nmsg <- atomically $ newTVar 0
  flooded <- atomically $ newTVar False
  time <- getCurrentTime
  forkIO $ timer nmsg flooded True time
  race server (receive nmsg flooded)
  return ()
 where
  receive nmsg flooded = do --Added timeout for receiving messages from clients
    mmsg <- timeout 20000000 $ hGetLine clientHandle
    case mmsg of
        Nothing -> return ()
        Just msg -> do
            atomically $ modifyTVar' nmsg (\i -> i + 1)
            isflooded <- atomically $ readTVar flooded
            if isflooded
                then do
                    atomically $ sendMessage client $ Notice "You are flooding the server!"
                    receive nmsg flooded
                else do
                        atomically $ sendMessage client (Command msg)
                        receive nmsg flooded

  server = join $ atomically $ do
    k <- readTVar clientKicked
    case k of
        Just reason -> return $
            hPutStrLn clientHandle $ "You have been kicked: " ++ reason
        Nothing -> do
            msg <- readTChan clientSendChan
            return $ do
                continue <- handleMessage serv client msg
                when continue server
  
  fillClientChan = forever $ do --hand over any messages in the broadcastChan to the sendChan of the same client
    atomically $ do
        msg <- readTChan clientBroadCastChan
        writeTChan clientSendChan msg
        return msg
  
  timer nmsg flooded started time --First start the timer
    | not started = do start <- getCurrentTime
                       timer nmsg flooded True start
    | otherwise = do
        end <- getCurrentTime
        let diff = diffUTCTime end time
        if diff >= 10 --if 10seconds have passed we reset everything
            then do
                atomically $ writeTVar flooded False
                atomically $ writeTVar nmsg 0
                timer nmsg flooded False time
            else do
                num <- atomically $ readTVar nmsg
                if num >= 20 --If 10 seconds haven passed, but more than 20 messages have been sent
                    then do --Then we stop allowing messages
                        atomically $ writeTVar flooded True
                        timer nmsg flooded started time
                    else timer nmsg flooded started time
            
-- <<handleMessage
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server@Server{..} client@Client{..} message = do
  case message of
     Notice msg         -> output $ "*** " ++ msg
     Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
     Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
     Command msg ->
       case words msg of
           ["/kick", who] -> do
               atomically $ kick server who client
               return True
           "/tell" : who : what -> do
               tell server client who (unwords what)
               return True
           ["/name", name] -> do
               changeName server client name
               return True
           ["/quit"] ->
               return False
           ["/names"] -> do
               listNames server clientHandle
               return True
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True
           _ -> do
               name <- atomically $ readTVar clientName
               atomically $ broadcast serverChan $ Broadcast name msg
               return True
 where
   output s = do hPutStrLn clientHandle s; return True
-- >>
