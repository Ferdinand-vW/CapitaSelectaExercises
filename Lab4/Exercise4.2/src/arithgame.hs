{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Network
import Data.Time.Clock
import qualified Data.Map as Map
import Data.Map (Map)

import Text.Printf
import System.IO
import System.Timeout
import System.Random


-- << main
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
type Points = Int
type Answer = String
type Question = String

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientSendChan :: TChan Message
  , clientBroadCastChan :: TChan Message
  }
  
-- <<newClient
newClient :: TChan Message -> ClientName -> Handle -> STM Client
newClient bchan name handle = do
  c <- newTChan
  dchan <- dupTChan bchan
  return Client { clientName          = name
                , clientHandle        = handle
                , clientSendChan      = c
                , clientBroadCastChan = dchan
                }
                
-- <<Server
data Server = Server
  { clients :: TVar (Map ClientName Client),
    time :: TVar UTCTime, --The time that the question appeared
    numQ :: TVar Int, --Number of questions appeared
    question :: TVar Question, --Current question
    answer :: TVar Answer, --Answer to the question
    points :: TVar (Map ClientName Points), --Points of each client
    rgen :: TVar StdGen, --Random number generator
    serverChan :: TChan Message --Channel used for broadcasting to all clients
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  t <- getCurrentTime
  time <- newTVarIO t
  nq <- newTVarIO 0
  q <- newTVarIO ""
  a <- newTVarIO ""
  p <- newTVarIO Map.empty
  g <- getStdGen
  gen <- newTVarIO g
  chan <- atomically newBroadcastTChan
  let server = Server { clients = c, time = time,numQ = nq, question = q, answer = a, points = p, rgen = gen, serverChan = chan}
  atomically $ generateNewQuestion server t --We start the first question
  return server
-- >>

-- <<Message
data Message = Notice String
             | Broadcast String
             | Command String
             deriving Show
-- >>

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
-- -----------------------------------------------------------------------------
-- The main server


talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  forkIO $ gameState server --Need a thread to keep a check on the gamestate
  readName
 where
-- <<readName
  readName = do
    hPutStrLn handle "What is your name?"
    mname <- timeout 20000000 $ hGetLine handle --We wait 20s before timing out
    case mname of
        Nothing -> do --If we didn receive anything, then we close the handle.
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
                              `finally` removeClient server name
                              
-- <<checkAddClient
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  case Map.lookup name clientmap of
    Just _ -> return Nothing
    Nothing -> do
        client <- newClient serverChan name handle
        pnts <- readTVar points
        writeTVar clients $ Map.insert name client clientmap
        writeTVar points $ Map.insert name 0 pnts --Also add an entry in points
        broadcast serverChan $ Notice (name ++ " has connected")
        q <- readTVar question
        sendMessage client $ Broadcast q
        return (Just client)
-- >>

-- <<removeClient
removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  modifyTVar' points $ Map.delete name --Remove the entry in points
  broadcast serverChan $ Notice (name ++ " has disconnected")
-- >>

--Checks whether a number of questions have been asked
--and for how long a question has been unanswered
gameState :: Server -> IO ()
gameState server@Server{..} = forever $ do
    nq <- atomically $ readTVar numQ
    newtime <- getCurrentTime
    if nq > 5 --If the number of questions that have been answered is 5
        then atomically $ do
            broadcast serverChan $ Broadcast "5 questions have been answered. The total number of points is:"
            printPoints serverChan points --We show the points total
            broadcast serverChan $ Broadcast ""
            generateNewQuestion server newtime --Generate a new question
            writeTVar numQ 0 --Reset points and number of questions
            modifyTVar points $ Map.map (\_ -> 0)
            
        else atomically $ do
            oldtime <- readTVar time --If a question has been unanswered for 10 seconds
            if diffUTCTime newtime oldtime >= 10
                then do
                    generateNewQuestion server newtime --We generate a new question
                else
                    return ()

-- <<runClient
runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  forkIO $ fillClientChan
  race server receive
  return ()
 where
  receive = forever $  do
    msg <- hGetLine clientHandle
    atomically $ sendMessage client (Command msg)

  server = join $ atomically $ do
    msg <- readTChan clientSendChan
    return $ do
        continue <- handleMessage serv client msg
        when continue server
  
  fillClientChan = forever $ do --Level over any messages in the broadcastChan to the sendChan of the same client
    atomically $ do
        msg <- readTChan clientBroadCastChan
        writeTChan clientSendChan msg
        return msg
                    
--Determines whether a given answer is the correct answer                   
answerQuestion :: Server -> Client -> String -> IO Bool
answerQuestion server@Server{..} client@Client{..} msg = do
    newtime <- getCurrentTime
    atomically $ do --It is important that all of the below is inside a single atomically
        a <- readTVar answer --otherwise multiple clients could have the correct answer and more than one question will be generated
        if a == msg --Answer is correct
            then do
                broadcast serverChan $ Broadcast $ clientName ++ "'s answer " ++ a ++ " was correct."
                modifyTVar' points $ Map.adjust (+1) clientName --Add a point for the client
                generateNewQuestion server newtime --generate a new question
                return True
            else do
                sendMessage client $ Notice "Wrong answer."
                return True

--Broadcasts each client's points
printPoints :: TChan Message -> TVar (Map ClientName Points) -> STM ()
printPoints serverChan points = do
    pnts <- readTVar points
    mapM_ (\(k,a) -> broadcast serverChan $ Broadcast $ k ++ " has " ++ show a ++ " points.") $ Map.toList pnts
            
generateNewQuestion :: Server -> UTCTime -> STM ()
generateNewQuestion server@Server{..} newtime = do
     gen <- readTVar rgen --Get the random number generator
     let (a,gen') = randomR (0,50 :: Int) gen --Take 3 random numbers, while
     let (b,gen'') = randomR (0,50) gen' --passing along the generator
     let (n,gen''') = randomR (0,2) gen''
     writeTVar rgen gen''' --Store the new generator into the TVar
     let op  = [(+),(-),(*)] !! n
     let sop = ["+","-","*"] !! n
     broadcast serverChan $ Broadcast $ show a ++ " " ++ sop ++ " " ++ show b
     writeTVar question $ show a ++ " " ++ sop ++ " " ++ show b --Insert the new question
     writeTVar answer $ show $ a `op` b --Insert the new answer
     modifyTVar' numQ (+1) --Increment the number of questions
     writeTVar time newtime --Insert the start time of the question
                    
-- <<handleMessage
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server@Server{..} client@Client{..} message = do
  case message of
     Notice msg    -> output $ "*** " ++ msg
     Broadcast msg -> output $ ">: " ++ msg
     Command msg ->
       case words msg of
           ["/quit"] ->
               return False
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True
           _ -> do --all other input is recognized as an attempt to answer the question
               answerQuestion server client msg
 where
   output s = do hPutStrLn clientHandle s; return True
-- >>
