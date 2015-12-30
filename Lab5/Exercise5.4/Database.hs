{-# LANGUAGE TemplateHaskell #-}

module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char
import Data.List
import Text.Printf
import Control.Monad
import Control.Monad.Loops
import Control.Concurrent.STM
import Worker
import Request

type Database = ProcessId

createDB :: [NodeId] -> Process Database
createDB nodes = spawnLocal (database nodes)

set :: Database -> Key -> Value -> Process ()
set db k v = do
    let req = SET k v
    send db req

get :: Database -> Key -> Process (Maybe Value)
get db k = do
    (s,r) <- newChan --Instead of using expect I now use a channel
    let req = GET k s --I also have to pass the sendport in the get
    send db req
    receiveChan r --Chan was necessary, because I get a double get response from the workers
                  --the result being that the next time i do a get i get the previous answer
  
database :: [NodeId] -> Process ()
database nodes = do
    ps <- mapM (\n -> spawn n $(mkStaticClosure 'worker)) nodes
    
    tps <- liftIO $ newTVarIO (pairs ps)
    spawnLocal $ do --I changed a bit how I monitor the processes
        mapM_ monitor ps --Start monitoring
        whileM_  ( do --Check if there are any processes left to monitor
            pids <- liftIO $ atomically $ readTVar tps
            return $ length pids > 0
                 )$
            receiveWait --Wait for a process to die
                [ match $ \(ProcessMonitorNotification ref deadpid reason) -> do
                      liftIO $ atomically $ modifyTVar' tps $ map (delete deadpid)
                      say $ printf "process %s died: %s" (show deadpid) (show reason)
                ]                      
                   
    forever $ do
     r <- expect :: Process Request
     ps' <- liftIO $ atomically $ readTVar tps
     case r of
        GET k s-> mapM_ (\p -> send p r) (npid k ps') --send a GET to both workers
        SET k v -> mapM_ (\p -> send p r) (npid k ps') --send a SET to both workers
    where
        pairs [] = [] --Determine Process pairs
        pairs [x] = []
        pairs (x:y:xs) = [x,y] : pairs xs
        npid k ps = ps !! (ord (head k) `mod` length ps) --Splicing of the key space
        

rcdata :: RemoteTable -> RemoteTable
rcdata = Worker.__remoteTable
