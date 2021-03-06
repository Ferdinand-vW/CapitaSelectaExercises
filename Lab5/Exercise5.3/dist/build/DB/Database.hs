{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Text.Printf
import Control.Monad
import Control.Concurrent.STM
import Worker
import Request

type Database = ProcessId

createDB :: [NodeId] -> Process Database
createDB nodes = spawnLocal (database nodes)

set :: Database -> Key -> Value -> Process ()
set db k v = do
    let req = SET k v
    pid <- getSelfPid
    send db (pid,req)

get :: Database -> Key -> Process (Maybe Value)
get db k = do
    let req = GET k
    pid <- getSelfPid
    send db (pid,req)
    expect :: Process (Maybe Value)
  
database :: [NodeId] -> Process ()
database nodes = do
    ps <- mapM (\n -> spawn n $(mkStaticClosure 'worker)) nodes

    mapM_ (\pid -> spawnLocal $ withMonitor pid $ do --We spawn a process for each process for monitoring
                    receiveWait --We wait until we receive one of the below messages
                        [ match $ \(ProcessMonitorNotification ref deadpid reason) -> do
                              say $ printf "process %s died: %s" (show deadpid) (show reason)
                              spid <- getSelfPid
                              exit spid "Monitoring done." --Shutdown the monitoring process
                        ]) ps
    forever $ do
     (pid,r) <- expect :: Process (ProcessId,Request)
     case r of
        GET k -> do
            send (npid k ps) (pid,GET k)
        SET k v -> do
            send (npid k ps) (pid,SET k v)
    where
        npid k ps = ps !! (ord (head k) `mod` length nodes)
        

rcdata :: RemoteTable -> RemoteTable
rcdata = Worker.__remoteTable
