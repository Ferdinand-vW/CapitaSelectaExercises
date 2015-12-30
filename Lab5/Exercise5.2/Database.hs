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
import Control.Monad
import Control.Concurrent.STM
import Worker
import Request

type Database = ProcessId

createDB :: [NodeId] -> Process Database
createDB nodes = spawnLocal (database nodes) --We now pass the list of peers

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
    ps <- mapM (\n -> spawn n $(mkStaticClosure 'worker)) nodes --We spawn a Process for each given node
    forever $ do
        (pid,r) <- expect :: Process (ProcessId, Request)
        case r of
            GET k -> do
                send (npid k ps) (pid,GET k) --We pick a pid by splicing the keyspace, which we then use for GET/SET
            SET k v -> do
                send (npid k ps) (pid,SET k v)
    where
        npid k ps = ps !! (ord (head k) `mod` length nodes) --Splice the key space
        

rcdata :: RemoteTable -> RemoteTable
rcdata = Worker.__remoteTable
