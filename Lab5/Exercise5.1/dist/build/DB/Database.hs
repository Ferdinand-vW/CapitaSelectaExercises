{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Serializable

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad
import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics
import Control.Concurrent.STM

type Key   = String
type Value = String

type Database = ProcessId

createDB :: [NodeId] -> Process Database
createDB nodes = spawnLocal database

set :: Database -> Key -> Value -> Process ()
set db k v = do
    let req = SET k v --Create a Request
    pid <- getSelfPid --Get the ProcessId of this process (master)
    send db (pid,req) --pass it to the database process. We pass the ProcessId, so that a response can be send

get :: Database -> Key -> Process (Maybe Value)
get db k = do
    let req = GET k
    pid <- getSelfPid
    send db (pid,req)
    expect :: Process (Maybe Value)
    

rcdata :: RemoteTable -> RemoteTable
rcdata = id
  
data Request = GET Key | SET Key Value deriving (Typeable,Generic) --Custom Datatype used for Messaging between nodes
instance Binary Request
  
  
database :: Process ()
database = do
    db <- liftIO $ newTVarIO Map.empty --Our database is a simple Map
    loop db
  where
    loop db = forever $ do
        (pid,req) <- expect :: Process (ProcessId, Request) --We expect messages coming from the master node
        handleRequest db pid req --Afterwards we handle the received message
        
handleRequest :: TVar (Map Key Value) -> ProcessId -> Request -> Process ()
handleRequest tdb pid req = do
    db <- liftIO $ atomically $ readTVar tdb
    case req of
        GET k -> do --Now just lookup the value and return the result
            let mval = Map.lookup k db
            send pid mval
        SET k v -> do --Insert a new value into the map
            let newdb = Map.insert k v db
            liftIO $ atomically $ writeTVar tdb newdb
            return ()
    
    
    
    
    
    
    
    
