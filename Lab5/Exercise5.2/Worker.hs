{-# LANGUAGE TemplateHaskell #-}

module Worker where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import qualified Data.Map as Map
import Data.Map (Map)
import Request

worker :: Process () --We moved the handleRequest to separate Nodes called workers
worker = loop Map.empty
    where
        loop db = do
            (pid,r) <- expect :: Process (ProcessId, Request)
            case r of
                SET k v -> do
                    let newdb = Map.insert k v db
                    loop newdb
                GET k -> do
                    let mval = Map.lookup k db
                    send pid mval
                    loop db
                    
                    
remotable ['worker]
