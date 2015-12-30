{-# LANGUAGE TemplateHaskell #-}

module Worker where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import qualified Data.Map as Map
import Data.Map (Map)
import Request

worker :: Process ()
worker = loop Map.empty
    where
        loop db = do
            r <- expect :: Process Request
            case r of
                SET k v -> do
                    let newdb = Map.insert k v db
                    loop newdb
                GET k s -> do
                    let mval = Map.lookup k db
                    sendChan s mval
                    loop db
                    
                    
remotable ['worker]
