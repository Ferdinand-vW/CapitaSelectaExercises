{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Message where

import Control.Distributed.Process
import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics

data M = SP (SendPort M) | M String deriving (Typeable,Generic,Show)
instance Binary M
