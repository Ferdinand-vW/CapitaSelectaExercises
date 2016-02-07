{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Request where

import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics

type Key   = String
type Value = String

data Request = GET Key | SET Key Value deriving (Typeable,Generic)
instance Binary Request
