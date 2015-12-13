{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP,BangPatterns #-}

module BoundedChan where

import Control.Concurrent.STM

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)
data BoundedChan a = BoundedChan (TVar (TVarList a))
                                 (TVar (TVarList a))
                                 (TVar Int)

newBoundedChan :: Int -> STM (BoundedChan a)
newBoundedChan n = do
    hole <- newTVar TNil
    read <- newTVar hole
    write <- newTVar hole
    size <- newTVar n
    return $ BoundedChan read write size

readBoundedChan :: BoundedChan a -> STM a
readBoundedChan (BoundedChan readVar writeVar sizeVar) = do
    size <- readTVar sizeVar
    writeTVar sizeVar (size + 1)
    listHead <- readTVar readVar
    head <- readTVar listHead
    case head of
        TNil -> retry
        TCons val tail -> do
            writeTVar readVar tail
            return val


writeBoundedChan :: BoundedChan a -> a -> STM ()
writeBoundedChan (BoundedChan readVar writeVar sizeVar) val = do
    size <- readTVar sizeVar
    if size == 0
        then retry
        else writeTVar sizeVar (size - 1)
    listEnd <- readTVar writeVar
    newListEnd <- newTVar TNil
    writeTVar listEnd (TCons val newListEnd)
    writeTVar writeVar newListEnd
            