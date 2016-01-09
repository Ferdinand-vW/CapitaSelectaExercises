{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP,BangPatterns #-}

module BoundedChan where

import Control.Concurrent.STM

--Unfortunately this implementation of the boundedchan deadlocks, unless
--a big enough size is given, which kind of defeats the purpose of a boundedchan
type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)
data BoundedChan a = BoundedChan (TVar (TVarList a)) --read end
                                 (TVar (TVarList a)) --write end
                                 (TVar Int) --Size of the boundedchan

newBoundedChan :: Int -> STM (BoundedChan a)
newBoundedChan n = do
    hole <- newTVar TNil --initialize the TVars
    read <- newTVar hole
    write <- newTVar hole
    size <- newTVar n
    return $ BoundedChan read write size

readBoundedChan :: BoundedChan a -> STM a
readBoundedChan (BoundedChan readVar writeVar sizeVar) = do
    size <- readTVar sizeVar --First we can increment the size of the boundedchan
    writeTVar sizeVar (size + 1)
    listHead <- readTVar readVar --Then we get the element from the boundedchan
    head <- readTVar listHead
    case head of
        TNil -> retry --If it was empty redo the transaction, thus the size increment is undone
        TCons val tail -> do
            writeTVar readVar tail --Otherwise make the tail the new readend
            return val --return the value of the previous readend


writeBoundedChan :: BoundedChan a -> a -> STM ()
writeBoundedChan (BoundedChan readVar writeVar sizeVar) val = do
    size <- readTVar sizeVar
    if size == 0 --If the size is 0 we cannot write to the boundedchan
        then retry
        else writeTVar sizeVar (size - 1) --otherwise decrement the size
    listEnd <- readTVar writeVar
    newListEnd <- newTVar TNil --Create a new write end
    writeTVar listEnd (TCons val newListEnd) --Insert the value into that writeend
    writeTVar writeVar newListEnd
            