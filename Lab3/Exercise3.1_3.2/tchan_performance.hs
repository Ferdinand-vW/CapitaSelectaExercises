{-# LANGUAGE BangPatterns #-}

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

main = do
    chan <- atomically newTChan
    race (writeValues chan 10000000) (readValues chan)

writeValues :: TChan Int -> Int -> IO ()
writeValues chan 0 = return ()
writeValues chan n = do
    atomically $ writeTChan chan n
    writeValues chan (n - 1)

readValues :: TChan Int -> IO ()
readValues chan = do
    let !n = atomically $ readTChan chan
    readValues chan