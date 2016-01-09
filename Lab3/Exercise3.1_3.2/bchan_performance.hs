{-# LANGUAGE BangPatterns #-}

import BoundedChan
import Control.Concurrent.STM
import Control.Concurrent.Async

main = do
    chan <- atomically $ newBoundedChan 10000000
    race (writeValues chan 10000000) (readValues chan)

writeValues :: BoundedChan Int -> Int -> IO ()
writeValues chan 0 = return ()
writeValues chan n = do
    atomically $ writeBoundedChan chan n
    writeValues chan (n - 1)

readValues :: BoundedChan Int -> IO ()
readValues chan = do
    let !n = atomically $ readBoundedChan chan --We have to force the evaluation of the read value
    readValues chan