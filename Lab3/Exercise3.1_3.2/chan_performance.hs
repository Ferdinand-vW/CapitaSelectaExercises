{-# LANGUAGE BangPatterns #-}

import Control.Concurrent.Chan
import Control.Concurrent.Async


main = do
    chan <- newChan
    race (writeValues chan 10000000) (readValues chan)


writeValues :: Chan Int -> Int -> IO ()
writeValues chan 0 = return ()
writeValues chan n = do
    writeChan chan n
    writeValues chan (n - 1)

readValues :: Chan Int -> IO ()
readValues chan = do
    let !n = readChan chan
    readValues chan