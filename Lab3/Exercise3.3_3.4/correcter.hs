import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Char

main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    tvar <- newTVarIO ""
    forkIO $ render tvar 0 --We start a render thread and a correcter thread, which are
    forkIO $ correcter tvar --given a shared TVar

    forever $ do --The main thread will write characters into the TVar
        c <- getChar
        atomically $ do
            s <- readTVar tvar
            writeTVar tvar $ s ++ [c]

render :: TVar String -> Int -> IO ()
render tvar n = do
    input <- atomically $ readTVar tvar
    loop input n
    where
        loop s i = do --Simply render the input
            renderString s i
            next <- atomically $ do --Wait until the input has changed
                        s' <- readTVar tvar
                        if s == s'
                            then retry
                            else return s'
            loop next (length s)

--removes current output
--and then writes the newoutput
renderString :: String -> Int -> IO ()
renderString s n = do
    putStr $ replicate n '\8'
    putStr $ replicate n ' '
    putStr $ replicate n '\8'
    putStr s

correcter :: TVar String -> IO ()
correcter tvar = do
    input <- atomically $ readTVar tvar
    loop input
    where
        loop s = do
            let correctedText = map toUpper s --My correcter is pretty simply, it just capitalizes input
            atomically $ do
                writeTVar tvar correctedText
            next <- atomically $ do
                s' <- readTVar tvar
                if s == s'
                    then retry
                    else return s'
            loop next
