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
    forkIO $ render tvar 0
    forkIO $ correcter tvar

    forever $ do
        c <- getChar
        atomically $ do
            s <- readTVar tvar
            writeTVar tvar $ s ++ [c]

render :: TVar String -> Int -> IO ()
render tvar n = do
    input <- atomically $ readTVar tvar
    loop input n
    where
        loop s i = do
            renderString s i
            next <- atomically $ do
                        s' <- readTVar tvar
                        if s == s'
                            then retry
                            else return s'
            loop next (length s)

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
            atomically $ writeTVar tvar $ map toUpper s
            next <- atomically $ do
                s' <- readTVar tvar
                if s == s'
                    then retry
                    else return s'
            loop next
