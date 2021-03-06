import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Char
import BingTranslate.BingTranslate

main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    tvar <- newTVarIO ""
    jvar <- newTVarIO "" --We add a new TVar, which holds the translation of whatever is in tvar
    forkIO $ render jvar 0 --We want to only render the translation
    forkIO $ correcter tvar jvar --corrector will do the translation so it needs both TVars

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

correcter :: TVar String -> TVar String -> IO ()
correcter tvar jvar = do
    input <- atomically $ readTVar tvar
    if null input --We can actually start translating until some input has been given
        then correcter tvar jvar
        else loop input
    where
        loop s = do
            translation <- translateText s "en" "ja" --Translate the input from english to japanese
            atomically $ do
                writeTVar jvar translation
            next <- atomically $ do
                s' <- readTVar tvar
                if s == s'
                    then retry
                    else return s'
            loop next
