{-# LANGUAGE PatternGuards #-}
import Control.Concurrent
import Control.Exception
import Control.Monad
import Text.Printf
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as B
import System.Environment
import Prelude hiding (catch)

import BingTranslate as Bing

main = do
  [text] <- fmap (fmap (B.unpack . UTF8.fromString)) getArgs

  languages <- Bing.getLanguages

  fromLang <- Bing.detectLanguage text
  printf "\"%s\" appears to be in language \"%s\"\n" text fromLang

--Use async to do the translations concurrently
  translations <- forM (filter (/= fromLang) languages) $ \toLang -> async $ do
     str <- Bing.translateText text fromLang toLang
     printf "%s: %s\n" toLang str
--Wait for all the results to be finished
  results <- mapM wait translations
  print "end"

-----------------------------------------------------------------------------
-- Our Async API:

data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
   var <- newEmptyMVar
   t <- forkIO ((do r <- action; putMVar var (Right r))
                  `catch` \e -> putMVar var (Left e))
   return (Async t var)

wait :: Async a -> IO (Either SomeException a)
wait (Async t var) = readMVar var

cancel :: Async a -> IO ()
cancel (Async t var) = throwTo t ThreadKilled
