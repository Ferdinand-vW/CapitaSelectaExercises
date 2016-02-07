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
import Data.Either

main = do
  [text] <- fmap (fmap (B.unpack . UTF8.fromString)) getArgs

--Also do the downloading and detection concurrently using async
  languages <- async Bing.getLanguages

  fromLang <- async $ Bing.detectLanguage text

  l' <- wait languages
  fl' <- wait fromLang --Wait for the results, before we can continue
  printf "\"%s\" appears to be in language \"%s\"\n" text fl'

  translations <- forM (filter (/= fl') l') $ \toLang -> async $ do
     str <- Bing.translateText text fl' toLang
     printf "%s: %s\n" toLang str
     
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

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async t var) = readMVar var

wait :: Async a -> IO a
wait a = do
   r <- waitCatch a
   case r of
	Left e -> throwIO e
	Right a -> return a


cancel :: Async a -> IO ()
cancel (Async t var) = throwTo t ThreadKilled
