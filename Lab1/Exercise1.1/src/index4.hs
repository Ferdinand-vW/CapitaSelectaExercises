
import Control.Monad.Par
import Control.Parallel.Strategies
import qualified Data.ByteString.Char8 as L
import qualified Data.ByteString as LL
import qualified Data.ByteString.Char8 as B
import Control.DeepSeq
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.IntSet as Set
import Data.IntSet (IntSet)

import System.Environment
import System.Exit
import System.IO
import Data.Array
import Data.Char
import Control.Monad
import Prelude hiding (Word)

-- A document index and search program.  Use it like this:
--
-- $ ./index docs/*
-- search: <enter search term here>
-- docs/file1
-- docs/file2
-- docs/file3
--
--

type Word = B.ByteString

-- Documents are numbered by the order they appear on the command line
type DocSet = IntSet

-- A DocIndex maps a word to the set of documents that contain the word
type DocIndex = Map Word DocSet

joinIndices :: [DocIndex] -> DocIndex
joinIndices = parFoldChunk (rpar . force) 50 (Map.unionWith Set.union) Map.empty

mkIndex :: Int -> L.ByteString -> DocIndex
mkIndex i s
  = Map.fromListWith Set.union [ (w, Set.singleton i)
                               | w <- ws ]
  where ws = L.splitWith (not . isAlphaNum) s

search :: DocIndex -> [Word] -> DocSet
search index words = foldr1 Set.intersection (map lookup words)
  where lookup w = Map.findWithDefault Set.empty w index

-- -----------------------------------------------------------------------------

main = do
  hSetBuffering stdout NoBuffering
  fs <- getArgs

  -- Step 1: build the index
  ss <- mapM L.readFile fs
  let
      -- indices is a separate index for each (numbered) document
      indices :: [DocIndex]
      indices = runPar $ parZipWith mkIndex [0..] ss

      -- union the indices together
      index = joinIndices indices

      -- array mapping doc number back to filename
      arr = listArray (0,length fs - 1) fs

  -- Step 2: perform search
  forever $ do
    putStr "search (^D to end): "
    eof <- isEOF
    when eof $ exitWith ExitSuccess
    s <- B.getLine
    putStr "wait... "

    let result :: DocSet  -- set of docs containing the words in the term
        result = search index (B.words s)

        -- map the result back to filenames
        files = map (arr !) (Set.toList result)

    putStrLn ("\n" ++ unlines files)

parZipWith :: NFData c => (a -> b -> c) -> [a] -> [b] -> Par [c]
parZipWith _ _ [] = return []
parZipWith _ [] _ = return []
parZipWith f (x:xs) (y:ys) = do
    nv <- new
    fork $ put nv $ f x y
    cs <- parZipWith f xs ys
    c' <- get nv
    return (c':cs)

parFoldChunk :: Strategy a -> Int -> (a -> a -> a) -> a -> [a] -> a
parFoldChunk strat num f ntr xs = foldr f ntr $ parFoldListChunk strat num f ntr xs
      
parFoldListChunk :: Strategy a -> Int -> (a -> a -> a) -> a -> [a] -> [a]
parFoldListChunk _ _ _ _ [] = []
parFoldListChunk strat' n g s xs =
        let (chunk,rest) = splitAt n xs
        in
          parFold strat' g s chunk : parFoldListChunk strat' n g s rest

parFold :: Strategy a -> (a -> a -> a) -> a -> [a] -> a
parFold strat f s xs = strat `withStrategy` foldr f s xs