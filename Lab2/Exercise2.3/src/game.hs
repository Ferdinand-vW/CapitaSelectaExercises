import Control.Concurrent
import System.Random
import Control.Monad
import System.IO
import Data.Char


data Event = KeyPress Int | Time


main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    mvar <- newEmptyMVar
    mp <- newMVar 0

    inT <- forkIO $ readKeyPress mvar
    timeT <- forkIO $ timer mvar mp
    gen <- getStdGen
    mpoints <- update mvar [] mp gen
    points <- takeMVar mpoints
    putStrLn $ "\nGame over. You got " ++ show points ++ " points!"


readKeyPress :: MVar Event -> IO ()
readKeyPress mvar = forever $ do
    key <- getChar
    digit <- return $ digitToInt key

    putMVar mvar (KeyPress digit)

timer :: MVar Event -> MVar Int -> IO ()
timer mvar mpoints = forever $ do
    points <- readMVar mpoints
    threadDelay (1000000 - (min 750000 $ points * 10000))
    putMVar mvar Time


update :: MVar Event -> [Int] -> MVar Int -> StdGen -> IO (MVar Int)
update mvar sq mpoints gen = do
    ev <- takeMVar mvar
    let (newsq, g) = updateSequence ev sq gen
    updateScreen (max (length sq) $ length newsq) newsq
    case length newsq >= 10 of
        True -> return mpoints
        False -> do
                    nmpoints <- updatePoints sq newsq mpoints
                    update mvar newsq nmpoints g


updatePoints :: [Int] -> [Int] -> MVar Int -> IO (MVar Int)
updatePoints oldsq newsq mpoints = do
    points <- takeMVar mpoints
    putMVar mpoints $ points + (length oldsq - min (length newsq) (length oldsq))
    return mpoints

updateSequence :: Event -> [Int] -> StdGen -> ([Int],StdGen)
updateSequence ev sq gen =
    case ev of
        KeyPress n -> (filter (/= n) sq,gen)
        Time -> let (n,g) = randomR (0,9) gen
                in (n : sq,g)


updateScreen :: Int -> [Int] -> IO ()
updateScreen n newsq = do
    putStr (replicate n '\8')
    putStr (replicate n ' ')
    putStr (replicate n '\8')
    mapM_ (putStr . show) newsq
