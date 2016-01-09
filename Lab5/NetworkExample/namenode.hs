import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Node
import Network.Transport.TCP
import Network.Transport hiding (send)
import Data.ByteString.Char8 (pack)
import System.Environment
import Data.Maybe
import Message

main = do
    Right t <- createTransport "127.0.0.1" "44444" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node namenode

namenode :: Process ()
namenode = do
    self <- getSelfPid
    register "namenodePID" self --Register it's own processID under namenodePID
                                --so that other nodes can ask for the processID
    (SP s) <- expect :: Process M --Wait for the sendport of the client
    say "Received sendport"
    sendChan s (M "hello client") --send a message to the client
    _ <- expect :: Process M
    return ()
