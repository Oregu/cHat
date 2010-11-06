module Main where

import Prelude hiding (catch)

import System.IO
import Network
import Control.Exception (finally, catch)
import Control.Concurrent
import Monad

portn = 14003

main = withSocketsDo $ do
	servSock <- listenOn $ PortNumber portn
	putStrLn $ "cHat server: listening on port " ++ show portn
	chan <- newChan
	forkIO $ acceptLoop servSock chan `finally` sClose servSock
	dispatchProc servSock chan []

data Client = Client Handle
data Command = Connect Client | Msg String

acceptLoop :: Socket -> Chan Command -> IO ()
acceptLoop servSock chan = do
	(handle, server, port) <- accept servSock
	putStrLn $ "(" ++ show server ++ ":" ++ show port ++ ") new client"
	hSetBuffering handle NoBuffering
	let client = Client handle
	forkIO $ processClient client chan
	writeChan chan $ Connect client
	acceptLoop servSock chan

processClient :: Client -> Chan Command -> IO ()
processClient c@(Client handle) chan =
	liftM Msg (hGetLine handle) >>= writeChan chan >> processClient c chan

dispatchProc :: Socket -> Chan Command -> [Client] -> IO ()
dispatchProc sock chan cls = do
	cmd <- readChan chan
	case cmd of
		(Connect cl) -> do
			let cls' = cl:cls
			putStrLn $ "Total " ++ (show $ length cls') ++ " clients"
			mapM_ (send "Client connected!") cls'
			putStrLn "Sent messages to clients"
			dispatchProc sock chan cls'
		(Msg str) -> do
			putStrLn "Got messages from client"
			mapM_ (send str) cls
			putStrLn "Sent to others"
			dispatchProc sock chan cls

send :: String -> Client -> IO ()
send msg (Client h) = hPutStrLn h msg >> hFlush h

