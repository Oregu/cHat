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
	acceptLoop servSock [] `finally` sClose servSock

data Client = Client Handle

acceptLoop :: Socket -> [Client] -> IO ()
acceptLoop servSock clients = do
	(handle, server, port) <- accept servSock
	putStrLn $ "(" ++ show server ++ ":" ++ show port ++ ") new client"
	let client = Client handle
	forkIO $ processClient client
	putStrLn $ show (1 + length clients) ++ " total registered clients"
	acceptLoop servSock $ client : clients

processClient :: Client -> IO ()
processClient c@(Client handle) = do
	text <- hGetLine handle
	hPutStrLn handle $ "Client, You said: " ++ text
	hFlush handle
	processClient c

