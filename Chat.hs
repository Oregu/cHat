module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network
import System.IO
import Monad

port = PortNumber 14003
host = "localhost"

main = do
	initGUI
	window <- createGUI
	widgetShowAll window
	mainGUI

createGUI :: IO Window
createGUI = do
	Just xml <- xmlNew "cHat.glade"
	sendButton <- xmlGetWidget xml castToButton "sendButton"
	messagesView <- xmlGetWidget xml castToTextView "messagesView"
	messageEntry <- xmlGetWidget xml castToEntry "messageEntry"
	serverConn <- connect
	onClicked sendButton $ sendClicked serverConn messageEntry messagesView
	window   <- xmlGetWidget xml castToWindow "chatWindow"
	onDestroy window $ hClose serverConn >> mainQuit
	return window

sendClicked :: Handle -> Entry -> TextView -> IO ()
sendClicked conn entry tv = do
	text <- get entry entryText
	unless (null text) $ do
		msgs <- sendToServer conn text
		tb <- textViewGetBuffer tv
		endIter <- textBufferGetEndIter tb
		textBufferInsert tb endIter msgs

connect :: IO Handle
connect = connectTo host port

sendToServer :: Handle -> String -> IO String
sendToServer h text = do
	hSetBuffering h LineBuffering
	hPutStrLn h text 
	msgs <- hGetLine h
	return $ msgs ++ "\n"

