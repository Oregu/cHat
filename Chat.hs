module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Network
import System.IO
import Monad

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
	onClicked sendButton $ sendClicked messageEntry messagesView
	window   <- xmlGetWidget xml castToWindow "chatWindow"
	onDestroy window mainQuit
	return window

sendClicked :: Entry -> TextView -> IO ()
sendClicked entry tv = do
	text <- get entry entryText
	unless (null text) $ do
		h <- connectTo "localhost" $ PortNumber 14003
		hSetBuffering h NoBuffering
		hPutStr h $ text ++ "\n"
		msgs <- hGetContents h
		tb <- textViewGetBuffer tv
		endIter <- textBufferGetEndIter tb
		textBufferInsert tb endIter msgs

