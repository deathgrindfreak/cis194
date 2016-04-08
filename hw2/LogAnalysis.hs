{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg =
     case (head msg) of
        'E' -> LogMessage (Error (errorCode msg)) (ts msg) (message msg)
        'W' -> LogMessage Warning (ts msg) (message msg)
        'I' -> LogMessage Info (ts msg) (message msg)
	_   -> Unknown msg
     where ts msg = 0
     	   message msg = ""
	   errorCode msg = 0

parse :: String -> [LogMessage]
parse = map parseMessage . lines

main :: IO ()
main = do print $ parse "E 254 Fuck ...\nW 154 Just a warning\nI 15 some mo Info"
