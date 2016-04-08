{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

timeStamp :: [String] -> Int
timeStamp m = case (head m) of
    'E' -> read (m !! 2) :: Int
    'W' -> read (m !! 1) :: Int
    'I' -> read (m !! 1) :: Int
    _   -> 0

parseMessage :: String -> LogMessage
parseMessage msg = let m = words msg in
     case (head m) of
        'E' -> LogMessage (Error (errorCode m)) (ts m) (message m)
        'W' -> LogMessage Warning (ts m) (message m)
        'I' -> LogMessage Info (ts m) (message m)
	    _   -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

main :: IO ()
main = do print $ parse "E 254 Fuck ...\nW 154 Just a warning\nI 15 some mo Info"
