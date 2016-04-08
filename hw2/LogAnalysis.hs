{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

data MessageType = Info
                 | Warning 
                 | Error Int
    deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage msg = LogMessage (msgType msg) (ts msg) (message msg) where
    msgType msg = case (head msg) of
        'E' -> Error (errorCode msg)
        'W' -> Warning
        'I' -> Info
        
        

parse :: String -> [LogMessage]
parse = unknown

main = do print "Blah"
