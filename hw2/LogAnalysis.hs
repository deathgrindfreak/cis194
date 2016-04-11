{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

timeStamp :: [String] -> Int
timeStamp m = case head m of
    "E" -> read (m !! 2) :: Int
    "W" -> read (m !! 1) :: Int
    "I" -> read (m !! 1) :: Int
    _   -> 0

message :: [String] -> String
message m = unwords $ case head m of
    "E" -> drop 3 m
    "W" -> drop 2 m
    "I" -> drop 2 m
    _   -> [""]

errorCode :: [String] -> Int
errorCode m = read (m !! 1) :: Int

parseMessage :: String -> LogMessage
parseMessage msg = let m = words msg in 
    case head m of
        "E" -> LogMessage (Error (errorCode m)) (timeStamp m) (message m)
        "W" -> LogMessage Warning (timeStamp m) (message m)
        "I" -> LogMessage Info (timeStamp m) (message m)
        _   -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- MessageTree functions

getTimeStamp :: LogMessage -> Int
getTimeStamp (Unknown _) = error "No timestamp for Unknown message"
getTimeStamp (LogMessage _ ts _) = ts

getMessage :: LogMessage -> String
getMessage (Unknown msg) = msg
getMessage (LogMessage _ _ msg) = msg

getErrorCode :: LogMessage -> Int
getErrorCode (LogMessage (Error code) _ _) = code
getErrorCode _ = error "Bad message type"

isError :: LogMessage -> Bool
isError (Unknown _) = False
isError (LogMessage msgType _ _) = case msgType of
    (Error _) -> True
    _         -> False

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert m@(LogMessage _ ts _) (Node left logMsg right)
    | ts < getTimeStamp logMsg = Node (insert m left) logMsg right
    | ts > getTimeStamp logMsg = Node left logMsg (insert m right)
    | otherwise = error "Can't handle equal time stamps"

build :: [LogMessage] -> MessageTree
build msgs = buildTree msgs Leaf where
    buildTree xs tree = foldl (flip insert) tree xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = errorsWithCode 50

errorsWithCode :: Int -> [LogMessage] -> [String]
errorsWithCode code = map getMessage . filter (\x -> isError x && getErrorCode x >= code)

main :: IO ()
main = print $ parse "E 2 254 Fuck ...\nW 154 Just a warning\nI 15 some mo Info"
