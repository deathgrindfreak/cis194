module Scrabble where

import Data.Monoid

-- Exercise 3
data Score = Score Int
    deriving (Show)
    
instance Monoid Score where
    mempty = Score 0
    mappend (Score n1) (Score n2) = Score (n1 + n2)
    
score :: Char -> Score
score c | c `elem` "AEIOULNSTRaeioulnstr" = Score 1
        | c `elem` "DGdg"                 = Score 2
        | c `elem` "BCMPbcmp"             = Score 3
        | c `elem` "FHVWYfhvwy"           = Score 4
        | c `elem` "Kk"                   = Score 5
        | c `elem` "JXjx"                 = Score 8
        | c `elem` "QZqz"                 = Score 10
        | otherwise                       = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score