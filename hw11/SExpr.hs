{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- Shamelessly stolen from many and some definitions in Alternative
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = many_v
    where some_v = many_v <|> pure []
          many_v = (fmap (:) p) <*> some_v

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = some_v
    where some_v = many_v <|> pure []
          many_v = (fmap (:) p) <*> some_v

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = oneOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> oneOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
  
parseAtom :: Parser Atom
parseAtom = (I <$> spaces) *> ((I <$> ident) <|> (N <$> posInt))

parseSExpr :: Parser SExpr
parseSExpr = undefined