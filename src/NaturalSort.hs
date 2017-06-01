module NaturalSort where

import Control.Applicative
import Data.Functor.Identity
import Data.Text
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parsec (runParserT, ParsecT)

ncompare :: Text -> Text -> Ordering
ncompare l r = parse l `compare` parse r

data TextOrNum = T Text
               | N Integer
               deriving (Eq, Show, Ord)

-- instance Ord TextOrNum where
--   compare (N _) (T _) = LT
--   compare (T _) (N _) = GT
--   compare (T l) (T r) = l `compare` r
--   compare (N l) (N r) = l `compare` r

parse :: Text -> [TextOrNum]
parse = par parts

par :: (ParsecT Text () Identity a) -> Text -> a
par p t =
  let f = runIdentity (runParserT p () "" t) in
    case f of
      Left e -> error (show e)
      Right b -> b


parts :: CharParsing m
      => m [TextOrNum]
parts = many part <* eof

part :: CharParsing m
      => m TextOrNum
part = (N <$> number) <|> (T . pack <$> (some (noneOf "0123456789")))

number :: CharParsing m
       => m Integer
number =
  read <$> some digit
