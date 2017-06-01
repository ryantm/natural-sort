module NaturalSort where

import Control.Applicative
import Data.Functor.Identity
import Data.Text
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parsec (runParserT, ParsecT)

par :: (ParsecT Text () Identity a) -> Text -> a
par p t =
  let f = runIdentity (runParserT p () "" t) in
    case f of
      Left e -> error (show e)
      Right b -> b

ncompare :: Text -> Text -> Ordering
ncompare l r = par parts l `compare` par parts r

number :: CharParsing m
       => m Integer
number = read <$> some digit

nonNumber :: CharParsing m
          => m Text
nonNumber = pack <$> some (noneOf "0123456789")

data TextOrNum = T Text
               | N Integer
               deriving (Show, Eq, Ord)

parts :: CharParsing m
      => m [TextOrNum]
parts = many ((T <$> nonNumber) <|> (N <$> number)) <* eof
