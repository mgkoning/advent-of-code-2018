module CommonHs.Parsing where
  
import Text.Parsec (getInput, setInput, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string)
import Control.Applicative (empty)
import Numeric (readDec, readSigned)

eol :: Parser String
eol = string "\r\n" <|> string "\n"

parseInt :: Parser Int
parseInt = do s <- getInput
              case readSigned readDec s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty
