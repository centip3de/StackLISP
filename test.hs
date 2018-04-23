import Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "ABC"
