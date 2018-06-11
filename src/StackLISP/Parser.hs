module StackLISP.Parser where
    {-
    Working through the EBNF:
    block = [, statement* , ]
    statement = 
        stack_ops | math_ops | primitive | loop_ops | block
    
    loop_ops = for_loop | while_loop
    for_loop = f
    while_loop = w
    
    stack_ops = pop | dup | reverse | swap | sort | execute
    pop = p
    dup = d
    reverse = r
    swap = s
    sort = t
    execute = e

    io_ops = print | input
    print = .
    input = ,

    math_ops = add | sub | mul | div | mod
    add = +
    sub = -
    mul = *
    div = /
    mod = %

    primitive = boolean | string | number
    boolean = true | false
    false = F
    true = T
    string = ", char*, "
    number = digit+
    digit = '0..9'
    char = anyChar
    -}

    import StackLISP.Tokens
    import Control.Monad
    import Control.Monad.Free
    import Text.ParserCombinators.Parsec hiding (spaces)

    parseComparison :: Parser (StatementM ())
    parseComparison = do
        x <- handleWhitespace $ oneOf ("=<>")
        return $ liftF $ case x of
            '=' -> Equals ()
            '<' -> LessThan ()
            '>' -> GreaterThan ()

    parseBoolean :: Parser (StatementM ())
    parseBoolean = do
        x <- handleWhitespace $ oneOf "TF"
        return $ liftF $ case x of
            'T' -> Boolean True ()
            'F' -> Boolean False ()

    parseMath :: Parser (StatementM ())
    parseMath = do
        x <- handleWhitespace $ oneOf "+-*/%"
        return $ liftF $ case x of
            '+' -> Add ()
            '-' -> Sub ()
            '*' -> Mul ()
            '/' -> Div ()
            '%' -> Mod ()

    parseIO :: Parser (StatementM ())
    parseIO = do
        x <- handleWhitespace $ oneOf ".,"
        return $ liftF $ case x of 
            '.' -> Print ()
            ',' -> Input ()

    parseStack :: Parser (StatementM ())
    parseStack = do
        x <- handleWhitespace $ oneOf "pdrste"
        return $ liftF $ case x of
            'p' -> Pop ()
            'd' -> Dup ()
            'r' -> Reverse ()
            's' -> Swap ()
            't' -> Sort ()
            'e' -> Execute ()

    parseLogicalOps :: Parser (StatementM ())
    parseLogicalOps = do
        x <- handleWhitespace $ oneOf "AO!"
        return $ liftF $ case x of
            'A' -> And ()
            'O' -> Or ()
            '!' -> Negate ()

    parseWhitespace :: Parser ()
    parseWhitespace = skipMany space

    parseString :: Parser (StatementM ())
    parseString = do
        char '"'
        x <- many (noneOf "\"")
        handleWhitespace $ char '"'
        return $ liftF $ Str x ()

    parseNumber :: Parser (StatementM ())
    parseNumber = do
        x <- handleWhitespace $ many1 digit
        return $ liftF $ Number (read(x)::Int) ()

    parsePrimitive :: Parser (StatementM ())
    parsePrimitive = parseNumber <|> parseBoolean <|> parseString

    parseLoop :: Parser (StatementM ())
    parseLoop = do
        x <- handleWhitespace $ oneOf "fw"
        return $ liftF $ case x of 
            'f' -> For ()
            'w' -> While ()

    parseIf :: Parser (StatementM ())
    parseIf = do
        handleWhitespace $ char 'i'
        return $ liftF $ If ()

    parseSingleStatement :: Parser (StatementM ())
    parseSingleStatement = parseStack 
        <|> parsePrimitive 
        <|> parseIO 
        <|> parseMath
        <|> parseLoop
        <|> parseBlock
        <|> parseIf
        <|> parseLogicalOps
        <|> parseComparison

    parseMultipleStatements :: Parser (StatementM ())
    parseMultipleStatements = do
        statements <- many1 parseSingleStatement
        return $ foldl (\acc x -> x >> acc) (liftF (Done ())) (reverse statements)
        
    handleWhitespace :: Parser a -> Parser a
    handleWhitespace p = p <* parseWhitespace

    parseBlock :: Parser (StatementM ())
    parseBlock = do
        handleWhitespace $ char '['
        statements <- many1 parseSingleStatement
        char ']'
        return $ liftF $ Block (foldl (\acc x -> x >> acc) (liftF (Done ())) (reverse statements)) ()

    parseProgram :: Parser (StatementM ())
    parseProgram = parseMultipleStatements <* eof

    parseFile :: String -> Either ParseError (StatementM ())
    parseFile contents = parse parseProgram "StackLISP" contents