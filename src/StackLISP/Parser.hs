module StackLISP.Parser where
    {-
    Working through the EBNF:
    block = [, statement* , ]
    statement = 
        stack_ops | math_ops | primitive | loop_ops | block
    
    loop_ops = for_loop | while_loop
    for_loop = f
    while_loop = w
    
    stack_ops = pop | dup | reverse | swap | sort
    pop = p
    dup = d
    reverse = r
    swap = s
    sort = t

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
    import Text.ParserCombinators.Parsec hiding (spaces)

    parseBoolean :: Parser PrimitiveToken
    parseBoolean = do
        x <- handleWhitespace $ oneOf "TF"
        return $ case x of
            'T' -> (BooleanToken True)
            'F' -> (BooleanToken False)

    parseMath :: Parser MathOps
    parseMath = do
        x <- handleWhitespace $ oneOf "+-*/%"
        return $ case x of
            '+' -> Add
            '-' -> Sub
            '*' -> Mul
            '/' -> Div
            '%' -> Mod

    parseIO :: Parser IOOps
    parseIO = do
        x <- handleWhitespace $ oneOf ".,"
        return $ case x of 
            '.' -> Print
            ',' -> Input

    parseStack :: Parser StackOps
    parseStack = do
        x <- handleWhitespace $ oneOf "pdrst"
        return $ case x of
            'p' -> Pop
            'd' -> Dup
            'r' -> Reverse
            's' -> Swap
            't' -> Sort

    parseWhitespace :: Parser ()
    parseWhitespace = skipMany space

    parseString :: Parser PrimitiveToken
    parseString = do
        handleWhitespace $ char '"'
        x <- many (noneOf "\"")
        char '"'
        return $ StringToken x

    parseNumber :: Parser PrimitiveToken
    parseNumber = liftM (NumberToken . read) $ handleWhitespace $ many1 digit

    parsePrimitive :: Parser PrimitiveToken
    parsePrimitive = parseNumber <|> parseBoolean <|> parseString

    parseLoop :: Parser LoopOps
    parseLoop = do
        x <- handleWhitespace $ oneOf "fw"
        return $ case x of 
            'f' -> For
            'w' -> While

    parseStatement :: Parser Statement
    parseStatement = (MathSt <$> parseMath) 
        <|> (PrimSt <$> parsePrimitive)
        <|> (IOSt <$> parseIO)
        <|> (StackSt <$> parseStack)
        <|> (LoopSt <$> parseLoop)
        <|> (BlockSt <$> parseBlock)
    
    handleWhitespace :: Parser a -> Parser a
    handleWhitespace p = p <* parseWhitespace

    parseBlock :: Parser BlockOp
    parseBlock = do
        handleWhitespace $ char '['
        x <- many1 parseStatement
        char ']'
        return $ BlockOp $ x ++ [EOB]

    parseProgram :: Parser Program
    parseProgram = Program <$> (many1 parseStatement <* eof)

    parseFile :: String -> Either ParseError Program
    parseFile contents = parse parseProgram "StackLISP" contents