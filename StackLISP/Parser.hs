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
        x <- oneOf "TF"
        return $ case x of
            'T' -> (BooleanToken True)
            'F' -> (BooleanToken False)

    parseMath :: Parser MathOps
    parseMath = do
        x <- oneOf "+-*/%"
        return $ case x of
            '+' -> Add
            '-' -> Sub
            '*' -> Mul
            '/' -> Div
            '%' -> Mod

    parseIO :: Parser IOOps
    parseIO = do
        x <- oneOf ".,"
        return $ case x of 
            '.' -> Print
            ',' -> Input

    parseStack :: Parser StackOps
    parseStack = do
        x <- oneOf "pdrst"
        return $ case x of
            'p' -> Pop
            'd' -> Dup
            'r' -> Reverse
            's' -> Swap
            't' -> Sort

    spaces :: Parser ()
    spaces = skipMany1 space

    parseString :: Parser PrimitiveToken
    parseString = do
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return $ StringToken x

    parseNumber :: Parser PrimitiveToken
    parseNumber = liftM (NumberToken . read) $ many1 digit

    parsePrimitive :: Parser PrimitiveToken
    parsePrimitive = parseNumber <|> parseBoolean <|> parseString

    parseStatement :: Parser Statement
    parseStatement = (MathSt <$> parseMath) 
        <|> (PrimSt <$> parsePrimitive)
        <|> (IOSt <$> parseIO)
        <|> (StackSt <$> parseStack)