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

    parseBoolean :: Parser Primitive
    parseBoolean = do
        x <- handleWhitespace $ oneOf "TF"
        return $ case x of
            'T' -> (Boolean True)
            'F' -> (Boolean False)

    parseMath :: Parser (MathF ())
    parseMath = do
        x <- handleWhitespace $ oneOf "+-*/%"
        return $ case x of
            '+' -> Add ()
            '-' -> Sub ()
            '*' -> Mul ()
            '/' -> Div ()
            '%' -> Mod ()

    parseIO :: Parser (IOF ())
    parseIO = do
        x <- handleWhitespace $ oneOf ".,"
        return $ case x of 
            '.' -> Print "" ()
            ',' -> Input (\x -> ())

    parseStack :: Parser (StackF ())
    parseStack = do
        x <- handleWhitespace $ oneOf "pdrste"
        return $ case x of
            'p' -> Pop ()
            'd' -> Dup ()
            'r' -> Reverse ()
            's' -> Swap ()
            't' -> Sort ()
            'e' -> Execute ()

    parseWhitespace :: Parser ()
    parseWhitespace = skipMany space

    parseString :: Parser Primitive
    parseString = do
        char '"'
        x <- many (noneOf "\"")
        handleWhitespace $ char '"'
        return $ Str x

    parseNumber :: Parser Primitive
    parseNumber = liftM (Number . read) $ handleWhitespace $ many1 digit

    parsePrimitive :: Parser Primitive
    parsePrimitive = parseNumber <|> parseBoolean <|> parseString

    parseLoop :: Parser (LoopF ())
    parseLoop = do
        x <- handleWhitespace $ oneOf "fw"
        return $ case x of 
            'f' -> For ()
            'w' -> While ()

{-
    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    ($)   ::                    (a -> b) ->   a ->   b

    (<$>) :: Functor f => (a -> b) -> f a -> f b

    f <$> x <*> y <*> ... <*> z

    f :: a -> b -> c -> d
    x :: a
    y :: b
    z :: c

    f <$> x :: f (b -> c -> d)
    (f <$> x) <$> y :: f (c -> d)
    ((f <$> x) <*> y) <*> z :: f d
-}

    parseStatement :: Parser (StatementF ())
    parseStatement = (MathSt <$> parseMath <*> pure ()) 
        <|> (PrimSt <$> parsePrimitive <*> pure ())
        <|> (IOSt <$> parseIO <*> pure ())
        <|> (StackSt <$> parseStack <*> pure ())
        <|> (LoopSt <$> parseLoop <*> pure ())
        <|> (BlockSt <$> parseBlock <*> pure ())
    
    handleWhitespace :: Parser a -> Parser a
    handleWhitespace p = p <* parseWhitespace

    parseBlock :: Parser (BlockF ())
    parseBlock = do
        handleWhitespace $ char '['
        x <- many1 parseStatement
        char ']'
        return $ BlockOp $ x 

    parseProgram :: Parser (ProgramF ())
    parseProgram = Program <$> (many1 parseStatement <* eof)

    parseFile :: String -> Either ParseError (ProgramF ())
    parseFile contents = parse parseProgram "StackLISP" contents