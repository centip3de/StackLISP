module StackLISP.Interp where
    import Control.Monad.Free
    import Control.Monad.State

    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens
    import StackLISP.Errors

    repeatStatement :: StatementM () -> Int -> StatementM ()
    repeatStatement statement num = do
        statement
        statement
        when (num /= 0) (repeatStatement statement (num-1))

    add :: Stack -> Either RuntimeError StackData
    add stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (StringData left, StringData right) -> Right (StringData $ left ++ right)
            (IntData left, IntData right) -> Right (IntData $ left + right)
            (StatementData left, StatementData right) -> Right (StatementData $ left >> right)
            _ -> Left (RuntimeError "Mismatched types: can only perform addition on matching types.")

    sub :: Stack -> Either RuntimeError StackData
    sub stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (StringData left, IntData right) -> Right (StringData $ drop right left)
            (IntData left, IntData right) -> Right (IntData $ left - right)
            --(StatementData left, IntData right) -> Right (StatementData $ drop right left)
            _ -> Left (RuntimeError "Mismatched types: Invalid types for subtraction.")

    mul :: Stack -> Either RuntimeError StackData
    mul stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (StringData left, IntData right) -> Right (StringData $ concat $ replicate right left)
            (IntData left, IntData right) -> Right (IntData $ left * right)
            (StatementData left, IntData right) -> Right (StatementData $ repeatStatement left right)
            _ -> Left (RuntimeError "Mismatched types: Invalid types for multiplication.")

    division :: Stack -> Either RuntimeError StackData
    division stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (IntData left, IntData right) -> Right (IntData $ left `quot` right)
            _ -> Left (RuntimeError "Mismatched types: Invalid types for division.")

    modulo :: Stack -> Either RuntimeError StackData
    modulo stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (IntData left, IntData right) -> Right (IntData $ left `mod` right)
            _ -> Left (RuntimeError "Mismatched types: Invalid types for modulo.")

    eval ::  Stack -> StatementM () -> StateT Stack IO ()
    -- Primitives
    eval stack (Free (Boolean bool next)) = do
        put (push (BooleanData bool) stack)
        newStack <- get
        eval newStack next
    eval stack (Free (Str string next)) = do
        put (push (StringData string) stack)
        newStack <- get
        eval newStack next 
    eval stack (Free (Number int next)) = do
        put (push (IntData int) stack)
        newStack <- get
        eval newStack next
    eval stack (Free (Block statements next)) = do
        put (push (StatementData statements) stack)
        newStack <- get
        eval newStack next

    -- Math commands
    eval stack (Free (Add next)) = case add stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right res) -> do
            put (push res stack)
            newStack <- get
            eval newStack next
    eval stack (Free (Sub next)) = case sub stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right res) -> do
            put (push res stack)
            newStack <- get
            eval newStack next
    eval stack (Free (Div next)) = case division stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right res) -> do
            put (push res stack)
            newStack <- get
            eval newStack next
    eval stack (Free (Mod next)) = case modulo stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right res) -> do
            put (push res stack)
            newStack <- get
            eval newStack next

    -- Stack commands
    eval stack (Free (Pop next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (_, newStack)) -> do
            put newStack
            eval newStack next
    eval stack (Free (Dup next)) = case stack of
        Empty -> eval stack next
        (Some xs) -> do
            put (Some $ concat $ replicate 2 xs)
            newStack <- get
            eval newStack next
    eval stack (Free (Reverse next)) = case stack of
        Empty -> eval stack next
        (Some xs) -> do
            put (Some $ reverse xs)
            newStack <- get
            eval newStack next
    eval stack (Free (Swap next)) = case stack of
        Empty -> eval stack next
        (Some (first:second:rest)) -> do
            put (Some $ (second:first:rest))
            newStack <- get
            eval newStack next
    eval stack (Free (Sort next)) = do
        lift $ putStrLn "Sort isn't currently implemented. Skipping."
        eval stack next
    eval stack (Free (Execute next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right ((StatementData statements), newStack)) -> do
            put newStack
            eval newStack statements
            newStack' <- get
            eval newStack' next
        (Right (_, newStack)) -> lift $ (putStrLn "Cannot execute item.")

    -- IO
    eval stack (Free (Print next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (item, newStack)) -> do
            lift $ putStrLn $ show item
            put newStack
            eval newStack next
    eval stack (Free (Input next)) = do
        input <- lift $ getLine
        put (push (StringData input) stack)
        newStack <- get
        eval newStack next

    -- Control flow
    eval stack (Free (Done _)) = return ()

    -- Fall through
    eval stack (x) = lift $ (putStrLn "Unimplemented OP")

    interp :: String -> IO ()
    interp contents = case parseFile contents of 
        (Left error) -> putStrLn $ "Parsing Error: " ++ (show error)
        (Right res) -> do
            output <- execStateT (eval Empty res) Empty
            putStrLn $ "Stack:" ++ show output