module StackLISP.Interp where
    import Control.Monad.Free
    import Control.Monad.State

    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens
    import StackLISP.Errors

    add :: Stack -> Either RuntimeError StackData
    add stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (StringData left, StringData right) -> Right (StringData $ left ++ right)
            (IntData left, IntData right) -> Right (IntData $ left + right)
            (StatementData left, StatementData right) -> Right (StatementData $ left >> right)
            _ -> Left (RuntimeError "Mismatched types: can only perform addition on matching types.")

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

    interp :: String -> IO ()
    interp contents = case parseFile contents of 
        (Left error) -> putStrLn $ "Parsing Error: " ++ (show error)
        (Right res) -> do
            output <- execStateT (eval Empty res) Empty
            putStrLn $ "Stack:" ++ show output