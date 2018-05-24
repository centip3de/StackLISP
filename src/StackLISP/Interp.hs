module StackLISP.Interp where
    import Control.Monad.Free
    import Control.Monad.State

    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens
    import StackLISP.Errors

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

    -- Stack commands
    eval stack (Free (Pop next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (_, newStack)) -> do
            put newStack
            eval newStack next
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
    eval stack (Free (Done _)) = lift $ putStrLn "Finished"

    interp :: String -> IO ()
    interp contents = case parseFile contents of 
        (Left error) -> putStrLn $ "Parsing Error: " ++ (show error)
        (Right res) -> do
            output <- execStateT (eval Empty res) Empty
            putStrLn $ show output