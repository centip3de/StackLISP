module StackLISP.Interp where
    import Control.Monad.Free
    import Control.Monad.State

    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens
    import StackLISP.Errors

    eval :: Stack -> StatementM a -> StateT Stack IO ()
    eval stack (Free (Boolean bool next)) = do
        modify (push (BooleanData bool))
        newStack <- get
        eval newStack next
    eval stack (Free (Str string next)) = do
        modify (push (StringData string))
        newStack <- get
        eval newStack next 
    eval stack (Free (Number int next)) = do
        modify (push (IntData int))
        newStack <- get
        eval newStack next
    eval stack (Free (Pop next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (_, newStack)) -> do
            put newStack
            eval newStack next
    eval stack (Free (Print next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (item, newStack)) -> do
            lift $ putStrLn $ show item
            put newStack
            eval newStack next
    eval stack (Free (Input next)) = do
        input <- lift $ getLine
        modify (push (StringData input))
        newStack <- get
        eval newStack next
    eval stack (Free (Done _)) = lift $ (putStrLn "Finished") 

    interp :: String -> IO ()
    interp contents = case parseFile contents of 
        (Left error) -> putStrLn $ "Parsing Error: " ++ (show error)
        (Right res) -> do
            output <- execStateT (eval Empty res) Empty
            putStrLn $ show output