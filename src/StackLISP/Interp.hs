module StackLISP.Interp where
    import Control.Monad.Free

    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens
    import StackLISP.Errors

    step :: Stack -> StatementM a -> IO a
    step stack (Free (Boolean bool next)) = step (push stack (BooleanData bool)) next
    step stack (Free (Str string next)) = step (push stack (StringData string)) next 
    step stack (Free (Print next)) = case pop stack of
        (Left error) -> (putStrLn $ "Runtime error:" ++ show error) >> return ()
        (Right (item, newStack)) -> (putStrLn $ show item) >> return ()
    step stack (Free _) = return ()

    eval :: StatementM a -> IO a
    eval program = step Empty program

    interp :: String -> IO ()
    interp contents = case parseFile contents of 
        (Left error) -> putStrLn $ "Parsing Error: " ++ (show error)
        (Right res) -> eval res