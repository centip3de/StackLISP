module StackLISP.Interp where
    import Control.Monad.Free

    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens
    import StackLISP.Errors

    data Interp = Interp {
        stack :: Stack,
        program :: ProgramM ()
    }

    interp :: String -> Either String Interp
    interp contents = case parseFile contents of 
        (Left error) -> Left $ "Parsing Error: " ++ (show error)
        (Right res) -> Right $ Interp {stack=Empty, program=liftF res}