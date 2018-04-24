module StackLISP.Interp where
    import StackLISP.Stack
    import StackLISP.Parser

    data Interp = Interp {
        stack :: Stack,
        ip :: Int
    }

    parse :: String -> String
    parse contents = case parseFile contents of 
        (Left error) -> "Invalid parse. Error: " ++ (show error)
        (Right res) -> show res