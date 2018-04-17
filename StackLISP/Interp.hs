module StackLISP.Interp where
    import StackLISP.Stack

    data Interp = Interp {
        stack :: Stack,
        ip :: Int
    }

    parse :: String -> AST
    parse 