{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances #-}
module StackLISP.Tokens where

    import Control.Monad.Free

    data StatementF a = Done a
        -- Stack OPs
        | Pop a 
        | Dup a 
        | Reverse a
        | Swap a
        | Sort a
        | Execute a

        -- Control Flow OPs
        | While a
        | For a
        | If a

        -- Math OPs
        | Add a
        | Sub a
        | Mul a
        | Div a
        | Mod a

        -- Comparison OPs
        | Equals a
        | LessThan a
        | GreaterThan a
        | Negate a

        -- Primitives
        | Str String a
        | Boolean Bool a
        | Number Int a
        | Block (StatementM ()) a

         -- IO
        | Print a
        | Input a -- This should probably be "Input (String -> a)", but we need to derive show for the moment
        deriving (Functor)

    type StatementM a = Free StatementF a

    instance (Show a) => Show (StatementF a) where
        show (Equals a) = "Equals"
        show (LessThan a) = "LT"
        show (GreaterThan a) = "GT"
        show (Negate a) = "Negate"
        show (Add a) = "Add"
        show (Sub a) = "Sub"
        show (Mul a) = "Mul"
        show (Div a) = "Div"
        show (Mod a) = "Mod"
        show (While a) = "While"
        show (For a) = "For"
        show (Dup a) = "Dup"
        show (Reverse a) = "Reverse"
        show (Swap a) = "Swap"
        show (Sort a) = "Sort"
        show (Execute a) = "Execute"
        show (Str string a) = "String: " ++ string
        show (Boolean bool a) = "Boolean: " ++ (show bool)
        show (Number int a) = "Number: " ++ (show int)
        show (Print a) = "Print"
        show (Input a) = "Input"
        show (Done a) = "Done"
        show (Block _ a) = "Block"
        show x = "???"