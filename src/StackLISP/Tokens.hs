{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances #-}
module StackLISP.Tokens where

    import Control.Monad.Free

    -- To port:
    data LoopF a = While a
        | For a
        deriving (Functor, Show)

    data StatementF a = Pop a -- Stack OPs
        | Dup a 
        | Reverse a
        | Swap a
        | Sort a
        | Execute a

         -- Math OPs
        | Add a
        | Sub a
        | Mul a
        | Div a
        | Mod a

        -- Primitives
        | Str String a
        | Boolean Bool a
        | Number Int a
        | Block (StatementM ()) a

         -- IO
        | Print a
        | Input a -- This should probably be "Input (String -> a)", but we need to derive show for the moment
        | Done a
        deriving (Functor)

    data ProgramF a = Program (StatementF a)
        deriving (Functor, Show)

    type LoopM a = Free LoopF a
    type StatementM a = Free StatementF a
    type ProgramM a = Free ProgramF a

    instance (Show a) => Show (StatementF a) where
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