{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module StackLISP.Tokens where

    import Control.Monad.Free

    {-
    data Primitive = Str String
        | Boolean Bool
        | Number Int
        deriving (Eq, Show)
    -}

    data MathF a = Add a
        | Sub a
        | Mul a
        | Div a
        | Mod a
        deriving (Functor, Show)

    data LoopF a = While a
        | For a
        deriving (Functor, Show)

    data StatementF a = Pop a
        | Dup a 
        | Reverse a
        | Swap a
        | Sort a
        | Execute a
        | Str String a
        | Boolean Bool a
        | Number Int a
        | Print a
        | Input a -- This should probably be "Input (String -> a)", but we need to derive show for the moment
        | Done a
        | Block (StatementF a) a
        deriving (Functor, Show)

    data ProgramF a = Program (StatementF a)
        deriving (Functor, Show)

    type MathM a = Free MathF a
    type LoopM a = Free LoopF a
    type StatementM a = Free StatementF a
    type ProgramM a = Free ProgramF a