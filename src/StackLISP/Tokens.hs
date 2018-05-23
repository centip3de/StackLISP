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

    {-
    data IOF a = Print String a
        | Input (String -> a) 
        deriving (Functor)

    data StackF a = Pop a
        | Dup a
        | Reverse a
        | Swap a
        | Sort a
        | Execute a
        deriving (Functor, Show)
    -}

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
        deriving (Functor, Show)
        -- | Input (String -> a)

    data BlockF a = BlockOp [(StatementF a)] 
        deriving (Functor, Show)

    data ProgramF a = Program (StatementF a)
        deriving (Functor, Show)

    type MathM a = Free MathF a
    --type IOM a = Free IOF a
    --type StackM a = Free StackF a
    type LoopM a = Free LoopF a
    type StatementM a = Free StatementF a
    type BlockM a = Free BlockF a
    type ProgramM a = Free ProgramF a