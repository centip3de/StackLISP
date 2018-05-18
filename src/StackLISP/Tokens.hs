{-# LANGUAGE DeriveFunctor #-}
module StackLISP.Tokens where

    import Control.Monad.Free

    data Primitive = Str String
        | Boolean Bool
        | Number Int
        deriving (Eq, Show)

    data MathF a = Add a
        | Sub a
        | Mul a
        | Div a
        | Mod a
        deriving (Functor)

    data IOF a = Print String a
        | Input (String -> a) 
        deriving (Functor)

    data StackF a = Pop a
        | Dup a
        | Reverse a
        | Swap a
        | Sort a
        | Execute a
        deriving (Functor)

    data LoopF a = While a
        | For a
        deriving (Functor)

    data StatementF a = StackSt (StackF a) a
        | MathSt (MathF a) a
        | PrimSt Primitive a
        | IOSt (IOF a) a
        | BlockSt (BlockF a) a
        | LoopSt (LoopF a) a
        deriving (Functor)

    data BlockF a = BlockOp [(StatementF a)] 
        deriving (Functor)

    data ProgramF a = Program [(StatementF a)] 
        deriving (Functor)

    type MathM a = Free MathF a
    type IOM a = Free IOF a
    type StackM a = Free StackF a
    type LoopM a = Free LoopF a
    type StatementM a = Free StatementF a
    type BlockM a = Free BlockF a
    type ProgramM a = Free ProgramF a