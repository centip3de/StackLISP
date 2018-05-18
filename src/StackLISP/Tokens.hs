{-# LANGUAGE DeriveFunctor #-}
module StackLISP.Tokens where

    import Control.Monad.Free

    data PrimitiveToken = StringToken String
        | BooleanToken Bool
        | NumberToken Int 
        deriving (Show, Eq)

    data MathOps = Add 
        | Sub 
        | Mul 
        | Div 
        | Mod 
        deriving (Show, Eq)

    data IOOps = Print
        | Input
        deriving (Show, Eq)

    data StringIO a = PrintStr String a
        | InputStr (String -> a) 
        deriving (Functor)

    type IOM a = Free StringIO a

    data StackOps = Pop 
        | Dup 
        | Reverse 
        | Swap 
        | Sort 
        | Execute 
        deriving (Show, Eq)

    data LoopOps = While 
        | For 
        deriving(Show, Eq)

    data Statement = StackSt StackOps 
        | MathSt MathOps 
        | PrimSt PrimitiveToken 
        | IOSt IOOps
        | BlockSt BlockOp
        | LoopSt LoopOps 
        | NOP 
        | EOB 
        deriving (Show, Eq)

    data BlockOp = BlockOp [Statement] deriving (Show, Eq)

    data Program = Program [Statement] deriving (Show, Eq)