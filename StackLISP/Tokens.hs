module StackLISP.Tokens where

    data PrimitiveToken = StringToken String
        | BooleanToken Bool
        | NumberToken Integer deriving (Show, Eq)

    data MathOps = Add 
        | Sub 
        | Mul 
        | Div 
        | Mod deriving (Show, Eq)

    data IOOps = Print 
        | Input deriving (Show, Eq)

    data StackOps = Pop 
        | Dup 
        | Reverse 
        | Swap 
        | Sort deriving (Show, Eq)

    data LoopOps = While 
        | For deriving(Show, Eq)

    data Statement = StackSt StackOps 
        | MathSt MathOps 
        | PrimSt PrimitiveToken 
        | IOSt IOOps
        | BlockSt BlockOp
        | LoopSt LoopOps deriving (Show, Eq)

    data BlockOp = BlockOp [Statement] deriving (Show, Eq)

    data Program = Program [BlockOp] deriving (Show, Eq)