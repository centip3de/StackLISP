module StackLISP.Tokens where

    data StringToken = StringToken String  deriving (Show, Eq)
    data NumberToken = NumberToken Integer deriving (Show, Eq)
    data TrueToken = T deriving (Show, Eq)
    data FalseToken = F deriving (Show, Eq)
    data CharToken = CharToken Char deriving (Show, Eq)
    data DigitToken = DigitToken Integer deriving (Show, Eq)

    data BooleanToken = TrueToken 
        | FalseToken deriving (Show, Eq)

    data PrimitiveToken = PrimStrTok StringToken 
        | PrimBoolTok BooleanToken 
        | PrimNumTok NumberToken deriving (Show, Eq)

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

    data Statement = StackSt StackOps 
        | MathSt MathOps 
        | PrimSt PrimitiveToken 
        | BlockSt [Statement] deriving (Show, Eq)

    data Program = Program [Statement]