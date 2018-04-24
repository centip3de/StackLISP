module StackLISP.Stack where

    import Data.Maybe
    import StackLISP.Tokens

    data StackData = IntData Int 
        | StringData String 
        | BooleanData Bool 
        | BlockData BlockOp
        | RecursiveData StackData deriving (Show)

    data Stack = Empty 
        | Some [StackData] deriving (Show)

    push :: Stack -> StackData -> Stack
    push (Empty) newEle = Some [newEle]
    push (Some stack) newEle = Some (newEle:stack)

    pop :: Stack -> Maybe (StackData, Stack)
    pop (Empty) = Nothing
    pop (Some (x:xs)) = Just (x, Some (xs))

    peek :: Stack -> Maybe StackData
    peek (Empty) = Nothing
    peek (Some (x:xs)) = Just x