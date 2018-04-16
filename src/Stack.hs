module StackLISP.Stack where

    import Data.Maybe

    type Code = String
    data StackData = IntData Int | StringData String | BooleanData Boolean | CodeData Code | RecursiveData StackData deriving (Show)
    data Stack = Empty | Some [StackData] deriving (Show)

    push :: Stack -> StackData -> Maybe Stack
    push (Some stack) data = Just $ data : stack
    push (Empty) data = Nothing

    pop :: Stack -> Maybe (StackData, Stack)
    pop (Empty) -> Nothing
    pop (Some (x:xs)) data = Just $ (x, xs)

