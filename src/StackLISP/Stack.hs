module StackLISP.Stack where

    import Data.Maybe

    type Code = String
    data StackData = IntData Int | StringData String | BooleanData Bool | CodeData Code | RecursiveData StackData deriving (Show)
    data Stack = Empty | Some [StackData] deriving (Show)

    push :: Stack -> StackData -> Maybe Stack
    push (Empty) _ = Nothing
    push (Some (stack)) newEle = Just $ Some (newEle:stack)

    pop :: Stack -> Maybe (StackData, Stack)
    pop (Empty) = Nothing
    pop (Some (x:xs)) = Just (x, Some (xs))

    peek :: Stack -> Maybe StackData
    peek (Empty) = Nothing
    peek (Some (x:xs)) = Just x