module StackLISP.Stack where

    import Data.Maybe
    import StackLISP.Tokens
    import StackLISP.Errors

    data StackData a = IntData Int 
        | StringData String 
        | BooleanData Bool 
        | StatementData [(StatementF a)]
        | RecursiveData (StackData a) 
        deriving (Show)

    data Stack = Empty 
        | Some [StackData ()]
        deriving (Show)

    push :: Stack -> StackData () -> Stack
    push (Empty) newEle = Some [newEle]
    push (Some stack) newEle = Some (newEle:stack)

    pop :: Stack -> Either RuntimeError (StackData (), Stack)
    pop (Empty) = Left (RuntimeError "Cannot pop an empty stack")
    pop (Some (x:xs)) = Right (x, Some (xs))

    peek :: Stack -> Maybe (StackData ())
    peek (Empty) = Nothing
    peek (Some (x:xs)) = Just x