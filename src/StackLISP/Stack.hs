module StackLISP.Stack where

    import Data.Maybe
    import Control.Monad.Free
    import StackLISP.Tokens
    import StackLISP.Errors

    data StackData = IntData Int 
        | StringData String 
        | BooleanData Bool 
        | StatementData (StatementM ())

    data Stack = Empty 
        | Some [StackData]

    instance Show StackData where
        show (IntData int) = show int
        show (StringData string) = string
        show (BooleanData bool) = show bool
        show (StatementData statements) = "Block data"

    instance Show Stack where
        show (Empty) = "Empty"
        show (Some stack) = foldl (\acc x -> acc ++ "\n" ++ show x) "" stack

    push :: StackData -> Stack -> Stack
    push  newEle (Empty) = Some [newEle]
    push  newEle (Some stack) = Some (newEle:stack)

    pop :: Stack -> Either RuntimeError (StackData, Stack)
    pop (Empty) = Left (RuntimeError "Cannot pop an empty stack")
    pop (Some []) = Left (RuntimeError "Cannot pop an empty stack")
    pop (Some (x:xs)) = Right (x, Some (xs))

    peek :: Stack -> Maybe (StackData)
    peek (Empty) = Nothing
    peek (Some []) = Nothing
    peek (Some (x:xs)) = Just x