module StackLISP.Stack where

    import Data.Maybe
    import Control.Monad.Free
    import StackLISP.Tokens
    import StackLISP.Errors

    data StackData a = IntData Int 
        | StringData String 
        | BooleanData Bool 
        | StatementData (StatementM (Free StatementF a)) 

    data Stack a = Empty 
        | Some [StackData a]

    instance Show a => Show (StackData a) where
        show (IntData int) = show int
        show (StringData string) = string
        show (BooleanData bool) = show bool
        show (StatementData statements) = "Block data"
        show x = "???"

    instance Show a => Show (Stack a) where
        show (Empty) = "Empty"
        show (Some stack) = foldl (\acc x -> acc ++ "\n" ++ show x) "" stack

    push :: StackData a -> Stack a -> Stack a
    push  newEle (Empty) = Some [newEle]
    push  newEle (Some stack) = Some (newEle:stack)

    pop :: Stack a -> Either RuntimeError (StackData a, Stack a)
    pop (Empty) = Left (RuntimeError "Cannot pop an empty stack")
    pop (Some (x:xs)) = Right (x, Some (xs))

    peek :: Stack a -> Maybe (StackData a)
    peek (Empty) = Nothing
    peek (Some (x:xs)) = Just x