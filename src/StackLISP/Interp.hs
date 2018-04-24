module StackLISP.Interp where
    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens

    data Interp = Interp {
        stack :: Stack,
        ip :: Int,
        program :: Program
    } deriving (Show)

    data RuntimeError = RuntimeError String deriving (Show)

    step :: Interp -> (Interp, Statement)
    step interp = (newInterp, nextStatement)
        where
            newIp = 1 + (ip interp)
            (Program (x:xs)) = program interp
            (BlockOp (nextStatement:rest)) = x
            newInterp = Interp {stack=stack interp, ip=newIp, program=Program [(BlockOp rest)]}

    {-
    Add semantics:
    String + String = String concatenation
    Int + Int = Addition
    Block + Block = Block concatenation
    -}
    add :: Either RuntimeError StackData -> Either RuntimeError StackData -> Either RuntimeError StackData
    add x y = 
        case x of
            Right (StringData left) -> case y of 
                Right (StringData right) -> Right (StringData $ left ++ right)
                Right _ -> Left mismatched
                Left error -> Left error
            Right (IntData left) -> case y of 
                Right (IntData right) -> Right (IntData $ left + right)
                Right _ -> Left mismatched 
                Left error -> Left error
            Right (BlockData (BlockOp left)) -> case y of
                Right (BlockData (BlockOp right)) -> Right (BlockData $ BlockOp $ left ++ right)
                Right _ -> Left mismatched
                Left error -> Left error
            Left error -> Left error
        where
            mismatched = (RuntimeError "Mismatched types: can only perform addition on matching types.")
    {-
    Sub semantics:
    String - Int = Drop characters
    Int - Int = Subtraction
    Block - Int = Drop code?
    -}
    sub :: Either RuntimeError StackData -> Either RuntimeError StackData -> Either RuntimeError StackData
    sub x y =
        case x of
            Right (StringData left) -> case y of
                Right (IntData right) -> Right (StringData $ drop right left)
                Right _ -> Left mismatched
                Left error -> Left error
            Right (IntData left) -> case y of
                Right (IntData right) -> Right (IntData $ left - right)
                Right _ -> Left mismatched
                Left error -> Left error
            Right (BlockData (BlockOp left)) -> case y of
                Right (IntData right) -> Right (BlockData $ BlockOp $ drop right left)
                Right _ -> Left mismatched
                Left error -> Left error
            Left error -> Left error
        where
            mismatched = (RuntimeError "Mismatched types: Invalid types for subtraction.")


    handleMath :: Interp -> MathOps -> Either RuntimeError (Interp, Statement)
    handleMath interp op =
        case op of 
            Add -> case add left right of 
                (Left x) -> (Left x)
                (Right res) -> Right (Interp {stack=push newStack res, ip=newIp, program=newProg}, nextStatement)
            Sub -> case sub left right of
                (Left x) -> (Left x)
                (Right res) -> Right (Interp {stack=push newStack res, ip=newIp, program=newProg}, nextStatement)
            _ -> (Left $ RuntimeError "Unsupported op")
        where 
            curStack = stack interp
            (left, nextStack) = case pop curStack of
                Just (x, Some (xs)) -> (Right x, Some (xs))
                _ -> (Left (RuntimeError "Cannot apply math ops to supplied types."), curStack)
            (right, newStack) = case pop nextStack of
                Just (x, Some (xs)) -> (Right x, Some (xs))
                _ -> (Left (RuntimeError "Cannot apply math ops to supplied types."), nextStack)
            newIp = 1 + (ip interp)
            (Program (x:xs)) = program interp
            (BlockOp (nextStatement:rest)) = x
            newProg = Program [(BlockOp rest)]

    handlePrimitive :: Interp -> PrimitiveToken -> (Interp, Statement)
    handlePrimitive interp op =
        case op of
            (StringToken string) -> (Interp {stack=push curStack (StringData string), ip=newIp, program=newProg}, nextStatement)
            (BooleanToken bool) -> (Interp {stack=push curStack (BooleanData bool), ip=newIp, program=newProg}, nextStatement)
            (NumberToken int) -> (Interp {stack=push curStack (IntData int), ip=newIp, program=newProg}, nextStatement)
        where
            curStack = stack interp
            newIp = 1 + (ip interp)
            (Program (x:xs)) = program interp
            (BlockOp (nextStatement:rest)) = x
            newProg = Program [(BlockOp rest)]

    eval :: Interp -> Statement -> Either RuntimeError Interp
    eval interp NOP = eval newInterp newStatement
            where
                (newInterp, newStatement) = step interp
    eval interp (PrimSt tok) = res
            where
                (newInterp, newStatement) = handlePrimitive interp tok
                res = eval newInterp newStatement
    eval interp (MathSt ops) = res
            where
                res = case handleMath interp ops of
                    (Left x) -> (Left x)
                    (Right (newInterp, newStatement)) -> eval newInterp newStatement
    eval interp (EOB) = Right interp

    interp :: String -> String
    interp contents = case parseFile contents of 
        (Left error) -> "Runtime Error: " ++ (show error)
        (Right res) -> show $ eval Interp {stack=Empty, ip=0, program=res} NOP