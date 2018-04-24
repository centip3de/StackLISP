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
        where
            mismatched = (RuntimeError "Mismatched types: can only perform addition on matching types.")

    handleMath :: Interp -> MathOps -> Either RuntimeError Interp
    handleMath interp op =
        case op of 
            Add -> case add left right of 
                (Left x) -> (Left x)
                (Right res) -> Right Interp {stack=push curStack res, ip=newIp, program=newProg}
            _ -> (Left $ RuntimeError "Unsupported op")
        where 
            curStack = stack interp
            left = case pop curStack of
                Just (x, Some (xs)) -> Right x
                _ -> Left (RuntimeError "Cannot apply math ops to supplied types.")
            right = case pop curStack of
                Just (x, Some (xs)) -> Right x
                _ -> Left (RuntimeError "Cannot apply math ops to supplied types.")
            (newInterp, newOp) = step interp
            newIp = ip newInterp
            newProg = program newInterp

    handlePrimitive :: Interp -> PrimitiveToken -> Interp
    handlePrimitive interp op =
        case op of
            (StringToken string) -> Interp {stack=push curStack (StringData string), ip=newIp, program=newProg}
            (BooleanToken bool) -> Interp {stack=push curStack (BooleanData bool), ip=newIp, program=newProg}
            (NumberToken int) -> Interp {stack=push curStack (IntData int), ip=newIp, program=newProg}
        where
            curStack = stack interp
            newIp = 1 + (ip interp)
            newProg = program interp



    eval :: Interp -> Statement -> Interp
    eval interp NOP = eval newInterp newTok 
            where
                (newInterp, newTok) = step interp

    interp :: String -> String
    interp contents = case parseFile contents of 
        (Left error) -> "Runtime Error: " ++ (show error)
        (Right res) -> show $ eval Interp {stack=Empty, ip=0, program=res} NOP