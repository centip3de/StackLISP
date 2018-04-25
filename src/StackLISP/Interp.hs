module StackLISP.Interp where
    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens
    import StackLISP.Errors

    data Interp = Interp {
        stack :: Stack,
        ip :: Int,
        program :: Program
    } deriving (Show)


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
    add :: StackData -> StackData -> Either RuntimeError StackData
    add x y = 
        case (x, y) of
            (StringData left, StringData right) -> Right (StringData $ left ++ right)
            (IntData left, IntData right) -> Right (IntData $ left + right)
            (BlockData (BlockOp left), BlockData (BlockOp right)) -> Right (BlockData $ BlockOp $ left ++ right)
            _ -> Left (RuntimeError "Mismatched types: can only perform addition on matching types.")

    {-
    Sub semantics:
    String - Int = Drop characters
    Int - Int = Subtraction
    Block - Int = Drop code?
    -}
    sub :: StackData -> StackData -> Either RuntimeError StackData
    sub x y =
        case (x, y) of
            (StringData left, IntData right) -> Right (StringData $ drop right left)
            (IntData left, IntData right) -> Right (IntData $ left - right)
            (BlockData (BlockOp left), IntData right) -> Right (BlockData $ BlockOp $ drop right left)
            _ -> Left (RuntimeError "Mismatched types: Invalid types for subtraction.")

    {-
    Mul semantics:
    String * Int = Duplicate string
    Int * Int = Multiplication
    Block * Int = Duplicate blocks
    -}
    mul :: StackData -> StackData -> Either RuntimeError StackData
    mul x y =
        case (x, y) of
            (StringData left, IntData right) -> Right (StringData $ concat $ replicate right left)
            (IntData left, IntData right) -> Right (IntData $ left * right)
            (BlockData (BlockOp left), IntData right) -> Right (BlockData $ BlockOp $ concat $ replicate right left)
            _ -> Left (RuntimeError "Mismatched types: Invalid types for multiplication.")

    translateOp :: MathOps -> Either RuntimeError (StackData -> StackData -> Either RuntimeError StackData)
    translateOp Add = Right add
    translateOp Sub = Right sub
    translateOp Mul = Right mul
    translateOp _   = Left $ RuntimeError "Unsupported op"

    handleMath :: Interp -> MathOps -> Either RuntimeError (Interp, Statement)
    handleMath interp op = do
        (left, nextStack) <- pop curStack
        (right, newStack) <- pop nextStack

        op' <- translateOp op
        result <- op' left right
        Right (Interp {stack=push newStack result, ip=newIp, program=newProg}, nextStatement)

        where 
            curStack = stack interp
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