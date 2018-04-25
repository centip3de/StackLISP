module StackLISP.Interp where
    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens
    import StackLISP.Errors

    data Interp = Interp {
        stack :: Stack,
        program :: Program
    } deriving (Show)

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

    {-
    Div semantics:
    Int / Int = Division
    -}
    division :: StackData -> StackData -> Either RuntimeError StackData
    division x y =
        case (x, y) of
            (IntData left, IntData right) -> Right (IntData $ left `quot` right)
            _ -> Left (RuntimeError "Mismatched types: Invalid types for division.")

    {-
    Mod semantics:
    Int % Int = Division
    -}
    modulo :: StackData -> StackData -> Either RuntimeError StackData
    modulo x y =
        case (x, y) of
            (IntData left, IntData right) -> Right (IntData $ left `mod` right)
            _ -> Left (RuntimeError "Mismatched types: Invalid types for modulo.")


    translateMathOp :: MathOps -> Either RuntimeError (StackData -> StackData -> Either RuntimeError StackData)
    translateMathOp Add = Right add
    translateMathOp Sub = Right sub
    translateMathOp Mul = Right mul
    translateMathOp Div = Right division
    translateMathOp Mod = Right modulo

    popStack :: Stack -> Either RuntimeError Stack
    popStack stack = case pop stack of
        (Left error) -> Left error
        (Right (x, xs)) -> Right xs
    
    dupStack :: Stack -> Either RuntimeError Stack
    dupStack stack = case stack of
        Empty -> Right Empty
        (Some xs) -> Right $ Some $ concat $ replicate 2 xs
    
    reverseStack :: Stack -> Either RuntimeError Stack
    reverseStack stack = case stack of
        Empty -> Right Empty
        (Some xs) -> Right $ Some $ reverse xs

    swapStack :: Stack -> Either RuntimeError Stack
    swapStack stack = case stack of
        Empty -> Right Empty
        (Some (first:second:rest)) -> Right (Some (second:first:rest))
    
    {-
    TODO: Figure out how we want stack sort to work
    -}
    translateStackOp :: StackOps -> Either RuntimeError (Stack -> Either RuntimeError Stack)
    translateStackOp Pop = Right popStack
    translateStackOp Dup = Right dupStack
    translateStackOp Reverse = Right reverseStack
    translateStackOp Swap = Right swapStack
    translateStackOp _ = Left (RuntimeError "Unsupported op")

    handleStack :: Interp -> StackOps -> Either RuntimeError Interp
    handleStack interp op = do
        op' <- translateStackOp op
        newStack <- op' $ stack interp
        Right Interp {stack=newStack, program=program interp}

    handleMath :: Interp -> MathOps -> Either RuntimeError Interp
    handleMath interp op = do
        (left, nextStack) <- pop $ stack interp
        (right, newStack) <- pop nextStack

        op' <- translateMathOp op
        result <- op' left right
        Right Interp {stack=push newStack result, program=program interp}

    handlePrimitive :: Interp -> PrimitiveToken -> Either RuntimeError Interp
    handlePrimitive interp op =
        case op of
            (StringToken string) -> Right $ Interp {stack=push curStack (StringData string), program=newProg}
            (BooleanToken bool) -> Right $ Interp {stack=push curStack (BooleanData bool), program=newProg}
            (NumberToken int) -> Right $ Interp {stack=push curStack (IntData int), program=newProg}
        where
            curStack = stack interp
            newProg = program interp

    step :: Interp -> Statement -> Either RuntimeError Interp
    step interp NOP = Right interp
    step interp (PrimSt tok) = handlePrimitive interp tok
    step interp (MathSt ops) = handleMath interp ops
    step interp (StackSt ops) = handleStack interp ops
    step interp EOB = Right interp

    handleStep :: Either RuntimeError Interp -> Statement -> Either RuntimeError Interp
    handleStep either statement = case either of
        (Left x) -> Left x
        (Right interp) -> step interp statement

    eval :: Interp -> Either RuntimeError Interp
    eval interp = 
        foldl handleStep (Right interp) (statements)
        where
            (Program statements) = program interp

    interp :: String -> String
    interp contents = case parseFile contents of 
        (Left error) -> "Runtime Error: " ++ (show error)
        (Right res) -> show $ eval Interp {stack=Empty, program=res}