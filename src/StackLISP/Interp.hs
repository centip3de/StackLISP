module StackLISP.Interp where
    import Control.Monad.Free

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
            (StatementData left, StatementData right) -> Right (StatementData $ left ++ right)
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
            (StatementData left, IntData right) -> Right (StatementData $ drop right left)
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
            (StatementData left, IntData right) -> Right (StatementData $ concat $ replicate right left)
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

    popStack :: Interp -> Either RuntimeError Interp
    popStack interp = case pop $ stack interp of
        (Left error) -> Left error
        (Right (x, Some [])) -> Right $ Interp {stack=Empty, program=program interp}
        (Right (x, xs)) -> Right $ Interp {stack=xs, program=program interp}
    
    dupStack :: Interp -> Either RuntimeError Interp
    dupStack interp = case stack interp of
        Empty -> Right interp
        (Some xs) -> Right $ Interp {stack=Some $ concat $ replicate 2 xs, program=program interp}
    
    reverseStack :: Interp -> Either RuntimeError Interp
    reverseStack interp = case stack interp of
        Empty -> Right interp
        (Some xs) -> Right $ Interp {stack=Some $ reverse xs, program=program interp}

    swapStack :: Interp -> Either RuntimeError Interp
    swapStack interp = case stack interp of
        Empty -> Right interp
        (Some (first:second:rest)) -> Right $ Interp {stack=Some (second:first:rest), program=program interp}

    executeStack :: Interp -> Either RuntimeError Interp
    executeStack interp = case pop $ stack interp of
        (Left error) -> Left error
        (Right ((StatementData ops), Some [])) -> doEval Empty ops
        (Right ((StatementData ops), rest)) -> doEval rest ops
        where
            doEval stackData prog = case eval Interp {stack=stackData, program=Program prog} of
                (Left error) -> Left error
                (Right newInterp) -> Right $ Interp{stack=stack newInterp, program=program interp}

    {-
    TODO: Figure out how we want stack sort to work
    -}
    translateStackOp :: StackOps -> Either RuntimeError (Interp -> Either RuntimeError Interp)
    translateStackOp Pop = Right popStack
    translateStackOp Dup = Right dupStack
    translateStackOp Reverse = Right reverseStack
    translateStackOp Swap = Right swapStack
    translateStackOp Execute = Right executeStack
    translateStackOp _ = Left (RuntimeError "Unsupported op")

    handleStack :: Interp -> StackOps -> Either RuntimeError Interp
    handleStack interp op = translateStackOp op >>= (\x -> x interp)

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

    handleBlock :: Interp -> [Statement] -> Either RuntimeError Interp
    handleBlock interp ops = Right $ Interp {stack=newStack, program=newProgram}
            where
                newStack = push (stack interp) (StatementData ops)
                newProgram = program interp

    getString :: IOM String
    getString = liftF $ InputStr id

    putString :: IOM String
    putString string = liftF $ PrintStr string ()

    doIO :: IOM a -> IO a 
    doIO = foldFree func
                where 
                    func (InputStr f) = f <$> getChar
                    func (PrintStr a b) = putStrLn a >> return b

    handleInput :: Interp -> Either RuntimeError Interp
    handleInput interp = do
        string <- getString
        string' <- doIO string
        Right $ Interp {stack=push (stack interp) string', program=program interp}

    handlePrint :: Interp -> Either RuntimeError Interp
    handlePrint interp = do
        (string, stack') <- pop $ stack interp
        x <- putString string
        doIO x
        Right $ Interp {stack=stack', program=program interp}

    handleIO :: Interp -> IOOps -> Either RuntimeError Interp
    handleIO interp ops = 
        case ops of
            (Print) -> handlePrint interp
            (Input) -> handleInput interp
        

    step :: Interp -> Statement -> Either RuntimeError Interp
    step interp NOP = Right interp
    step interp (PrimSt tok) = handlePrimitive interp tok
    step interp (MathSt ops) = handleMath interp ops
    step interp (StackSt ops) = handleStack interp ops
    step interp (BlockSt (BlockOp ops)) = handleBlock interp ops
    step interp (IOSt ops) = handleIO interp ops
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