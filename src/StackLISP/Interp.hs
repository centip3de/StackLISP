module StackLISP.Interp where
    import Control.Monad.Free
    import Control.Monad.State

    import StackLISP.Stack
    import StackLISP.Parser
    import StackLISP.Tokens
    import StackLISP.Errors

    repeatStatement :: StatementM () -> Int -> StatementM ()
    repeatStatement statement num = do
        statement
        statement
        when (num /= 0) (repeatStatement statement (num-1))

    stackIsFalsy :: Stack -> Bool
    stackIsFalsy stack = case peek stack of
        (Just (IntData 0)) -> True
        (Just (BooleanData False)) -> True
        (Just (StringData "")) -> True
        (Just (StatementData _)) -> True
        _ -> False

    stackIsTruthy :: Stack -> Bool
    stackIsTruthy stack = not $ stackIsFalsy stack

    add :: Stack -> Either RuntimeError (StackData, Stack)
    add stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (StringData left, StringData right) -> Right (StringData $ left ++ right, stack'')
            (IntData left, IntData right) -> Right (IntData $ left + right, stack'')
            (StatementData left, StatementData right) -> Right (StatementData $ left >> right, stack'')
            _ -> Left (RuntimeError "Mismatched types: can only perform addition on matching types.")

    sub :: Stack -> Either RuntimeError (StackData, Stack)
    sub stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (StringData left, IntData right) -> Right (StringData $ drop right left, stack'')
            (IntData left, IntData right) -> Right (IntData $ left - right, stack'')
            -- TODO:
            --(StatementData left, IntData right) -> Right (StatementData $ drop right left)
            _ -> Left (RuntimeError "Mismatched types: Invalid types for subtraction.")

    mul :: Stack -> Either RuntimeError (StackData, Stack)
    mul stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (StringData left, IntData right) -> Right (StringData $ concat $ replicate right left, stack'')
            (IntData left, IntData right) -> Right (IntData $ left * right, stack'')
            (StatementData left, IntData right) -> Right (StatementData $ repeatStatement left right, stack')
            _ -> Left (RuntimeError "Mismatched types: Invalid types for multiplication.")

    division :: Stack -> Either RuntimeError (StackData, Stack)
    division stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (IntData left, IntData right) -> Right (IntData $ left `quot` right, stack'')
            _ -> Left (RuntimeError "Mismatched types: Invalid types for division.")

    modulo :: Stack -> Either RuntimeError (StackData, Stack)
    modulo stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (IntData left, IntData right) -> Right (IntData $ left `mod` right, stack'')
            _ -> Left (RuntimeError "Mismatched types: Invalid types for modulo.")

    equals :: Stack -> Either RuntimeError (Bool, Stack)
    equals stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (IntData left, IntData right) -> Right (left == right, stack'')
            (StringData left, StringData right) -> Right (left == right, stack'')
            _ -> Left (RuntimeError "Mismatched types: Invalid types for equality comparison.")

    lessThan :: Stack -> Either RuntimeError (Bool, Stack)
    lessThan stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (IntData left, IntData right) -> Right (left < right, stack'')
            _ -> Left (RuntimeError "Mismatched types: Invalid types for lessThan comparison.")

    greaterThan :: Stack -> Either RuntimeError (Bool, Stack)
    greaterThan stack = do
        (lt, stack') <- lessThan stack
        Right $ (not lt, stack')

    -- TODO: We should allow these to apply to other ops
    negation :: Stack -> Either RuntimeError (Bool, Stack)
    negation stack = do
        (x, stack') <- pop stack
        case x of 
            (BooleanData bool) -> Right $ (not bool, stack')
            _ -> Left (RuntimeError "Mismatched types: Invalid types for negation")

    logicalAnd :: Stack -> Either RuntimeError (Bool, Stack)
    logicalAnd stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (BooleanData left, BooleanData right) -> Right $ (left && right, stack'')
            _ -> Left (RuntimeError "Matmatched types: Invalid types for logical and")

    logicalOr :: Stack -> Either RuntimeError (Bool, Stack)
    logicalOr stack = do
        (x, stack') <- pop stack
        (y, stack'') <- pop stack'
        case (x, y) of
            (BooleanData left, BooleanData right) -> Right $ (left || right, stack'')
            _ -> Left (RuntimeError "Matmatched types: Invalid types for logical or")


    eval ::  Stack -> StatementM () -> StateT Stack IO ()
    -- Primitives
    eval stack (Free (Boolean bool next)) = do
        put (push (BooleanData bool) stack)
        newStack <- get
        eval newStack next
    eval stack (Free (Str string next)) = do
        put (push (StringData string) stack)
        newStack <- get
        eval newStack next 
    eval stack (Free (Number int next)) = do
        put (push (IntData int) stack)
        newStack <- get
        eval newStack next
    eval stack (Free (Block statements next)) = do
        put (push (StatementData statements) stack)
        newStack <- get
        eval newStack next

    -- Equality commands
    eval stack (Free (Equals next)) = case equals stack of
        (Left error) -> lift $ putStrLn $ show error
        (Right (res, stack')) -> do
            put (push (BooleanData res) stack')
            newStack <- get
            eval newStack next
    eval stack (Free (LessThan next)) = case lessThan stack of
        (Left error) -> lift $ putStrLn $ show error
        (Right (res, stack')) -> do
            put (push (BooleanData res) stack')
            newStack <- get
            eval newStack next
    eval stack (Free (GreaterThan next)) = case greaterThan stack of
        (Left error) -> lift $ putStrLn $ show error
        (Right (res, stack')) -> do
            put (push (BooleanData res) stack')
            newStack <- get
            eval newStack next
    eval stack (Free (Negate next)) = case negation stack of
        (Left error) -> lift $ putStrLn $ show error
        (Right (res, stack')) -> do
            put (push (BooleanData res) stack')
            newStack <- get
            eval newStack next

    -- Logical commands
    eval stack (Free (And next)) = case logicalAnd stack of
        (Left error) -> lift $ putStrLn $ show error
        (Right (res, stack')) -> do
            put (push (BooleanData res) stack')
            newStack <- get
            eval newStack next
    eval stack (Free (Or next)) = case logicalOr stack of
        (Left error) -> lift $ putStrLn $ show error
        (Right (res, stack')) -> do
            put (push (BooleanData res) stack')
            newStack <- get
            eval newStack next

    -- Math commands
    eval stack (Free (Add next)) = case add stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (res, stack')) -> do
            put (push res stack')
            newStack <- get
            eval newStack next
    eval stack (Free (Sub next)) = case sub stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (res, stack')) -> do
            put (push res stack')
            newStack <- get
            eval newStack next
    eval stack (Free (Div next)) = case division stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (res, stack')) -> do
            put (push res stack')
            newStack <- get
            eval newStack next
    eval stack (Free (Mul next)) = case mul stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (res, stack')) -> do
            put (push res stack')
            newStack <- get
            eval newStack next
    eval stack (Free (Mod next)) = case modulo stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (res, stack')) -> do
            put (push res stack')
            newStack <- get
            eval newStack next

    -- Stack commands
    eval stack (Free (Pop next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (_, newStack)) -> do
            put newStack
            eval newStack next
    eval stack (Free (Dup next)) = case stack of
        Empty -> eval stack next
        (Some []) -> eval stack next
        (Some (x:xs)) -> do
            put (Some (x:x:xs))
            newStack <- get
            eval newStack next
    eval stack (Free (Reverse next)) = case stack of
        Empty -> eval stack next
        (Some []) -> eval stack next
        (Some xs) -> do
            put (Some $ reverse xs)
            newStack <- get
            eval newStack next
    eval stack (Free (Swap next)) = case stack of
        Empty -> eval stack next
        (Some []) -> eval stack next
        (Some (first:second:rest)) -> do
            put (Some $ (second:first:rest))
            newStack <- get
            eval newStack next
    eval stack (Free (Sort next)) = do
        lift $ putStrLn "Sort isn't currently implemented. Skipping."
        eval stack next
    eval stack (Free (Execute next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right ((StatementData statements), newStack)) -> do
            put newStack
            eval newStack statements
            newStack' <- get
            eval newStack' next
        (Right (_, newStack)) -> lift $ (putStrLn "Cannot execute item.")

    -- IO
    eval stack (Free (Print next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right (item, newStack)) -> do
            lift $ putStrLn $ show item
            put newStack
            eval newStack next
    eval stack (Free (Input next)) = do
        input <- lift $ getLine
        put (push (StringData input) stack)
        newStack <- get
        eval newStack next

    -- Control flow
    eval stack (Free (While next)) = case pop stack of
        (Left error) -> lift $ (putStrLn $ show error)
        (Right ((StatementData statements), newStack)) -> do
            let while nextStack = do
                    put nextStack
                    eval nextStack statements
                    newStack' <- get
                    unless (stackIsFalsy newStack') (while newStack')
            while newStack
            newStack' <- get
            eval newStack' next
        (Right (_, newStack)) -> lift $ (putStrLn "Cannot execute item.")
    eval stack (Free (For next)) = case pop stack of
        (Left error) -> lift $ putStrLn $ show error
        (Right ((StatementData statements), newStack)) -> do
            let for nextStack i = do
                    put (push (IntData i) nextStack)
                    nextStack' <- get
                    eval nextStack' statements
                    newStack' <- get
                    unless (i == 0) (for newStack' (i-1))

            case pop newStack of
                (Left error) -> lift $ putStrLn $ show error
                (Right ((IntData int), otherStack)) -> do
                    for otherStack int
                    newStack' <- get
                    eval newStack' next
                (Right _) -> lift $ putStrLn "Cannot (currently) iterate over that type."
        (Right _) -> lift $ putStrLn "Cannot execute item."

    -- TODO: This is gross.
    eval stack (Free (If next)) = case pop stack of
        (Left error) -> lift $ putStrLn $ show error
        (Right ((StatementData ifBlock), newStack)) ->
            case pop newStack of
                (Left error) -> lift $ putStrLn $ show error
                (Right ((StatementData elseBlock), newStack')) -> 
                    if stackIsTruthy newStack' then
                        do
                            put newStack'
                            eval newStack' ifBlock
                            newStack'' <- get
                            eval newStack'' next
                    else
                        do
                            put newStack'
                            eval newStack' elseBlock
                            newStack'' <- get
                            eval newStack'' next
                (Right _) -> lift $ putStrLn "Cannot execute current stack top"
        (Right _) -> lift $ putStrLn "Cannot execute current stack top"
    eval stack (Free (Done _)) = return ()

    -- Fall through
    eval stack (x) = lift $ (putStrLn "Unimplemented OP")

    interp :: String -> Bool -> IO ()
    interp contents debug = case parseFile contents of 
        (Left error) -> putStrLn $ "Parsing Error: " ++ (show error)
        (Right res) -> do
            if debug then
                putStrLn $ "Debug mode enabled"
            else
                return ()

            output <- execStateT (eval Empty res) Empty

            if debug then
                putStrLn $ "Stack (after execution): " ++ show output
            else
                return ()