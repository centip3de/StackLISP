{-# LANGUAGE FlexibleContexts #-}
module StackLISP.Test where
    import StackLISP.Tokens
    import Control.Monad.Free

    translate :: StatementF a -> IO a
    translate (Print next) = putStrLn "Print" >> return next
    translate (Pop next) = putStrLn "Pop" >> return next
    translate (Str string next) = putStrLn string >> return next
    translate (Execute next) = return next
    transltae (()) = putStrLn "End" >> return ()

    testFunc :: StatementM a -> IO a
    testFunc x = foldFree translate x

    printString :: StatementM ()
    printString = liftF $ Print () 

    pop :: StatementM ()
    pop = liftF $ Pop ()

    string :: String -> StatementM ()
    string str = liftF $ Str str () 

    hiddenExe :: StatementM ()
    hiddenExe = liftF $ Execute ()

    combine :: StatementM ()
    combine = do
        printString
        pop
        hiddenExe
        string "test"

    thing :: IO ()
    thing = testFunc combine