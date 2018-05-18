import StackLISP.Interp
import System.Environment

main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    case interp contents of
        (Left error) -> putStrLn error
        (Right interp) -> putStrLn "Finished"