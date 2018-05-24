import StackLISP.Interp
import System.Environment

main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    interp contents False