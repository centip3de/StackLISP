import StackLISP.Interp
import System.Environment

realMain filename = do
    contents <- readFile filename
    interp contents

main = do
    (filename:_) <- getArgs
    realMain filename
