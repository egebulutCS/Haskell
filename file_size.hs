import System.IO
import System.Environment

main = do
 [f] <- getArgs
 s   <- readFile f
 putStrLn (show (filesize s))
 
filesize s = length s