import System.Directory
import System.Environment
import System.IO
import Data.List

despach = [("add", add)
	, ("view", view)
	, ("remove", remove)]

main = do
     (command: args) <- getArgs
     let (Just cmd) = lookup command despach
     cmd args

add [x, y] = appendFile x $ y ++ "\n"

view [x] = do
     ts <- readFile x
     let tdt = lines ts
     	 numbr = zipwith (\x y -> show x ++ " - " ++ y) [0..] tdt
     putStr $ unlines numbr

remove [name, nbstrng] = do 
       handle <- openFile name ReadMode
       tempdir <- getTemporaryDirectory
       (tempn, temph) <- openTempFile tempdir "temp"
       contents <- hGetContents handle
       let number = read nbstrng
       	   tdt = lines contents
	   newtd = delete (tdt !! number) tdt
       hPutStr temph $ unlines newtd
       hclose handle
       hclose temph
       removeFile name
       renameFile tempn name 

hclose = hClose
zipwith = zipWith