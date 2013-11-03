import System.IO
import System.Directory
import Data.List

main = do 
     handle <- openFile "todo.txt" ReadMode
     tempdir <- getTemporaryDirectory
     (tempName, tempHandle) <- openTempFile tempdir "temp"
     contents <- hGetContents handle
     let todoTasks = lines contents
     	 numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
     putStrLn "These are your todo Items"
     putStr $ unlines numberedTasks
     putStrLn "Which one do you woant to ldelete"
     numberString <- getLine
     let number = read numberString
     	 newTodoItems = delete (todoTasks !! number) todoTasks
     hPutStr tempHandle $ unlines newTodoItems
     hClose handle
     hClose tempHandle
     removeFile "todo.txt"
     renameFile tempName "todo.txt"