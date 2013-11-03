import System.Environment
import Data.List

main = do
     putStrLn "Walter the chimpanzee!"
     args <- getArgs
     name <- getProgName
     mapM_ print args
     putStrLn "Man, Arthur the chimpanzee!"
     putStrLn name