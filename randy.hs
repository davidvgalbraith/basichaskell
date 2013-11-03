import System.Random
import Control.Monad(when)

main = do
  gen <- getStdGen
  ask gen
  
ask :: StdGen -> IO() 

ask gen = do
  let (randy, newgen) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Walter the chimpanzee!"
  number <- getLine
  when (not $ null number) $ do
    let nume = read number
    if randy == nume
       then putStrLn "The chimpanzee are Arthured!"
       else putStrLn "The chimpanzee are not Arthured!"
    ask newgen