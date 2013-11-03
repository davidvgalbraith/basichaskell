import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
  (fone:ftwo:_) <- getArgs
  copey fone ftwo
  
copey source deset = do
  contents <- B.readFile source
  B.writeFile dest contents
  