import Lucretia.Syntax
import Lucretia.Interpreter
-- import qualified Lucretia.ParsecParser as Parser
import qualified Lucretia.ApplicativeParser as Parser

import IO ( stdin, hGetContents )
import System ( getArgs, getProgName )

main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= runText "<stdin>" 
            fs -> mapM_ runFile  fs

runFile :: FilePath -> IO ()
runFile f = readFile f >>= runText f

runText name text = case Parser.runParser name text of
  Left e -> putStr "Parse error: " >> print e
  Right p ->  do
    putStrLn $ "Parsed OK: " ++ show p
    runProg p
