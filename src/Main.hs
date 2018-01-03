-------------------------------------------------
-------------------------------------------------
-- MAIN KDL LANGUAGE
-------------------------------------------------
-------------------------------------------------
import            System.Environment
import            Statements
import            Parser
import            RunKDL
import            Control.Monad.State
import qualified  Data.Map
import            Evaluator


main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  file <- readFile filename
  let kdlpar = parse parseKDL file
  print kdlpar
  let kdlm = Data.Map.empty :: KDLMap
  _ <- runStateT (run kdlpar) kdlm
  return ()
