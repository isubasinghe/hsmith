module Spec where
import Test.QuickCheck 
import qualified AST as A 
import qualified SAST as S 
import qualified Synth as Sy

propConstant ty = do 
  (ty',expr) <- Sy.synthesizeConstant ty 
  pure (ty==ty')



main :: IO ()
main = putStrLn "Test suite not yet implemented"
