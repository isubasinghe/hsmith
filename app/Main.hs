module Main where

import Language.C.Data.InputStream
import Language.C.Data.Position
import Language.C.Parser
import Text.Pretty.Simple (pPrint)
import Synth
import Control.Monad.State.Lazy

main :: IO ()
main = do 
  let state = emptyProgramState 
  (structs, state') <- runStateT synthesizeStructs state
  print structs

