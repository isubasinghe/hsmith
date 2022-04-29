module Main where

import Text.Pretty.Simple (pPrint)
import Synth
import Control.Monad.State.Lazy
import SAST
import Prettyprinter
import Prettyprinter.Render.Text

main :: IO ()
main = do 
  putDoc $ pretty exampleStatement

