module Main where

import Text.Pretty.Simple (pPrint)
import Synth
import Control.Monad.State.Lazy
import SAST
import Prettyprinter
import Prettyprinter.Render.Text
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do 
  let x = pretty exampleStatement
  let render = TIO.putStrLn . renderLazy . layoutPretty defaultLayoutOptions
  render x

