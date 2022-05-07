module Main where

import Text.Pretty.Simple (pPrint)
import Synth
import Control.Monad.State.Lazy
import Control.Monad.Except
import SAST
import Prettyprinter
import Prettyprinter.Render.Text
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do 
  let eitherOut =  runExceptT synthesizeProgram
  (out, _) <- runStateT eitherOut emptyProgramState 
  case out of 
    Right program -> do 
      render $ pretty program 
    Left e -> print e
  where 
    render = TIO.putStrLn . renderLazy . layoutPretty defaultLayoutOptions

