{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- Synthesizer for ANSI C
module Synth where

import qualified AST as A
import Control.Lens
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Emit
import qualified SAST as S
import System.Random

type ProgramGenerator = StateT ProgramState IO

data BlockContext = BlockContext
  { _fnreturn :: A.Type,
    _fnvariables :: M.Map Text A.Bind,
    _fndefinedVariables :: M.Map Text A.Bind
  }

data ProgramState = ProgramState
  { _functions :: M.Map Text A.Function,
    _variables :: M.Map Text A.Bind,
    _definedVariables :: M.Map String S.GlobalVar,
    _structs :: M.Map Text A.Struct,
    _context :: [BlockContext]  -- for when we enter a block statement
  }

makeLenses ''ProgramState
makeLenses ''BlockContext


maybeHead :: [a] -> Maybe a 
maybeHead [] = Nothing 
maybeHead (a:as) = Just a 

pop :: [a] -> Maybe a 
pop = maybeHead

push :: a -> [a] -> [a] 
push a as = a:as

newBlockContext :: BlockContext -> BlockContext 
newBlockContext b = b { _fndefinedVariables = M.empty }

emptyProgramState = ProgramState 
  { _functions = M.empty
  , _variables = M.empty
  , _definedVariables = M.empty
  , _structs = M.empty
  , _context = [] }

-- random Integer between characters
randInt :: IO Int
randInt = getStdRandom $ randomR (1, 9)

randIntRange :: (Int, Int) -> IO Int
randIntRange rng = getStdRandom $ randomR rng

randIdent :: IO Text
randIdent = do
  ident <- fmap (take 16 . randomRs ('a', 'z')) newStdGen
  pure $ T.pack ident

data RandType
  = Pointer
  | TyInt
  | TyChar
  | TyBool
  | TyFloat
  | TyVoid
  | TyStruct
  deriving (Eq, Show)

randType :: Bool -> Bool -> ProgramGenerator A.Type
randType avoidVoid avoidRecurse = do
  let fns = if avoidVoid then stripTy TyVoid tyFns else tyFns
  let fns' = if avoidRecurse then stripTy Pointer fns else fns
  index <- getStdRandom $ randomR (0, length fns' - 1)
  let ty = fns' !! index
  case ty of
    Pointer -> do
      pty <- randType False True
      pure $ A.Pointer pty
    TyStruct -> do
      s <- get
      let ss = s ^. structs
      let size = M.size ss
      case size of
        0 -> randType avoidVoid avoidRecurse
        _ -> do
          let l = M.toList ss
          mindex <- getStdRandom $ randomR (0, length l - 1)
          let val = fst $ l !! mindex
          pure $ A.TyStruct val
    _ -> pure $ convertTy ty
  where
    tyFns = [Pointer, TyInt, TyChar, TyBool, TyFloat, TyVoid, TyStruct]
    stripTy :: RandType -> [RandType] -> [RandType]
    stripTy ty [] = []
    stripTy ty' (ty : tys) = if ty' == ty then tys else ty : stripTy ty' tys
    convertTy :: RandType -> A.Type
    convertTy rty =
      case rty of
        TyInt -> A.TyInt
        TyChar -> A.TyChar
        TyBool -> A.TyBool
        TyFloat -> A.TyFloat
        TyVoid -> A.TyVoid
        _ -> undefined --- already handled by above code

synthesizeField :: ProgramGenerator A.Bind
synthesizeField = do
  ident <- liftIO randIdent
  ty <- randType True False
  pure $ A.Bind {A.bindType = ty, A.bindName = ident}

synthesizeFields :: ProgramGenerator [A.Bind]
synthesizeFields = do
  numFields <- liftIO randInt
  synthesizeRepeat numFields synthesizeField

structsMap :: Lens' ProgramState (M.Map Text A.Struct)
structsMap func pstate@ProgramState {_structs = s} =
  func s <&> \newS -> pstate {_structs = newS}

addStruct :: ProgramState -> A.Struct -> ProgramState
addStruct pstate st = pstate & structsMap %~ M.insert (A.structName st) st

synthesizeStruct :: ProgramGenerator A.Struct
synthesizeStruct = do
  numFields <- liftIO randInt
  structName <- liftIO randIdent
  structFields <- synthesizeFields
  let struct = A.Struct {A.structName = structName, A.structFields = structFields}
  modify (`addStruct` struct)
  pure struct

synthesizeStructs :: ProgramGenerator [A.Struct]
synthesizeStructs = do
  cnt <- liftIO randInt
  synthesizeRepeat cnt synthesizeStruct

synthesizeRepeat :: Int -> ProgramGenerator a -> ProgramGenerator [a]
synthesizeRepeat 0 fn = pure []
synthesizeRepeat cnt fn = do
  v <- fn
  others <- synthesizeRepeat (cnt - 1) fn
  pure (v : others)

synthesizeGlobalVariable :: ProgramGenerator A.Bind
synthesizeGlobalVariable = do
  bind <- synthesizeField
  undefined

synthesizeGlobalVariables :: ProgramGenerator [A.Bind]
synthesizeGlobalVariables = do
  numGVars <- liftIO randInt
  synthesizeRepeat numGVars synthesizeField

synthesizeExpression :: ProgramGenerator S.SExpr
synthesizeExpression = undefined

synthesizeInt :: ProgramGenerator Int
synthesizeInt = do
  liftIO randomIO

synthesizeBool :: ProgramGenerator Bool
synthesizeBool = do
  liftIO randomIO

synthesizeChar :: ProgramGenerator Int
synthesizeChar = do
  liftIO $ randIntRange (0, 255)

synthesizeFloat :: ProgramGenerator Double
synthesizeFloat = do
  liftIO randomIO

synthesizeConstant :: A.Type -> ProgramGenerator [S.SExpr]
synthesizeConstant ty = case ty of
  A.Pointer ty' -> do
    ty' <- randType True True
    synthesizeConstant ty'
  A.TyInt -> do
    val <- synthesizeInt
    pure [(A.TyInt, S.SLiteral val)]
  A.TyBool -> do
    val <- synthesizeBool
    pure [(A.TyBool, S.SBoolLit val)]
  A.TyChar -> do
    val <- synthesizeChar
    pure [(A.TyChar, S.SCharLit val)]
  A.TyFloat -> do
    val <- synthesizeFloat
    pure [(A.TyFloat, S.SFlit val)]
  A.TyVoid -> do
    ty' <- randType True True
    synthesizeConstant ty'
  A.TyStruct ident -> do
    st <- get
    let s = st ^. structs
    let struct = fromMaybe undefined (M.lookup ident s) -- undefined is safe here always, if we get a A.TyStruct that struct must have been inserted
    tys <- traverse (synthesizeConstant . A.bindType) $ A.structFields struct
    pure $ concat tys

synthesizeDefinedGvar :: ProgramGenerator S.GlobalVar
synthesizeDefinedGvar = do
  field <- synthesizeField
  expr <- synthesizeConstant (A.bindType field)
  pure S.GlobalVar {S.gbindType = A.bindType field, S.gbindName = A.bindName field, S.gexp = expr}

synthesizeGlobalVariablesInitialised :: ProgramGenerator [S.GlobalVar]
synthesizeGlobalVariablesInitialised = do
  numGVars <- liftIO randInt
  synthesizeRepeat numGVars synthesizeDefinedGvar

synthesizeStatement :: ProgramGenerator S.SStatement
synthesizeStatement = do
  undefined

synthesizeStatements :: ProgramGenerator [S.SStatement]
synthesizeStatements = do 
  st <- get 
  let fnreturn = maybeHead $ st^.context 
  undefined

synthesizeFunction :: ProgramGenerator S.SFunction
synthesizeFunction = do
  ty <- randType False False
  ident <- liftIO randIdent
  formals <- synthesizeFields
  locals <- synthesizeFields
  let newBlockContext = BlockContext { _fnreturn = ty, _fnvariables = M.empty, _fndefinedVariables = M.empty }
  undefined


synthesizeProgram :: ProgramGenerator S.SProgram 
synthesizeProgram = do 
  strcts <- synthesizeStructs
  vars <- synthesizeGlobalVariables
  definedVars <- synthesizeGlobalVariablesInitialised
  pure (strcts, vars, definedVars, [])
