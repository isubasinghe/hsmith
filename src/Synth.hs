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

data RandError
  = InvalidIndex String
  | NotYetImplemented String

instance Show RandError where
  show (InvalidIndex s) = "An internal program error was encountered, the random integer generated does not match to a valid constructor.\n" ++ s ++ "\n"
  show (NotYetImplemented s) = "This feature has not yet been implemented.\n" ++ s ++ "\n"

type ProgramGenerator = ExceptT RandError (StateT ProgramState IO)

data ProgramState = ProgramState
  { _functions :: M.Map Text A.Function,
    _variables :: M.Map (Text, Int) A.Bind,
    _definedVariables :: M.Map (Text, Int) S.GlobalVar,
    _structs :: M.Map Text A.Struct,
    _contextIndent :: Int
  }

makeLenses ''ProgramState

incrementContextIndent :: ProgramState -> ProgramState
incrementContextIndent = over contextIndent (+ 1)

decrementContextIndent :: ProgramState -> ProgramState
decrementContextIndent = over contextIndent (\s -> s - 1)

emptyProgramState =
  ProgramState
    { _functions = M.empty,
      _variables = M.empty,
      _definedVariables = M.empty,
      _structs = M.empty,
      _contextIndent = 0
    }

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
  let fns = if avoidVoid then filter (== TyVoid) tyFns else tyFns
  let fns' = if avoidRecurse then filter (== Pointer) fns else fns
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


variablesMap :: Lens' ProgramState (M.Map (Text, Int) A.Bind)
variablesMap func pstate@ProgramState {_variables = vs} = 
  func vs <&> \newVs -> pstate {_variables = newVs}

definedVariablesMap :: Lens' ProgramState (M.Map (Text, Int) S.GlobalVar)
definedVariablesMap func pstate@ProgramState {_definedVariables = dvs} = 
  func dvs <&> \newdVs -> pstate {_definedVariables = newdVs}

addStruct :: ProgramState -> A.Struct -> ProgramState
addStruct pstate st = pstate & structsMap %~ M.insert (A.structName st) st

addVars :: ProgramState -> [A.Bind] -> ProgramState 
addVars pstate vs = pstate & undefined

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

synthesizeRestrictedExpression :: A.Type -> ProgramGenerator S.SExpr
synthesizeRestrictedExpression ty = undefined

synthesizeStatement :: A.Type -> ProgramGenerator S.SStatement
synthesizeStatement ty = do
  num <- liftIO $ randIntRange (0,5)
  case num of
    0 -> do S.SExpr <$> synthesizeExpression
    1 -> do S.SBlock <$> synthesizeStatements ty
    2 -> do S.SReturn <$> synthesizeRestrictedExpression ty
    3 -> do
      expr <- synthesizeRestrictedExpression A.TyBool
      b1 <- synthesizeStatements ty
      b2 <- synthesizeStatements ty
      pure $ S.SIf expr (S.SBlock b1) (S.SBlock b2)
    4 -> do
      expr <- synthesizeRestrictedExpression A.TyBool
      ss <- synthesizeStatements ty
      pure $ S.SDoWhile expr (S.SBlock ss)
    5 -> do
      expr <- synthesizeRestrictedExpression A.TyBool
      ss <- synthesizeStatements ty
      pure $ S.SWhile expr (S.SBlock ss)
    _ -> throwError $ InvalidIndex $ "synthesizeStatement " ++ show num

synthesizeStatements :: A.Type -> ProgramGenerator [S.SStatement]
synthesizeStatements ty = do
  num <- liftIO randInt
  synthesizeRepeat num (synthesizeStatement ty)

synthesizeFunction :: ProgramGenerator S.SFunction
synthesizeFunction = do
  ty <- randType False False
  ident <- liftIO randIdent
  formals <- synthesizeFields
  locals <- synthesizeFields
  body <- synthesizeStatements ty
  pure $ S.SFunction ty ident formals locals (S.SBlock body)

synthesizeProgram :: ProgramGenerator S.SProgram
synthesizeProgram = do
  strcts <- synthesizeStructs
  vars <- synthesizeGlobalVariables
  definedVars <- synthesizeGlobalVariablesInitialised
  num <- liftIO randInt
  fns <- synthesizeRepeat num synthesizeFunction
  pure (strcts, vars, definedVars, fns)

