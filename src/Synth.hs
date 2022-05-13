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
import GHC.Int
import qualified SAST as S
import System.Random

data RandError
  = InvalidIndex String
  | NotYetImplemented String
  | InvalidArgument String
  | MissingData String

instance Show RandError where
  show (InvalidIndex s) = "An internal program error was encountered, the random integer generated does not match to a valid constructor.\n" ++ s ++ "\n"
  show (NotYetImplemented s) = "This feature has not yet been implemented.\n" ++ s ++ "\n"
  show (InvalidArgument s) = "The supplied argument was invalid.\n" ++ s ++ "\n"
  show (MissingData s) = "This is typically a generator error. Data was missing when it was supposed to be present.\n" ++ s ++ "\n"

type ProgramGenerator = ExceptT RandError (StateT ProgramState IO)

data ProgramState = ProgramState
  { _functions :: M.Map Text A.Function,
    _variables :: M.Map Text A.Bind,
    _definedVariables :: M.Map Text S.GlobalVar,
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
randInt = getStdRandom $ randomR (1, 30)

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
  deriving (Eq, Show, Enum, Bounded)

randType :: Bool -> Bool -> Bool -> ProgramGenerator A.Type
randType avoidVoid avoidRecurse avoidStruct = do
  let fns = if avoidVoid then filter (/= TyVoid) tyFns else tyFns
  let fns = if avoidRecurse then filter (/= Pointer) fns else fns
  let fns = if avoidRecurse then filter (/= TyStruct) fns else fns
  index <- getStdRandom $ randomR (0, length fns - 1)
  let ty = fns !! index
  case ty of
    Pointer -> A.Pointer <$> randType False True False
    TyStruct -> do
      s <- get
      let ss = s ^. structs
      let size = M.size ss
      case size of
        0 -> randType avoidVoid avoidRecurse avoidStruct
        _ -> do
          let l = M.toList ss
          mindex <- getStdRandom $ randomR (0, length l - 1)
          let val = fst $ l !! mindex
          pure $ A.TyStruct val
    TyInt -> pure A.TyInt
    TyChar -> pure A.TyChar
    TyBool -> pure A.TyBool
    TyFloat -> pure A.TyFloat
    TyVoid -> pure A.TyVoid
  where
    tyFns = [Pointer, TyInt, TyChar, TyBool, TyFloat, TyVoid, TyStruct]

synthesizeField :: ProgramGenerator A.Bind
synthesizeField = do
  ident <- liftIO randIdent
  ty <- randType True False False
  pure $ A.Bind {A.bindType = ty, A.bindName = ident}

synthesizeFields :: ProgramGenerator [A.Bind]
synthesizeFields = do
  numFields <- liftIO randInt
  synthesizeRepeat numFields synthesizeField

structsMap :: Lens' ProgramState (M.Map Text A.Struct)
structsMap func pstate@ProgramState {_structs = s} =
  func s <&> \newS -> pstate {_structs = newS}

variablesMap :: Lens' ProgramState (M.Map Text A.Bind)
variablesMap func pstate@ProgramState {_variables = vs} =
  func vs <&> \newVs -> pstate {_variables = newVs}

definedVariablesMap :: Lens' ProgramState (M.Map Text S.GlobalVar)
definedVariablesMap func pstate@ProgramState {_definedVariables = dvs} =
  func dvs <&> \newdVs -> pstate {_definedVariables = newdVs}

insertList :: Ord a => [(a, b)] -> M.Map a b -> M.Map a b
insertList [] m = m
insertList ((a, b) : abs) m = insertList abs (M.insert a b m)

addStruct :: ProgramState -> A.Struct -> ProgramState
addStruct pstate st = pstate & structsMap %~ M.insert (A.structName st) st

addVars :: ProgramState -> [A.Bind] -> ProgramState
addVars pstate vs = pstate & variablesMap %~ insertList (map (\b -> (A.bindName b, b)) vs)

addVar :: ProgramState -> A.Bind -> ProgramState
addVar pstate v = pstate & variablesMap %~ M.insert (A.bindName v) v

addDefinedVariables :: ProgramState -> [S.GlobalVar] -> ProgramState
addDefinedVariables pstate dvs = pstate & definedVariablesMap %~ insertList (map (\g -> (S.gbindName g, g)) dvs)

addDefinedVariable :: ProgramState -> S.GlobalVar -> ProgramState
addDefinedVariable pstate dv = pstate & definedVariablesMap %~ M.insert (S.gbindName dv) dv

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
synthesizeGlobalVariable = synthesizeField

synthesizeGlobalVariables :: ProgramGenerator [A.Bind]
synthesizeGlobalVariables = do
  numGVars <- liftIO randInt
  synthesizeRepeat numGVars synthesizeGlobalVariable

synthesizeSCallExpr :: ProgramGenerator S.SExpr
synthesizeSCallExpr = do undefined

synthesizeSCastExpr :: ProgramGenerator S.SExpr
synthesizeSCastExpr = do undefined

synthesizeLvalExpr :: ProgramGenerator S.SExpr
synthesizeLvalExpr = do undefined

synthesizeAssignExpr :: ProgramGenerator S.SExpr
synthesizeAssignExpr = do undefined

synthesizeAddrExpr :: ProgramGenerator S.SExpr
synthesizeAddrExpr = do undefined

synthesizeExpression :: ProgramGenerator S.SExpr
synthesizeExpression = do
  index <- liftIO $ randIntRange (0, 1)
  case index of
    0 -> (\s -> (A.TyInt, S.SLiteral s)) <$> synthesizeInt
    1 -> (\s -> (A.TyFloat, S.SFlit s)) <$> synthesizeFloat
    2 -> (\s -> (A.Pointer A.TyChar, S.SStrLit s)) <$> liftIO randIdent
    3 -> (\s -> (A.TyChar, S.SCharLit s)) <$> synthesizeChar
    4 -> (\s -> (A.TyBool, S.SBoolLit s)) <$> synthesizeBool
    5 -> pure (A.TyVoid, S.SNull)
    6 -> do
      opIndex <- liftIO $ randIntRange (0, 13)
      ty <- randType True True True
      op <- case opIndex of
        0 -> pure A.Add
        1 -> pure A.Sub
        2 -> pure A.Mult
        3 -> pure A.Div
        4 -> pure A.Equal
        5 -> pure A.Neq
        6 -> pure A.Less
        7 -> pure A.Leq
        8 -> pure A.Greater
        9 -> pure A.Geq
        10 -> pure A.And
        11 -> pure A.Or
        12 -> pure A.BitAnd
        13 -> pure A.BitOr
        _ -> throwError $ InvalidIndex "synthesizeExpression:opIndex"
      lhs <- synthesizeRestrictedExpression ty
      rhs <- synthesizeRestrictedExpression ty
      pure (ty, S.SBinOp op lhs rhs)
    7 -> do
      opIndex <- liftIO $ randIntRange (0, 1)
      ty <- randType True True True
      op <- case opIndex of
        0 -> pure A.Neg
        1 -> pure A.Not
        _ -> throwError $ InvalidIndex ""
      expr <- synthesizeRestrictedExpression ty
      pure (ty, S.SUnOp op expr)
    8 -> synthesizeSCallExpr
    9 -> synthesizeSCastExpr
    10 -> synthesizeLvalExpr
    11 -> synthesizeAssignExpr
    12 -> synthesizeAddrExpr
    13 -> do
      ty <- randType True False False
      pure (ty, S.SSizeof ty)
    14 -> pure (A.TyVoid, S.SNoexpr)
    _ -> throwError $ InvalidIndex "synthesizeExpression"

synthesizeDerefLVal :: ProgramGenerator S.LValue
synthesizeDerefLVal = do undefined

synthesizeAccessLVal :: ProgramGenerator S.LValue
synthesizeAccessLVal = do undefined

synthesizeIdLVal :: ProgramGenerator S.LValue
synthesizeIdLVal = do undefined

synthesizeLVal :: ProgramGenerator S.LValue
synthesizeLVal = do
  index <- liftIO $ randIntRange (0, 2)
  case index of
    0 -> synthesizeDerefLVal
    1 -> synthesizeAccessLVal
    2 -> synthesizeAccessLVal
    _ -> throwError $ InvalidIndex ""

synthesizeInt32 :: ProgramGenerator Int32
synthesizeInt32 = liftIO randomIO

synthesizeInt :: ProgramGenerator Int
synthesizeInt = fromIntegral <$> synthesizeInt32

synthesizeBool :: ProgramGenerator Bool
synthesizeBool = do
  liftIO randomIO

synthesizeChar :: ProgramGenerator Int
synthesizeChar = do
  liftIO $ randIntRange (0, 127)

synthesizeFloat :: ProgramGenerator Double
synthesizeFloat = do
  liftIO randomIO

synthesizeConstant :: A.Type -> ProgramGenerator [S.SExpr]
synthesizeConstant ty = case ty of
  A.Pointer ty' -> do
    s <- get
    let gvars = s ^. definedVariables
    let validGVars = filter (\(_, gvar) -> S.gbindType gvar == ty') (M.toList gvars)
    case length validGVars of
      0 -> pure [(A.TyVoid, S.SNull)]
      _ -> do
        index <- liftIO $ randIntRange (0, length validGVars - 1)
        let (ident, _) = validGVars !! index
        pure [(A.Pointer ty', S.SAddr (S.SId ident))]
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
    ty' <- randType True True False
    synthesizeConstant ty'
  A.TyStruct ident -> do
    st <- get
    let s = st ^. structs
    case M.lookup ident s of
      Just x -> concat <$> traverse (synthesizeConstant . A.bindType) (A.structFields x)
      Nothing -> throwError $ MissingData ("Expected " ++ T.unpack ident ++ " to be present but it was not")

synthesizeDefinedGvar :: ProgramGenerator S.GlobalVar
synthesizeDefinedGvar = do
  field <- synthesizeField
  expr <- synthesizeConstant (A.bindType field)
  let gvar = S.GlobalVar {S.gbindType = A.bindType field, S.gbindName = A.bindName field, S.gexp = expr}
  modify (`addDefinedVariable` gvar)
  pure gvar

synthesizeGlobalVariablesInitialised :: ProgramGenerator [S.GlobalVar]
synthesizeGlobalVariablesInitialised = do
  numGVars <- liftIO randInt
  synthesizeRepeat numGVars synthesizeDefinedGvar

synthesizeRestrictedExpression :: A.Type -> ProgramGenerator S.SExpr
synthesizeRestrictedExpression ty = case ty of
  A.TyInt -> undefined
  A.TyChar -> undefined
  A.TyFloat -> undefined
  A.TyBool -> undefined
  A.TyVoid -> undefined
  A.TyStruct ident -> undefined
  A.Pointer ty -> undefined

withVariable :: A.Bind -> ProgramGenerator a -> ProgramGenerator a
withVariable var a = do
  modify (`addVar` var)
  a

withShiftedDefinedVariable :: Text -> ProgramGenerator a -> ProgramGenerator a
withShiftedDefinedVariable ident a = undefined

synthesizeStatement :: A.Type -> ProgramGenerator S.SStatement
synthesizeStatement ty = do
  num <- liftIO $ randIntRange (0, 5)
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
  ty <- randType False False False
  ident <- liftIO randIdent
  formals <- synthesizeFields
  locals <- synthesizeFields
  -- body <- synthesizeStatements ty
  pure $ S.SFunction ty ident formals locals (S.SBlock [])

synthesizeProgram :: ProgramGenerator S.SProgram
synthesizeProgram = do
  strcts <- synthesizeStructs
  vars <- synthesizeGlobalVariables
  modify (`addVars` vars)
  definedVars <- synthesizeGlobalVariablesInitialised
  num <- liftIO randInt
  fns <- synthesizeRepeat num synthesizeFunction
  pure $ S.SProgram strcts vars definedVars fns
