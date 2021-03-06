-- Random program Synthesizer for C
-- Written by Isitha Subasinghe for ETH Zuerich's Automated Software Testing course
-- Programming rules
-- 1) recursion is allowed when eventual termination is guaranteed
-- 2) Prove non-trivial properties with dependent types where possible (See SAST.Var)
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- Synthesizer for ANSI C
module Synth where

import qualified AST as A
import Control.Lens hiding (index, op)
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Array.IO hiding (index, newArray)
import qualified Data.Map.Strict as M
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import GHC.Int
import qualified SAST as S

data RandError
  = InvalidIndex String
  | NotYetImplemented String
  | InvalidArgument String
  | MissingData String
  | NoFuns String
  | NoVariables String
  | UnknownError String
  | OtherError String

instance Show RandError where
  show (InvalidIndex s) = "An internal program error was encountered, the random integer generated does not match to a valid constructor.\n" ++ s ++ "\n"
  show (NotYetImplemented s) = "This feature has not yet been implemented.\n" ++ s ++ "\n"
  show (InvalidArgument s) = "The supplied argument was invalid.\n" ++ s ++ "\n"
  show (MissingData s) = "This is typically a generator error. Data was missing when it was supposed to be present.\n" ++ s ++ "\n"
  show (NoFuns s) = "A decision to generate a function was made, yet no viable candidates exist\n" ++ s ++ "\n"
  show (UnknownError s) = "Unknown error occurred.\n" ++ s ++ "\n"
  show (NoVariables s) = "This is typically an internal generator error. No valid variables were found\n" ++ s ++ "\n"
  show (OtherError s) = s

type ProgramGenerator = ExceptT RandError (StateT ProgramState IO)

data ProgramState = ProgramState
  { _functions :: M.Map Text S.SFunction,
    _unInitialisedVars :: M.Map Text (S.Var 'S.UnInit),
    _preInitialisedVars :: M.Map Text (S.Var 'S.Init),
    _initialisedVars :: M.Map Text (S.Var 'S.Unk),
    _structs :: M.Map Text A.Struct,
    _statements :: [S.SStatement], -- only valid when generating functions
    _contextIndent :: Int,
    _statementDepth :: Int,
    _expressionDepth :: Int
  }
  deriving (Show)

makeLenses ''ProgramState

maxStatementDepth :: Int
maxStatementDepth = 4

maxExpressionDepth :: Int
maxExpressionDepth = 14

incrementContextIndent :: ProgramState -> ProgramState
incrementContextIndent = over contextIndent (+ 1)

decrementContextIndent :: ProgramState -> ProgramState
decrementContextIndent = over contextIndent (\s -> s - 1)

incrementStatementDepth :: ProgramState -> ProgramState
incrementStatementDepth = over statementDepth (+ 1)

decrementStatementDepth :: ProgramState -> ProgramState
-- it should never be the case where we decrement when its already zero but just in case
decrementStatementDepth = over statementDepth (\d -> if d <= 0 then 0 else d - 1)

incrementExpressionDepth :: ProgramState -> ProgramState
incrementExpressionDepth = over expressionDepth (+ 1)

decrementExpressionDepth :: ProgramState -> ProgramState
decrementExpressionDepth = over expressionDepth (\d -> if d <= 0 then 0 else d - 1)

emptyProgramState :: ProgramState
emptyProgramState =
  ProgramState
    { _functions = M.empty,
      _unInitialisedVars = M.empty,
      _preInitialisedVars = M.empty,
      _initialisedVars = M.empty,
      _structs = M.empty,
      _statements = [],
      _contextIndent = 0,
      _statementDepth = 0,
      _expressionDepth = 0
    }

-- random Integer between characters
randInt :: IO Int
randInt = getStdRandom $ randomR (1, 3)

randIntRange :: (Int, Int) -> IO Int
randIntRange rng = getStdRandom $ randomR rng

randIdent :: IO Text
randIdent = do
  ident <- fmap (take 16 . randomRs ('a', 'z')) newStdGen
  pure $ T.pack ident

randBool :: IO Bool
randBool = randomIO

data RandType
  = Pointer
  | TyInt
  | TyChar
  | TyBool
  | TyFloat
  | TyVoid
  | TyStruct
  deriving (Eq, Show, Enum, Bounded)

-- safe for recursion because this will terminate eventually
randType :: Bool -> Bool -> Bool -> ProgramGenerator A.Type
randType avoidVoid avoidRecurse avoidStruct = do
  let fns = if avoidVoid then filter (/= TyVoid) tyFns else tyFns
  let fns' = if avoidRecurse then filter (/= Pointer) fns else fns
  let fns'' = if avoidRecurse then filter (/= TyStruct) fns' else fns'
  index <- getStdRandom $ randomR (0, length fns'' - 1)
  let ty = fns'' !! index
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

variablesMap :: Lens' ProgramState (M.Map Text (S.Var 'S.UnInit))
variablesMap func pstate@ProgramState {_unInitialisedVars = vs} =
  func vs <&> \newVs -> pstate {_unInitialisedVars = newVs}

functionsMap :: Lens' ProgramState (M.Map Text S.SFunction)
functionsMap func pstate@ProgramState {_functions = fns} =
  func fns <&> \newFns -> pstate {_functions = newFns}

statementsMap :: Lens' ProgramState [S.SStatement]
statementsMap func pstate@ProgramState {_statements = ss} =
  func ss <&> \newSS -> pstate {_statements = newSS}

definedVariablesMap :: Lens' ProgramState (M.Map Text (S.Var 'S.Init))
definedVariablesMap func pstate@ProgramState {_preInitialisedVars = dvs} =
  func dvs <&> \newdVs -> pstate {_preInitialisedVars = newdVs}

insertList :: Ord a => [(a, b)] -> M.Map a b -> M.Map a b
insertList [] m = m
insertList ((a, b) : abVals) m = insertList abVals (M.insert a b m)

addStruct :: ProgramState -> A.Struct -> ProgramState
addStruct pstate st = pstate & structsMap %~ M.insert (A.structName st) st

addVars :: ProgramState -> [S.Var 'S.UnInit] -> ProgramState
addVars pstate vs = pstate & variablesMap %~ insertList (map (\b -> (S.varName b, b)) vs)

addStatement :: ProgramState -> S.SStatement -> ProgramState
addStatement pstate s = pstate & statementsMap %~ (++ [s]) -- use a difference list maybe

clearStatements :: ProgramState -> ProgramState
clearStatements pstate = pstate & statementsMap %~ const []

deleteList :: [Text] -> M.Map Text b -> M.Map Text b
deleteList xs s = foldl (flip M.delete) s xs

deleteVars :: ProgramState -> [S.Var 'S.UnInit] -> ProgramState
deleteVars pstate vs = pstate & variablesMap %~ deleteList (map S.varName vs)

addFunction :: ProgramState -> S.SFunction -> ProgramState
addFunction pstate fn = pstate & functionsMap %~ M.insert (S.sname fn) fn

addVar :: ProgramState -> S.Var 'S.UnInit -> ProgramState
addVar pstate v = pstate & variablesMap %~ M.insert (S.varName v) v

addDefinedVariables :: ProgramState -> [S.Var 'S.Init] -> ProgramState
addDefinedVariables pstate vis = pstate & definedVariablesMap %~ insertList (map (\g -> (S.varName g, g)) vis)

addDefinedVariable :: ProgramState -> S.Var 'S.Init -> ProgramState
addDefinedVariable pstate dv = pstate & definedVariablesMap %~ M.insert (S.varName dv) dv

synthesizeStruct :: ProgramGenerator A.Struct
synthesizeStruct = do
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
synthesizeRepeat 0 _ = pure []
synthesizeRepeat cnt fn = do
  v <- fn
  others <- synthesizeRepeat (cnt - 1) fn
  pure (v : others)

synthesizeGlobalVariable :: ProgramGenerator (S.Var 'S.UnInit)
synthesizeGlobalVariable = S.UnInitialised <$> synthesizeField

synthesizeGlobalVariables :: ProgramGenerator [S.Var 'S.UnInit]
synthesizeGlobalVariables = do
  numGVars <- liftIO randInt
  synthesizeRepeat numGVars synthesizeGlobalVariable

synthesizeSCallExpr :: ProgramGenerator S.SExpr
synthesizeSCallExpr = do
  s <- get
  let fns = s ^. functions
  case M.size fns of
    0 -> throwError $ NoFuns "synthesizeSCallExpr"
    _ -> do
      let fns' = M.toList fns
      index <- liftIO $ randIntRange (0, length fns' - 1)
      let (fnName, fn) = fns' !! index
      let rety = S.sty fn
      let btys = map A.bindType (S.sformals fn)
      exprs <- mapM synthesizeRestrictedExpression btys
      pure (rety, S.SCall fnName exprs)

synthesizeSCastExpr :: ProgramGenerator S.SExpr
synthesizeSCastExpr = do
  ty <- randType True True True
  expr <- synthesizeExpression
  pure (ty, S.SCast ty expr)

toEitherT :: (Monad m) => Maybe a -> e -> ExceptT e m a
toEitherT (Just a) _ = pure a
toEitherT Nothing e = throwError e

randElem :: (MonadError e ProgramGenerator) => [a] -> e -> ProgramGenerator a
randElem [] e = throwError e
randElem as _ = do
  index <- liftIO $ randIntRange (0, length as - 1)
  pure (as !! index)

-- approximately equal
(~=) :: A.Type -> A.Type -> Bool
(~=) (A.TyStruct _) (A.TyStruct _) = True
(~=) (A.Pointer _) (A.Pointer _) = True
(~=) lhs rhs = lhs == rhs

tySearch :: (MonadError e ProgramGenerator) => A.Type -> e -> ProgramGenerator (S.Var 'S.Unk)
tySearch ty e = do
  s <- get
  let tys = map (S.superDowngradeVar . snd) $ M.toList $ s ^. preInitialisedVars
  let otherTys = map (S.superDowngradeVar . snd) $ M.toList $ s ^. initialisedVars

  let tys' = filter (\ty' -> A.bindType ty' ~= ty) (tys ++ otherTys)
  b <- randElem tys' e
  pure $ S.Unknown b

tySearchStructsDFS :: (MonadError e ProgramGenerator) => A.Type -> [([A.Bind], A.Bind)] -> e -> ProgramGenerator ([A.Bind], A.Bind)
tySearchStructsDFS ty bs e = do
  newBS <-
    concat
      <$> mapM
        ( \(path, b) ->
            if A.bindType b == ty
              then pure [(path, b)]
              else do
                s <- get
                let strcts = s ^. structs
                let binds = maybe [] A.structFields (M.lookup (A.bindName b) strcts)
                let pathsAndBinds = map (path ++ [b],) binds
                pure pathsAndBinds
        )
        bs
  let shouldRecurse = any (\(_, b) -> A.bindType b /= ty) newBS
  if shouldRecurse then tySearchStructsDFS ty newBS e else randElem newBS $ OtherError "tySearchStructsDFS:empty"

tySearchStruct :: (MonadError e ProgramGenerator) => A.Type -> e -> ProgramGenerator ([A.Bind], A.Bind)
tySearchStruct ty e = do
  s <- get
  let tys = map (S.superDowngradeVar . snd) $ M.toList $ s ^. preInitialisedVars
  let otherTys = map (S.superDowngradeVar . snd) $ M.toList $ s ^. initialisedVars
  let allTys = map ([],) (tys ++ otherTys)
  tySearchStructsDFS ty allTys e

data Recoverable a = Recoverable (Either RandError (ProgramGenerator ())) (ProgramGenerator a)

runRecoverable :: Recoverable a -> ProgramGenerator a
runRecoverable (Recoverable erfn fn) = do
  s <- get
  let eitherOut = runExceptT fn
  (perr, s') <- liftIO (runStateT eitherOut s)
  case perr of
    Right a -> do
      put s'
      pure a
    Left _ -> do
      case erfn of
        Right rfn -> do
          let eitherOut' = runExceptT rfn
          (perr', s'') <- liftIO (runStateT eitherOut' s)
          case perr' of
            Right _ -> do
              put s''
              fn
            Left e -> do throwError e
        Left e -> throwError e

synthesizeLocalVariable :: A.Type -> ProgramGenerator ()
synthesizeLocalVariable ty = do
  expr <- synthesizeConstant ty
  ident <- liftIO randIdent
  modify (`addStatement` S.SDeclAssign ty ident expr)
  pure ()

data RLVal
  = GenAccess
  | GenDeref
  | GenId
  deriving (Show, Eq, Bounded, Enum)

randomRLVal :: IO RLVal
randomRLVal = do
  toEnum <$> randIntRange (fromEnum (minBound :: RLVal), fromEnum (maxBound :: RLVal))

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n' = newListArray (1, n')

runSynthesizers :: (MonadError e ProgramGenerator) => [ProgramGenerator a] -> e -> ProgramGenerator a
runSynthesizers [] e = throwError e
runSynthesizers (synth : ss) e = do
  s <- get
  let eitherOut = runExceptT synth
  (out, s') <- liftIO $ runStateT eitherOut s
  case out of
    Right val -> do
      put s'
      pure val
    Left _ -> runSynthesizers ss e

synthesizeDeref :: A.Type -> ProgramGenerator S.LValue
synthesizeDeref ty = do
  expr <- synthesizeRestrictedExpression (A.Pointer ty)
  pure $ S.SDeref expr

synthesizeAccess :: A.Type -> ProgramGenerator S.LValue
synthesizeAccess ty = do
  (path, bind) <- tySearchStruct ty $ OtherError "synthesizeAccess"
  let accessPath = map A.bindName (path ++ [bind])
  pure $ S.SAccess accessPath

synthesizeId :: A.Type -> ProgramGenerator S.LValue
synthesizeId ty = do
  v <- tySearch ty $ OtherError "synthesizeId"
  pure $ S.SId (S.varName v)

synthesizeLVal :: A.Type -> ProgramGenerator S.LValue
synthesizeLVal ty = do
  shuffledActions <- case ty of
    (A.TyStruct _) -> do liftIO $ shuffle [synthesizeDeref ty, synthesizeAccess ty, synthesizeId ty]
    _ -> do liftIO $ shuffle [synthesizeDeref ty, synthesizeId ty]

  runSynthesizers shuffledActions $ OtherError "synthesizeLVal"

synthesizeLvalExpr :: A.Type -> ProgramGenerator S.SExpr
synthesizeLvalExpr ty = do
  lval <- synthesizeLVal ty
  pure (ty, S.LVal lval)

synthesizeAssignExpr :: A.Type -> ProgramGenerator S.SExpr
synthesizeAssignExpr ty = do
  expr <- synthesizeRestrictedExpression ty
  lval <- synthesizeLVal ty
  pure (ty, S.SAssign lval expr)

synthesizeAddrExpr :: A.Type -> ProgramGenerator S.SExpr
synthesizeAddrExpr ty = do
  lval <- synthesizeLVal ty
  pure (A.Pointer ty, S.SAddr lval)

synthesizeExpression :: ProgramGenerator S.SExpr
synthesizeExpression = do
  ty <- randType True False False
  modify incrementExpressionDepth
  expr <- synthesizeRestrictedExpression ty
  modify decrementExpressionDepth
  pure expr

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

synthesizeConstant :: A.Type -> ProgramGenerator S.SExpr
synthesizeConstant ty = case ty of
  A.Pointer ty' -> do
    catchError
      ( (\v -> (A.Pointer ty', S.SAddr (S.SId (S.varName v))))
          <$> tySearch ty' (NoVariables "synthesizeConstant:A.Pointer ty'")
      )
      ( \case
          (NoVariables _) ->
            if ty' /= A.TyInt
              then do
                -- cast our NULL pointer to ty
                let expr = S.SCast ty (A.TyInt, S.SNull)
                pure (ty, expr)
              else pure (ty, S.SNull)
          _ -> throwError $ UnknownError "synthesizeConstant"
      )
  A.TyInt -> do
    val <- synthesizeInt
    pure (A.TyInt, S.SLiteral val)
  A.TyBool -> do
    val <- synthesizeBool
    pure (A.TyBool, S.SBoolLit val)
  A.TyChar -> do
    val <- synthesizeChar
    pure (A.TyChar, S.SCharLit val)
  A.TyFloat -> do
    val <- synthesizeFloat
    pure (A.TyFloat, S.SFlit val)
  A.TyVoid -> do
    ty' <- randType True True False
    synthesizeConstant ty'
  A.TyStruct ident -> do
    st <- get
    let s = st ^. structs
    case M.lookup ident s of
      Just x -> do
        m <- traverse (synthesizeConstant . A.bindType) (A.structFields x)
        pure (A.TyStruct ident, S.SStructLit ident m)
      Nothing -> throwError $ MissingData ("Expected " ++ T.unpack ident ++ " to be present but it was not")

synthesizeDefinedGvar :: ProgramGenerator (S.Var 'S.Init)
synthesizeDefinedGvar = do
  field <- synthesizeField
  expr <- synthesizeConstant (A.bindType field)
  let bind = A.Bind {A.bindType = A.bindType field, A.bindName = A.bindName field}
  let gvar = S.Initialised bind expr
  modify (`addDefinedVariable` gvar)
  pure gvar

synthesizeGlobalVariablesInitialised :: ProgramGenerator [S.Var 'S.Init]
synthesizeGlobalVariablesInitialised = do
  numGVars <- liftIO randInt
  synthesizeRepeat numGVars synthesizeDefinedGvar

data ROp
  = GenAdd
  | GenSub
  | GenMult
  | GenDiv
  | GenEqual
  | GenNeq
  | GenLess
  | GenLeq
  | GenGreater
  | GenGeq
  | GenAnd
  | GenOr
  | GenBitAnd
  | GenBitOr
  deriving (Show, Eq, Bounded, Enum)

randROp :: IO ROp
randROp = do
  toEnum <$> randIntRange (fromEnum (minBound :: ROp), fromEnum (maxBound :: ROp))

synthesizeOp :: ProgramGenerator A.Op
synthesizeOp = do
  op <- liftIO randROp
  case op of
    GenAdd -> pure A.Add
    GenSub -> pure A.Sub
    GenMult -> pure A.Mult
    GenDiv -> pure A.Div
    GenEqual -> pure A.Equal
    GenNeq -> pure A.Neq
    GenLess -> pure A.Less
    GenLeq -> pure A.Leq
    GenGreater -> pure A.Greater
    GenGeq -> pure A.Geq
    GenAnd -> pure A.And
    GenOr -> pure A.Or
    GenBitAnd -> pure A.BitAnd
    GenBitOr -> pure A.BitOr

synthesizeUOp :: ProgramGenerator A.Uop
synthesizeUOp = do
  opIndex <- liftIO $ randIntRange (0, 1)
  case opIndex of
    0 -> pure A.Neg
    1 -> pure A.Not
    _ -> throwError $ InvalidIndex "synthesizeUOp"

data RExpr
  = GenConstant
  | GenBinOp
  | GenUOp
  | GenCall
  | GenCast
  | GenLVal
  | GenAssign
  | GenAddr
  deriving (Show, Eq, Enum, Bounded)

randRExpr :: IO RExpr
randRExpr = do
  toEnum <$> randIntRange (fromEnum (minBound :: RExpr), fromEnum (maxBound :: RExpr))

synthesizeRestrictedExpression :: Int -> Int -> A.Type -> ProgramGenerator S.SExpr
synthesizeRestrictedExpression depth maxDepth ty = do
  modify incrementExpressionDepth
  s <- get
  let canDeepen = (s ^. expressionDepth) < maxExpressionDepth
  if not canDeepen
    then do
      synthesizeConstant ty
    else do
      genAction <- liftIO randRExpr
      expr <- case (genAction, ty) of
        (GenConstant, _) -> do
          -- liftIO $ putStrLn "generate constant"
          synthesizeConstant ty
        (GenBinOp, _) -> do
          -- liftIO $ putStrLn "generate binop"
          lhs <- synthesizeRestrictedExpression (depth+1) maxDepth ty
          rhs <- synthesizeRestrictedExpression (depth+1) maxDepth ty
          op <- synthesizeOp
          pure (ty, S.SBinOp op lhs rhs)
        (GenUOp, A.TyStruct _) -> do
          -- recurse because we cannot generate uops for structs
          -- liftIO $ putStrLn "generate struct recurse"
          synthesizeRestrictedExpression (depth+1) maxDepth ty
        (GenUOp, _) -> do
          -- liftIO $ putStrLn "generate uop"
          op <- synthesizeUOp
          expr <- synthesizeRestrictedExpression (depth +1) maxDepth ty
          pure (ty, S.SUnOp op expr)
        (GenCall, _) -> do
          -- call a function
          let fns = filter (\(_, f) -> S.sty f == ty) $ M.toList $ s ^. functions
          case length fns of
            0 -> synthesizeRestrictedExpression ty
            _ -> do
              fnIndex <- liftIO $ randIntRange (0, length fns)
              let (fnName, fn) = fns !! fnIndex
              let sformals = S.sformals fn
              exprs <- mapM (synthesizeRestrictedExpression . A.bindType) sformals
              pure (ty, S.SCall fnName exprs)
        (GenCast, _) -> do
          -- liftIO $ putStrLn "generate cast"
          expr <- synthesizeExpression
          pure (ty, S.SCast ty expr)
        (GenLVal, _) -> do
          -- liftIO $ putStrLn "generate lvalExpr"
          synthesizeLvalExpr ty
        (GenAssign, _) -> do
          -- liftIO $ putStrLn "generate assign"
          synthesizeAssignExpr ty
        (GenAddr, _) -> do
          -- liftIO $ putStrLn "generate addr"
          synthesizeAddrExpr ty
      modify decrementExpressionDepth
      pure expr

data RStatement
  = GenExpr
  | GenBlock
  | GenReturn
  | GenIf
  | GenDoWhile
  | GenWhile
  deriving (Show, Eq, Bounded, Enum)

randomBiasedRStatement :: IO RStatement
randomBiasedRStatement = do
  fromList [(GenExpr, 82 % 100), (GenBlock, 1 % 100), (GenReturn, 4 % 100), (GenIf, 7 % 100), (GenDoWhile, 1 % 100), (GenWhile, 4 % 100)]

randomRStatement :: IO RStatement
randomRStatement = do
  toEnum <$> randIntRange (fromEnum (minBound :: RStatement), fromEnum (maxBound :: RStatement))

synthesizeStatement :: A.Type -> ProgramGenerator S.SStatement
synthesizeStatement ty = do
  genAction <- liftIO randomBiasedRStatement
  s <- case genAction of
    GenExpr -> do
      -- liftIO $ putStrLn "generate S.SExpr"
      S.SExpr <$> synthesizeExpression
    GenBlock -> do S.SBlock <$> synthesizeStatements ty
    GenReturn -> do
      if ty == A.TyVoid
        then do
          pure $ S.SReturn (A.TyVoid, S.SNoExpr)
        else do
          -- liftIO $ putStrLn "generate S.SReturn"
          S.SReturn <$> synthesizeRestrictedExpression ty
    GenIf -> do
      -- liftIO $ putStrLn "generate S.SIf"
      expr <- synthesizeRestrictedExpression A.TyBool
      b1 <- synthesizeStatements ty
      b2 <- synthesizeStatements ty
      pure $ S.SIf expr (S.SBlock b1) (S.SBlock b2)
    GenDoWhile -> do
      -- liftIO $ putStrLn "generate S.SDoWhile"
      expr <- synthesizeRestrictedExpression A.TyBool
      ss <- synthesizeStatements ty
      pure $ S.SDoWhile expr (S.SBlock ss)
    GenWhile -> do
      -- liftIO $ putStrLn "generate S.SWhile"
      expr <- synthesizeRestrictedExpression A.TyBool
      ss <- synthesizeStatements ty
      pure $ S.SWhile expr (S.SBlock ss)
  modify (`addStatement` s)
  pure s

synthesizeStatements :: A.Type -> ProgramGenerator [S.SStatement]
synthesizeStatements ty = do
  s <- get
  let depth = s ^. statementDepth
  if depth >= maxStatementDepth
    then pure []
    else do
      -- liftIO $ putStrLn "synthesizeStatements"
      num <- liftIO randInt
      modify incrementStatementDepth
      ss <- synthesizeRepeat num (synthesizeStatement ty)
      modify decrementStatementDepth
      pure ss

withVariables :: [S.Var 'S.UnInit] -> ProgramGenerator b -> ProgramGenerator b
withVariables vs fn = do
  modify (`addVars` vs)
  out <- fn
  modify (`deleteVars` vs)
  pure out

synthesizeFunction :: ProgramGenerator S.SFunction
synthesizeFunction = do
  -- liftIO $ putStrLn "synthesizeFunction\n\n\n\n"
  ty <- randType False False False
  ident <- liftIO randIdent
  formals <- map S.UnInitialised <$> synthesizeFields
  let formals' = map S.superDowngradeVar formals
  locals <- map S.UnInitialised <$> synthesizeFields
  let locals' = map S.superDowngradeVar locals
  body <- withVariables (formals ++ locals) $ do
    _ <- synthesizeStatements ty
    s <- get
    let stmts = s ^. statements
    let stmt = S.SBlock stmts
    pure stmt
  let fn = S.SFunction ty ident formals' locals' body
  modify (`addFunction` fn)
  modify clearStatements
  pure fn

synthesizeProgram :: ProgramGenerator S.SProgram
synthesizeProgram = do
  strcts <- synthesizeStructs
  vars <- synthesizeGlobalVariables
  modify (`addVars` vars)
  definedVars <- synthesizeGlobalVariablesInitialised
  num <- liftIO randInt
  fns <- synthesizeRepeat num synthesizeFunction
  pure $ S.SProgram strcts vars definedVars fns
