{-# LANGUAGE OverloadedStrings #-}

module SAST where

import AST
import Data.Char (chr)
import Data.Text (Text)
import Prettyprinter

type SExpr = (Type, SExpr')

data SExpr'
  = SLiteral Int
  | SFlit Double
  | SStrLit Text
  | SCharLit Int
  | SBoolLit Bool
  | SNull
  | SBinOp Op SExpr SExpr
  | SUnOp Uop SExpr
  | SCall Text [SExpr]
  | SCast Type SExpr
  | LVal LValue
  | SAssign LValue SExpr
  | SAddr LValue
  | SSizeof Type
  | SNoexpr
  deriving (Show, Eq)


data LValue
  = SDeref SExpr
  | SAccess LValue Int
  | SId Text
  deriving (Show, Eq)

data SStatement
  = SExpr SExpr
  | SBlock [SStatement]
  | SReturn SExpr
  | SIf SExpr SStatement SStatement
  | SDoWhile SExpr SStatement
  | SWhile SExpr SStatement
  deriving (Show, Eq)

exampleSExpr' = SLiteral 10
exampleSExpr = (TyInt, exampleSExpr')
exampleStatement = SIf exampleSExpr (SExpr exampleSExpr) (SExpr exampleSExpr)

data SFunction = SFunction
  { sty :: Type,
    sname :: Text,
    sformals :: [Bind],
    slocals :: [Bind],
    sbody :: SStatement
  }
  deriving (Show, Eq)

instance Pretty SFunction where
  pretty (SFunction {sty = s}) = undefined

data GlobalVar = GlobalVar {gbindType :: Type, gbindName :: Text, gexp :: [SExpr]} -- list needed for structs
  deriving (Show, Eq)

instance Pretty GlobalVar where
  pretty g = undefined

type SProgram = ([Struct], [Bind], [GlobalVar], [SFunction])

type Name = Text

data BindingLoc = F Function | S Struct | TopLevel
  deriving (Show, Eq)

data SemantError
  = IllegalBinding Name BindingKind VarKind BindingLoc
  | UndefinedSymbol Name SymbolKind Expr
  | TypeError {expected :: [Type], got :: Type, errorLoc :: Statement}
  | CastError {to :: Type, from :: Type, castLoc :: Statement}
  | ArgError {nExpected :: Int, nGot :: Int, callSite :: Expr}
  | Redeclaration Name
  | NoMain
  | AddressError Expr
  | AssignmentError {lhs :: Expr, rhs :: Expr}
  | AccessError {struct :: Expr, field :: Expr}
  | DeadCode Statement
  deriving (Show, Eq)

data BindingKind = Duplicate | Void deriving (Show, Eq)

data SymbolKind = Var | Func deriving (Show, Eq)

data VarKind = Global | Formal | Local | StructField
  deriving (Show, Eq, Ord)

instance Pretty SExpr' where
  pretty (SLiteral val) = pretty val
  pretty (SFlit val) = pretty val
  pretty (SStrLit val) = "\"" <> pretty val <> "\""
  pretty (SCharLit val) = "'" <> pretty (chr val) <> "'"
  pretty (SBoolLit b) = if b then "1" else "0"
  pretty SNull = "NULL"
  pretty (SBinOp op s1 s2) = pretty (snd s1) <+> pretty op <+> pretty (snd s2)
  pretty (SUnOp op s) = pretty op <> pretty (snd s)
  pretty (SCall ident params) = pretty ident <> encloseSep lparen rparen comma (map (\s -> pretty (fst s) <+> pretty (snd s)) params)
  pretty (SCast ty s) = parens (pretty ty) <> pretty (snd s)
  pretty (LVal lval) = pretty lval
  pretty (SAssign lval s) = pretty lval <+> equals <+> pretty (snd s)
  pretty (SAddr lval) = "&" <> pretty lval
  pretty (SSizeof ty) = parens "sizeof" <> pretty ty
  pretty SNoexpr = mempty

instance Pretty LValue where
  pretty (SDeref s) = "*" <> pretty (snd s)
  pretty (SAccess lval val) = pretty lval <> "[" <> pretty val <> "]"
  pretty (SId ident) = viaShow ident

instance Pretty SStatement where
  pretty (SExpr s) = pretty (snd s) <> ";"
  pretty (SBlock ss) = lbrace <> nest 4 (vsep [pretty s <> semi | s <- ss]) <> rbrace
  pretty (SReturn s) = "return" <+> pretty (snd s) <> semi
  pretty (SIf s s1 s2) =
    "if" <> lparen <> pretty (snd s) <> rparen <+> lbrace <> hardline 
      <> indent 2 (pretty s1) <> hardline
      <> rbrace <> hardline
      <> "else" <+> lbrace <> hardline
      <> indent 2 (pretty s2) <> hardline
      <> rbrace
  pretty (SDoWhile se s) = undefined
  pretty (SWhile se s) =
    "while" <> lparen <> pretty (snd se) <> rparen <> lbrace
      <> nest 4 (pretty s)
      <> rbrace
