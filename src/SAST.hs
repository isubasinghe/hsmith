{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module SAST where

import AST hiding (ty)
import Data.Char (chr)
import Data.Text (Text)
import Prettyprinter

type SExpr = (Type, SExpr')

myindent :: Doc ann -> Doc ann
myindent = indent 2

data SExpr'
  = SLiteral Int
  | SFlit Double
  | SStrLit Text
  | SCharLit Int
  | SBoolLit Bool
  | SStructLit Text [SExpr]
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
  = SDeref SExpr -- deref an expression
  | SAccess LValue Text -- access a struct member
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

data SFunction = SFunction
  { sty :: Type,
    sname :: Text,
    sformals :: [Bind],
    slocals :: [Bind],
    sbody :: SStatement
  }
  deriving (Show, Eq)

prettyStatements :: [SStatement] -> Doc ann
prettyStatements ss = vsep (map (\s -> pretty s <> semi) ss)

instance Pretty SFunction where
  pretty (SFunction {sty = fty, sname = n, sformals = sfms, slocals = scls, sbody = _}) =
    pretty fty <+> pretty n <> lparen <> hsep (punctuate comma (map pretty sfms)) <> rparen <+> lbrace <> line <> myindent (vsep (map (\l -> pretty l <> semi) scls)) <> line <> rbrace

data GlobalVar = GlobalVar {gbindType :: Type, gbindName :: Text, gexp :: SExpr}
  deriving (Show, Eq)

instance Pretty GlobalVar where
  pretty GlobalVar {gbindName = gname, gbindType = fty, gexp = expr} = pretty fty <+> pretty gname <+> "=" <+> pretty (snd expr) <> semi

data SProgram = SProgram [Struct] [Bind] [GlobalVar] [SFunction]

instance Pretty SProgram where
  pretty (SProgram ss bs gs fs) =
    "#include <stdio.h>" <> line <> "#include <stdlib.h>" <> line <> line
      <> vsep (punctuate line [vsep (punctuate line (map pretty ss)), vsep (map (\s -> pretty s <> semi) bs), vsep (map pretty gs), vsep (punctuate line (map pretty fs))])

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
  pretty (SStructLit _ ss) = lbrace <> myindent (vsep (punctuate comma (map (pretty . snd) ss))) <> rbrace

instance Pretty LValue where
  pretty (SDeref s) = "*" <> pretty (snd s)
  pretty (SAccess lval val) = pretty lval <> "[" <> pretty val <> "]"
  pretty (SId ident) = pretty ident

instance Pretty SStatement where
  pretty (SExpr s) = pretty (snd s) <> ";"
  pretty (SBlock ss) = lbrace <> myindent (vsep [pretty s <> semi | s <- ss]) <> rbrace
  pretty (SReturn s) = "return" <+> pretty (snd s) <> semi
  pretty (SIf s s1 s2) =
    "if" <> lparen <> pretty (snd s) <> rparen <+> lbrace <> hardline
      <> myindent (pretty s1)
      <> hardline
      <> rbrace
      <> hardline
      <> "else" <+> lbrace
      <> hardline
      <> myindent (pretty s2)
      <> hardline
      <> rbrace
  pretty (SDoWhile _ _) = undefined
  pretty (SWhile se s) =
    "while" <> lparen <> pretty (snd se) <> rparen <> space <> lbrace
      <> myindent (pretty s)
      <> rbrace
