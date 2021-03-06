{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
  | SNoExpr
  deriving (Show, Eq)

exampleSExpr' = SLiteral 2

exampleSExpr = (TyInt, exampleSExpr')

data LValue
  = SDeref SExpr -- deref an expression
  | SAccess [Text]
  | SId Text
  deriving (Show, Eq)

data SStatement
  = SExpr SExpr
  | SBlock [SStatement]
  | SReturn SExpr
  | SIf SExpr SStatement SStatement
  | SDeclAssign Type Text SExpr
  | SDoWhile SExpr SStatement
  | SWhile SExpr SStatement
  deriving (Show, Eq)

exampleSExprBool :: SExpr
exampleSExprBool = (TyBool, (SBinOp Less (TyInt, (SLiteral 2)) (TyInt, (SLiteral 3))))

exampleStatement1 =
  SBlock
    [ SExpr exampleSExprBool,
      SWhile exampleSExprBool (SBlock [])
    ]

exampleStatement2 =
  SBlock
    [ SExpr exampleSExprBool,
      SReturn (TyInt, SLiteral 0)
    ]

exampleIf = SIf exampleSExprBool exampleStatement1 exampleStatement2

exampleDecl = SDeclAssign TyInt "variable" exampleSExpr

exampleStatements =
  SBlock
    [ SExpr exampleSExpr,
      exampleIf,
      exampleDecl,
      SWhile exampleSExprBool (SBlock [])
    ]

data SFunction = SFunction
  { sty :: Type,
    sname :: Text,
    sformals :: [Bind],
    slocals :: [Bind],
    sbody :: SStatement
  }
  deriving (Show, Eq)

exampleFormals = [Bind {bindName="argc", bindType=TyInt}, Bind{bindName="argv", bindType=Pointer (Pointer TyChar)} ]

exampleFunction = SFunction {sty = TyInt, sname ="main", sformals=exampleFormals, slocals=[], sbody=exampleStatements}

prettyStatements :: [SStatement] -> Doc ann
prettyStatements ss = vsep (map (\s -> pretty s <> semi) ss)

instance Pretty SFunction where
  pretty (SFunction {sty = fty, sname = n, sformals = sfms, slocals = scls, sbody = stmts}) =
    pretty fty <+> pretty n <> lparen <> hsep (punctuate comma (map pretty sfms)) <> rparen <> hardline <> lbrace
      <> hardline 
      <> myindent (vsep (map (\l -> pretty l <> semi) scls))
      <> line
      <> myindent (pretty stmts)
      <> line 
      <> rbrace

data VarTy = Init | Unk | UnInit deriving (Show, Eq)

data Var (n :: VarTy) where
  Initialised :: Bind -> SExpr -> Var 'Init
  UnInitialised :: Bind -> Var 'UnInit
  Unknown :: Bind -> Var 'Unk

instance Show (Var n) where
  show (Initialised b s) = show b ++ " : " ++ show s
  show (UnInitialised b) = show b
  show (Unknown b) = show b

varName :: Var n -> Text
varName (Initialised b _) = bindName b
varName (UnInitialised b) = bindName b
varName (Unknown b) = bindName b

varType :: Var n -> Type
varType (Initialised b _) = bindType b
varType (UnInitialised b) = bindType b
varType (Unknown b) = bindType b

downgradeVar :: Var 'Init -> Var 'Unk
downgradeVar (Initialised b _) = Unknown b

superDowngradeVar :: Var n -> Bind
superDowngradeVar (Initialised b _) = b
superDowngradeVar (UnInitialised b) = b
superDowngradeVar (Unknown b) = b

data SProgram = SProgram [Struct] [Var 'UnInit] [Var 'Init] [SFunction] deriving (Show)


exampleProgram = SProgram [] [] [] [exampleFunction]

instance Pretty SProgram where
  pretty (SProgram ss bs gs fs) =
    "#include <stdio.h>" <> line <> "#include <stdlib.h>" <> line <> "#undef NULL" <> line <> "#define NULL ((int*)0)" <> line
      <> vsep (punctuate line [vsep (punctuate line (map pretty ss)), mapPrinter bs, mapPrinter gs, vsep (punctuate line (map pretty fs))])
    where
      mapPrinter vs = vsep (punctuate line (map (\v -> pretty v <> semi) vs))

type Name = Text

instance Pretty (Var n) where
  pretty (Initialised b e) = pretty b <+> "=" <+> pretty (snd e)
  pretty (Unknown b) = pretty b
  pretty (UnInitialised b) = pretty b

data BindingLoc = F Function | S Struct | TopLevel
  deriving (Show, Eq)

data BindingKind = Duplicate | Void deriving (Show, Eq)

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
  pretty (SStructLit _ ss) = lbrace <> myindent (vsep (punctuate comma (map (pretty . snd) ss))) <> rbrace
  pretty SNoExpr = mempty

instance Pretty LValue where
  pretty (SDeref s) = "*" <> pretty (snd s)
  pretty (SAccess bs) = hcat $ punctuate "." (map pretty bs)
  pretty (SId ident) = pretty ident

instance Pretty SStatement where
  pretty (SExpr s) = pretty (snd s) <> ";"
  pretty (SBlock ss) = lbrace <> hardline <> myindent (vsep [pretty s | s <- ss]) <> hardline <> rbrace 
  pretty (SReturn s) = "return" <+> pretty (snd s) <> semi
  pretty (SIf s s1 s2) =
    "if" <> lparen <> pretty (snd s) <> rparen <> hardline
      <> pretty s1
      <> hardline
      <> "else" 
      <> hardline 
      <> pretty s2
      <> hardline
  pretty (SDoWhile expr body') = "do" <> hardline <> pretty body' <> "while" <> lparen <> pretty (snd expr) <> rparen <> semi
  pretty (SWhile se s) =
    "while" <> lparen <> pretty (snd se) <> rparen <> hardline <> pretty s
  pretty (SDeclAssign sdaTy sdaName expr) = pretty sdaTy <+> pretty sdaName <+> pretty (snd expr) <> semi
