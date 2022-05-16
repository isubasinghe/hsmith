{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module AST where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

data Op
  = Add
  | Sub
  | Mult
  | Div
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | BitAnd
  | BitOr
  deriving (Show, Eq)

instance Pretty Op where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mult = "*"
  pretty Div = "/"
  pretty Equal = "=="
  pretty Neq = "!="
  pretty Less = "<"
  pretty Leq = "<="
  pretty Greater = ">"
  pretty Geq = ">="
  pretty And = "&&"
  pretty Or = "||"
  pretty BitAnd = "&"
  pretty BitOr = "|"

data Uop
  = Neg
  | Not
  deriving (Show, Eq)

instance Pretty Uop where
  pretty Neg = "-"
  pretty Not = "!"

data Expr
  = Literal Int
  | StrLit Text
  | CharLit Int
  | FLit Double
  | BoolLit Bool
  | Null
  | Id Text
  | BinOp Op Expr Expr
  | Unop Uop Expr
  | Call Text [Expr]
  | Cast Type Expr
  | Access Expr Expr
  | Deref Expr
  | Addr Expr
  | Assign Expr Expr
  | Sizeof Type
  | Noexpr
  deriving (Show, Eq)

data Statement
  = Expr Expr
  | Block [Statement]
  | Return Expr
  | If Expr Statement Statement
  | For Expr Expr Expr Statement
  | While Expr Statement
  deriving (Show, Eq)

data Type
  = Pointer Type
  | TyInt
  | TyBool
  | TyChar
  | TyFloat
  | TyVoid
  | TyStruct Text
  deriving (Show, Eq)

instance Pretty Type where
  pretty (Pointer ty') = pretty ty' <+> "*"
  pretty TyInt = "int"
  pretty TyBool = "char"
  pretty TyChar = "char"
  pretty TyFloat = "float"
  pretty TyVoid = "void"
  pretty (TyStruct ident) = "struct" <+> pretty (T.unpack ident)

data Bind = Bind {bindType :: Type, bindName :: Text}
  deriving (Show, Eq)

class Binder a where 
  getType :: a -> Type 
  getName :: a -> Text

instance Binder Bind where 
  getType = bindType 
  getName = bindName 

instance Pretty Bind where
  pretty Bind {bindType = bty, bindName = bname} = pretty bty <> space <> pretty (T.unpack bname)

data Struct = Struct {structName :: Text, structFields :: [Bind]}
  deriving (Show, Eq)

instance Pretty Struct where
  pretty Struct {structName = sname, structFields = fs} =
    "struct" <+> pretty (T.unpack sname) <+> lbrace <> line
      <> indent 4 (vsep (map (\s -> pretty s <> semi) fs))
      <> line
      <> rbrace
      <> semi

data Function = Function
  { ty :: Type,
    name :: Text,
    formals :: [Bind],
    locals :: [Bind],
    body :: [Statement]
  }
  deriving (Show, Eq)

data Program = Program [Struct] [Bind] [Function]
  deriving (Show, Eq)
