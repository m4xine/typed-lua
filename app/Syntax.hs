module Syntax 
  ( Param(..)
  , RecordLabel(..)
  , RecordType(..)
  , Type(..)
  , RecordField(..)
  , RecordValue(..)
  , RecordSelect(..)
  , Expr(..)
  , LocalDef(..)
  , Stmt(..)
  , FunDef(..)
  , Def(..)
  , Ast(..)
  ) where

import qualified Data.Text          as T 
import           Source                   (SrcSpan, Source)

-- | Parameter representation
data Param = MkParam
  { paramNameSpan :: SrcSpan
  , paramName     :: T.Text
  , paramTypeSpan :: Maybe SrcSpan
  , paramType     :: Type
  } deriving Show 

-- | Record label with type representation.
data RecordLabel = MkRecordLabel
  { recordLabelNameSpan :: SrcSpan
  , recordLabelName     :: T.Text
  , recordLabelTypeSpan :: Maybe SrcSpan
  , recordLabelType     :: Type 
  } deriving Show 

-- | Record type representation.
newtype RecordType = MkRecordType
  { recordTypeLabels :: [RecordLabel]
  } deriving Show 

-- | Type representation.
data Type 
  = TRecord   RecordType
  | TArray    Type 
  | TString
  | TName     T.Text
  | TDynamic
  | TUnit
  deriving Show 

-- | Record field representation.
data RecordField = MkRecordField
  { recordFieldNameSpan   :: SrcSpan 
  , recordFieldName       :: T.Text
  , recordFieldValueSpan  :: SrcSpan
  , recordFieldValue      :: Expr 
  } deriving Show

-- | Record value/constructor representation.
newtype RecordValue = MkRecordValue
  { recordValueFields :: [RecordField]
  } deriving Show 

-- | Record selection representation.
data RecordSelect = MkRecordSelect
  { recordSelectRecord    :: Expr
  , recordSelectLabelSpan :: SrcSpan 
  , recordSelectLabel     :: T.Text 
  } deriving Show 

-- | Expression representation.
data Expr
  = ERecord RecordValue
  | ESelect RecordSelect
  | EInvoke Expr [Expr]
  | EString T.Text 
  | EName   T.Text 
  | EUnit
  deriving Show 

-- | Local variable definition representation.
data LocalDef = MkLocalDef 
  { localDefSpan      :: SrcSpan
  , localDefName      :: T.Text
  , localDefTypeSpan  :: Maybe SrcSpan
  , localDefType      :: Maybe Type
  , localDefValSpan   :: SrcSpan
  , localDefVal       :: Expr 
  } deriving Show 

-- | Statement representation.
data Stmt
  = SLocalDef LocalDef
  | SReturn   Expr
  | Stmt      Expr -- ^ Expression terminated with a newline or semicolon
  deriving Show 

-- | Function definition representation. 
data FunDef = MkFunDef
  { funDefNameSpan    :: SrcSpan
  , funDefName        :: T.Text
  , funDefParams      :: [Param]
  , funDefRetTypeSpan :: Maybe SrcSpan
  , funDefRetType     :: Type 
  , funDefBodySpan    :: SrcSpan
  , funDefBody        :: [Stmt]
  } deriving Show 

-- | Top-level definition.
data Def 
  = DLocalDef LocalDef 
  | DFunDef FunDef 
  deriving Show 

-- | Top-level representation of a parsed source file.
data Ast = MkAst 
  { astSource   :: Source
  , astTopLevel :: [Def]
  } deriving Show 