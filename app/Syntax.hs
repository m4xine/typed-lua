module Syntax 
  ( ExprF(..)
  , Expr
  , Param(..)
  , VarDef(..)
  , Stmt(..)
  , FunDef(..)
  , Toplevel(..)
  , Ast(..)
  ) where

import            Control.Comonad.Cofree        (Cofree)
import qualified  Data.Text               as T
import            Text.Show.Deriving            (deriveShow1)
import            Source                        (Source, SrcSpan)

-- | Representation of an expression.
-- See 'Expr' for use with 'Cofree'.  
data ExprF a
  = DynamicT                -- ^ > Dynamic type     ~ ?
  | ArrayT    a             -- ^ > Array type       ~ [T]  
  | ArrayL    [a]           -- ^ > Array literal    ~ [x, y, z]
  | RecordT   [(T.Text, a)] -- ^ > Record type      ~ {a : A, b : T}
  | RecordL   [(T.Text, a)] -- ^ > Record literal   ~ {a = x, b = y}
  | Select    a T.Text      -- ^ > Record selection ~ r.x
  | Invoke    a [a]         -- ^ > Invocation       ~ f(x, y, z)
  | StringL   T.Text        -- ^ > String literal   ~ "Hello, world!"
  | Name      T.Text        -- ^ > Name             ~ fooBar
  | UnitT                   -- ^ > Unit type        ~ {}
  | UnitL                   -- ^ > Unit literal     ~ {}
  deriving Functor

deriveShow1 ''ExprF 

type Expr x = Cofree ExprF x 

-- | Parameter representation.
data Param x = MkParam
  { paramNameSpan :: SrcSpan
  , paramName     :: T.Text 
  , paramType     :: Expr x 
  }
deriving instance Show x => Show (Param x)

-- | Variable definition representation.
data VarDef x = MkVarDef
  { varDefName  :: T.Text
  , varDefType  :: Maybe (Expr x)
  , varDefValue :: Expr x
  }
deriving instance Show x => Show (VarDef x)

-- | Statement representation.
data Stmt x
  = LocalDef  (VarDef x)
  | Return    (Expr x)
  -- | Regular expression terminated with a newline or semicolon.
  | Stmt      (Expr x)    
deriving instance Show x => Show (Stmt x)

-- | Function definition representation.
data FunDef x = MkFunDef
  { funDefName    :: T.Text
  , funDefParams  :: [Param x]
  , funDefRetType :: Maybe (Expr x)
  , funDefBody    :: [Stmt x]
  }
deriving instance Show x => Show (FunDef x)

-- | Top level item representation.
data Toplevel x
  = FunDef (FunDef x)
  | VarDef (VarDef x)
deriving instance Show x => Show (Toplevel x)

-- | Parsed abstract syntax tree with the origin 'Source'.
data Ast x = MkAst
  { astSource   :: Source
  , astTopLevel :: [Toplevel x] 
  } 
deriving instance Show x => Show (Ast x)