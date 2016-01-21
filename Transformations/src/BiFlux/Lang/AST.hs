module BiFlux.Lang.AST where

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.List
import Control.Monad
import BiFlux.XPath.HXT.XPathDataTypes as XPath hiding (Name(..))

data Program  = Program [ImportSchema] [VarBind] Start [Decl]
                deriving (Eq,Show)

data VarBind = VarBind Var FilePath AstType
                deriving (Eq,Show)

data ImportSchema = Import FilePath String deriving (Eq,Show)

data Decl = ProcedureDecl Procedure
          | TypeDecl String AstType
  deriving (Eq,Show)

data Procedure = Procedure Name [(ProcVar,AstType)] Stmts
  deriving (Eq,Show)

data Start  = Start Name [XQExpr]
             deriving (Eq,Show)

data ProcVar = ProcSVar Var -- source $x AS a
             | ProcVVar Var -- view $x AS a
             | ProcEVar Var -- (normal) $x as a
  deriving (Eq,Show)

type Stmts = [Stmt]

data Stmt = StmtUpd Upd [WhereCond]
          | StmtIF  BoolExpr Stmts Stmts
          | StmtLet Pat  XQExpr Stmts
          | StmtP Name [XQExpr]
          | StmtCase XQExpr [(Pat,Stmts)]
            deriving (Eq,Show)


data WhereCond = WhereBool BoolExpr
               | WhereBind Var XQExpr
                 deriving (Eq,Show)

data SInsertionLocation = UBEFORE | UAFTER deriving (Eq,Show)
data PInsertionLocation = ULAST  | UFIRST deriving (Show,Eq)

data Upd  = AdaptSource Stmts
          | SingleInsertion SInsertionLocation (Maybe Pat) CPath XQExpr
          | PluralInsertion PInsertionLocation (Maybe Pat) CPath XQExpr
          | SingleDeletion (Maybe Pat) CPath
          | PluralDeletion (Maybe Pat) CPath
          | SingleReplace (Maybe Pat) CPath XQExpr
          | PluralReplace (Maybe Pat) CPath XQExpr
          | UpdateSource (Maybe Pat) CPath Stmts
          | UpdateView (Maybe Pat) CPath ViewStmts (Maybe Pat) CPath (Maybe MatchCond)
          | UpdateKeep CPath -- the intuition for keep as first/last is not very good, we should just drop it
          | UpdateCreate XQExpr
            deriving (Eq,Show)

type ViewStmts = [ViewStmt]

data ViewStmt = ViewStmtMatch Stmts
              | ViewStmtUMV   Stmts
              | ViewStmtUMS   Stmts
              deriving (Eq,Show)

data MatchCond = MatchBY [CPath]
               | MatchSV [CPath] [CPath]
               deriving (Eq,Show)

type BoolExpr = XQExpr

data Pat = SequencePat [Pat]
         | ElementPat Name [Pat]
         | AttributePat Name VarP
         | StringPat String
         | VarPat VarP
         deriving (Eq,Show)

data VarP = VarV Var
          | VarT Var AstType
          | VarTP AstType
          deriving (Eq,Show)

data AstType = EmptyT
             | NameT Name -- reference to a type declaration name
             | ElementT String AstType
             | AttributeT String AstType
             | SequenceT AstType AstType
             | StarT AstType
             | ChoiceT AstType AstType
             | StringT
             deriving (Eq,Show)
-- t? and t+ should be parsed as derived forms t|() and t,t*

-- | The type of paths handled by our language
data CPath = CPathSelf    -- . ; self::a = CPathSlash CPathSelf (CPathNodeTest (NameTest a))
           | CPathChild    -- axis
           | CPathAttribute  -- axis
           | CPathDoS     -- a = CPathSlash CPathDoS (CPathNodeTest (NameTest a)); descendant-or-self axis: not supported for lenses or generic functions, only uXQ expressions
           | CPathNodeTest XPath.NodeTest
           | CPathSlash CPath CPath -- p / p'
           | CPathFilter XQExpr -- focused on a single element   p[e] = CPathSlash p (CPathFilter e)
           | CPathVar Var -- variable $x
           | CPathString String -- 'abc'
           | CPathBool Bool -- true | false
           | CPathSnapshot Pat CPath -- special case, NOT needed for a parser
           | CPathFct String [XQExpr]
           | CPathIndex Int -- newly added for index like a[1]
  deriving (Eq,Show)

-- | The type of general expressions handled by our language
data XQExpr = XQEmpty              -- ()
            | XQProd XQExpr XQExpr        -- e,e'
            | XQElem String XQExpr        -- n[e] when we construct an element we must put all attributes first and sorted
            | XQAttr String XQExpr        -- @n='e'
            | XQLet Pat XQExpr XQExpr      -- let x = e in e'
            | XQIf XQExpr XQExpr XQExpr      -- if c then e else e'
            | XQBinOp XPath.Op XQExpr XQExpr  -- e ~~ e'
--            | XQTreeVar Var          -- x- (tree variables, i.e., variables of atomic types) (we generalize and don't consider specific tree variables)
            | XQFor Var XQExpr XQExpr      -- for x- \in e return e' (still for tree variables)
            | XQPath CPath            -- special case to consider core paths (since their implementation as core uXQ is very different)
  deriving (Eq,Show)

type Var = String
type Name = String



