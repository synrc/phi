
module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn, optimizeCoreFn' , ObjAccs, Objs, ObjAccMap, ObjMap) where

import Protolude hiding (Type)

import Data.List (lookup)
import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.Names (Ident(..), Qualified(Qualified), ModuleName(..), ProperName(..))
import Language.PureScript.Label
import Language.PureScript.Types
import qualified Language.PureScript.Constants as C
import qualified Data.Map as M
import Language.PureScript.PSString (PSString)

-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: Module Ann -> Module Ann
optimizeCoreFn m = m {moduleDecls = optimizeModuleDecls $ moduleDecls m}

optimizeModuleDecls :: [Bind Ann] -> [Bind Ann]
optimizeModuleDecls = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs = optimizeUnusedPartialFn . optimizeClosedRecordUpdate . etaReudce

optimizeClosedRecordUpdate :: Expr Ann -> Expr Ann
optimizeClosedRecordUpdate ou@(ObjectUpdate a@(_, _, Just t, _) r updatedFields) =
  case closedRecordFields t of
    Nothing -> ou
    Just allFields -> Literal a (ObjectLiteral (map f allFields))
      where f (Label l) = case lookup l updatedFields of
              Nothing -> (l, Accessor (nullSourceSpan, [], Nothing, Nothing) l r)
              Just e -> (l, e)
optimizeClosedRecordUpdate e = e

-- | Return the labels of a closed record, or Nothing for other types or open records.
closedRecordFields :: Type a -> Maybe [Label]
closedRecordFields (TypeApp _ (TypeConstructor _ C.Record) row) =
  collect row
  where
    collect :: Type a -> Maybe [Label]
    collect (REmpty _) = Just []
    collect (RCons _ l _ r) = collect r >>= return . (l :)
    collect _ = Nothing
closedRecordFields _ = Nothing

-- | See https://github.com/purescript/purescript/issues/3157
optimizeUnusedPartialFn :: Expr a -> Expr a
optimizeUnusedPartialFn (Let _
  [NonRec _ UnusedIdent _]
  (App _ (App _ (Var _ (Qualified _ UnusedIdent)) _) originalCoreFn)) =
  originalCoreFn
optimizeUnusedPartialFn e = e

-- | etaReduce --
etaReudce :: Expr a -> Expr a
etaReudce = \case
  Abs _ id1 (App _ e1@Var{} (Var _ (Qualified Nothing id1'))) | id1 == id1'-> e1
  Abs _ id1 (Abs _ id2 (App _ (App _ e2@Var{} (Var _ (Qualified Nothing id1'))) (Var _ (Qualified Nothing id2')))) 
      | id1 == id1' && id2 == id2' -> e2
  e -> e

type ObjAccs = [(Qualified Ident, PSString)]
type Objs = [(Qualified Ident,[(PSString, Qualified Ident)])]

type ObjAccMap = M.Map (Qualified Ident) PSString 
type ObjMap = M.Map (Qualified Ident) (M.Map PSString (Qualified Ident))

synonym :: Qualified Ident -> Qualified Ident 
synonym (Qualified (Just (ModuleName [ProperName "Control", ProperName "Monad" ])) (Ident "discard")) = 
  Qualified (Just (ModuleName [ProperName "Control", ProperName "Monad" ])) (Ident "bind")
synonym other = other

inlineDict :: ObjAccMap -> ObjMap -> Expr a -> Expr a 
inlineDict objAccMap objMap e = 
  case e of 
    App ann (Var _ accQi@(Qualified (Just _) _)) (Var _ objQi@(Qualified (Just _) _)) -> 
      let res = do
            acc <- M.lookup (synonym accQi) objAccMap
            obj <- M.lookup objQi objMap
            M.lookup acc obj
      in case res of 
          Nothing -> e 
          Just qi -> Var ann qi
    _ -> e

--
optimizeCoreFn' :: ObjAccMap -> ObjMap -> Module Ann -> Module Ann
optimizeCoreFn' objAccMap objMap m = m {moduleDecls = optimizeModuleDecls' objAccMap objMap $ moduleDecls m}

optimizeModuleDecls' :: ObjAccMap -> ObjMap -> [Bind Ann] -> [Bind Ann]
optimizeModuleDecls' objAccMap objMap = map transformBinds
  where
  (transformBinds, _, _) = everywhereOnValues identity transformExprs identity
  transformExprs = inlineDict objAccMap objMap
