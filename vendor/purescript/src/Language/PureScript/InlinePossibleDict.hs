module Language.PureScript.InlinePossibleDict where

import Data.Bifunctor (second)
import Data.Function ((&))
import Data.List (foldl')
import Data.Maybe (fromJust, isJust, maybeToList)
import Language.PureScript (Qualified (Qualified))
import Language.PureScript.CoreFn
import Language.PureScript.Names (Ident)
import Language.PureScript.PSString (PSString)
import Prelude hiding (id)

-- object getter
-- object setter

newtype ObjAcc = ObjAcc
  { unObjAcc :: PSString
  }

-- data ObjPair a = ObjPair PSString (Qualified Ident)

newtype Obj a = Obj { unObj :: [(PSString, Qualified Ident)]}

getObjAcc :: Expr a -> Maybe ObjAcc
getObjAcc = \case
  Abs _ id (Accessor _ ps (Var _ (Qualified Nothing id1))) | id == id1 -> Just (ObjAcc ps)
  _ -> Nothing

bindToObjAcc :: Bind a -> Maybe (Ident, ObjAcc)
bindToObjAcc (NonRec _ id expr) = (id,) <$> getObjAcc expr
bindToObjAcc _ = Nothing

getQualifiedIdent :: Expr a -> Maybe (Qualified Ident)
getQualifiedIdent = \case
  Var _ qi -> Just qi
  _ -> Nothing

getObj :: Expr a -> Maybe (Obj a)
getObj = \case
  Literal _ (ObjectLiteral ls) ->
    fmap (second getQualifiedIdent) ls
      & filter (isJust . snd)
      & ( \x ->
            if null x
              then Nothing
              else Just $ Obj $ fmap (second fromJust) x
        )
  _ -> Nothing

bindToObj :: Bind a -> [(Ident, Obj a)]
bindToObj (NonRec _ id expr) = maybeToList $ (id,) <$> getObj expr
bindToObj (Rec ls) = concatMap bindToObj (fmap (\((a, b), c) -> NonRec a b c) ls)

fromBinds :: [Bind a] -> ([(Ident, ObjAcc)], [(Ident, Obj a)])
fromBinds = foldl' go ([], [])
  where
    go (a, b) bind =
      let va = bindToObjAcc bind
          vb = bindToObj bind
       in (a ++ maybeToList va, b ++ vb)