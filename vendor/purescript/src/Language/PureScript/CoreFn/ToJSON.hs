{-# LANGUAGE OverloadedStrings #-}
-- |
-- Dump the core functional representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  ) where

import           Prelude.Compat

import           Control.Arrow ((***))
import           Data.Either (isLeft)
import           Data.Maybe (maybe)
import           Data.Aeson
import           Data.Version (Version, showVersion)
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.PureScript.AST.Literals
import           Language.PureScript.AST.SourcePos (SourceSpan(SourceSpan))
import           Language.PureScript.CoreFn
import           Language.PureScript.Names
import           Language.PureScript.PSString (PSString)

constructorTypeToJSON :: ConstructorType -> Value
constructorTypeToJSON ProductType = toJSON "ProductType"
constructorTypeToJSON SumType = toJSON "SumType"

metaToJSON :: Meta -> Value
metaToJSON (IsConstructor t is)
  = object
    [ "metaType"         .= "IsConstructor"
    , "constructorType"  .= constructorTypeToJSON t
    , "identifiers"      .= identToJSON `map` is
    ]
metaToJSON IsNewtype              = object [ "metaType"  .= "IsNewtype" ]
metaToJSON IsTypeClassConstructor = object [ "metaType"  .= "IsTypeClassConstructor" ]
metaToJSON IsForeign              = object [ "metaType"  .= "IsForeign" ]
metaToJSON IsWhere                = object [ "metaType"  .= "IsWhere" ]

sourceSpanToJSON :: SourceSpan -> Value
sourceSpanToJSON (SourceSpan _ spanStart spanEnd) =
  object [ "start" .= spanStart
         , "end"   .= spanEnd
         ]

annToJSON :: Ann -> Value
annToJSON (ss, _, _, m) = object [ "sourceSpan"  .= sourceSpanToJSON ss
                                 , "meta"        .= maybe Null metaToJSON m
                                 ]

literalToJSON :: (a -> Value) -> Literal a -> Value
literalToJSON _ (NumericLiteral (Left n))
  = object
    [ "literalType" .= "IntegerLiteral"
    , "value"       .= n
    ]
literalToJSON _ (NumericLiteral (Right n))
  = object
      [ "literalType"  .= "FloatLiteral"
      , "value"        .= n
      ]
literalToJSON _ (StringLiteral s)
  = object
    [ "literalType"  .= "StringLiteral"
    , "value"        .= s
    ]
literalToJSON _ (AtomLiteral s)
  = object
    [ "literalType"  .= "AtomLiteral"
    , "value"        .= s
    ]
literalToJSON _ (CharLiteral c)
  = object
    [ "literalType"  .= "CharLiteral"
    , "value"        .= c
    ]
literalToJSON _ (BooleanLiteral b)
  = object
    [ "literalType"  .= "BooleanLiteral"
    , "value"        .= b
    ]
literalToJSON t (ListLiteral xs)
  = object
    [ "literalType"  .= "ListLiteral"
    , "value"        .= map t xs
    ]
literalToJSON t (ObjectLiteral xs)
  = object
    [ "literalType"    .= "ObjectLiteral"
    , "value"          .= recordToJSON t xs
    ]
literalToJSON _ (BinaryLiteral xs)
  = object
    [ "literalType"    .= "BinaryLiteral"
    , "value"          .= xs
    ]
literalToJSON t (Tuple2Literal a b)
  = object
    [ "literalType"    .= "Tuple2Literal"
    , "value"          .= (t a, t b)
    ]
literalToJSON t (TupleLiteral xs)
  = object
    [ "literalType"    .= "TupleLiteral"
    , "value"          .= map t xs
    ]

identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

properNameToJSON :: ProperName a -> Value
properNameToJSON = toJSON . runProperName

qualifiedToJSON :: (a -> Text) -> Qualified a -> Value
qualifiedToJSON f (Qualified mn a) = object
  [ "moduleName"   .= maybe Null moduleNameToJSON mn
  , "identifier"   .= toJSON (f a)
  ]

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON (ModuleName pns) = toJSON $ properNameToJSON `map` pns

moduleToJSON :: Version -> Module Ann -> Value
moduleToJSON v m = object
  [ "sourceSpan" .= sourceSpanToJSON (moduleSourceSpan m)
  , "moduleName" .= moduleNameToJSON (moduleName m)
  , "modulePath" .= toJSON (modulePath m)
  , "imports"    .= map importToJSON (moduleImports m)
  , "exports"    .= map identToJSON (moduleExports m)
  , "foreign"    .= map identToJSON (moduleForeign m)
  , "decls"      .= map bindToJSON (moduleDecls m)
  , "builtWith"  .= toJSON (showVersion v)
  , "comments"   .= map toJSON (moduleComments m)
  ]

  where
  importToJSON (ann,mn) = object
    [ "annotation" .= annToJSON ann
    , "moduleName" .= moduleNameToJSON mn
    ]

bindToJSON :: Bind Ann -> Value
bindToJSON (NonRec ann n e)
  = object
    [ "bindType"   .= "NonRec"
    , "annotation" .= annToJSON ann
    , "identifier" .= identToJSON n
    , "expression" .= exprToJSON e
    ]
bindToJSON (Rec bs)
  = object
    [ "bindType"   .= "Rec"
    , "binds"      .= map (\((ann, n), e)
                                  -> object
                                      [ "identifier"  .= identToJSON n
                                      , "annotation"   .= annToJSON ann
                                      , "expression"   .= exprToJSON e
                                      ]) bs
    ]

recordToJSON :: (a -> Value) -> [(PSString, a)] -> Value
recordToJSON f = toJSON . map (toJSON *** f)

exprToJSON :: Expr Ann -> Value
exprToJSON (Var ann i)              = object [ "type"        .= toJSON "Var"
                                             , "annotation"  .= annToJSON ann
                                             , "value"       .= qualifiedToJSON runIdent i
                                             ]
exprToJSON (Literal ann l)          = object [ "type"        .= "Literal"
                                             , "annotation"  .= annToJSON ann
                                             , "value"       .=  literalToJSON exprToJSON l
                                             ]
exprToJSON (Constructor ann d c is) = object [ "type"        .= "Constructor"
                                             , "annotation"  .= annToJSON ann
                                             , "typeName"    .= properNameToJSON d
                                             , "constructorName" .= properNameToJSON c
                                             , "fieldNames"  .= map identToJSON is
                                             ]
exprToJSON (Accessor ann f r)       = object [ "type"        .= "Accessor"
                                             , "annotation"  .= annToJSON ann
                                             , "fieldName"   .= f
                                             , "expression"  .= exprToJSON r
                                             ]
exprToJSON (ObjectUpdate ann r fs)  = object [ "type"        .= "ObjectUpdate"
                                             , "annotation"  .= annToJSON ann
                                             , "expression"  .= exprToJSON r
                                             , "updates"     .= recordToJSON exprToJSON fs
                                             ]
exprToJSON (Abs ann p b)            = object [ "type"        .= "Abs"
                                             , "annotation"  .= annToJSON ann
                                             , "argument"    .= identToJSON p
                                             , "body"        .= exprToJSON b
                                             ]
exprToJSON (App ann f x)            = object [ "type"        .= "App"
                                             , "annotation"  .= annToJSON ann
                                             , "abstraction" .= exprToJSON f
                                             , "argument"    .= exprToJSON x
                                             ]
exprToJSON (Case ann ss cs)         = object [ "type"        .= "Case"
                                             , "annotation"  .= annToJSON ann
                                             , "caseExpressions"
                                                                    .= map exprToJSON ss
                                             , "caseAlternatives"
                                                                    .= map caseAlternativeToJSON cs
                                             ]
exprToJSON (Let ann bs e)           = object [ "type"        .= "Let" 
                                             , "annotation"  .= annToJSON ann
                                             , "binds"       .= map bindToJSON bs
                                             , "expression"  .= exprToJSON e
                                             ]
exprToJSON (Receive ann (Just(va, vb)) cs) = object [ "type" .= "ReceiveJust"
                                             , "annotation"  .= annToJSON ann
                                             , "justA"
                                                                    .= va
                                             , "justB"
                                                                    .= exprToJSON vb
                                             , "caseAlternatives"
                                                                    .= map caseAlternativeToJSON cs
                                             ]
exprToJSON (Receive ann Nothing cs) = object [ "type"        .= "ReceiveNothing"
                                             , "annotation"  .= annToJSON ann
                                             , "caseAlternatives"
                                                                    .= map caseAlternativeToJSON cs
                                             ]
exprToJSON (List ann a b)           = object [ "type"        .= "List"
                                             , "annotation"  .= annToJSON ann
                                             , "listA"
                                                                    .= map exprToJSON a
                                             , "listB"
                                                                    .= exprToJSON b
                                             ]



caseAlternativeToJSON :: CaseAlternative Ann -> Value
caseAlternativeToJSON (CaseAlternative bs r') =
  let isGuarded = isLeft r'
  in object
      [ "binders"     .= toJSON (map binderToJSON bs)
      , "isGuarded"   .= toJSON isGuarded
      , (if isGuarded then "expressions" else "expression")
         .= case r' of
             Left rs -> toJSON $ map (\(g, e) -> object [ "guard" .= exprToJSON g, "expression" .= exprToJSON e]) rs
             Right r -> exprToJSON r
      ]

binderToJSON :: Binder Ann -> Value
binderToJSON (VarBinder ann v)              = object [ "binderType"  .= "VarBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "identifier"  .= identToJSON v
                                                     ]
binderToJSON (NullBinder ann)               = object [ "binderType"  .= "NullBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     ]
binderToJSON (LiteralBinder ann l)          = object [ "binderType"  .= "LiteralBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "literal"     .= literalToJSON binderToJSON l
                                                     ]
binderToJSON (ConstructorBinder ann d c bs) = object [ "binderType"  .= "ConstructorBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "typeName"    .= qualifiedToJSON runProperName d
                                                     , "constructorName"
                                                                            .= qualifiedToJSON runProperName c
                                                     , "binders"     .= map binderToJSON bs
                                                     ]
binderToJSON (NamedBinder ann n b)          = object [ "binderType"  .= "NamedBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "identifier"  .= identToJSON n
                                                     , "binder"      .= binderToJSON b
                                                     ]
binderToJSON (MapBinder ann vb)             = object [ "binderType"  .= "MapBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "mapBiner"    .= map (\(a,b) -> (binderToJSON a, binderToJSON b)) vb
                                                     ]
binderToJSON (BinaryBinder ann vb)          = object [ "binderType"  .= "BinaryBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "binaryBinder".= map (\(a,b,c) -> (binderToJSON a, b, c)) vb
                                                     ]
binderToJSON (ListBinder ann va vb)         = object [ "binderType"  .= "ListBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "listBinderA" .= map binderToJSON va
                                                     , "listBinderB" .= binderToJSON vb
                                                     ]
