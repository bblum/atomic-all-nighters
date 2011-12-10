-- Attribute translation for atomic all nighters.
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Attributes (attrToAnnotation,attrIsAnnotation) where

import Language.C.Data
import Language.C.Syntax.AST
import Language.C.Syntax.Constants

import Rules

stringToEffect :: String -> Maybe Effect
stringToEffect s =
    case s of
        "no_change" -> Just $ IncDec 0
        "exit_nested" -> Just $ IncDec (-1)
        "enter_nested" -> Just $ IncDec 1
        "force_enable" -> Just $ Enable
        "force_disable" -> Just $ Disable
        _ -> Nothing

stringToRule :: String -> Maybe Rule
stringToRule s =
    case s of
        "might_sleep" -> Just $ Rule $ Nested 0
        "nested_one" -> Just $ Rule $ Nested 1
        "wont_sleep" -> Just $ Rule Infinity
        _ -> Nothing

attrToAnnotation :: [CExpr] -> Maybe Annotation
attrToAnnotation [x@(CConst (CStrConst _ nobe))] = -- implicit no_change allowed
    attrToAnnotation [x, CConst $ CStrConst (cString "no_change") nobe]
attrToAnnotation [CConst (CStrConst s1 _), CConst (CStrConst s2 _)] =
    let (rs,es) = (getCString s1, getCString s2)
    in do r <- stringToRule rs
          e <- stringToEffect es
          return $ Annotation (r,e)
attrToAnnotation _ = Nothing

attrIsAnnotation :: Ident -> Bool
attrIsAnnotation name = identToString name == "atomic_all_nighters"

