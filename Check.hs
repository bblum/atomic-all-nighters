-- Checking the AST, with appropriate handling.
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Check where

import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.Map as Map
import Language.C.Data.Ident (Ident)
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST

import Rules

data Type = Base
          | Arrow [Type] Type (Maybe Annotation)
          | Pointer Type
          | Struct (Map.Map Ident Type)
          deriving (Show,Eq)

data Checker = Checker {
    context :: Context,
    types :: Map.Map Ident Type,
    msgs :: [String]
}

--
-- Helpers.
--

getType :: Ident -> State Checker (Maybe Type)
getType name = Map.lookup name <$> types <$> get

getState :: State Checker Checker
getState = get

getContext :: State Checker Context
getContext = context <$> get

restoreState :: Checker -> State Checker ()
restoreState oldstate =
    modify (\s -> s { context = context oldstate, types = types oldstate })

mergeState :: Checker -> Checker -> Checker -> State Checker ()
mergeState oldstate state1 state2 = undefined

modifyContext :: (Context -> Context) -> State Checker ()
modifyContext f = modify (\s -> s { context = f $ context s })

intersectType :: NodeInfo -> Type -> Type -> State Checker Type
intersectType nobe t1 t2 = undefined -- TODO: intersect & print out a message if mismatch

msg :: (Show a) => String -> NodeInfo -> String -> [a] -> State Checker ()
msg prefix nobe str ts = undefined

err :: (Show a) => NodeInfo -> String -> [a] -> State Checker ()
err = msg "ERROR" -- TODO: make this fail

warn :: (Show a) => NodeInfo -> String -> [a] -> State Checker ()
warn = msg "warning"

--
-- Verification.
--

-- TODO: need to worry about extra pointer indirections around arrows? &malloc
verifyAssign :: NodeInfo -> Type -> Type -> State Checker ()
verifyAssign nobe t1@(Arrow args1 ret1 a1) t2@(Arrow args2 ret2 a2) =
    do when (length args1 /= length args2) $
           warn nobe "verification argument count mismatch" [t1,t2]
       case (liftM2 subtype a1 a2) of
           Just False -> 
               err nobe "illegal function pointer assignment"
                   [("dest (req'd supertype)", a1), ("src (req'd subtype)", a2)]
           Just True -> return ()
           Nothing ->
               warn nobe "missing annotation during assignment"
                   [("dest (req'd supertype)", a1), ("src (req'd subtype)", a2)]
       verifyAssign nobe ret1 ret2
       mapM_ (uncurry $ verifyAssign nobe) (zip args2 args1) -- contravariant!
verifyAssign nobe (Pointer t1) (Pointer t2) = verifyAssign nobe t1 t2
verifyAssign nobe (Struct m1) (Struct m2) =
    mapM_ (uncurry $ verifyAssign nobe) (zip (Map.elems m1) (Map.elems m2))
verifyAssign nobe Base Base = return ()
verifyAssign nobe t1 t2 = warn nobe "verification type mismatch" [t1,t2]

verifyCall :: NodeInfo -> Annotation -> State Checker ()
verifyCall nobe a =
    do g <- getContext
       when (not $ satisfies a g) $
           err nobe "illegal function call"
               [("target function", show a), ("current context", show g)] -- dum

--
-- Main iteration.
--

check :: CTranslUnit -> State Checker ()
check (CTranslUnit decls nobe) = mapM_ checkExtDecl decls


checkExtDecl :: CExtDecl -> State Checker ()
checkExtDecl _ = undefined

checkFunDef :: CFunDef -> State Checker ()
checkFunDef _ = undefined

checkDecl :: CDecl -> State Checker Type
checkDecl _ = undefined

checkStructUnion :: CStructUnion -> State Checker ()
checkStructUnion _ = undefined

checkStructTag :: CStructTag -> State Checker ()
checkStructTag _ = undefined

checkEnum :: CEnum -> State Checker ()
checkEnum _ = undefined

checkDeclSpec :: CDeclSpec -> State Checker ()
checkDeclSpec _ = undefined

checkStorageSpec :: CStorageSpec -> State Checker ()
checkStorageSpec _ = undefined

checkTypeSpec :: CTypeSpec -> State Checker ()
checkTypeSpec _ = undefined

checkTypeQual :: CTypeQual -> State Checker ()
checkTypeQual _ = undefined

checkAttr :: CAttr -> State Checker ()
checkAttr _ = undefined

checkDeclr :: CDeclr -> State Checker ()
checkDeclr _ = undefined

checkDerivedDeclr :: CDerivedDeclr -> State Checker ()
checkDerivedDeclr _ = undefined

checkArrSize :: CArrSize -> State Checker ()
checkArrSize _ = undefined

checkInit :: Type -> CInit -> State Checker ()
checkInit t (CInitExpr e nobe) =
    do t' <- checkExpr e
       verifyAssign nobe t t'
checkInit t (CInitList inits nobe) = mapM_ (checkInitListElem t) inits

checkInitListElem :: Type -> ([CDesignator], CInit) -> State Checker ()
checkInitListElem _ _ = error "initialiser lists are not supported yet" -- TODO

checkDesignator :: CDesignator -> State Checker ()
checkDesignator _ = undefined

checkStat :: CStat -> State Checker Type
checkStat _ = undefined

checkBlockItem :: CBlockItem -> State Checker ()
checkBlockItem _ = undefined

checkAsmStmt :: CAsmStmt -> State Checker ()
checkAsmStmt _ = undefined

checkAsmOperand :: CAsmOperand -> State Checker ()
checkAsmOperand _ = undefined

-- TODO; deal with nobe
checkExpr :: CExpr -> State Checker Type
checkExpr (CComma es nobe) = do mapM_ checkExpr es; return Base
checkExpr (CAssign _ e1 e2 nobe) =
    do t1 <- checkExpr e1
       t2 <- checkExpr e2
       verifyAssign nobe t1 t2
       return t1
checkExpr (CCond e1 e2 e3 nobe) =
    do t1 <- checkExpr e1
       oldstate <- getState
       (t2,state2,t3,state3) <-
           case e2 of
               Just e2' ->
                   do t2 <- checkExpr e2'
                      state2 <- getState
                      restoreState oldstate
                      t3 <- checkExpr e3
                      state3 <- getState
                      return (t2,state2,t3,state3)
               Nothing -> -- "x ?: y" means "x ? x : y"
                   do t3 <- checkExpr e3
                      state3 <- getState
                      return (t1,oldstate,t3,state3)
       mergeState oldstate state2 state3
       intersectType nobe t2 t3
checkExpr (CBinary _ e1 e2 nobe) =
    do t1 <- checkExpr e1
       t2 <- checkExpr e2
       case (t1,t2) of
           (Pointer t1', Pointer t2') -> return Base
           (Pointer t1', _) -> return t1'
           (_, Pointer t2') -> return t2'
           (_, _) -> return Base
checkExpr (CCast d e nobe) =
    do t1 <- checkDecl d
       t2 <- checkExpr e
       verifyAssign nobe t1 t2
       return t1
checkExpr (CUnary o e nobe) =
    do t <- checkExpr e
       case o of -- check for &foo or *foo
            CAdrOp -> return $ Pointer t
            CIndOp ->
                case t of Pointer t' -> return t'
                          _ -> do warn nobe "can't dereference type" [t]
                                  return Base
            _ -> return t
checkExpr (CSizeofExpr e nobe) = return Base
checkExpr (CSizeofType d nobe) = return Base
checkExpr (CAlignofExpr e nobe) = return Base
checkExpr (CAlignofType d nobe) = return Base
checkExpr (CComplexReal e nobe) = error "complexreal not supported yet"
checkExpr (CComplexImag e nobe) = error "compleximag not supported yet"
checkExpr (CIndex e1 e2 nobe) =
    do t1 <- checkExpr e1
       t2 <- checkExpr e2
       case (t1,t2) of
           (Pointer t1', Pointer t2') ->
               do warn nobe "can't index pointer with pointer" [t1,t2]
                  return t1'
           (Pointer t1', _) -> return t1'
           (_, Pointer t2') -> return t2'
           (_, _) ->
               do warn nobe "can't index non-pointer with non-pointer" [t1,t2]
                  return Base
checkExpr (CCall e args nobe) =
    let checkArg (t,e) = do t' <- checkExpr e; verifyAssign nobe t t'
    in do t <- checkExpr e
          case t of
              Arrow argtypes ret a ->
                  do when (length args /= length argtypes) $
                         warn nobe "argument number mismatch"
                              [("expected", argtypes)]
                     mapM checkArg $ zip argtypes args
                     case a of
                         Just a' ->
                             do verifyCall nobe a'; modifyContext $ effect a'
                         Nothing ->
                             do g <- getContext
                                warn nobe "missing annotation during call"
                                    [("current context", g)]
                     return ret
              _ ->
                  do warn nobe "can't call non-arrow type" [t]
                     return Base
checkExpr (CMember e name isderef nobe) =
    let memberType contence =
            case Map.lookup name contence of
                Just t -> return t
                Nothing ->
                    do warn nobe "missing struct member" [name]; return Base
    in do t <- checkExpr e
          case isderef of -- if/else didn't parse for some reason...
              True ->
                  case t of Pointer (Struct contence) -> memberType contence
                            _ -> do warn nobe "bad type for struct->member" [t]
                                    return Base
              False ->
                  case t of Struct contence -> memberType contence
                            _ -> do warn nobe "bad type for struct.member" [t]
                                    return Base
checkExpr (CVar name nobe) =
    do x <- getType name
       case x of Just t -> return t
                 Nothing ->
                     do warn nobe "variable not in context" [name]; return Base
checkExpr (CConst _) = return Base
checkExpr (CCompoundLit d inits nobe) =
    do t <- checkDecl d
       mapM (checkInitListElem t) inits
       return t
checkExpr (CStatExpr s nobe) = checkStat s
checkExpr (CLabAddrExpr name nobe) = return Base
checkExpr (CBuiltinExpr b) = return Base
