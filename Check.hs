-- Checking the AST, with appropriate handling.
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Check where

import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.Traversable as T (sequence,mapM)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Language.C.Data.Ident (Ident)
import Language.C.Data.Node (NodeInfo)
import Language.C.Syntax.AST

import Rules

data Type = Base
          | Arrow [Type] Type (Maybe Annotation)
          | Pointer Type
          | Struct (Map.Map Ident Type)
          deriving (Show,Eq)

data TypeName = VarName Ident | StructName Ident | TypedefName Ident
                deriving (Show,Eq,Ord)

data Checker = Checker {
    context :: Context,
    types :: Map.Map TypeName Type,
    msgs :: [String]
}

--
-- Helpers.
--

getType :: TypeName -> State Checker (Maybe Type)
getType name = Map.lookup name <$> types <$> get

getTypeOrBase :: NodeInfo -> TypeName -> State Checker Type
getTypeOrBase nobe name =
    do t' <- getType name
       case t' of Just t -> return t
                  Nothing -> do warn nobe "ident's type not in context" [name]
                                return Base

addType :: NodeInfo -> TypeName -> Type -> State Checker ()
addType nobe name t =
    do present <- Map.member name <$> types <$> get
       when present $ warn nobe "type is being shadowed" [name]
       modify (\s -> s { types = Map.insert name t $ types s })

getState :: State Checker Checker
getState = get

getContext :: State Checker Context
getContext = context <$> get

restoreState :: Checker -> State Checker ()
restoreState oldstate =
    modify (\s -> s { context = context oldstate, types = types oldstate })

mergeState :: NodeInfo -> Checker -> Checker -> State Checker ()
mergeState nobe state1 state2 =
    let (g1,g2) = (context state1, context state2)
    in do when (g1 /= g2) $ warn nobe "context mismatch in flow control" [g1,g2]
          modify (\s -> s { context = min g1 g2 }) -- use the subbest type

modifyContext :: (Context -> Context) -> State Checker ()
modifyContext f = modify (\s -> s { context = f $ context s })

-- if doDisjoin, then disjoin the types; else, intersect the types.
-- this comes into play when doing arrow annotations.
mergeType :: Bool -> NodeInfo -> Type -> Type -> State Checker Type
mergeType doDisjoin nobe (Base) (Base) = return Base
mergeType doDisjoin nobe (Pointer t1) (Pointer t2) =
    Pointer <$> mergeType doDisjoin nobe t1 t2
mergeType doDisjoin nobe (Struct m1) (Struct m2) =
    if (m1 == m2) then
        -- would disjoin structs, but (a) technical difficulties and (b) stupid
        liftM Struct $ T.sequence $
            Map.intersectionWith (mergeType doDisjoin nobe) m1 m2
    else
        do warn nobe "incompatible structs during type intersection" [m1,m2]
           return Base
mergeType doDisjoin nobe t1@(Arrow args1 ret1 a1) t2@(Arrow args2 ret2 a2) =
    do -- contravariance on the disjoin/intersect operator
       -- actually only theoretically sound because of the total ordering.
       args <- zipWithM (mergeType (not doDisjoin) nobe) args1 args2
       ret <- mergeType doDisjoin nobe ret1 ret2
       case (a1,a2) of
           (Just a1', Just a2') -> -- good case
               case (if doDisjoin then disjoin else intersect) a1' a2' of
                   a@(Just _) -> return $ Arrow args ret a
                   Nothing -> do err nobe "unmergable annotations" [a1',a2']
                                 return $ Arrow args ret (Just a1') -- boo
           (a@(Just _), Nothing) ->
               do warn nobe "missing annotation for merge on right branch" [t1,t2]
                  return $ Arrow args ret a
           (Nothing, a@(Just _)) ->
               do warn nobe "missing annotation for merge on left branch" [t1,t2]
                  return $ Arrow args ret a
           (Nothing, Nothing) ->
               do warn nobe "missing annotation for merge on both branches" [t1,t2]
                  return $ Arrow args ret Nothing
mergeType doDisjoin nobe t1 t2 =
    do warn nobe "type mismatch during merge" [t1,t2]; return Base

emptyMsg = [] :: [String]

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

check :: CTranslUnit -> State Checker () -- TODO turn this into execState
check (CTranslUnit decls nobe) = mapM_ checkExtDecl decls


checkExtDecl :: CExtDecl -> State Checker ()
checkExtDecl (CDeclExt d) = checkDecl_ d
checkExtDecl (CFDefExt f) = checkFunDef f
checkExtDecl (CAsmExt _) = return ()

checkFunDef :: CFunDef -> State Checker ()
checkFunDef (CFunDef specs declr oldstyle body nobe) = undefined
    --do (ret',a') <- checkDeclSpecs nobe specs

checkDecl :: CDecl -> State Checker (Maybe Ident, Type)
checkDecl _ = undefined

checkDecl_ d = checkDecl d >> return ()

checkStructUnion :: CStructUnion -> State Checker Type
checkStructUnion (CStruct tag (Just name) Nothing attrs nobe) =
    getTypeOrBase nobe $ StructName name
checkStructUnion (CStruct tag name' (Just decls) attrs nobe) =
    let namedOnly (Just x, y) = Just (x, y)
        namedOnly (Nothing, _) = Nothing
    in do contence <- Map.fromList <$> mapMaybe namedOnly <$> mapM checkDecl decls
          case name' of
              Just name -> addType nobe (StructName name) (Struct contence)
              Nothing -> return ()
          return $ Struct contence
checkStructUnion (CStruct tag Nothing Nothing attrs nobe) =
    do warn nobe "illegal struct structure" emptyMsg; return Base

checkEnum :: CEnum -> State Checker ()
checkEnum (CEnum _ Nothing _ _) = return ()
checkEnum (CEnum _ (Just list) _ _) =
    mapM_ (\(_,x) -> T.mapM checkExpr x) list

checkDeclSpec :: NodeInfo -> (Maybe Type, Maybe Annotation) -> CDeclSpec
                 -> State Checker (Maybe Type, Maybe Annotation)
checkDeclSpec nobe (t0',a0') (CStorageSpec _) = return (t0',a0')
checkDeclSpec nobe (t0',a0') (CTypeSpec spec) =
    do t <- checkTypeSpec spec
       case t0' of
           Just t0 ->
               do warn nobe "multi-typed declspec!"
                      [("overriding:",t0), ("with new type:",t)]
                  return (Just t, a0')
           Nothing -> return (Just t, a0')
checkDeclSpec nobe (t0', a0') (CTypeQual qual) =
    do a' <- checkTypeQual qual
       case (a0',a') of
           (Just a0, Just a) ->
               do warn nobe "multi-annotated declspec!"
                      [("overriding:",a0), ("with new annotation:",a)]
                  return (t0',a')
           (Nothing, Just _) -> return (t0',a')
           _ -> return (t0',a0')

checkDeclSpecs :: NodeInfo -> [CDeclSpec]
                  -> State Checker (Maybe Type, Maybe Annotation)
checkDeclSpecs nobe specs =
    foldM (checkDeclSpec nobe) (Nothing, Nothing) specs

checkTypeSpec :: CTypeSpec -> State Checker Type
checkTypeSpec (CVoidType nobe) = return Base
checkTypeSpec (CCharType nobe) = return Base
checkTypeSpec (CShortType nobe) = return Base
checkTypeSpec (CIntType nobe) = return Base
checkTypeSpec (CLongType nobe) = return Base
checkTypeSpec (CFloatType nobe) = return Base
checkTypeSpec (CDoubleType nobe) = return Base
checkTypeSpec (CSignedType nobe) = return Base
checkTypeSpec (CUnsigType nobe) = return Base
checkTypeSpec (CBoolType nobe) = return Base
checkTypeSpec (CComplexType nobe) = return Base
checkTypeSpec (CSUType strux nobe) = checkStructUnion strux
checkTypeSpec (CEnumType enum nobe) = return Base
checkTypeSpec (CTypeDef name nobe) = getTypeOrBase nobe $ TypedefName name
checkTypeSpec (CTypeOfExpr e nobe) =
    do warn nobe "using typeof(expr) may emit spurious warnings." emptyMsg
       checkExpr e
checkTypeSpec (CTypeOfType d nobe) =
    do warn nobe "using typeof(type) may emit spurious warnings." emptyMsg
       snd <$> checkDecl d

checkTypeQual :: CTypeQual -> State Checker (Maybe Annotation)
checkTypeQual (CAttrQual a) = checkAttr a
checkTypeQual _ = return Nothing

checkAttr :: CAttr -> State Checker (Maybe Annotation)
checkAttr _ = undefined

checkDeclr :: CDeclr -> State Checker ()
checkDeclr _ = undefined

checkDerivedDeclr :: CDerivedDeclr -> State Checker ()
checkDerivedDeclr _ = undefined

checkArrSize :: CArrSize -> State Checker ()
checkArrSize (CNoArrSize _) = return ()
checkArrSize (CArrSize _ e) = checkExpr_ e

checkInit :: Type -> CInit -> State Checker ()
checkInit t (CInitExpr e nobe) =
    do t' <- checkExpr e
       verifyAssign nobe t t'
checkInit t (CInitList inits nobe) = mapM_ (checkInitListElem t) inits

checkInitListElem :: Type -> ([CDesignator], CInit) -> State Checker ()
checkInitListElem _ _ = error "initialiser lists are not supported yet" -- TODO

checkDesignator :: CDesignator -> State Checker ()
checkDesignator _ = error "designators not supported yet" -- TODO

-- Statemence.
checkStat :: CStat -> State Checker Type
checkStat _ = undefined

checkStat_ s = checkStat s >> return ()

checkBlockItem :: CBlockItem -> State Checker ()
checkBlockItem (CBlockStmt s) = checkStat_ s
checkBlockItem (CBlockDecl d) = checkDecl_ d
checkBlockItem (CNestedFunDef f) =
    do s <- getState
       checkFunDef f
       restoreState s

checkAsmStmt :: CAsmStmt -> State Checker ()
checkAsmStmt (CAsmStmt qual asm outops inops clobbers nobe) =
    do mapM_ checkAsmOperand outops; mapM_ checkAsmOperand inops

checkAsmOperand :: CAsmOperand -> State Checker ()
checkAsmOperand (CAsmOperand name constraint e nobe) = checkExpr_ e

-- Expressions.
checkExpr :: CExpr -> State Checker Type
checkExpr (CComma es nobe) = do mapM checkExpr_ es; return Base
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
       mergeState nobe state2 state3
       mergeType True nobe t2 t3
checkExpr (CBinary _ e1 e2 nobe) =
    do t1 <- checkExpr e1
       t2 <- checkExpr e2
       case (t1,t2) of
           (Pointer t1', Pointer t2') -> return Base
           (Pointer t1', _) -> return t1'
           (_, Pointer t2') -> return t2'
           (_, _) -> return Base
checkExpr (CCast d e nobe) =
    do (_,t1) <- checkDecl d
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
checkExpr (CVar name nobe) = getTypeOrBase nobe $ VarName name
checkExpr (CConst _) = return Base
checkExpr (CCompoundLit d inits nobe) =
    do (_,t) <- checkDecl d
       mapM (checkInitListElem t) inits
       return t
checkExpr (CStatExpr s nobe) = checkStat s
checkExpr (CLabAddrExpr name nobe) = return Base
checkExpr (CBuiltinExpr b) = return Base

checkExpr_ e = checkExpr e >> return ()

