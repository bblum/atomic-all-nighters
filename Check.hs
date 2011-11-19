-- Checking the AST, with appropriate handling.
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Check where

import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.Traversable as T (sequence,mapM)
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Maybe (mapMaybe,catMaybes,fromMaybe,isNothing,fromJust)
import Language.C.Data.Ident (Ident)
import Language.C.Data.Node (NodeInfo,fileOfNode,posOfNode)
import Language.C.Data.Position (posRow,posColumn)
import Language.C.Syntax.AST

import Rules
import Attributes

data Type = Base
          | Arrow [Type] Type (Maybe Annotation)
          | Pointer Type
          | Struct (Map.Map Ident Type)
          deriving Eq

data TypeName = VarName Ident | StructName Ident | TypedefName Ident
                deriving (Show,Eq,Ord)

-- TODO: data Message

data MessageLine a = (Show a) => M String a

data Checker = Checker {
    context :: Context,
    types :: Map.Map TypeName Type,
    msgs :: [String]
}

defaultChecker = Checker undefined Map.empty []

--
-- Instants.
--

instance Show Type where
    show Base = "T"
    show (Arrow args ret a') =
        let argstr = case args of [] -> "unit"
                                  _ -> intercalate ", " (map show args)
        in "(" ++ argstr ++ " -> " ++ show ret
           ++ " {" ++ maybe "???" show a' ++ "})"
    show (Pointer t) = show t ++ "*"
    show (Struct contence) = "struct {" ++ show contence ++ "}"

instance Show (MessageLine a) where
    show (M str a) = str ++ " \t" ++ show a

--
-- Helpers.
--

getType :: TypeName -> State Checker (Maybe Type)
getType name =
    do t' <- Map.lookup name <$> types <$> get
       -- strip outer "pointer" from function pointers. see also checkDeclr
       case t' of (Just (Pointer (x@(Arrow _ _ _)))) -> return $ Just x
                  _ -> return t'

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

setContext :: Context -> State Checker ()
setContext g = modify (\s -> s { context = g })

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
msg prefix nobe str noobs =
    let (row,col) = (posRow $ posOfNode nobe, posColumn $ posOfNode nobe)
        mess0 = prefix ++ ": at " ++ fileOfNode nobe ++ ":"
                ++ (show row) ++ "," ++ (show col) ++ ": " ++ str
        mess = foldl (\output noob -> output ++ "\n\t" ++ show noob) mess0 noobs
    in modify (\s -> s { msgs = mess:(msgs s) })

err :: (Show a) => NodeInfo -> String -> [a] -> State Checker ()
err = msg "ERROR" -- TODO: make this fail

warn :: (Show a) => NodeInfo -> String -> [a] -> State Checker ()
warn = msg "warning"

info :: (Show a) => NodeInfo -> String -> [a] -> State Checker ()
info = msg "(info)"

--
-- Verification.
--

-- TODO: need to worry about extra pointer indirections around arrows? &malloc
-- The bool argument expresses whether subtyping is allowed.
verifyAssign :: NodeInfo -> Bool -> Type -> Type -> State Checker ()
verifyAssign nobe subtyping t1@(Arrow args1 ret1 a1) t2@(Arrow args2 ret2 a2) =
    let verifyAnnotation True =
            case (liftM2 subtype a1 a2) of
                Just False ->
                    err nobe "illegal subtyped function pointer assignment"
                        [M "dest (req'd supertype)" $ fromJust a1,
                         M "src (req'd subtype)" $ fromJust a2]
                Just True -> return ()
                Nothing ->
                  warn nobe "missing annotation during assignment"
                      [M "dest (req'd supertype)" a1, M "src (req'd subtype)" a2]
        verifyAnnotation False =
            case (liftM2 (==) a1 a2) of
                Just False ->
                    err nobe "illegal invariant function pointer assignment"
                        [M "dest" $ fromJust a1, M "src" $ fromJust a2]
                Just True -> return ()
                Nothing ->
                    warn nobe "missing annotation during invariant assignment"
                        [M "dest" a1, M "src" a2]
    in do when (length args1 /= length args2) $
              warn nobe "verification argument count mismatch" [t1,t2]
          verifyAnnotation subtyping
          verifyAssign nobe subtyping ret1 ret2
          mapM_ (uncurry $ verifyAssign nobe subtyping)
                (zip args2 args1) -- contravariant!
          info nobe "verified assignment" [M "dest type" t1, M "src type" t2]
verifyAssign nobe subtyping (Pointer t1) (Pointer t2) =
    verifyAssign nobe False t1 t2 -- Reference cells are invariant.
verifyAssign nobe subtyping (Struct m1) (Struct m2) =
    mapM_ (uncurry $ verifyAssign nobe subtyping) -- Structs aren't quite refs.
          (zip (Map.elems m1) (Map.elems m2))
verifyAssign nobe subtyping Base Base = return ()
verifyAssign nobe subtyping t1 t2 =
    warn nobe "verification type mismatch" [t1,t2]

verifyCall :: NodeInfo -> Annotation -> State Checker ()
verifyCall nobe a =
    do g <- getContext
       when (not $ satisfies a g) $
           err nobe "illegal function call"
               [M "target function" $ show a, M "while in context" $ show g]

-- Mashes an annotation into an arrow type that might already have one.
injectAnnotation :: NodeInfo -> Type -> Maybe Annotation -> State Checker Type
injectAnnotation nobe (Arrow args ret (Just a0)) (Just a) =
    do warn nobe "multiply-differently-annotated function" [a0,a]
       return $ Arrow args ret (Just a)
injectAnnotation nobe (Arrow args ret Nothing) (Just a) =
    return $ Arrow args ret (Just a)
injectAnnotation nobe t (Just a) =
    do warn nobe "ignoring annotation on non-function" [a]
       return t
injectAnnotation nobe t (Nothing) = return t

--
-- Main iteration.
--

check :: CTranslUnit -> [String]
check (CTranslUnit decls nobe) =
    reverse $ msgs $ execState (mapM_ checkExtDecl decls) defaultChecker

checkExtDecl :: CExtDecl -> State Checker ()
checkExtDecl (CDeclExt d) = checkDecl_ d
checkExtDecl (CFDefExt f) = checkFunDef f
checkExtDecl (CAsmExt _) = return ()

checkFunDef :: CFunDef -> State Checker ()
checkFunDef (CFunDef specs declr oldstyle body nobe) =
    let addFunc (Just name) t =
            do told' <- getType (VarName name)
               -- If previously declared, check that the defined type matches.
               case told' of
                   Just told -> verifyAssign nobe True told t
                   Nothing -> addType nobe (VarName name) t
        addFunc Nothing t = return ()
    in do (t0',a',_) <- checkDeclSpecs nobe specs -- 'typedef' never on functions
          -- do add posible args to context.
          oldstate <- getState
          (name',t0) <- checkDeclr (fromMaybe Base t0') True declr
          when (not $ null oldstyle) $
              warn nobe "old-style args not supported in function def'n" [name']
          t <- injectAnnotation nobe t0 a'
          -- add function name to context
          -- this has to be done twice, for the function to be scoped inside
          -- itself and also after dropping the type mappings from inside.
          addFunc name' t
          -- traverse function body; save old context in case of nested function
          g <- case a' of Just a -> return $ entryContext a
                          Nothing -> do warn nobe "missing annotation" [name']
                                        return $ entryDefault
          info nobe "entering function with context" [g]
          gold <- getContext
          setContext g
          checkStat body
          -- check exit context against advertised effect
          gnew <- getContext
          case a' of Just a ->
                         when (effect a g /= Just gnew) $
                             err nobe "exit context != advertised effect"
                                 [M "entry context" g, M "exit context" gnew]
                     Nothing -> return ()
          -- restore old context and types mapping
          restoreState oldstate
          -- second time - make this function be scoped in future functions
          -- TODO: what about pre-declared functions
          addFunc name' t

checkDecl :: Bool -> CDecl -> State Checker [(Maybe Ident, Type)]
checkDecl remember (CDecl specs noobs nobe) =
    let checkTriple a' kindName t0 (declr', val', bitfield') =
            do -- Sometimes the noob is unnamed.
               (name',t) <- maybe (return (Nothing,t0)) (checkDeclr t0 False) declr'
               t2 <- injectAnnotation nobe t a'
               -- Check the initialiser
               maybe (return ()) (checkInit t) val'
               maybe (return ()) checkExpr_ bitfield'
               -- Record and return
               when remember $
                   maybe (return ()) (\name -> addType nobe (kindName name) t2)
                         name'
               return (name',t2)
    in do (t',a',kindName) <- checkDeclSpecs nobe specs
          let t = maybe Base id t'
          mapM (checkTriple a' kindName t) noobs

checkDecl_ d = checkDecl True d >> return ()

checkOneDecl :: CDecl -> State Checker (Maybe Ident, Type)
checkOneDecl d@(CDecl _ _ nobe)  =
    do list <- checkDecl True d
       case list of [] -> error "empty decl??"
                    [x] -> return x
                    x:rest -> do warn nobe "ignoring extra decls" rest; return x

checkStructUnion :: CStructUnion -> State Checker Type
checkStructUnion (CStruct tag (Just name) Nothing attrs nobe) =
    do t' <- getType $ StructName name
       case t' of -- Honour incomplete struct declarations
           Just t -> return t
           Nothing -> do addType nobe (StructName name) Base; return Base
checkStructUnion (CStruct tag name' (Just decls) attrs nobe) =
    let namedOnly (Just x, y) = Just (x, y)
        namedOnly (Nothing, _) = Nothing
    in do contence <- Map.fromList <$> mapMaybe namedOnly <$>
                          concat <$> mapM (checkDecl False) decls
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

-- Declaration specifiers
checkDeclSpec :: NodeInfo -> (Maybe Type, Maybe Annotation, Ident -> TypeName)
                 -> CDeclSpec
                 -> State Checker (Maybe Type, Maybe Annotation,
                                   Ident -> TypeName)
checkDeclSpec nobe (t0',a0',_) (CStorageSpec (CTypedef _)) =
    return (t0',a0',TypedefName) -- typedefs get different context constructors
checkDeclSpec nobe (t0',a0',x) (CStorageSpec _) = return (t0',a0',x)
checkDeclSpec nobe (t0',a0',x) (CTypeSpec spec) =
    do t <- checkTypeSpec spec
       case t0' of
           Just t0 ->
               do warn nobe "multi-typed declspec!"
                      [M "overriding:" t0, M "with new type:" t]
                  return (Just t, a0', x)
           Nothing -> return (Just t, a0', x)
checkDeclSpec nobe (t0',a0',x) (CTypeQual qual) =
    do a' <- checkTypeQual qual
       case (a0',a') of
           (Just a0, Just a) ->
               do warn nobe "multi-annotated declspec!"
                      [M"overriding:" a0, M "with new annotation:" a]
                  return (t0',a',x)
           (Nothing, Just _) -> return (t0',a',x)
           _ -> return (t0',a0',x)

checkDeclSpecs :: NodeInfo -> [CDeclSpec]
                  -> State Checker (Maybe Type, Maybe Annotation,
                                    Ident -> TypeName)
checkDeclSpecs nobe specs =
    foldM (checkDeclSpec nobe) (Nothing, Nothing, VarName) specs

-- Type specifiers
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
       checkExpr e -- TODO
checkTypeSpec (CTypeOfType d nobe) =
    do warn nobe "using typeof(type) may emit spurious warnings." emptyMsg
       snd <$> checkOneDecl d -- TODO

checkTypeQual :: CTypeQual -> State Checker (Maybe Annotation)
checkTypeQual (CAttrQual a) = checkAttr a
checkTypeQual _ = return Nothing

checkAttr :: CAttr -> State Checker (Maybe Annotation)
checkAttr (attr@(CAttr name es nobe)) =
    if attrIsAnnotation name then
       do let a' = attrToAnnotation es
          when (isNothing a') $ warn nobe "invalid annotation!" [show name]
          return a'
    else return Nothing

checkAttrs :: NodeInfo -> [CAttr] -> State Checker (Maybe Annotation)
checkAttrs nobe attrs =
    do annos <- catMaybes <$> mapM checkAttr attrs
       case annos of
           [] -> return Nothing
           [a] -> return $ Just a
           a:rest -> do warn nobe "ignoring extra annotations" rest
                        return $ Just a

-- Declarators
-- When called from fundef, need to add the args to the context. otherwise not.
checkDeclr :: Type -> Bool -> CDeclr -> State Checker (Maybe Ident, Type)
checkDeclr t0 addArgs (CDeclr name' deriveds asmname attrs nobe) =
    do t' <- checkDerivedDeclrs t0 addArgs deriveds
       -- strip the outermost "pointer" type derived-decl from function pointers.
       -- see also: getType
       let t = case t' of (Pointer x@(Arrow _ _ _)) -> x; _ -> t'
       a' <- checkAttrs nobe attrs
       t2 <- injectAnnotation nobe t a'
       return (name', t2)

checkDerivedDeclrs :: Type -> Bool -> [CDerivedDeclr] -> State Checker Type
checkDerivedDeclrs t0 addArgs [] = return t0
checkDerivedDeclrs t0 addArgs ((CPtrDeclr quals nobe):rest) =
    Pointer <$> checkDerivedDeclrs t0 addArgs rest
checkDerivedDeclrs t0 addArgs ((CArrDeclr quals size nobe):rest) =
    do checkArrSize size; Pointer <$> checkDerivedDeclrs t0 addArgs rest
checkDerivedDeclrs t0 addArgs ((CFunDeclr args'' attrs nobe):rest) =
    do t <- checkDerivedDeclrs t0 addArgs rest
       a' <- checkAttrs nobe attrs
       args <- case args'' of
                   Left oldstyle ->
                       do warn nobe "old-style args ignored" [Arrow [] t a']
                          return []
                   Right (decls,isVariadic) ->
                       do args <- map snd <$> concat <$>
                                      mapM (checkDecl addArgs) decls
                          when isVariadic $
                              warn nobe "variadic function not supported"
                                  [Arrow args t a']
                          return args
       info nobe "processed FunDeclr" [Arrow args t a']
       return $ Arrow args t a'

-- Misc
checkArrSize :: CArrSize -> State Checker ()
checkArrSize (CNoArrSize _) = return ()
checkArrSize (CArrSize _ e) = checkExpr_ e

checkInit :: Type -> CInit -> State Checker ()
checkInit t (CInitExpr e nobe) =
    do t' <- checkExpr e
       verifyAssign nobe True t t'
checkInit t (CInitList inits nobe) = mapM_ (checkInitListElem t) inits

checkInitListElem :: Type -> ([CDesignator], CInit) -> State Checker ()
checkInitListElem _ _ = error "initialiser lists are not supported yet" -- TODO

checkDesignator :: CDesignator -> State Checker ()
checkDesignator _ = error "designators not supported yet" -- TODO

-- Statemence.
checkStat :: CStat -> State Checker Type
checkStat (CLabel name s attrs nobe) = error "label not supported"
checkStat (CCase e s nobe) = error "case not supported"
checkStat (CCases e1 e2 s nobe) = error "cases not supported"
checkStat (CDefault s nobe) = error "default not supported"
checkStat (CExpr e' nobe) = maybe (return Base) checkExpr e'
checkStat (CCompound labels blox nobe) = mapM checkBlockItem blox >> return Base
checkStat (CIf e s1 s2' nobe) = error "if not supported"
checkStat (CSwitch e s nobe) = error "switch not supported"
checkStat (CWhile e s isDoWhile nobe) = error "while not supported"
checkStat (CFor e1'' e2' e3' s nobe) = error "for not supported"
checkStat (CGoto name nobe) = error "goto not supported"
checkStat (CGotoPtr e nobe) = error "gotoptr not supported"
checkStat (CCont nobe) = error "continue not supported"
checkStat (CBreak nobe) = error "break not supported"
checkStat (CReturn e' nobe) = error "return not supported"
checkStat (CAsm asm nobe) = checkAsmStmt asm >> return Base

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
       verifyAssign nobe True t1 t2
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
       t23 <- mergeType True nobe t2 t3
       info nobe "merged types" [M "left" t2, M "right" t3, M "result" t23]
       return t23
checkExpr (CBinary _ e1 e2 nobe) =
    do t1 <- checkExpr e1
       t2 <- checkExpr e2
       case (t1,t2) of
           (Pointer t1', Pointer t2') -> return Base
           (Pointer t1', _) -> return t1'
           (_, Pointer t2') -> return t2'
           (_, _) -> return Base
checkExpr (CCast d e nobe) =
    do t1 <- snd <$> checkOneDecl d
       t2 <- checkExpr e
       verifyAssign nobe True t1 t2
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
    let checkArg (t,e) = do t' <- checkExpr e; verifyAssign nobe True t t'
    in do t <- checkExpr e
          case t of
              Arrow argtypes ret a' ->
                  do when (length args /= length argtypes) $
                         warn nobe "argument number mismatch"
                              [M "expected" argtypes]
                     mapM checkArg $ zip argtypes args
                     case a' of
                         Just a ->
                             do verifyCall nobe a
                                g <- getContext
                                case effect a g of
                                    Just g2 -> do info nobe "changed context"
                                                      [g2]
                                                  setContext g2
                                    Nothing -> warn nobe "illegal context effect"
                                                   [M "attempted call" $ show a,
                                                    M "current context" $ show g]
                         Nothing ->
                             do g <- getContext
                                warn nobe "missing annotation during call"
                                    [M "current context" g]
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
    do t <- snd <$> checkOneDecl d
       mapM (checkInitListElem t) inits
       return t
checkExpr (CStatExpr s nobe) = checkStat s
checkExpr (CLabAddrExpr name nobe) = return Base
checkExpr (CBuiltinExpr b) = return Base

checkExpr_ e = checkExpr e >> return ()

