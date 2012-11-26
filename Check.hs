-- Checking the AST, with appropriate handling.
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Check where

import Control.Applicative ((<$>))
import Control.Monad.State
import qualified Data.Traversable as T (sequence,mapM)
import qualified Data.Map as Map
import Data.List (intercalate)
import qualified Data.Foldable as F (any)
import Data.Maybe (mapMaybe,catMaybes,fromMaybe,isNothing,fromJust)
import Language.C.Data.Ident (Ident,builtinIdent)
import Language.C.Data.Node (NodeInfo,fileOfNode,posOfNode)
import Language.C.Data.Position (posRow) -- ,posColumn)
import Language.C.Syntax.AST

import Rules
import Attributes
import Constraints

data TypeName = VarName Ident | StructName Ident | TypedefName Ident
                deriving (Show,Eq,Ord)

data Type = Base
          -- [Type]: arguments; Type: return type; Bool: isVariadic
          | Arrow [Type] Type Bool (Maybe Annotation)
          | Pointer Type
          | Struct (Maybe Ident) (Map.Map Ident Type)
          | IncompleteStruct Ident
          deriving Eq

-- TODO: data Message

data MessageLine a = (Show a) => M String a

data Checker = Checker {
    -- The current context of the code
    context :: Context,
    -- identifier mappings
    types :: Map.Map TypeName Type,
    -- output
    msgs :: [String],
    -- value is "Left gs" if the label hasn't been hit yet (a list of all
    -- contexts that the label is entered with), and "Right g" if it has (the
    -- context that was decided upon for its entry, for future backwards gotos)
    labels :: Map.Map Ident (Either [Context] Context),
    -- break and continue targets. these represent the contexts encountered at
    -- each break and continue statement, to be collected by the call at the
    -- construct they belong to.
    breaks :: [[Context]],
    continues :: [[Context]],
    -- The end contexts for all the case branches in a switch statement.
    branches :: [[Context]],
    -- 'Return' tracking.
    returned :: [[Bool]],
    ends :: [[Context]]
}

builtinTypes = Map.fromList
    [(TypedefName $ builtinIdent "__builtin_va_list", Base)]

defaultChecker = Checker undefined builtinTypes [] Map.empty [] [] [] [] []

--
-- Instants.
--

instance Show Type where
    show Base = "T"
    show (Arrow args ret isVariadic a') =
        let argstrs = map show args ++ (if isVariadic then ["..."] else [])
            argstr = case args of [] -> "unit"
                                  _ -> intercalate ", " argstrs
        in "(" ++ argstr ++ " -> " ++ show ret
           ++ " {" ++ maybe "???" show a' ++ "})"
    show (Pointer t) = show t ++ "*"
    show (Struct (Just name) contence) =
        "struct " ++ show name ++ " {" ++ show contence ++ "}"
    show (Struct Nothing contence) = "struct {" ++ show contence ++ "}"
    show (IncompleteStruct name) = "struct " ++ show name

instance Show (MessageLine a) where
    show (M str a) = str ++ " \t" ++ show a

--
-- Flow control helpers.
--

enterLoop :: State Checker ()
enterLoop =
    modify (\s -> s { breaks = []:(breaks s), continues = []:(continues s) })

-- Returns the broken contexts and the continued contexts, respectively.
exitLoop :: State Checker ([Context], [Context])
exitLoop =
    do bs <- breaks <$> get
       cs <- continues <$> get
       case (bs,cs) of
           (b:bs', c:cs') ->
               do modify (\s -> s { breaks = bs', continues = cs' })
                  return (b,c)
           _ -> error "inconsistent break/continue stack exiting loop"

breakLoop :: State Checker ()
breakLoop =
    do g <- getContext
       bs <- breaks <$> get
       case bs of
           (b:bs') -> modify (\s -> s { breaks = (g:b):bs' })
           [] -> error "inconsistent break stack in break"

continueLoop :: State Checker ()
continueLoop =
    do g <- getContext
       cs <- continues <$> get
       case cs of
           (c:cs') -> modify (\s -> s { continues = (g:c):cs' })
           [] -> error "inconsistent continue stack in continue"

-- Switch statemence

enterSwitch :: State Checker ()
enterSwitch =
    modify (\s -> s { breaks = []:(breaks s), branches = []:(branches s),
                      returned = []:(returned s) })

exitSwitch :: State Checker [Context]
exitSwitch =
    do ks <- breaks <$> get
       bs <- branches <$> get
       rs <- returned <$> get
       case (ks,bs,rs) of
           (k:ks', b:bs', r:(r1:r1s):rs') ->
               do modify (\s -> s { breaks = ks', branches = bs',
                                    returned = ((and $ r1:r):r1s):rs' })
                  return k -- XXX FIXME: doesn't account for fallthrough;
           _ -> error "inconsistent break or return stack exiting switch"

enterCase :: State Checker Context
enterCase =
    do rs <- returned <$> get
       case rs of
           (r:rs') -> modify (\s -> s { returned = (False:r):rs' })
           [] -> error "inconsistent return stack entering case"
       getContext

exitCase :: Context -> State Checker ()
exitCase g0 =
    do g <- getContext
       ss <- branches <$> get
       case ss of
           (gs:ss') -> do modify (\s -> s { branches = (g:gs):ss' })
           [] -> error "inconsistent branches stack exiting case"
       setContext g0 -- XXX FIXME: this silently breaks in fall-through cases

-- If statemence

enterIf :: State Checker ()
enterIf =
    modify (\s -> s { branches = []:(branches s), returned = []:(returned s) })

exitIf :: NodeInfo -> State Checker (Bool, Bool)
exitIf nobe =
    do bs <- branches <$> get
       rs <- returned <$> get
       case (bs,rs) of
           (b:bs', (r@[case1,case2]):(r1:r1s):rs') ->
               do modify (\s -> s { branches = bs',
                                    returned = ((and $ r1:r):r1s):rs' })
                  return (case1,case2)
           _ -> error $ "inconsistent branch or return stack exiting if"
                        ++ show (bs,rs) ++ show nobe

enterBranch :: State Checker Context
enterBranch =
    do rs <- returned <$> get
       case rs of
           (r:rs') -> modify (\s -> s { returned = (False:r):rs' })
           [] -> error "inconsistent return stack entering branch"
       getContext

exitBranch :: Context -> State Checker Context
exitBranch g0 =
    do g <- getContext
       ss <- branches <$> get
       case ss of
           (gs:ss') -> do modify (\s -> s { branches = (g:gs):ss' })
           [] -> error "inconsistent branches stack exiting branch"
       setContext g0
       return g

-- Miscellaneous flow

doReturn :: NodeInfo -> State Checker () -- can't really call it 'return'
doReturn nobe =
    do g <- getContext
       endings <- ends <$> get
       case endings of
           (gs:rest) -> modify (\s -> s { ends = (g:gs):rest })
           [] -> error "attempt to return outside of a function??"
       rs <- returned <$> get
       case rs of
           ((True:r1s):rs') -> do warn nobe "double return" emptyMsg
                                  modify (\s -> s { returned = (True:r1s):rs' })
           ((False:r1s):rs') -> modify (\s -> s { returned = (True:r1s):rs' })
           _ -> error "inconsistent returned stack"

checkMergeContexts :: NodeInfo -> Context -> [Context] -> State Checker ()
checkMergeContexts nobe g0 gs =
    when (any (/= g0) gs) $
        warn nobe "merging flow" $
            [M "current context" g0] ++ map (M "incoming context") gs
            ++ [M "most restrictive" $ minimum $ g0:gs]

mergeContexts :: NodeInfo -> Context -> [Context] -> State Checker ()
mergeContexts nobe g0 gs =
    do let g = minimum $ g0:gs
       checkMergeContexts nobe g0 gs
       modify (\s -> s { context = g })

checkBackEdge :: NodeInfo -> Context -> Context -> State Checker ()
checkBackEdge nobe g0 g =
    do g <- getContext
       when (g < g0) $
           warn nobe "backward jump with a more restrictive context"
               [M "old context" g0, M "incoming context" g]

gotoLabel :: NodeInfo -> Context -> Ident -> State Checker ()
gotoLabel nobe g name =
    do ls <- labels <$> get
       case Map.lookup name ls of
           Just (Left gs) ->
               -- Append the new context to the list of incoming contexts.
               do modify (\s -> s { labels = Map.insert name (Left $ g:gs) ls })
                  info nobe "forward goto to label with context"
                       [M "target" $ show name, M "incoming" $ show g]
           Just (Right g0) ->
               -- Check the new context against the already decided-upon one.
               checkBackEdge nobe g0 g
           Nothing ->
               do modify (\s -> s { labels = Map.insert name (Left [g]) ls})
                  info nobe "forward goto to label with context"
                       [M "target" $ show name, M "incoming" $ show g]

meetLabel :: NodeInfo -> Context -> Ident -> State Checker ()
meetLabel nobe g0 name =
    do ls <- labels <$> get
       case Map.lookup name ls of
           Just (Left gs) ->
               do let g = minimum $ g0:gs
                  mergeContexts nobe g0 gs
                  modify (\s -> s { labels = Map.insert name (Right g) ls})
           Just (Right _) ->
               do err nobe "meeting an already-met label (duplicate label?)"
                      [M "label name" name]
                  modify (\s -> s { labels = Map.insert name (Right g0) ls})
           Nothing ->
               modify (\s -> s { labels = Map.insert name (Right g0) ls})

--
-- Helpers.
--

getType :: TypeName -> State Checker (Maybe Type)
getType name =
    do t' <- Map.lookup name <$> types <$> get
       -- strip outer "pointer" from function pointers. see also checkDeclr
       case t' of Just (Pointer (x@(Arrow _ _ _ _))) -> return $ Just x
                  Just (IncompleteStruct sub) ->
                      do t2' <- Map.lookup (StructName sub) <$> types <$> get
                         case t2' of Just t2@(Struct _ _) -> return $ Just t2
                                     _ -> return t'
                  _ -> return t'

getTypeOrBase :: NodeInfo -> TypeName -> State Checker Type
getTypeOrBase nobe name =
    do t' <- getType name
       case t' of Just t -> return t
                  Nothing -> do warn nobe "ident's type not in context" [name]
                                return Base

addType :: NodeInfo -> TypeName -> Type -> State Checker ()
addType nobe name t =
    do prior' <- Map.lookup name <$> types <$> get
       case prior' of
           Just (IncompleteStruct _) ->
               info nobe "incomplete struct being defined"
                   [M "named   " $ show name, M "contence" $ show t]
           Just t0 ->
               if t == t0 then
                   info nobe "type is being shadowed (same type)"
                       [M "named" $ show name, M "type " $ show t]
               else
                   warn nobe "type is being shadowed (different type)"
                       [M "named   " $ show name, M "old type" $ show t0,
                        M "new type" $ show t]
           Nothing -> return ()
       modify (\s -> s { types = Map.insert name t $ types s })

getState :: State Checker Checker
getState = get

getContext :: State Checker Context
getContext = context <$> get

restoreState :: Checker -> State Checker ()
restoreState oldstate =
    modify (\s -> s { context = context oldstate, types = types oldstate })

-- TODO: deprecate
mergeState :: NodeInfo -> Checker -> Checker -> State Checker ()
mergeState nobe state1 state2 =
    let (g1,g2) = (context state1, context state2)
    in do when (g1 /= g2) $ warn nobe "context mismatch in flow control" [g1,g2]
          modify (\s -> s { context = min g1 g2 }) -- use the subbest type

setContext :: Context -> State Checker ()
setContext g = modify (\s -> s { context = g })

getTypes :: State Checker (Map.Map TypeName Type)
getTypes = types <$> get

setTypes :: Map.Map TypeName Type -> State Checker ()
setTypes ts = modify (\s -> s { types = ts })

--
-- Messaging
--

emptyMsg = [] :: [String]

msg :: (Show a) => String -> NodeInfo -> String -> [a] -> State Checker ()
msg prefix nobe str noobs =
    let -- (row,col) = (posRow $ posOfNode nobe, posColumn $ posOfNode nobe)
        row = posRow $ posOfNode nobe
        file = case fileOfNode nobe of Just f -> f; Nothing -> "((unknown))"
        mess0 = prefix ++ ": at " ++ file ++ ":" ++ (show row) ++ ": " ++ str
        mess = foldl (\output noob -> output ++ "\n\t" ++ show noob) mess0 noobs
    in modify (\s -> s { msgs = mess:(msgs s) })

err :: (Show a) => NodeInfo -> String -> [a] -> State Checker ()
err = msg "ERROR" -- TODO: make this fail

warn :: (Show a) => NodeInfo -> String -> [a] -> State Checker ()
warn = msg "warning"

info :: (Show a) => NodeInfo -> String -> [a] -> State Checker ()
-- info = msg "(info)"
info _ _ _ = return () -- TODO: better msg datatype

--
-- Verification.
--

containsArrows :: Type -> Bool
containsArrows (Base) = False
containsArrows (Pointer t) = containsArrows t
containsArrows (Arrow _ _ _ _) = True
containsArrows (Struct _ contence) = F.any containsArrows contence
containsArrows (IncompleteStruct _) = False

-- if doDisjoin, then disjoin the types; else, intersect the types.
-- this comes into play when doing arrow annotations.
mergeType :: Bool -> NodeInfo -> Type -> Type -> State Checker Type
mergeType doDisjoin nobe (Base) (Base) = return Base
mergeType doDisjoin nobe (Pointer t1) (Pointer t2) =
    Pointer <$> mergeType doDisjoin nobe t1 t2
mergeType doDisjoin nobe (Struct name1 m1) (Struct name2 m2) =
    if (m1 == m2) then
        -- would disjoin structs, but (a) technical difficulties and (b) stupid
        liftM (Struct name1) $ T.sequence $
            Map.intersectionWith (mergeType doDisjoin nobe) m1 m2
    else
        do warn nobe "incompatible structs during type intersection" [m1,m2]
           return Base
mergeType doDisjoin nobe t1@(Arrow args1 ret1 iv1 a1) t2@(Arrow args2 ret2 iv2 a2) =
    do -- contravariance on the disjoin/intersect operator
       -- actually only theoretically sound because of the total ordering.
       args <- zipWithM (mergeType (not doDisjoin) nobe) args1 args2
       ret <- mergeType doDisjoin nobe ret1 ret2
       when (iv1 /= iv2) $ warn nobe "variadicity mismatch in fn merge" emptyMsg
       let iv = iv1 || iv2
       case (a1,a2) of
           (Just a1', Just a2') -> -- good case
               case (if doDisjoin then disjoin else intersect) a1' a2' of
                   a@(Just _) -> return $ Arrow args ret iv a
                   Nothing -> do err nobe "unmergable annotations" [a1',a2']
                                 return $ Arrow args ret iv (Just a1') -- boo
           (a@(Just _), Nothing) ->
               do warn nobe "missing annotation for merge on right branch" [t1,t2]
                  return $ Arrow args ret iv a
           (Nothing, a@(Just _)) ->
               do warn nobe "missing annotation for merge on left branch" [t1,t2]
                  return $ Arrow args ret iv a
           (Nothing, Nothing) ->
               do warn nobe "missing annotation for merge on both branches" [t1,t2]
                  return $ Arrow args ret iv Nothing
mergeType doDisjoin nobe (IncompleteStruct _) t2 =
    mergeType doDisjoin nobe Base t2
mergeType doDisjoin nobe t1 (IncompleteStruct _) =
    mergeType doDisjoin nobe t1 Base
mergeType doDisjoin nobe t1 t2 =
    do warn nobe "type mismatch during merge" [t1,t2]; return Base

-- TODO: need to worry about extra pointer indirections around arrows? &malloc
-- The bool argument expresses whether subtyping is allowed.
verifyAssign :: NodeInfo -> Bool -> Type -> Type -> State Checker ()
verifyAssign nobe subtyping t1@(Arrow args1 ret1 iv1 a1) t2@(Arrow args2 ret2 iv2 a2) =
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
          when (iv1 /= iv2) $
              warn nobe "varidicity mismatch in fn verification" emptyMsg
          verifyAnnotation subtyping
          verifyAssign nobe subtyping ret1 ret2
          mapM_ (uncurry $ verifyAssign nobe subtyping)
                (zip args2 args1) -- contravariant!
          info nobe "verified assignment" [M "dest type" t1, M "src type" t2]
-- Pointers
verifyAssign nobe subtyping (Pointer Base) (Pointer Base) = return ()
verifyAssign nobe subtyping (Pointer Base) t2 =
    info nobe "allowing cast to void *" [t2]
verifyAssign nobe subtyping t1 (Pointer Base) =
    info nobe "allowing cast from void *" [t1]
verifyAssign nobe subtyping (Pointer t1) (Pointer t2) =
    verifyAssign nobe False t1 t2 -- Reference cells are invariant.
-- Structs
verifyAssign nobe subtyping (Struct _ m1) (Struct _ m2) =
    mapM_ (uncurry $ verifyAssign nobe subtyping) -- Structs aren't quite refs.
          (zip (Map.elems m1) (Map.elems m2))
verifyAssign nobe subtyping Base Base = return ()
verifyAssign nobe subtyping (IncompleteStruct _) t2 =
    verifyAssign nobe subtyping Base t2 -- trying to resolve this causes stack overflow
verifyAssign nobe subtyping t1 (IncompleteStruct _) =
    verifyAssign nobe subtyping t1 Base
-- No match
verifyAssign nobe subtyping t1 t2 =
    if containsArrows t1 || containsArrows t2 then
        warn nobe "verification type mismatch, with arrows" [t1,t2]
    else
        info nobe "verification type mismatch (no arrows)" [t1,t2]

verifyCall :: NodeInfo -> Annotation -> State Checker ()
verifyCall nobe a =
    do g <- getContext
       when (not $ satisfies a g) $
           err nobe "illegal function call"
               [M "target function" $ show a, M "while in context" $ show g]

-- Mashes an annotation into an arrow type that might already have one.
injectAnnotation :: NodeInfo -> Type -> Maybe Annotation -> State Checker Type
injectAnnotation nobe (Arrow args ret iv (Just a0)) (Just a) =
    do warn nobe "multiply-differently-annotated function" [a0,a]
       return $ Arrow args ret iv (Just a)
injectAnnotation nobe (Arrow args ret iv Nothing) (Just a) =
    return $ Arrow args ret iv (Just a)
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
checkExtDecl (CAsmExt _ _) = return ()

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
          modify (\s -> s { returned = [False]:(returned s), ends = []:(ends s)})
          checkStat body
          -- check exit context against advertised effect
          gnew <- getContext
          case a' of Just a ->
                         when (effect a g /= Just gnew) $
                             err nobe "exit context != advertised effect"
                                 [M "entry context" g, M "exit context" gnew]
                     Nothing -> return ()
          -- check all returned contexts against each other
          endings <- ends <$> get
          case endings of
              (gs:rest) ->
                  do when (not $ all (== gnew) gs) $
                         err nobe "not all exit contexts match each other"
                             (gnew:gs)
                     modify (\s -> s { ends = rest })
              _ -> error "inconsistent ends stack at function end"
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
       case list of [] -> return (Nothing, Base) -- error "empty decl??"
                    [x] -> return x
                    x:rest -> do warn nobe "ignoring extra decls" rest; return x

checkStructUnion :: CStructUnion -> State Checker Type
checkStructUnion (CStruct tag (Just name) Nothing attrs nobe) =
    do t' <- getType $ StructName name
       case t' of -- Honour incomplete struct declarations
           Just t -> return t
           Nothing -> do addType nobe (StructName name) (IncompleteStruct name)
                         return $ IncompleteStruct name
checkStructUnion (CStruct tag name' (Just decls) attrs nobe) =
    let namedOnly (Just x, y) = Just (x, y)
        namedOnly (Nothing, _) = Nothing
    in do contence <- Map.fromList <$> mapMaybe namedOnly <$>
                          concat <$> mapM (checkDecl False) decls
          case name' of
              Just name -> addType nobe (StructName name)
                                   (Struct (Just name) contence)
              Nothing -> return ()
          return $ Struct name' contence
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
       case (t,t0') of
           -- For qualifiers like "unsigned", "long", "double", etc.
           (Base, Just Base) -> return (Just Base, a0', x)
           (_, Just t0) ->
               do warn nobe "multi-typed declspec!"
                      [M "overriding:" t0, M "with new type:" t]
                  return (Just t, a0', x)
           (_, Nothing) -> return (Just t, a0', x)
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
       let t = case t' of (Pointer x@(Arrow _ _ _ _)) -> x; _ -> t'
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
       (args,isVariadic) <-
           case args'' of
               Left oldstyle ->
                   do warn nobe "old-style args ignored" [Arrow [] t False a']
                      return ([],False)
               Right (decls,isVariadic) ->
                   do args <- map snd <$> concat <$>
                                  mapM (checkDecl addArgs) decls
                      return (args,isVariadic)
       info nobe "processed FunDeclr" [Arrow args t isVariadic a']
       -- XXX: some bug causes this to not like unnamed arguments.
       return $ Arrow args t isVariadic a'

-- Misc
checkArrSize :: CArrSize -> State Checker ()
checkArrSize (CNoArrSize _) = return ()
checkArrSize (CArrSize _ e) = checkExpr_ e

checkInit :: Type -> CInit -> State Checker ()
checkInit t0 (CInitExpr e nobe) =
    do t <- checkExpr e
       verifyAssign nobe True t0 t
checkInit t0 (CInitList inits nobe) = mapM_ (checkInitListElem nobe t0) inits

checkInitListElem :: NodeInfo -> Type -> ([CDesignator], CInit)
                     -> State Checker ()
checkInitListElem nobe (Pointer t) ([], i) = checkInit t i
checkInitListElem nobe (Base) ([], i) = checkInit Base i -- dur
checkInitListElem nobe _ ([], i) =
    do warn nobe "empty struct member designator not supported." emptyMsg
       checkInit Base i
checkInitListElem nobe t0 (list, i) =
    do t <- foldM checkDesignator t0 list
       checkInit t i

checkDesignator :: Type -> CDesignator -> State Checker Type
checkDesignator t0 (CArrDesig e nobe) =
    do checkExpr_ e
       case t0 of (Pointer t) -> return t
                  _ -> do warn nobe "array designator in non-array type" [t0]
                          return Base
checkDesignator t0 (CRangeDesig e1 e2 nobe) =
    do checkExpr_ e1
       checkExpr_ e2
       case t0 of (Pointer t) -> return t
                  _ -> do warn nobe "array designator in non-array type" [t0]
                          return Base
checkDesignator t0@(Struct _ contence) (CMemberDesig name nobe) =
    case Map.lookup name contence of
        Just t -> return t
        Nothing -> do warn nobe "member designator not found in struct type"
                          [M "type" $ show t0, M "member" $ show name]
                      return Base
checkDesignator t0 (CMemberDesig name nobe) =
    do warn nobe "member designator for non-struct type"
           [M "type" $ show t0, M "member" $ show name]
       return Base

-- Statemence.
checkStat :: CStat -> State Checker Type
checkStat (CLabel name s attrs nobe) =
    do g0 <- getContext
       meetLabel nobe g0 name
       checkStat s
checkStat (CCase e s nobe) =
    do g0 <- enterCase
       checkExpr_ e
       checkStat_ s
       exitCase g0
       return Base
checkStat (CCases e1 e2 s nobe) =
    do g0 <- enterCase
       checkExpr_ e1
       checkExpr_ e2
       checkStat_ s
       exitCase g0
       return Base
checkStat (CDefault s nobe) =
    do g0 <- enterCase
       checkStat_ s
       exitCase g0
       return Base
checkStat (CExpr e' nobe) = maybe (return Base) checkExpr e'
checkStat (CCompound labels blox nobe) =
    do ts <- getTypes
       mapM_ checkBlockItem blox
       setTypes ts
       return Base
checkStat (CIf e s1 s2' nobe) =
    do checkExpr_ e
       enterIf
       g0 <- enterBranch
       checkStat_ s1
       g1 <- exitBranch g0
       _ <- enterBranch
       maybe (return ()) checkStat_ s2'
       g2 <- exitBranch g0
       (returned1,returned2) <- exitIf nobe -- See which branches did return or not.
       case (returned1,returned2) of
           (False,False) -> mergeContexts nobe g1 [g2]
           (True,False) -> setContext g2
           (False,True) -> setContext g1
           _ -> return () -- Means we returned wholesale.
       return Base
checkStat (CSwitch e s nobe) =
    do checkExpr_ e
       g0 <- getContext
       enterSwitch
       checkStat_ s
       gs <- exitSwitch
       mergeContexts nobe g0 gs
       return Base
checkStat (CWhile e s isDoWhile nobe) =
    do g0 <- getContext
       when (not isDoWhile) $ checkExpr_ e
       enterLoop
       checkStat_ s
       (bs,cs) <- exitLoop
       when (isDoWhile) $ checkExpr_ e
       g <- getContext
       checkMergeContexts nobe g0 cs -- continues go backwards
       checkBackEdge nobe g0 g
       mergeContexts nobe g bs -- breaks go forwards
       return Base
checkStat (CFor e1'' e2' e3' s nobe) =
    do ts <- getTypes
       case e1'' of
           (Left Nothing) -> return ()
           (Left (Just e)) -> checkExpr_ e
           (Right d) -> checkDecl_ d
       g0 <- getContext
       maybe (return ()) checkExpr_ e2'
       enterLoop
       checkStat_ s
       (bs,cs) <- exitLoop
       g1 <- getContext
       mergeContexts nobe g1 cs -- continues go forwards
       maybe (return ()) checkExpr_ e3'
       g2 <- getContext
       checkBackEdge nobe g0 g2
       mergeContexts nobe g2 bs -- breaks go even more forwards
       return Base
checkStat (CGoto name nobe) =
    do g <- getContext
       gotoLabel nobe g name
       return Base
checkStat (CGotoPtr e nobe) =
    do warn nobe "dynamic pointer goto being ignored." emptyMsg
       checkExpr_ e
       return Base
checkStat (CCont nobe) = continueLoop >> return Base
checkStat (CBreak nobe) = breakLoop >> return Base
checkStat (CReturn e' nobe) =
    do t <- maybe (return Base) checkExpr e'
       doReturn nobe
       return t
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
    let checkArg (t,e) =
            do t0 <- checkExpr e; verifyAssign nobe True t t0; return t0
    in do t <- checkExpr e
          case t of
              Arrow argtypes ret isVariadic a' ->
                  do callargtypes <- mapM checkArg $ zip argtypes args
                     case isVariadic of
                         True ->
                             let varargs = drop (length argtypes) callargtypes
                             in when (any containsArrows varargs) $
                                    warn nobe "variadic args contain arrows"
                                        [M "... =" varargs]
                         False ->
                             when (length args /= length argtypes) $
                                 warn nobe "argument number mismatch"
                                     [M "expected" argtypes]
                     case a' of
                         Just a ->
                             do verifyCall nobe a
                                g <- getContext
                                case effect a g of
                                    Just g2 -> do info nobe "changed context"
                                                      [g2]
                                                  setContext g2
                                    Nothing -> err nobe "illegal context effect"
                                                   [M "attempted call " $ show a,
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
                Just (Pointer (IncompleteStruct sub)) ->
                    do t <- getType $ StructName sub
                       maybe (return $ Pointer Base) (return . Pointer) t
                Just (IncompleteStruct sub) ->
                    do t <- getType $ StructName sub
                       maybe (return Base) return t
                Just t -> return t
                Nothing ->
                    do warn nobe "missing struct member" [name]; return Base
    in do t <- checkExpr e
          case isderef of -- if/else didn't parse for some reason...
              True ->
                  case t of Pointer (Struct _ contence) -> memberType contence
                            Pointer (IncompleteStruct sub) ->
                                do t2 <- getType $ StructName sub
                                   case t2 of
                                       Just (Struct _ contence) ->
                                           memberType contence
                                       _ ->
                                           do warn nobe "bad incomplete->member"
                                                  [t]
                                              return Base
                            _ -> do warn nobe "bad type for struct->member" [t]
                                    return Base
              False ->
                  case t of Struct _ contence -> memberType contence
                            IncompleteStruct sub ->
                                do t2 <- getType $ StructName sub
                                   case t2 of
                                       Just (Struct _ contence) ->
                                           memberType contence
                                       _ ->
                                           do warn nobe "bad incomplete.member"
                                                  [t]
                                              return Base
                            _ -> do warn nobe "bad type for struct.member" [t]
                                    return Base
checkExpr (CVar name nobe) = getTypeOrBase nobe $ VarName name
checkExpr (CConst _) = return Base
checkExpr (CCompoundLit d inits nobe) =
    do t <- snd <$> checkOneDecl d
       mapM (checkInitListElem nobe t) inits
       return t
checkExpr (CStatExpr s nobe) = checkStat s
checkExpr (CLabAddrExpr name nobe) = return Base
checkExpr (CBuiltinExpr b) = return Base

checkExpr_ e = checkExpr e >> return ()

