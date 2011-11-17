-- Rules for checking
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Rules 
--(Context,Annotation,effect,subtype,satisfies,intersect,disjoin,entryContext,entryDefault)
where

data Context = Nested Int | Infinity deriving (Show,Eq) -- user-defined

data Effect = IncDec Int | Enable | Disable deriving Eq -- user-defined

newtype Rule = Rule Context deriving (Eq,Ord)
newtype Annotation = Annotation (Rule,Effect) deriving Eq

instance Ord Context where -- subtyping relation; user-defined
    (Nested x) <= (Nested y) = y <= x
    (Nested _) <= Infinity = False
    Infinity <= (Nested _) = True
    Infinity <= Infinity = True

instance Show Rule where -- user-defined
    show (Rule (Nested 0)) = "might_sleep"
    show (Rule (Nested x)) = "[unknown rule: " ++ show x ++ "]"
    show (Rule Infinity) = "wont_sleep"

instance Show Effect where -- user-defined
    show (IncDec 0) = "no_change"
    show (IncDec (-1)) = "exit_nested"
    show (IncDec 1) = "enter_nested"
    show Enable = "force_enable"
    show Disable = "force_disable" -- is this ever used?
    show (IncDec x) = "[unknown effect: " ++ show x ++ "]"

instance Show Annotation where
    show (Annotation (r, IncDec 0)) = show r
    show (Annotation (r,e)) = show r ++ ", " ++ show e

-- Change the context somehow.
effect :: Annotation -> Context -> Maybe Context -- user-defined
effect (Annotation (_,Enable)) _ = Just $ Nested 0
effect (Annotation (_,Disable)) _ = Just Infinity
effect (Annotation (_,_)) Infinity = Just Infinity
effect (Annotation (_,IncDec y)) (Nested x) =
    if x + y < 0 then Nothing else Just $ Nested $ x + y

-- Is the second argument a subtype of the first? (is assignment legal?)
subtype :: Annotation -> Annotation -> Bool
subtype (Annotation (Rule r1,_)) (Annotation (Rule r2,_)) = r2 <= r1

-- Does the provided code context satisfy the annotation?
satisfies :: Annotation -> Context -> Bool
satisfies (Annotation (Rule r,_)) c = r <= c

-- For intersect and disjoin, below.
merge :: (Context -> Context -> Context)
         -> Annotation -> Annotation -> Maybe Annotation
merge f (Annotation (Rule r1, e1)) (Annotation (Rule r2, e2)) =
    if e1 == e2 then Just $ Annotation (Rule $ f r1 r2, e1) else Nothing

-- Gives an annotation that is a subtype of both inputs.
intersect :: Annotation -> Annotation -> Maybe Annotation
intersect = merge min

-- Gives an annotation that both inputs are subtypes of.
disjoin :: Annotation -> Annotation -> Maybe Annotation
disjoin = merge max

-- Gives the most restrictive context that a function may be called from.
entryContext :: Annotation -> Context
entryContext (Annotation (Rule r, _)) = r

-- Gives a default "assume the best" context.
entryDefault :: Context -- user-defined
entryDefault = Nested 0
