-- Constraint definition and solving.
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Constraints where

import Data.List (intercalate)

import qualified Rules as R

-- Identifiers for symbolic (i.e., unsolved) function rules and effects.
data RV = RV Int String
data EV = EV Int String

type Unknown = (RV,EV)

instance Eq RV where
    (RV a _) == (RV b _) = a == b
instance Eq EV where
    (EV a _) == (EV b _) = a == b
-- instance Ord RV where -- in case I need to put them in maps, I guess?
--     (RV a _) <= (RV b _) = a <= b
-- instance Ord EV where
--     (EV a _) <= (EV b _) = a <= b
instance Show RV where
    show (RV _ name) = "R{" ++ name ++ "}"
instance Show EV where
    show (EV _ name) = "E{" ++ name ++ "}"

-- The elements of constraint expressions. Can be variables or fixed constants.
data R = RuleVar   RV | RuleConst   R.Rule   deriving Eq
data E = EffectVar EV | EffectConst R.Effect deriving Eq

instance Show R where
    show (RuleVar rv) = show rv
    show (RuleConst rule) = show rule
instance Show E where
    show (EffectVar ev) = show ev
    show (EffectConst effect) = show effect

-- Note: the "<=" in the rule constraint is the subtyping comparison, not
-- the numeric comparison, so it's reversed (e.g., infinity <= 0).
data Constraint = EffectConstraint E [E] -- e = e1 + e2 + ... en
                | RuleConstraint R R [E] -- r <= r1 + e1 + e2 + ... en
                deriving Eq

instance Show Constraint where
    show (EffectConstraint e es) =
        show e ++ "=" ++ (intercalate "+" $ map show es)
    show (RuleConstraint r r1 es) =
        show r ++ "<=" ++ show r1 ++ "+" ++ (intercalate "+" $ map show es)
