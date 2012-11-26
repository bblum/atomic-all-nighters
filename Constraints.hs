-- Constraint definition and solving.
-- Author: Ben Blum <bblum@andrew.cmu.edu>

module Constraints where

import qualified Rules as R

-- Identifiers for symbolic (i.e., unsolved) function rules and effects.
data RV = RV Int String
data EV = EV Int String

instance Eq RV where
    (RV a _) == (RV b _) = a == b
instance Eq EV where
    (EV a _) == (EV b _) = a == b
instance Ord RV where
    (RV a _) <= (RV b _) = a <= b
instance Ord EV where
    (EV a _) <= (EV b _) = a <= b

-- The elements of constraint expressions. Can be variables or fixed constants.
type R = Either RV R.Rule
type E = Either EV R.Effect

-- Note: the "<=" in the rule constraint is the subtyping comparison, not
-- the numeric comparison, so it's reversed (e.g., infinity <= 0).
data Constraint = EffectConstraint E [E] -- e = e1 + e2 + ... en
                | RuleConstraint R R [E] -- r <= r1 + e1 + e2 + ... en
                deriving Eq
