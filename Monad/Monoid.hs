-- | A /monoid/ is a type with a designated element 'mUnit', and a way to 
-- combine any two elements to get a new one (`mJoin`), subject to the 
-- monoid equations.
--
-- NOTE: This module is not specific to monads, and should probably be moved.
module Monad.Monoid where

import Data.Monoid


-- | A monoid on type 'a'.
data MonoidOn a     = Monoid { mUnit :: a, mJoin :: a -> a -> a }

-- | The monoid laws.  The parameters are universally quantified.
propLUnit m x       = mJoin m (mUnit m) x == x
propRUnit m x       = mJoin m x (mUnit m) == x
propAssoc m x y z   = mJoin m (mJoin m x y) z == mJoin m x (mJoin m y z)

-- | Join together a list of elments (i.e. lists are the free monoid on 'a').
fold               :: MonoidOn a -> [a] -> a
fold m xs           = foldr (mJoin m) (mUnit m) xs

-- | A way to name the monoid structure on types that have only one.
theMonoid          :: Monoid a => MonoidOn a 
theMonoid           = Monoid { mUnit = mempty, mJoin = mappend }

-- | The monoid of lists.
list               :: MonoidOn [a]
list                = theMonoid 

-- | The monoid of functions (endmorphisms).
fun                :: MonoidOn (a -> a) 
fun                 = theMonoid

-- | A product of 0 monoids.
unit               :: MonoidOn ()
unit                = theMonoid

-- | An additive monoid.
numAdd             :: Num a => MonoidOn a 
numAdd              = Monoid { mUnit = 0, mJoin = (+) }

-- | A multiplicative monoid.
numMult            :: Num a => MonoidOn a
numMult             = Monoid { mUnit = 1, mJoin = (*) }


-- | Product of 2 monoids.
pair2              :: MonoidOn a -> MonoidOn b 
                   -> MonoidOn (a,b)
pair2 m1 m2         = Monoid { mUnit = empty, mJoin = append }
  where
  empty             = (mUnit m1, mUnit m2)
  append ~(x1,y1) ~(x2,y2) =
              ( mJoin m1 x1 x2
              , mJoin m2 y1 y2
              )

-- | Product of 3 monoids.
pair3              :: MonoidOn a -> MonoidOn b -> MonoidOn c 
                   -> MonoidOn (a,b,c)
pair3 m1 m2 m3      = Monoid { mUnit = empty, mJoin = append }
  where
  empty             = (mUnit m1, mUnit m2, mUnit m3)
  append ~(x1,y1,z1) ~(x2,y2,z2) =
              ( mJoin m1 x1 x2
              , mJoin m2 y1 y2
              , mJoin m3 z1 z2
              )

-- | Product of 4 monoids.
pair4              :: MonoidOn a -> MonoidOn b -> MonoidOn c -> MonoidOn d 
                   -> MonoidOn (a,b,c,d)
pair4 m1 m2 m3 m4   = Monoid { mUnit = empty, mJoin = append }
  where
  empty             = (mUnit m1, mUnit m2, mUnit m3, mUnit m4)
  append ~(x1,y1,z1,a1) ~(x2,y2,z2,a2) =
              ( mJoin m1 x1 x2
              , mJoin m2 y1 y2
              , mJoin m3 z1 z2
              , mJoin m4 a1 a2
              )

