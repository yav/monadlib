-- | An implementation of the exception monad.
module Monad.Except (Except, runExcept, module Monad.Prelude) where

import Monad.Prelude
import Control.Monad.Fix


-- | A computation that produces a value of type /a/, and may
-- raise exceptions of type /x/.
newtype Except x a  = E (forall r. (a -> r) -> (x -> r) -> r)

-- | Execute a computation: if it succeeds the result can
-- be found in the right component of the result (the _right_ answer :-).
-- If the computation failed, the exception is reported
-- in the left component of the result.
runExcept          :: Except x a -> Either x a
runExcept (E f)     = f Right Left


instance Functor (Except r) where
  fmap f (E g)      = E (\ok fail -> g (ok . f) fail)

instance Monad (Except r) where
  return a          = E (\ok _ -> ok a)
  E f >>= k         = E (\ok fail -> f (\a -> let E g = k a in g ok fail) fail)

instance BaseM (Except r) (Except r) where inBase x = x

instance MonadFix (Except e) where
  mfix f            = E (\ok fail -> let E g   = f a
                                         (a,r) = g (\a -> (a,ok a)) (\x -> (error "Except: mfix looped", fail x))
                                     in r)

instance ExceptM (Except x) x where
  raise e           = E (\_ fail -> fail e)
  handle (E f) h    = E (\ok fail -> f ok (\x -> let E g = h x in g ok fail))







