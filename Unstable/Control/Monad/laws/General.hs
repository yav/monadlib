module General where

import Prop
import Unstable.Control.Monad.Trans

munit_left f x    = (return x >>= f) === f x
munit_right m     = (m >>= return) === m
massoc m f g      = (m >>= f >>= g) === (m >>= \x -> f x >>= g)

lift_return x     = lift (return x) === return x
lift_bind m f     = lift (m >>= f) === (lift m >>= lift . f)

inBase_return x   = return x === inBase (return x)


