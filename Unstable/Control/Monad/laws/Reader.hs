module Reader where

import Prop
import Unstable.Control.Monad.Trans


local_return f m    = local f (return m) === return m
-- The operation 'local f' has no effect on variables.

local_bind f m s    = local f (m >>= s) === (local f m >>= local f . s) 
-- To apply 'local f' to a term obtained by applying a substitution
-- 's' to a term 'm', we apply 'local f' to 'm', and the apply a new
-- substitution, that first applies 's', but then also applies 'local f' to
-- the result.

local_ask f         = local f ask === (ask >>= return . f)
-- Applying 'local f' to 'ask' is the same as apply the substitution that
-- renames variables by 'f' to 'ask'.

local_local f g m   = local f (local g m) === local (g . f) m

get_bind m n        = (m >> n) === n
-- Only true for reader on its own




