module Nondet where

import Prop
import Unstable.Control.Monad.Trans
import Control.Monad(MonadPlus(..))


findAll_return x    = findAll (return x) === return [x]
findAll_bind m f    = findAll (m >>= f) === do xs <- findAll m
                                               fmap concat (mapM (findAll . f) xs)

findAll_findAll m   = findAll (findAll m) === fmap (\x -> [x]) (findAll m) 
findAll_mzero       :: MonadNondet m => Prop (m [a])
findAll_mzero       = findAll mzero === return []

findAll_mplus m1 m2 = findAll (m1 `mplus` m2) === do xs <- findAll m1
                                                     ys <- findAll m2
                                                     return (xs ++ ys)

findAll_commit m    = do xs <- findAll (commit m) 
                         assert (length xs <= 1)

commit_return x     = commit (return x) === return x
commit_findAll m    = commit (findAll m) === findAll m

commit_mzero        :: MonadNondet m => Prop (m a)
commit_mzero        = commit mzero === mzero

monoL m             = mzero `mplus` m === m
monoR m             = m `mplus` mzero === m
monoA m1 m2 m3      = m1 `mplus` (m2 `mplus` m3) === (m1 `mplus` m2) `mplus` m3






