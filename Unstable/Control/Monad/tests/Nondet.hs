import Unstable.Control.Monad.Nondet

assoc1 a b c = (a `mplus` b) `mplus` c
assoc2 a b c = a `mplus` (b `mplus` c)


t1 = runNondet (return 1 `mplus` undefined)
t2 = runNondets (return 1 `mplus` undefined)
t3 = let x = return 1 `mplus` x in runNondets x



