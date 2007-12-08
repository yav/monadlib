import MonadLib
import Data.Monoid

data X = X

instance Monoid X where
  mempty      = X
  mappend x y = seq y (seq x X)   -- NOTE: the monoid has control over
                                  -- evaluation order

ex1 = test (runId   . runWriterT)           -- prints ()
ex2 = test (runLift . runWriterT)           -- exception "step 1"
ex3 = test (runId   . runStrictWriterT)     -- exception "out 2"
ex4 = test (runLift . runStrictWriterT)     -- exception "step 1"

test run  = let x :: X
                (a,x) = run (error "step 1" >> put (error "out 2"))
            in print a
