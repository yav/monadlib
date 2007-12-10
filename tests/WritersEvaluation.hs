import MonadLib
import Data.Monoid

data X = X

instance Monoid X where
  mempty      = X
  mappend x y = seq y (seq x X)   -- NOTE: the monoid has control over

test1 run = let x :: X
                (a,x) = run (error "step 1" >> put (error "out 2"))
            in print a

ex1 = test1 (runId   . runWriterT)           -- prints ()
ex2 = test1 (runLift . runWriterT)           -- exception "step 1"
ex3 = test1 (runId   . runStrictWriterT)     -- exception "out 2"
ex4 = test1 (runLift . runStrictWriterT)     -- exception "step 1"

test2 run = case run $ runExceptionT $ lift (error "expensive writer") of
              (Right _,x) -> let _ = x :: Y in "yes"
              (Left  _,_) -> "no"

data Y = Y
instance Monoid Y where
  mempty  = Y
  mappend _ _ = Y

ex5 = test2 (runId   . runWriterT)          -- "yes"
ex6 = test2 (runId   . runStrictWriterT)    -- exception "expensive writer"
ex7 = test2 (runLift . runWriterT)          -- ditto
ex8 = test2 (runLift . runStrictWriterT)    -- ditto

