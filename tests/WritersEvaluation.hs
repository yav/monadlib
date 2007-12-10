import MonadLib
import Data.Monoid

data X = X
instance Monoid X where
  mempty      = X
  mappend x y = seq y (seq x X)   -- evaluate second, then first

data Lazy a = Lazy a
instance Monoid a => Monoid (Lazy a) where
  mempty                      = Lazy mempty
  mappend ~(Lazy a) ~(Lazy b) = Lazy (mappend a b)

test1 run put = let (x,_) = run $ runWriterT
                          $ do error "step 1"
                               put (error "out 2")
                in print x

putX :: Monad m => X -> WriterT X m ()
putX = put

putY :: Monad m => X -> WriterT (Lazy X) m ()
putY = put . Lazy

ex1 = test1 runId   putX    -- exception "out 2"
ex2 = test1 runLift putX    -- exception "step 1"
ex3 = test1 runId   putY    -- print ()
ex4 = test1 runLift putY    -- exception "step 1"


