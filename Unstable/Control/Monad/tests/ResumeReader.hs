import Unstable.Control.Monad.Transformers
import Unstable.Control.Monad.Identity

type R = (String,String,String)

t1,t2,t3 :: (MonadReader String m, MonadResume m) => m R

t1 = do e1 <- ask
        e2 <- local ('a':) ask
        e3 <- ask
        return (e1,e2,e3)

t1_1 = ("x","ax","x")
t1_2 = ("x","ax","x")


t2 = do e1 <- ask
        e2 <- delay ask
        e3 <- ask 
        return (e1,e2,e3)
      
t2_1 = ("x","x","x")
t2_2 = ("x","x","x")


t3 = do e1 <- ask
        e2 <- local ('a':) $ delay ask
        e3 <- ask
        return (e1,e2,e3)
    
t3_1 = ("x","x","x")
t3_2 = ("x","ax","x")

{-
t4 = do e1 <- ask
        e2 <- local ('a':) $ delay ask
        e3 <- ask
        return (e1,e2,e3)
 -}



run1 m = runIdentity $ runReader "x" $ hyper $ m
run2 m = runIdentity $ hyper $ runReader "x" $ m



test' :: (forall m. (MonadReader String m, MonadResume m) => m R) -> R -> R -> Bool
test' m r1 r2  = run1 m == r1 && run2 m == r2


tests = [ test' t1 t1_1 t1_2,
          test' t2 t2_1 t2_2,
          test' t3 t3_1 t3_2
        ]

main = print (and tests)
      





