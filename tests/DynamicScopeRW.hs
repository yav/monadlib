import MonadLib


test1 = runId $ runReaderT 'a' $ runContT return $
     do a <- ask
        b <- local 'b' ask
        c <- ask
        d <- local 'c' ask
        e <- ask
        return (a,b,c,d,e)

test2 = runId $ runWriterT $ runContT return $
  do put "a"
     (_,w1) <- collect (put "b")
     put "c"
     (_,w2) <- collect (put "d")
     return (w1,w2)

