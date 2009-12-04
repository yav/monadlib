import MonadLib

testR () = twice $
  do a <- ask
     b <- local 'b' ask
     c <- ask
     d <- local 'c' ask
     e <- ask
     return (a,b,c,d,e)

testRDyn  = runId $ runReaderT 'a'  $ runContT return $ testR ()
testRStat = runId $ runContT return $ runReaderT 'a'  $ testR ()


--------------------------------------------------------------------------------
testW () = twice $
  do put "a"
     (_,w1) <- collect (put "b")
     put "c"
     (_,w2) <- collect (put "d")
     return (w1,w2)

testWDyn  = runId $ runWriterT      $ runContT return $ testW ()
testWStat = runId $ runContT return $ runWriterT      $ testW ()

--------------------------------------------------------------------------------
twice m = do
  (loop, label) <- labelCC Nothing
  x <- m
  case loop of
    Nothing -> jump (Just x) label
    Just y -> return (y,x)


