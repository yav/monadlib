import Monad

data TreeT m a      = Tip
                    | Leaf a
                    | Node (TreeT m a) (TreeT m a)
                    | Eff (m (TreeT m a))

lift m              = Eff (liftM Leaf m) 

instance Monad m => Monad (TreeT m) where
  return            = Leaf
  
  Tip >>= _         = Tip
  Leaf a >>= k      = k a
  Node l r >>= k    = Node (l >>= k) (r >>= k)
  Eff m >>= k       = Eff (liftM (>>= k) m)
  

instance Monad m => MonadPlus (TreeT m) where
  mzero             = Tip
  mplus             = Node

searchBF m          = search [m] []
  where
  search [] []      = return Nothing
  search [] ys      = search (reverse ys) []
  search (x:xs) ys  = case x of
                        Tip         -> search xs ys
                        Leaf a      -> return (Just (a,jn xs ys))
                        Node l r    -> search xs (r:l:ys)
                        Eff m       -> do x <- m
                                          search (x:xs) ys 
                      
  jn xs []          = xs
  jn xs ys          = xs ++ reverse ys

searchDF m          = search [m]
  where
  search (x:xs)     = case x of
                        Tip         -> search xs
                        Leaf a      -> return (Just (a,msum xs))
                        Node l r    -> search (l:r:xs)
                        Eff m       -> do x <- m
                                          search (x:xs)
  search []         = return Nothing
                      


--------------------------------------------------------------------------------

run m     = do x <- searchBF m
               case x of
                 Nothing    -> putStrLn "(no result)"
                 Just (x,_) -> print x

loop     :: TreeT IO a
loop      = Eff (return loop)

test1    :: TreeT IO Int
test1     = Node test1 loop

test2    :: TreeT IO Int
test2     = Node loop (Leaf 1) 



                
