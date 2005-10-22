module Common where

import Monad.Prelude


-- | Interaction of readers and exceptions.
reader_except      :: (ReaderM m Int, ExceptM m String, BaseM m IO) => m ()
reader_except       = do printEnv "e1"
                         local (+1) 
                           $ do printEnv "e2"
                                raise "Exception!"
                       `handle` \_ -> 
                          do printEnv "e3"


-- | Interaction of readers and searching.
reader_search      :: (ReaderM m Int, SearchM m, BaseM m IO) => m ()
reader_search       = do x <- findOne (printEnv "e1" `mplus` printEnv "e2")
                         case x of
                           Nothing    -> return ()
                           Just (_,m) -> letLocal 42
                                           $ do printEnv "e3"
                                                m

                     
-- | Interaction of readers and continuations.
reader_cont        :: (ReaderM m Int, ContM m, BaseM m IO) => m ()
reader_cont         = do x <- callcc $ \k -> return (One k) 
                         printEnv "e1"
                         case x of
                           One k  -> letLocal 43
                                       $ do printEnv "e2"
                                            k Two
                           Two    -> return ()

                                
-- | Interaction of writers and exceptions.
writer_except      :: (WriterM m [Int], ExceptM m String, BaseM m IO) => m ()
writer_except       = do put [1]
                         do put [2]
                            raise "Exception!"
                          `handle` \_ -> 
                            do put [3]


-- | Interaction of writers and searching.
writer_search      :: (WriterM m [Int], SearchM m, BaseM m IO) => m ()
writer_search       = do put [1]
                         x <- findOne 
                            $ do put [2]
                                 mzero
                            `mplus`
                              do put [3]
                                 return ()
                            `mplus`
                               do put [4]
                                  return ()
                         case x of
                           Nothing    -> return ()
                           Just (_,m) -> m

-- | Interaction of writers and continuations.
writer_cont        :: (WriterM m [Int], ContM m, BaseM m IO) => m ()
writer_cont         = do put [1]
                         x <- callcc $ \k -> return (One k) 
                         put [2]
                         case x of
                           One k  -> do put [3]
                                        k Two
                           Two    -> return ()


-- | Interaction of state and exceptions.
state_except       :: (StateM m Int, ExceptM m String, BaseM m IO) => m ()
state_except        = do printSt "e1"
                         do printSt "e2"
                            raise "Exception!"
                          `handle` \_ -> 
                            printSt "e3"


-- | Interaction of state and searching.
state_search       :: (StateM m Int, SearchM m, BaseM m IO) => m ()
state_search        = do x <- findOne (printSt "e1" `mplus` printSt "e2")
                         case x of
                           Nothing    -> return ()
                           Just (_,m) -> do printSt "e3"
                                            m


-- | Interaction of state and continuations.
state_cont         :: (StateM m Int, ContM m, BaseM m IO) => m ()
state_cont          = do printSt "e1"
                         x <- callcc $ \k -> printSt "e2" >> return (One k) 
                         printSt "e3"
                         case x of
                           One k  -> do printSt "e4"
                                        k Two
                           Two    -> return ()


-- Utilities used by the examples ----------------------------------------------

-- | For testing readers.
printEnv           :: (ReaderM m Int, BaseM m IO) => String -> m ()
printEnv msg        = do e <- get
                         inBase 
                           $ do putStr (msg ++ ": ")
                                print e

-- | For testing state
printSt            :: (StateM m Int, BaseM m IO) => String -> m ()
printSt msg         = do e <- peek
                         inBase 
                           $ do putStr (msg ++ ": ")
                                print e
                         poke_ (e+1)

-- | For testing continuations
data K m            = One (K m -> m ())
                    | Two





