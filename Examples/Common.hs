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
reader_cont         = do x <- callcc $ \k -> return (GoOn k) 
                         printEnv "e1"
                         case x of
                           GoOn k -> letLocal 43
                                       $ do printEnv "e2"
                                            k Stop
                           Stop   -> return ()

                                
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
                         x <- callcc $ \k -> return (GoOn k) 
                         put [2]
                         case x of
                           GoOn k -> do put [3]
                                        k Stop
                           Stop   -> return ()


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
                         x <- callcc $ \k -> printSt "e2" >> return (GoOn k) 
                         printSt "e3"
                         case x of
                           GoOn k -> do printSt "e4"
                                        k Stop
                           Stop   -> return ()

except_cont        :: (ExceptM m Int, ContM m, BaseM m IO) => m ()
except_cont         = do x <- return 0 `handle` \x -> return (x+1)
                         raise x

except_cont'       :: (ExceptM m Int, ContM m, BaseM m IO) => m ()
except_cont'        = do x <- callcc (\k -> pr "callcc" >> return (GoOn k))
                              `handle` \_ -> do pr "handler" 
                                                return (GoOn (\_ -> return ()))
                         case x of
                           GoOn k -> pr "GoOn" >> k Stop
                           Stop   -> pr "Stop"  >> raise 0



-- Utilities used by the examples ----------------------------------------------

pr x                = inBase (putStrLn x)

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
data K m            = GoOn (K m -> m ())
                    | Stop







