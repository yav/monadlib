-- Shows the interaction of continuatiosn with data carrying transformers.
module Examples.Cont where

import Monad.ReaderT
import Monad.WriterT
import Monad.StateT

import Monad.ContT


data Try a          = One a | Two 
prt x               = lift $ lift $ print x
twice k             = return (One (k Two))


testReader ()       = do l <- callcc twice
                         prt =<< get
                         case l of
                           One jumpBack -> letLocal 7 jumpBack 
                           Two          -> return ()

runReaderOut        = (runCont $ runReader 3 $ testReader ()) >>= print
runReaderIn         = (runReader 3 $ runCont $ testReader ()) >>= print


testWriter ()       = do put "A"
                         l <- callcc twice
                         put "*"
                         case l of
                           One jumpBack -> jumpBack
                           Two          -> return ()
                         put "B"
                        
runWriterOut        = (runCont $ execWriter $ testWriter ()) >>= print
runWriterIn         = (execWriter $ runCont $ testWriter ()) >>= print

                        


testState ()        = do l <- callcc twice
                         prt =<< peek
                         case l of
                           One jumpBack -> poke 7 >> jumpBack
                           Two          -> return ()

runStateOut         = (runCont $ execState 3 $ testState ()) >>= print
runStateIn          = (execState 3 $ runCont $ testState ()) >>= print







