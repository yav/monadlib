-- Shows the interaction of the exceptions with the data carrying transformers.
module Examples.Except where

import Monad.ReaderT
import Monad.WriterT
import Monad.StateT

import Monad.ExceptT


testReader () = letLocal 7 (raise ()) `handle` \() -> get

runReaderOut  = (runExcept $ runReader 3 $ testReader ()) >>= print
runReaderIn   = (runReader 3 $ runExcept $ testReader ()) >>= print


testWriter () = put "hello" >> raise () >> return ()

runWriterOut  = (runExcept $ runWriter $ testWriter ()) >>= print
runWriterIn   = (runWriter $ runExcept $ testWriter ()) >>= print


testState ()  = (poke 7 >> raise ()) `handle` \() -> peek

runStateOut   = (runExcept $ runState 3 $ testState ()) >>= print
runStateIn    = (runState 3 $ runExcept $ testState ()) >>= print

