-- Shows the interaction of backtracking with data carrying transformers.
module Examples.Back where

import Monad.ReaderT
import Monad.WriterT
import Monad.StateT

import Monad.BackT

-- | Test interaction of readers and backtracking.
testReader () = letLocal 7 mzero `mplus` get

-- | Reader on the outside.
runReaderOut  = (findOne depthFirst $ runReader 3 $ testReader ()) >>= print

-- | Reader on the inside.
runReaderIn   = (runReader 3 $ findOne depthFirst $ testReader ()) >>= print



-- | Test interaction of writers and backtracking.
testReader    = (letLocal 7 mzero `mplus` get) >>= print
testWriter    = ((put "hello" >> mzero) `mplus` put "world") >>= print

-- | Writer on the outside.
runWriterOut  = (findOne depthFirst $ runWriter $ testWriter ()) >>= print

-- | Writer on the inside.
runWriterIn   = (runWriter $ findOne depthFirst $ testWriter ()) >>= print



-- | Test interaction of state and backtracking.
testState ()  = (poke 7 >> mzero) `mplus` peek

-- | State on the outside.
runStateOut   = (findOne depthFirst $ runState 3 $ testState ()) >>= print

-- | State on the inside.
runStateIn    = (runState 3 $ findOne depthFirst $ testState ()) >>= print


