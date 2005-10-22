import Common

import Monad.ReaderT
import Monad.ContT

-- See also: <Reader.Search>

example            :: ContT r (ReaderT Int IO) ()
example             = reader_cont

main                = runReader 0 (runCont example) >>= print






