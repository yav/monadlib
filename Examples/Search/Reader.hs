import Common

import Monad.ReaderT
import Monad.SearchT

-- See also: <Reader.Search>

example            :: SearchT (ReaderT Int IO) ()
example             = reader_search

main                = runReader 0 (unsafeRunSearch example) >>= print






