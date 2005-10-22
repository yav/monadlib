import Common

import Monad.ReaderT
import Monad.ExceptT

-- See also: <Reader.Except>

example            :: ExceptT String (ReaderT Int IO) ()
example             = reader_except

main                = runReader 0 (runExcept example) >>= print






