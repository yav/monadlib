import Common

import Monad.ReaderT
import Monad.ExceptT

-- See also: <Except.Reader>

example            :: ReaderT Int (ExceptT String IO) ()
example             = reader_except

main                = runExcept (runReader 0 example) >>= print




