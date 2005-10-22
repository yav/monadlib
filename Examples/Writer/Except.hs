import Common

import Monad.WriterT
import Monad.ExceptT

-- See also: <Except.Writer>

example            :: WriterT [Int] (ExceptT String IO) ()
example             = writer_except

main                = runExcept (execWriter example) >>= print




