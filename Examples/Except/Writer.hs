import Common

import Monad.WriterT
import Monad.ExceptT

-- See also: <Writer.Except>

example            :: ExceptT String (WriterT [Int] IO) ()
example             = writer_except

main                = execWriter (runExcept example) >>= print




