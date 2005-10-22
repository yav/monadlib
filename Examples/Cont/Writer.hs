import Common

import Monad.WriterT
import Monad.ContT

-- See also: <Writer.Search>

example            :: ContT r (WriterT [Int] IO) ()
example             = writer_cont

main                = execWriter (runCont example) >>= print




