import Common

import Monad.WriterT
import Monad.SearchT

-- See also: <Writer.Search>

example            :: SearchT (WriterT [Int] IO) ()
example             = writer_search

main                = execWriter (unsafeRunSearch example) >>= print




