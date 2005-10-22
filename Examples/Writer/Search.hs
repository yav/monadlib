import Common

import Monad.WriterT
import Monad.SearchT

-- see also: Search.Writer

example            :: WriterT [Int] (SearchT IO) ()
example             = writer_search

main                = unsafeRunSearch (execWriter example) >>= print
                     








                      

