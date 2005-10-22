import Common

import Monad.ReaderT
import Monad.SearchT

-- see also: Search.Reader

example            :: ReaderT Int (SearchT IO) ()
example             = reader_search

main                = unsafeRunSearch (runReader 0 example) >>= print
                     








                      

