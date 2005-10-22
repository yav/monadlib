import Common

import Monad.StateT
import Monad.SearchT

-- see also: Search.State

example            :: StateT Int (SearchT IO) ()
example             = state_search

main                = unsafeRunSearch (execState 0 example) >>= print
                     








                      

