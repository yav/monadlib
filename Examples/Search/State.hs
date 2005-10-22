import Common

import Monad.StateT
import Monad.SearchT

-- see also: State.Except

example            :: SearchT (StateT Int IO) ()
example             = state_search
                            
main                = execState 0 (unsafeRunSearch example) >>= print




