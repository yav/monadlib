import Common

import Monad.StateT
import Monad.ExceptT

-- see also: Except.State

example            :: StateT Int (ExceptT String IO) ()
example             = state_except
                            
main                = runExcept (execState 0 example) >>= print




