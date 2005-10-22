import Common

import Monad.StateT
import Monad.ExceptT

-- see also: State.Except

example            :: ExceptT String (StateT Int IO) ()
example             = state_except
                            
main                = execState 0 (runExcept example) >>= print




