import Common

import Monad.StateT
import Monad.ContT

-- see also: Cont.State

example            :: StateT Int (ContT r IO) ()
example             = state_cont

main                = runCont (execState 0 example) >>= print
                                
                          



