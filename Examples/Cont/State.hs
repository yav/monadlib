import Common
import Monad.StateT
import Monad.ContT

-- see also: State.Cont

example            :: ContT r (StateT Int IO) ()
example             = state_cont
                            
main                = execState 0 (runCont example) >>= print




