-- | The jump 'k Two' undoes the output that happened after the callcc [2,3].
-- On the second iteration we output the [2] again, so the final output is [1,2]

import Common

import Monad.WriterT
import Monad.ContT

-- see also: Cont.Writer

example            :: WriterT [Int] (ContT r IO) ()
example             = writer_cont

main                = runCont (execWriter example) >>= print
                                
                          



