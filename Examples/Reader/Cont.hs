import Common

import Monad.ReaderT
import Monad.ContT

example            :: ReaderT Int (ContT () IO) ()
example             = reader_cont

main                = runCont (runReader 0 example) >>= print
                                
                          



