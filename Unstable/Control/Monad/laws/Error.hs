import Prop
import Unstable.Control.Monad.Trans


raise_ x f            = (raise x >>= f) === raise x

handle_return x h     = handle (return x) h === return x
handle_raise x h      = handle (raise x) h === h x
handle_handle m h1 h2 = handle (handle m h1) h2 === handle m (\x -> handle (h1 x) h2)




