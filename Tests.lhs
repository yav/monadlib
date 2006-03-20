> import Monad.Transformers

> prop_All = and [prop_WriterT, prop_StateT
>                , prop_ExceptT, prop_SearchT, prop_ContT]



--------------------------------------------------------------------------------
                  == Examples from "Monad.WriterT" ==
--------------------------------------------------------------------------------


When we 'put' the new output is to the left of previous output.

> prop_WriterT'WriterM = test == ["World", "Hello"]
>   where test  = runId $ execWriter
>               $ do put ["Hello"]
>                    put ["World"]


Raising an exception undoes the output. 

> prop_WriterT'HandlerM = test == Right (42,[])
>   where test  = runId $ runExcept $ runWriter 
>               $ do put ["Hello"]
>                    raise "Error"
>                  `handle_` return 42


Backtracking undoes the output.

> prop_WriterT'MonadPlus = test == Just ["World"]
>   where test  = runId $ runSearchOne $ execWriter
>               $ do put ["Hello"]
>                    mzero 
>                  `mplus` put ["World"]


Jumping to a continuation undoes the output.

> prop_WriterT'ContM = test == ["World"]
>   where test = runId $ runCont $ execWriter
>              $ do (stop,k) <- returnCC False
>                   if stop then put ["World"]
>                           else do put ["Hello"]
>                                   cJump True k


> prop_WriterT = and [ prop_WriterT'WriterM, prop_WriterT'HandlerM
>                    , prop_WriterT'MonadPlus, prop_WriterT'ContM ]





--------------------------------------------------------------------------------
                  == Examples from "Monad.StateT" ==
--------------------------------------------------------------------------------


Raising an exception undoes changes to the state.

> prop_StateT'HandlerM = test == Right (42,42)
>   where test  = runId $ runExcept $ runState 42 
>               $ do set 17
>                    raise "error"
>                  `handle_` get



Backtracking undoes changes to the state.

> prop_StateT'MonadPlus = test == Just (42,42)
>   where test = runId $ runSearchOne $ runState 42 
>              $ do set 17 
>                   mzero 
>                 `mplus` get



Jumping to a continuation undoes changes to the state.

> prop_StateT'ContM = test == (42,42) 
>   where test = runId $ runCont $ runState 42
>              $ do (stop,k) <- returnCC False
>                   if stop then get
>                           else do set 17
>                                   cJump True k


> prop_StateT = and [ prop_StateT'HandlerM
>                   , prop_StateT'MonadPlus, prop_StateT'ContM ]




--------------------------------------------------------------------------------
                == Examples from "Monad.ExceptT" ==
--------------------------------------------------------------------------------


Raising an exception does not affect the output. 

> prop_ExceptT'WriterM = test == (Right 42, ["Hello"])
>   where test  = runId $ runWriter $ runExcept 
>               $ do put ["Hello"]
>                    raise "Error"
>                  `handle_` return 42


Raising an exception does not affect the state. 

> prop_ExceptT'StateM = test == (Right 17, 17)
>   where test  = runId $ runState 42 $ runExcept 
>               $ do set 17
>                    raise "Error"
>                  `handle_` get 


Raising an exception does not prevent backtracking.

> prop_ExceptT'MonadPlus = test == [Left "Hello", Right 42]
>   where test  = runId $ runSearchAll $ runExcept 
>               $ raise "Hello" `mplus` return 42



Raising an exception does not prevent us from jumping to a continutaion.

> prop_ExceptT'ContM = test == Right 42
>   where test  = runId $ runCont $ runExcept
>               $ do (stop,k) <- returnCC False
>                    do if stop then return 42
>                               else raise "Error"
>                     `handle_` cJump True k

> prop_ExceptT  = and [ prop_ExceptT'WriterM, prop_ExceptT'StateM 
>                     , prop_ExceptT'MonadPlus, prop_ExceptT'ContM ]



--------------------------------------------------------------------------------
                    == Examples from "Monad.SearchT" ==
--------------------------------------------------------------------------------


Modifying the context in selected alternatives.

> prop_SearchT'ReadUpdM = test == [(1,42),(2,42),(42,42)] 
>   where test  = runId $ runReader 42 $ runSearchAll
>               $ do xs <- setR 2 (return 1 `mplus` getR) `mplus` getR
>                    x  <- getR
>                    return (xs,x)


Backtracking does not affect the output. 

> prop_SearchT'WriterM = test == ["World","Hello"]
>   where test  = runId $ execWriter $ runSearchOne
>               $ do put ["Hello"]
>                    mzero
>                  `mplus` 
>                 put ["World"]


Backtracking does not affect the state. 

> prop_SearchT'StateM = test == (Just 17, 17)
>   where test  = runId $ runState 42 $ runSearchOne
>               $ do set 17
>                    mzero
>                 `mplus`
>                   get


Backtracking is cancelled by an exception.

> prop_SearchT'ExceptM = test == Left "Error"
>   where test  = runId $ runExcept $ runSearchOne
>               $ raise "Error" `mplus` return 42



Jumping to a continuation cancells (local) backtracking.

> prop_SearchT'ContM = test == [42,10]
>   where test  = runId $ runCont $ runSearchAll
>               $ do (stop,k) <- returnCC False
>                    if stop then return 42 
>                            else cJump True k `mplus` return 17
>                  `mplus` return 10

> prop_SearchT  = and [ prop_SearchT'WriterM, prop_SearchT'StateM 
>                     , prop_SearchT'ExceptM, prop_SearchT'ContM ]



--------------------------------------------------------------------------------
                    == Examples from "Monad.ContT" ==
--------------------------------------------------------------------------------


Jumping to a continuation does not affect the output.

> prop_ContT'WriterM = test == ["World", "Hello"]
>   where test = runId $ execWriter $ runCont
>              $ do (stop,k) <- returnCC False
>                   if stop then put ["World"]
>                           else do put ["Hello"]
>                                   cJump True k


Jumping to a continuation does not affect the state.

> prop_ContT'StateM = test == (17,17)
>   where test = runId $ runState 42 $ runCont
>              $ do (stop,k) <- returnCC False
>                   if stop then get
>                           else do set 17
>                                   cJump True k



Jumping to a continuation does not cancel backtracking.

> prop_ContT'MonadPlus = test == [42,17]
>   where test = runId $ runSearchAll $ runCont
>              $ do (stop,k) <- returnCC False 
>                   if stop then return 42
>                           else cJump True k `mplus` return 17
>                               


> prop_ContT = and [ prop_ContT'WriterM, prop_ContT'StateM
>                  , prop_ContT'MonadPlus ]



