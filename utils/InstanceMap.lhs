> import Data.List

> data Class        = ReaderM | ReadUpdM | WriterM | CollectorM | StateM
>                   | ExceptM | HandlerM | MonadPlus | ContM | MonadFix
>                   | BaseM 
>                     deriving (Eq,Show,Enum)

> data MonadT       = ReaderT | WriterT | StateT | ExceptT | SearchT | ContT
>                     deriving (Eq,Show,Enum)

> classes           = enumFrom ReaderM
> impls             = enumFrom ReaderT

> insts ReaderT     = classes
> insts WriterT     = classes
> insts StateT      = classes
> insts ExceptT     = classes \\ [CollectorM]
> insts SearchT     = classes \\ [CollectorM, HandlerM, MonadFix]
> insts ContT       = classes \\ [ReadUpdM, CollectorM, HandlerM, MonadFix]

> supports t f 
>   | f `elem` insts t  = "<td id='yes'>Yes</td>"
>   | otherwise         = "<td id='no'>No</td>"

> save f            = writeFile f
>                   $ unlines [ "<html><body>", table
>                             , "</body></html>" ]

> table             = unlines ( "<table align='center'>" 
>                             : map (tr . unwords) (headings : map row classes)
>                            ++ ["</table>" ])
>   where headings  = map header ("" : map show impls)
>         row f     = header (show f) : map (`supports` f) impls

> tr x              = "<tr>" ++ x ++ "</tr>"
> header txt        = "<td id='header'>" ++ txt ++ "</td>"








