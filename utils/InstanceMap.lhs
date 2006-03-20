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
>   | f `elem` insts t  = td cYes "Yes"
>   | otherwise         = td cNo "No"

> save f            = writeFile f
>                   $ unlines [ "<html><body>", table
>                             , "</body></html>" ]

> table             = unlines ( "<table>" 
>                             : map (tr . unwords) (headings : map row classes)
>                            ++ ["</table>" ])
>   where headings  = map (td cHead) ("" : map (code . show) impls)
>         row f     = td cHead (code (show f)) : map (`supports` f) impls

> code x            = "<code>" ++ x ++ "</code>"
> tr x              = "<tr>" ++ x ++ "</tr>"
> td c x            = "<td align='center' bgcolor=" ++ show c ++ ">" 
>                       ++ x ++ "</td>"

> cYes              = "#99ff99"
> cNo               = "#ff9999"
> cHead             = "#ffff99"






