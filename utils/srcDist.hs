import System

main  = do f <- readFile "monadLib.cabal"
           case break ("Version:" ==) (concatMap words (lines f)) of
             (_,_:v:_) -> do let name = "monadLib-" ++ v
                                 files = ["README", "INSTALL", "Setup.hs", "monadLib.cabal", "Monad/*.hs", "Examples/*.hs"
                                         ]
                             system ("ln -s . " ++ name)
                             system ("tar -czvf " ++ name ++ ".tar.gz " ++ unwords [ name ++ "/" ++ f | f <- files])

