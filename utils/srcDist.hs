import System

main = 
  do f <- readFile "monadLib.cabal"
     case break ("Version:" ==) (concatMap words (lines f)) of
       (_,_:v:_) -> 
         do let name = "monadLib-" ++ v
                files = [ "README", "INSTALL"
                        , "Setup.hs", "monadLib.cabal", "Makefile"
                        , "Monad/*.hs", "Examples/*.hs"
                        , "doc/*" ]
            system ("ln -s . " ++ name)
            system ("tar -czvf " ++ name ++ "-src.tar.gz " 
                                 ++ unwords [ name ++ "/" ++ f | f <- files])
            system ("rm " ++ name)

