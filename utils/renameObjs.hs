import System.Directory
import System
import Control.Monad(zipWithM_)

getDirContRec         = f []
  where f a []        = return a
        f a (d:ds)    = do b <- doesDirectoryExist d
                           if b then do _:_:xs <-getDirectoryContents d
                                        f a ((map (addPrefix d) xs)++ds)
                                else do b <- doesFileExist d
                                        if b then f (d:a) ds else f a ds

addPrefix d f         = d ++ "/" ++ f
cp x y                = let cmd = "ln " ++ x ++ " " ++ y
                        in do putStrLn cmd
                              system cmd

main = do a:b:_ <- getArgs
          xs <- getDirContRec [a]
          zipWithM_ cp xs (map (addPrefix b . (++".o") . show) [0..])
                     


