{-# LANGUAGE FlexibleContexts #-}
import MonadLib
import Data.Char

type M = ExceptionT Int (ExceptionT String IO)


main :: IO ()
main = do x <- runExceptionT $ runExceptionT ex_catch
          case x of
            Left n          -> putStrLn ("Invalid text: " ++ show n)
            Right (Left n)  -> putStrLn ("Invalid int: " ++ show n)
            Right (Right n) -> putStrLn ("Success: " ++ show n)

-- This function demonstrates how we can throw different types
-- of exceptions in the same monad.
ex_raise :: M Int
ex_raise = do txt <- inBase getLine
              case readMaybe txt of
                Just n
                  | n < 10    -> raise n
                  | otherwise -> return n
                Nothing       -> raise txt

-- This function demonstrates how we can handle different types of exception.
ex_catch :: M Int
ex_catch = ex_raise
  `handler` (\c -> pr ("Ignoring " ++ c) >> return 10)
  `handler` (\n -> return (n+10))

-- Misc. utils -----------------------------------------------------------------
pr :: BaseM m IO => String -> m ()
pr x = inBase (putStrLn x)

readMaybe :: Read a => String -> Maybe a
readMaybe txt = case reads txt of
                  [(a,cs)] | all isSpace cs -> Just a
                  _ -> Nothing

handler :: RunExceptionM m x => m a -> (x -> m a) -> m a
m `handler` f = do x <- try m
                   case x of
                     Right a -> return a
                     Left e  -> f e


