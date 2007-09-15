{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
{-|  This module contains a collection of monads that 
   are defined in terms of the monad transformers from
   "MonadLib".   The definitions in this module are
   completely mechanical and so this module may become
   obsolete if support for automated derivations for instances
   becomes well supported across implementations.
 -}
module Monads (
  Reader, Writer, State, Exception, Cont, 
  runReader, runWriter, runState, runException, runCont,
  module MonadLib
) where
import MonadLib
import Control.Monad
import Control.Monad.Fix
import Data.Monoid

newtype Reader    i a     = R' { unR :: ReaderT     i Id a }
newtype Writer    i a     = W' { unW :: WriterT     i Id a }
newtype State     i a     = S' { unS :: StateT      i Id a }
newtype Exception i a     = X' { unX :: ExceptionT  i Id a }
newtype Cont      i a     = C' { unC :: ContT       i Id a }

instance               BaseM (Reader    i) (Reader    i) where inBase = id
instance (Monoid i) => BaseM (Writer    i) (Writer    i) where inBase = id
instance               BaseM (State     i) (State     i) where inBase = id
instance               BaseM (Exception i) (Exception i) where inBase = id
instance               BaseM (Cont      i) (Cont      i) where inBase = id

instance Monad (Reader i) where
  return x    = R' (return x)
  fail x      = R' (fail x)
  m >>= f     = R' (unR m >>= (unR . f))

instance (Monoid i) => Monad (Writer i) where
  return x    = W' (return x)
  fail x      = W' (fail x)
  m >>= f     = W' (unW m >>= (unW . f))

instance Monad (State i) where
  return x    = S' (return x)
  fail x      = S' (fail x)
  m >>= f     = S' (unS m >>= (unS . f))

instance Monad (Exception i) where
  return x    = X' (return x)
  fail x      = X' (fail x)
  m >>= f     = X' (unX m >>= (unX . f))

instance Monad (Cont i) where
  return x    = C' (return x)
  fail x      = C' (fail x)
  m >>= f     = C' (unC m >>= (unC . f))

instance               Functor (Reader    i) where fmap = liftM
instance (Monoid i) => Functor (Writer    i) where fmap = liftM
instance               Functor (State     i) where fmap = liftM
instance               Functor (Exception i) where fmap = liftM
instance               Functor (Cont      i) where fmap = liftM

instance               MonadFix (Reader    i) where mfix f = R' (mfix (unR . f))
instance (Monoid i) => MonadFix (Writer    i) where mfix f = W' (mfix (unW . f))
instance               MonadFix (State     i) where mfix f = S' (mfix (unS . f))
instance               MonadFix (Exception i) where mfix f = X' (mfix (unX . f))

instance ReaderM (Reader i) i where ask = R' ask
instance (Monoid i) => WriterM (Writer i) i where put = W' . put 
instance StateM (State i) i where get = S' get; set = S' . set 
instance ExceptionM (Exception i) i where raise = X' . raise 
instance ContM (Cont i) where callCC f = C' (callCC (unC . f . (C' .)))

runReader     :: i -> Reader i a -> a
runWriter     :: Writer i a -> (a,i)
runState      :: i -> State i a -> (a,i)
runException  :: Exception i a -> Either i a
runCont       :: (a -> i) -> Cont i a -> i

runReader i  = runId . runReaderT i          . unR 
runWriter    = runId . runWriterT            . unW
runState  i  = runId . runStateT i           . unS
runException = runId . runExceptionT         . unX
runCont   i  = runId . runContT (return . i) . unC 

instance RunReaderM (Reader i) i where local i = R' . local i . unR
instance (Monoid i) => RunWriterM (Writer i) i where
  collect = W' . collect . unW
instance RunExceptionM (Exception i) i where try = X' . try     . unX



