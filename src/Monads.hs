{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
{-|  This module contains a collection of monads that 
   are defined in terms of the monad transformers from
   "MonadLib".   The definitions in this module are
   completely mechanical and so this module may become
   obsolete if support for automated derivations for instances
   becomes well supported across implementations.
 -}
module Monads (
  Reader, Writer, StrictWriter, State, Exception, Cont,
  runReader, runWriter, runStrictWriter, runState, runException, runCont,
  module MonadLib
) where
import MonadLib
import Control.Monad
import Control.Monad.Fix
import Data.Monoid

newtype Reader       i a     = R'  { unR  :: ReaderT       i Id a }
newtype Writer       i a     = W'  { unW  :: WriterT       i Id a }
newtype StrictWriter i a     = SW' { unSW :: StrictWriterT i Id a }
newtype State        i a     = S'  { unS  :: StateT        i Id a }
newtype Exception    i a     = X'  { unX  :: ExceptionT    i Id a }
newtype Cont         i a     = C'  { unC  :: ContT         i Id a }

iso_R   = Iso R' unR
iso_W   = Iso W' unW
iso_SW  = Iso SW' unSW
iso_S   = Iso S' unS
iso_X   = Iso X' unX
iso_C   = Iso C' unC

instance               BaseM (Reader    i) (Reader    i) where inBase = id
instance (Monoid i) => BaseM (Writer    i) (Writer    i) where inBase = id
instance (Monoid i) => BaseM (StrictWriter i) (StrictWriter i) where inBase = id
instance               BaseM (State     i) (State     i) where inBase = id
instance               BaseM (Exception i) (Exception i) where inBase = id
instance               BaseM (Cont      i) (Cont      i) where inBase = id

instance Monad (Reader i) where
  return  = derive_return iso_R
  fail    = derive_fail iso_R
  (>>=)   = derive_bind iso_R

instance (Monoid i) => Monad (Writer i) where
  return  = derive_return iso_W
  fail    = derive_fail iso_W
  (>>=)   = derive_bind iso_W

instance (Monoid i) => Monad (StrictWriter i) where
  return  = derive_return iso_SW
  fail    = derive_fail iso_SW
  (>>=)   = derive_bind iso_SW

instance Monad (State i) where
  return  = derive_return iso_S
  fail    = derive_fail iso_S
  (>>=)   = derive_bind iso_S

instance Monad (Exception i) where
  return  = derive_return iso_X
  fail    = derive_fail iso_X
  (>>=)   = derive_bind iso_X

instance Monad (Cont i) where
  return  = derive_return iso_C
  fail    = derive_fail iso_C
  (>>=)   = derive_bind iso_C

instance               Functor (Reader    i) where fmap = derive_fmap iso_R
instance (Monoid i) => Functor (Writer    i) where fmap = derive_fmap iso_W
instance (Monoid i) => Functor (StrictWriter i) where fmap = derive_fmap iso_SW
instance               Functor (State     i) where fmap = derive_fmap iso_S
instance               Functor (Exception i) where fmap = derive_fmap iso_X
instance               Functor (Cont      i) where fmap = derive_fmap iso_C

instance               MonadFix (Reader    i) where mfix = derive_mfix iso_R
instance (Monoid i) => MonadFix (Writer    i) where mfix = derive_mfix iso_W
instance (Monoid i) => MonadFix (StrictWriter i) where mfix = derive_mfix iso_SW
instance               MonadFix (State     i) where mfix = derive_mfix iso_S
instance               MonadFix (Exception i) where mfix = derive_mfix iso_X

instance ReaderM (Reader i) i where ask = derive_ask iso_R
instance (Monoid i) => WriterM (Writer i) i where put = derive_put iso_W
instance (Monoid i) => WriterM (StrictWriter i) i where put = derive_put iso_SW
instance StateM (State i) i where get = derive_get iso_S; set = derive_set iso_S
instance ExceptionM (Exception i) i where raise = derive_raise iso_X
instance ContM (Cont i) where callCC = derive_callCC iso_C

runReader     :: i -> Reader i a -> a
runWriter     :: Writer i a -> (a,i)
runStrictWriter :: StrictWriter i a -> (a,i)
runState      :: i -> State i a -> (a,i)
runException  :: Exception i a -> Either i a
runCont       :: (a -> i) -> Cont i a -> i

runReader i  = runId . runReaderT i          . unR
runWriter    = runId . runWriterT            . unW
runStrictWriter = runId . runStrictWriterT   . unSW
runState  i  = runId . runStateT i           . unS
runException = runId . runExceptionT         . unX
runCont   i  = runId . runContT (return . i) . unC

instance RunReaderM (Reader i) i where
  local = derive_local iso_R

instance (Monoid i) => RunWriterM (Writer i) i where
  collect = derive_collect iso_W

instance (Monoid i) => RunWriterM (StrictWriter i) i where
  collect = derive_collect iso_SW

instance RunExceptionM (Exception i) i where
  try = derive_try iso_X



