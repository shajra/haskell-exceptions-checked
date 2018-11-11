{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


{-|
Description: Statically checked exceptions

This module provides an API to statically check exceptions. The design is
heavily derived from Pepe Iborra's
<https://hackage.haskell.org/package/control-monad-exception control-monad-exception>
package and supporting paper
<https://dl.acm.org/citation.cfm?id=2127644 Explicitly typed exceptions for Haskell>.

One improvement is the delegation of exception handling to the
<https://hackage.haskell.org/package/safe-exceptions safe-exceptions> library
(instead of @Control.Exception@) for improved handling of synchronous versus
asynchronous exceptions.

= Example Usage

If we have the following exceptions:

>>> :{
data FooE = FooE deriving (Show, Typeable)
data BarE = BarE deriving (Show, Typeable)
data BazE = BazE deriving (Show, Typeable)
instance Exception FooE
instance Exception BarE
instance Exception BazE
:}

We could define a function that throws one exception with 'Throws' and 'throw':

>>> :{
throwsFoo :: Throws FooE e => CheckedIO e a
throwsFoo = throw FooE
:}

In cases throwing multiple exceptions, we can use 'ThrowsAll' with GHC's
@DataKinds@ extension, for a compact syntax:

>>> :{
throwsFooBarBaz :: ThrowsAll '[FooE, BarE, BazE] e => CheckedIO e a
throwsFooBarBaz = throw FooE *> throw BarE *> throw BazE
:}

You can start checking a computation such as 'IO' you already have in hand with
'throwsNone', 'throws', and 'throwsAll'.

We can handle one of these exceptions with 'catch':

>>> :{
action :: ThrowsAll '[FooE, BazE] e => CheckedIO e ()
action = throwsFooBarBaz `catch` (\(_::BarE) -> pure ())
:}

We can handle more of these exceptions with 'catches' and '<::>':

>>> :{
action' :: ThrowsAll '[FooE, BazE] e => CheckedIO e ()
action' = throwsFooBarBaz
        `catches` (\(_::BarE) -> throw FooE)
             <::> (\(_::FooE) -> throw BazE)
:}

But the compiler prevents us from running it since our 'ThrowsAll' constraint
still indicates unhandled exceptions:

>>> runChecked action'
...
...prevents the constraint ...(Throws FooE ...)... from being solved...
...

But if we handle everything (note we can use '<:>' with a final '<::>' to chain
as many handlers as required)

>>> :{
handled :: CheckedIO e ()
handled = throwsFooBarBaz
        `catches` (\(_::BazE) -> throwsNone $ print "handledBaz")
             <:>  (\(_::BarE) -> throwsNone $ print "handledBar")
             <::> (\(_::FooE) -> throwsNone $ print "handledFoo")
:}

then we can run it with 'runChecked':

>>> runChecked handled
"handledFoo"

If you like, you could chain on a finalizer on your action too with 'finally':

>>> :{
handled' :: CheckedIO e ()
handled' = throwsFooBarBaz
        `catches` (\(_::BazE) -> throwsNone $ print "handledBaz")
             <:>  (\(_::BarE) -> throwsNone $ print "handledBar")
             <::> (\(_::FooE) -> throwsNone $ print "handledFoo")
        `finally` (throwsNone $ print "finalized")
:}

>>> runChecked handled'
"handledFoo"
"finalized"

Hopefully that's enough to get you started and you can follow the types of the
other exposed functions in this module to figure out the rest.

= Regarding “Impure” Exceptions From Partial Functions

With 'Checked' we can statically check exceptions, but that's still a long way
from bridging that gap to totality. The provided 'Checked' type is only useful
with base monadic types with instances of 'Safe.MonadCatch' or
'Safe.MonadMask'. Note this excludes 'Data.Functor.Identity', which means to
statically check the kinds of exceptions we get with partial function (which
Michael Snoyman has
<https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell coined as “impure”>),
we must lift these computations into something like 'IO' or
'Control.Monad.STM'.
-}
module Control.Exception.Checked (
    -- * Types and Type Classes
      Checked
    , CheckedIO
    , Throws
    , ThrowsAll

    -- * Running
    , runChecked
    , unsafeRunChecked

    -- * Constructors
    , throw
    , impureThrow
    , throwsNone
    , throws
    , throwsAll
    , throws'
    , throwsAll'
    , throwTo
    , throwString

    -- * Handling Errors

    -- ** Trying
    , try
    , tryIO
    , tryAny
    , tryDeep
    , tryAnyDeep
    , tryJust
    , tryAsync

    -- ** Catching
    , catch
    , catchIO
    , catchAny
    , catchDeep
    , catchAnyDeep
    , catchJust
    , catchAsync

    -- ** Handling
    , handle
    , handleIO
    , handleAny
    , handleDeep
    , handleAnyDeep
    , handleJust
    , handleAsync

    -- * Handling Multiple Cases
    , Handler
    , catches
    , catchesDeep
    , catchesAsync
    , handles
    , handlesDeep
    , handlesAsync
    , handler
    , (<:>)
    , (<::>)
    , (<++>)
    , appendHandler
    , emptyHandler

    -- * Cleanup
    , finally
    , onException
    , withException
    , bracket
    , bracket_
    , bracketOnError
    , bracketOnError_
#if MIN_VERSION_safe_exceptions(0,1,7)
    , bracketWithError
#endif

    -- * Re-exported for convenience
    , Safe.isAsyncException
    , Safe.isSyncException
    , Safe.toAsyncException
    , Safe.toSyncException
    , Safe.AsyncExceptionWrapper(..)
    , Safe.Exception (..)
    , Safe.IOException
    , Safe.MonadCatch
    , Safe.MonadMask
    , Safe.MonadThrow
    , Safe.SomeAsyncException (..)
    , Safe.SomeException (..)
    , Safe.SyncExceptionWrapper(..)
    , Safe.Typeable

    -- * Implementation details
    -- | These abstractions tie everything together at the type-level, but are
    -- not something a user is expected to need directly.
    , Caught
    , ToCaught
    , Append
    ) where


import qualified Control.Applicative    as App
import qualified Control.Concurrent     as Concurrent
import qualified Control.Monad          as Monad
import qualified Control.Monad.Fail     as Fail
import qualified Control.Monad.Fix      as Fix
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Zip      as Zip
import qualified Data.Functor.Classes   as Functor
import qualified Data.Kind              as Kind

import qualified Control.DeepSeq        as DeepSeq
import qualified Control.Exception.Safe as Safe
import qualified Control.Monad.Cont     as Cont
import qualified Control.Monad.Except   as Except
import qualified Control.Monad.Morph    as Morph
import qualified Control.Monad.Reader   as Reader
import qualified Control.Monad.State    as State
import qualified Control.Monad.Writer   as Writer


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeApplications
-- >>> import Control.Exception.Checked


infixl 8
    `try`
    , `tryIO`
    , `tryAny`
    , `tryDeep`
    , `tryAnyDeep`
    , `tryAsync`
    , `catch`
    , `catchIO`
    , `catchAny`
    , `catchDeep`
    , `catchAnyDeep`
    , `catchAsync`
    , `handle`
    , `handleIO`
    , `handleAny`
    , `handleDeep`
    , `handleAnyDeep`
    , `handleAsync`
    , `catches`
    , `catchesDeep`
    , `catchesAsync`
    , `handles`
    , `handlesDeep`
    , `handlesAsync`
    , `finally`
    , `onException`
    , `withException`
infixr 9 <:>, <::>, <++>, `appendHandler`


{-| Wrapper type that tracks exceptions.

    Exceptions are tracked in the phantom parameter @e@. In practice, @e@ will
    remain polymorphic with only 'Throws' constraints on it.

    Note that although @m a@ may be a monad transformer stack, we don't want to
    be able to 'Control.Monad.Trans.Class.lift' into 'Checked' because we want
    to avoid casually lifting something that might throw an exception without
    tracking the possibility safely.
-}
newtype Checked e m a
    = Checked {
        -- | Safely get your @m a@.
        --
        -- Most importantly, you won't be able to call this function if the
        -- phantom parameter @l@ has any 'Throws' constraints on it.
        runChecked :: m a }
    deriving
    ( Eq, Functor.Eq1
    , Ord, Functor.Ord1
    , Read, Functor.Read1
    , Show, Functor.Show1
    , Foldable
    , Traversable
    , Functor
    , Applicative
    , App.Alternative
    , Monad
    , Monad.MonadPlus
    , Fix.MonadFix
    , Cont.MonadCont
    , (Reader.MonadReader r)
    , (Writer.MonadWriter w)
    , (State.MonadState s)
    , (Except.MonadError e')
    , Zip.MonadZip
    , Fail.MonadFail )

instance Morph.MFunctor (Checked e) where
    hoist f =  Checked . f . runChecked

-- | Convenience type alias for when working directly with 'IO'.
type CheckedIO e a = Checked e IO a

-- | Used for `catches` and `handles`.
newtype Handler m a handled e = Handler [Safe.Handler m a]

-- | Inferred instance of Throws.
data Caught exception e

-- | Enables unsafe running.  Not exported for safety.
data AnyException

-- | Unexported class preventing additional Throws instances.
class Allowed e
instance Allowed (Caught exception e)
instance Allowed AnyException

-- | Declares an exception has been thrown.
--
-- In @Throws exception e@ the @exception@ parameter is the exception we're
-- declaring has been thrown, and @e@ is a type-level list that contains
-- @exception@. This list will be solved for by the compiler, so in typical
-- usage you will specify @exception@ concretely leave @e@ polymorphic.
--
-- See 'throws', 'throwsAll' for examples of introducing this constraint, and
-- see 'catch', 'handle', 'catches', and 'handles' for example of removing this
-- constraint.
--
-- Finally, note that @Allowed@ is not exported, which prevent users from
-- implementing more instances of 'Throws' and bypassing safety.
class Allowed e => Throws exception e
instance {-# OVERLAPS #-} Throws exception (Caught exception e)
instance {-# OVERLAPPABLE #-} Throws exception e => Throws exception (Caught exception' e)
instance {-# OVERLAPPING #-} Throws exception (Caught Safe.SomeException e)
instance {-# OVERLAPPING #-} Throws exception AnyException

-- | A syntactic convenience to expand to multiple Throws.
--
-- @ThrowsAll '[ExceptionA, ExceptionB, ...] e => ...@
--
-- expands to
--
-- @(Throws ExceptionA e, Throws ExceptionB e, ...) => ...@
type family ThrowsAll exceptions e where
    ThrowsAll (exception : rest) e = (Throws exception e, ThrowsAll rest e)
    ThrowsAll '[] e = (() :: Kind.Constraint)

-- | Implementation detail enabling 'catches' and 'handles'.
type family ToCaught asList asCaught where
    ToCaught (exception : rest) e = Caught exception (ToCaught rest e)
    ToCaught '[] e = e

-- | Implementation detail enabling 'appendHandler' and '<++>'.
type family Append exceptions exceptions' where
    Append (exception : rest) e = exception : Append rest e
    Append '[] e =  e


-- | Ignore all type-level markers that an exception hasn't been handled.
--
-- This function is provided for completeness, but may you never find a reason
-- to use this.
unsafeRunChecked
    :: Checked AnyException m a
    -> m a
unsafeRunChecked = runChecked

-- | Throw an exception @e@.
--
-- Note, this delegates to @safe-exception@'s 'Safe.throw'.
throw
    :: ( Throws exception e, Safe.Exception exception, Safe.MonadThrow m)
    => exception
    -> Checked e m a
throw = Checked . Safe.throw

-- | Throw an exception @e@.
--
-- Note, this delegates to @safe-exception@'s 'Safe.impureThrow'.
impureThrow
    :: ( Throws exception e, Safe.Exception exception, Safe.MonadThrow m)
    => exception
    -> Checked e m a
impureThrow = Checked . Safe.impureThrow

-- | Start checking some monadic @m a@, declaring it as throwing nothing yet.
--
throwsNone
    :: m a
    -> Checked e m a
throwsNone = Checked

-- | Start checking a monadic @m a@, declaring it as throwing an @exception@.
--
-- This API is designed for GHC's @TypeApplications@ extension. See 'throws''
-- for an alternative.
--
-- >>> :{
-- checkedReadFile :: Throws IOException e => FilePath -> CheckedIO e String
-- checkedReadFile = throws @IOException . readFile
-- :}
--
-- Note that technically, you can use 'throwsNone' and widen the constraints
-- but this can lead to confusing code:
--
-- >>> :{
-- checkedReadFile :: Throws IOException e => FilePath -> CheckedIO e String
-- checkedReadFile = throwsNone . readFile
-- :}
--
--  See 'throwsAll' if your @m a@ throws more than one exception.
throws
    :: forall exception m e a .
        (Safe.Exception exception, Throws exception e, Safe.MonadThrow m)
    => m a
    -> Checked e m a
throws = Checked

-- | Start checking some monadic @m a@, declaring it as throwing @exceptions@.
-- @exceptions@ should be a type-level list.
--
-- This API is designed for GHC's @TypeApplications@ extension. See
-- 'throwsAll'' for an alternative.
--
-- >>> import qualified Control.Exception as E
-- >>> :{
-- readFirst :: ThrowsAll '[IOException, E.ErrorCall] e
--           => [FilePath] -> CheckedIO e String
-- readFirst = throwsAll @'[IOException, E.ErrorCall] . readFile . head
-- :}
--
-- Note that technically, you can use 'throwsNone' and widen the constraints
-- but this can lead to confusing code:
--
-- >>> :{
-- readFirst :: ThrowsAll '[IOException, E.ErrorCall] e
--           => [FilePath] -> CheckedIO e String
-- readFirst = throwsNone . readFile . head
-- :}
--
--  See 'throwsAll' if your @m a@ throws more than one exception.
throwsAll
    :: forall exceptions m e a . (ThrowsAll exceptions e, Safe.MonadThrow m)
    => m a
    -> Checked e m a
throwsAll = Checked

-- | Same a 'throws', but called with a proxy instead of a type application.
--
-- >>> import qualified Data.Proxy as P
-- >>> :{
-- checkedReadFile :: Throws IOException e => FilePath -> CheckedIO e String
-- checkedReadFile = throws' (P.Proxy :: P.Proxy IOException) . readFile
-- :}
throws'
    :: (Safe.Exception exception, Throws exception e, Safe.MonadThrow m)
    => proxy exception
    -> m a
    -> Checked e m a
throws' _ = Checked

-- | Same a 'throwsAll', but called with a proxy instead of a type application.
--
-- >>> import qualified Data.Proxy as P
-- >>> import qualified Control.Exception as E
-- >>> :{
-- readFirst :: ThrowsAll '[IOException, E.ErrorCall] e
--           => [FilePath] -> CheckedIO e String
-- readFirst = throwsAll' (P.Proxy :: P.Proxy '[IOException, E.ErrorCall])
--     . readFile . head
-- :}
throwsAll'
    :: (ThrowsAll exceptions e, Safe.MonadThrow m)
    => proxy exceptions
    -> m a
    -> Checked e m a
throwsAll' _ = Checked

-- | Throw an asynchronous exception to another thread.
--
-- Note, this delegates to @safe-exception@'s 'Safe.throwTo'.
throwTo
    :: (Safe.Exception exception, IO.MonadIO m)
    => Concurrent.ThreadId -> exception -> Checked e m ()
throwTo = (fmap. fmap) Checked Safe.throwTo


-- | Throw a semantically-neutral exception.
--
-- Note, this delegates to @safe-exception@'s 'Safe.throwString'.
--
-- Also, semantically, it's far better to make custom exception types. When
-- everything thrown is a 'Safe.StringException', we can't handle/catch
-- anything with any delicacy.
throwString
    :: (Throws Safe.StringException e, Safe.MonadThrow m)
    => String -> Checked e m a
throwString = Checked . Safe.throwString

-- | Catch an exception and return it via an 'Either'.
--
-- Note, this delegates to @safe-exception@'s 'Safe.try'.
try
    :: (Safe.Exception exception, Safe.MonadCatch m)
    => Checked (Caught exception e) m a
    -> Checked e m (Either exception a)
try = Checked . Safe.try . runChecked

-- | Catch an 'Safe.IOException' and return it via an 'Either'.
--
-- Note, this delegates to @safe-exception@'s 'Safe.tryAny'.
tryIO
    :: Safe.MonadCatch m
    => Checked (Caught Safe.IOException e) m a
    -> Checked e m (Either Safe.IOException a)
tryIO = Checked . Safe.tryIO . runChecked

-- | Catch any exception and return it via an 'Either'.
--
-- Note, this delegates to @safe-exception@'s 'Safe.tryAny'.
tryAny
    :: Safe.MonadCatch m
    => Checked (Caught Safe.SomeException e) m a
    -> Checked e m (Either Safe.SomeException a)
tryAny = Checked . Safe.tryAny . runChecked

-- | Force a computation, catch a exception and return it via an 'Either'.
--
-- Note, this delegates to @safe-exception@'s 'Safe.tryDeep'.
tryDeep
    :: (Safe.Exception exception, Safe.MonadCatch m, IO.MonadIO m, DeepSeq.NFData a)
    => Checked (Caught exception e) m a
    -> Checked e m (Either exception a)
tryDeep = Checked . Safe.tryDeep . runChecked

-- | Force a computation, catch any exception and return it via an 'Either'.
--
-- Note, this delegates to @safe-exception@'s 'Safe.tryAnyDeep'.
tryAnyDeep
    :: (Safe.MonadCatch m, IO.MonadIO m, DeepSeq.NFData a)
    => Checked (Caught Safe.SomeException e) m a
    -> Checked e m (Either Safe.SomeException a)
tryAnyDeep = Checked . Safe.tryAnyDeep . runChecked

-- | A variant of 'try' using a predicate to select which exceptions to catch.
--
-- Note, this delegates to @safe-exception@'s 'Safe.tryJust'.
tryJust
    :: (Safe.MonadCatch m, Safe.Exception exception)
    => (exception -> Maybe b)
    -> Checked e m a
    -> Checked e m (Either b a)
tryJust p = Checked . Safe.tryJust p . runChecked

-- | Catch a possibly asynchronous exception and return it via an 'Either'.
--
-- Note, this delegates to @safe-exception@'s 'Safe.tryAsync'. Also, catching
-- asynchronous is discouraged, but this function is provided for completeness.
tryAsync
    :: (Safe.Exception exception, Safe.MonadCatch m)
    => Checked (Caught exception e) m a
    -> Checked e m (Either exception a)
tryAsync = Checked . Safe.tryAsync . runChecked

-- | Catch an exception with a provided handler.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catch'.
catch
    :: (Safe.Exception exception, Safe.MonadCatch m)
    => Checked (Caught exception e) m a
    -> (exception -> Checked e m a)
    -> Checked e m a
catch m h =
    Checked $ Safe.catch (runChecked m) (runChecked . h)

-- | Catch an 'Safe.IOException' with a provided handler.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catchIO'.
catchIO
    :: Safe.MonadCatch m
    => Checked (Caught Safe.IOException e) m a
    -> (Safe.IOException -> Checked e m a)
    -> Checked e m a
catchIO m h =
    Checked $ Safe.catchIO (runChecked m) (runChecked . h)

-- | Catch any exception with a provided handler.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catchAny'.
catchAny
    :: Safe.MonadCatch m
    => Checked (Caught Safe.SomeException e) m a
    -> (Safe.SomeException -> Checked e m a)
    -> Checked e m a
catchAny m h =
    Checked $ Safe.catchAny (runChecked m) (runChecked . h)

-- | Force a computation, and catch an exception with a provided handler.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catchDeep'.
catchDeep
    :: (Safe.Exception exception, Safe.MonadCatch m, IO.MonadIO m, DeepSeq.NFData a)
    => Checked (Caught exception e) m a
    -> (exception -> Checked e m a)
    -> Checked e m a
catchDeep m h =
    Checked $ Safe.catchDeep (runChecked m) (runChecked . h)

-- | Force a computation, and catch any exception with a provided handler.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catchAnyDeep'.
catchAnyDeep
    :: (Safe.MonadCatch m, IO.MonadIO m, DeepSeq.NFData a)
    => Checked (Caught Safe.SomeException e) m a
    -> (Safe.SomeException -> Checked e m a)
    -> Checked e m a
catchAnyDeep m h =
    Checked $ Safe.catchAnyDeep (runChecked m) (runChecked . h)

-- | A variant of 'catch' using a predicate to select which exceptions to catch.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catchJust'.
catchJust
    :: (Safe.MonadCatch m, Safe.Exception exception)
    => (exception -> Maybe b)
    -> Checked (Caught exception e) m a
    -> (b -> Checked e m a)
    -> Checked e m a
catchJust f a b =
    Checked $ Safe.catchJust f (runChecked a) (runChecked . b)

-- | Catch a possibly asynchronous exception with a provided handler.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catchAsync'. Also, catching
-- asynchronous is discouraged, but this function is provided for completeness.
catchAsync
    :: (Safe.Exception exception, Safe.MonadCatch m)
    => Checked (Caught exception e) m a
    -> (exception -> Checked e m a)
    -> Checked e m a
catchAsync m h =
    Checked $ Safe.catchAsync (runChecked m) (runChecked . h)

-- | Flipped `catch`.
handle
    :: (Safe.Exception exception, Safe.MonadCatch m)
    => (exception -> Checked e m a)
    -> Checked (Caught exception e) m a
    -> Checked e m a
handle = flip catch

-- | Flipped `catchIO`.
handleIO
    :: Safe.MonadCatch m
    => (Safe.IOException -> Checked e m a)
    -> Checked (Caught Safe.IOException e) m a
    -> Checked e m a
handleIO = flip catchIO

-- | Flipped `catchAny`.
handleAny
    :: Safe.MonadCatch m
    => (Safe.SomeException -> Checked e m a)
    -> Checked (Caught Safe.SomeException e) m a
    -> Checked e m a
handleAny = flip catchAny

-- | Flipped `catchDeep`.
handleDeep
    :: (Safe.Exception exception, Safe.MonadCatch m, IO.MonadIO m, DeepSeq.NFData a)
    => (exception -> Checked e m a)
    -> Checked (Caught exception e) m a
    -> Checked e m a
handleDeep = flip catchDeep

-- | Flipped `catchAnyDeep`.
handleAnyDeep
    :: (Safe.MonadCatch m, IO.MonadIO m, DeepSeq.NFData a)
    => (Safe.SomeException -> Checked e m a)
    -> Checked (Caught Safe.SomeException e) m a
    -> Checked e m a
handleAnyDeep = flip catchAnyDeep

-- | Flipped `catchJust`.
handleJust
    :: (Safe.MonadCatch m, Safe.Exception exception)
    => Checked (Caught exception e) m a
    -> (exception -> Maybe b)
    -> (b -> Checked e m a)
    -> Checked e m a
handleJust = flip catchJust

-- | Flipped `catchAsync`.
--
-- Note, catching asynchronous is discouraged, but this function is provided
-- for completeness.
handleAsync
    :: (Safe.Exception exception, Safe.MonadCatch m)
    => (exception -> Checked e m a)
    -> Checked (Caught exception e) m a
    -> Checked e m a
handleAsync = flip catchAsync

-- | Handle many exceptions at once.
--
-- Just as with 'Control.Exception.catches', it's important to note that
-- there's a difference between calling
--
--     * 'catches' with a single 'Handler' built with many handler functions
--
--     * 'catch' multiple times, once for each handler function.
--
-- With @catches@, if one of the handlers throws an exception, that exception
-- is guaranteed to surface. With @catch@, if a handler throws an exception, it
-- may be handled by a successive call to @catch@. Fortunately, with exceptions
-- checked at the type-level, the compiler helps makes this explicit.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catches'.
catches
    :: Safe.MonadCatch m
    => Checked (ToCaught handled e) m a
    -> Handler m a handled e
    -> Checked e m a
catches f (Handler handlers) =
    Checked $ Safe.catches (runChecked f) handlers

-- | Same as `catches`, but forces computation first.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catchesDeep'.
catchesDeep
    :: (Safe.MonadCatch m, IO.MonadIO m, DeepSeq.NFData a)
    => Checked (ToCaught handled e) m a
    -> Handler m a handled e
    -> Checked e m a
catchesDeep f (Handler handlers) =
    Checked $ Safe.catchesDeep (runChecked f) handlers

-- | Same as `catches`, but includes asynchronous exceptions as well.
--
-- Note, this delegates to @safe-exception@'s 'Safe.catchesAsync'. Also,
-- catching asynchronous is discouraged, but this function is provided for
-- completeness.
catchesAsync
    :: Safe.MonadCatch m
    => Checked (ToCaught handled e) m a
    -> Handler m a handled e
    -> Checked e m a
catchesAsync f (Handler handlers) =
    Checked $ Safe.catchesAsync (runChecked f) handlers

-- | Flipped `catches`.
handles
    :: Safe.MonadCatch m
    => Handler m a handled e
    -> Checked (ToCaught handled e) m a
    -> Checked e m a
handles = flip catches

-- | Flipped `catchesDeep`.
handlesDeep
    :: (Safe.MonadCatch m, IO.MonadIO m, DeepSeq.NFData a)
    => Handler m a handled e
    -> Checked (ToCaught handled e) m a
    -> Checked e m a
handlesDeep = flip catchesDeep

-- | Flipped `catchesAsync`.
--
-- Note, catching asynchronous is discouraged, but this function is provided
-- for completeness.
handlesAsync
    :: Safe.MonadCatch m
    => Handler m a handled e
    -> Checked (ToCaught handled e) m a
    -> Checked e m a
handlesAsync = flip catchesAsync

-- | Builds a 'Handler' to use with 'catches' or 'handles'.
--
-- You can put more handler functions into the returned @Handler@ with
-- 'appendHandler', '<++>',  or '<:>'.
handler
    :: Safe.Exception exception
    => (exception -> Checked e m a)
    -> Handler m a '[exception] e
handler f = Handler [Safe.Handler $ runChecked . f]

-- | A trivial empty 'Handler'.
--
-- This may not be useful in the common case, but is here for as a convenience.
emptyHandler :: Handler m a '[] e
emptyHandler = Handler []

-- | Appends two 'Handler's.
appendHandler
    :: Handler m a handled e
    -> Handler m a handled' e
    -> Handler m a (Append handled handled') e
appendHandler (Handler hs) (Handler hs') = Handler $ hs ++ hs'

-- | Infix operator alias for 'appendHandler'
(<++>)
    :: Handler m a handled e
    -> Handler m a handled' e
    -> Handler m a (Append handled handled') e
(<++>) = appendHandler

-- | Chain a handler function into a 'Handler'.
(<:>)
    :: Safe.Exception exception
    => (exception -> Checked e m a)
    -> Handler m a handled e
    -> Handler m a (exception : handled) e
f <:> (Handler hs) = Handler $ Safe.Handler (runChecked . f) : hs

-- | Chain two handler functions together into a 'Handler'.
--
-- This can start a right-associative chain of handlers, which you can add to
-- with '<:>':
--
-- >>> import qualified Control.Exception as E
-- >>> :{
-- completeHandler
--     :: Handler IO ()
--         '[E.IOException, E.ErrorCall, E.ArithException, E.SomeException] e
-- completeHandler =
--          (\(_::E.IOException)    -> throwsNone $ print "handledIO")
--     <:>  (\(_::E.ErrorCall)      -> throwsNone $ print "handledError")
--     <:>  (\(_::E.ArithException) -> throwsNone $ print "handledArith")
--     <::> (\(_::E.SomeException)  -> throwsNone $ print "handledSome")
-- :}
(<::>)
    :: (Safe.Exception exception, Safe.Exception exception')
    => (exception -> Checked e m a)
    -> (exception' -> Checked e m a)
    -> Handler m a '[exception, exception'] e
f <::> g = f <:> handler g

-- | A specialised variant of bracket with just a computation to run afterward.
--
-- Delegates to 'Safe.finally'.
finally
    :: Safe.MonadMask m
    => Checked e m a   -- ^ action
    -> Checked e' m b  -- ^ finalizer
    -> Checked e m a
finally action finalizer =
    Checked $ Safe.finally (runChecked action) (runChecked finalizer)

-- | Like 'finally', but finalizes only if an exception has been raised.
--
-- Delegates to 'Safe.onException'.
onException
    :: Safe.MonadMask m
    => Checked e m a   -- ^ action
    -> Checked e' m b  -- ^ run if action throws an exception
    -> Checked e m a
onException action onError =
    Checked $ Safe.onException (runChecked action) (runChecked onError)

-- | Like 'onException', but provides the handler the thrown exception.
--
-- Delegates to 'Safe.withException'.
withException
    :: (Safe.MonadMask m, Safe.Exception exception)
    => Checked e m a
        -- ^ action
    -> (exception -> Checked e' m b)
        -- ^ run if action throws a specified exception
    -> Checked e m a
withException action onError =
    Checked $ Safe.withException (runChecked action) (runChecked . onError)

-- | Acquire a resource, run an action with it, and release the resource.
--
-- Delegates to 'Safe.bracket'.
bracket
    :: Safe.MonadMask m
    => Checked e m a          -- ^ acquire resource
    -> (a -> Checked e' m b)  -- ^ action
    -> (a -> Checked e m c)   -- ^ release resource
    -> Checked e m c
bracket acquire release action = Checked $ Safe.bracket
    (runChecked acquire) (runChecked . release) (runChecked . action)

-- | Bracket an action with one before and after.
--
-- Delegates to 'Safe.bracket'.
bracket_
    :: Safe.MonadMask m
    => Checked e m a    -- ^ run before action
    -> Checked e' m b   -- ^ run after action
    -> Checked e m c    -- ^ action
    -> Checked e m c
bracket_ before after action = Checked $ Safe.bracket_
    (runChecked before) (runChecked after) (runChecked action)

-- | Setup some context, run an action, and run another action upon an error.
--
-- Delegates to 'Safe.bracketOnError'.
bracketOnError
    :: Safe.MonadMask m
    => Checked e m a          -- ^ setup before action
    -> (a -> Checked e' m b)  -- ^ run if action throws an exception
    -> (a -> Checked e m c)   -- ^ action
    -> Checked e m c
bracketOnError setup onError action = Checked $ Safe.bracketOnError
    (runChecked setup) (runChecked . onError) (runChecked . action)

-- | run some setup, run an action, and run another action upon an error.
--
-- Delegates to 'Safe.bracketOnError_'.
bracketOnError_
    :: Safe.MonadMask m
    => Checked e m a   -- ^ setup before action
    -> Checked e' m b  -- ^ run if action throws an exception
    -> Checked e m c   -- ^ action
    -> Checked e m c
bracketOnError_ before after action = Checked $ Safe.bracketOnError_
    (runChecked before) (runChecked after) (runChecked action)

#if MIN_VERSION_safe_exceptions(0,1,7)
-- | General bracketing.
--
-- Delegates to 'Safe.bracketWithError'.
bracketWithError
    :: Safe.MonadMask m
    => Checked e m a
        -- ^ acquire resource
    -> (Maybe Safe.SomeException -> a -> Checked e' m b)
        -- ^ cleanup, possibly handling an exception from action
    -> (a -> Checked e m c)
        -- ^ action
    -> Checked e m c
bracketWithError before after action = Checked $ Safe.bracketWithError
    (runChecked before) ((fmap . fmap) runChecked after) (runChecked . action)
#endif
