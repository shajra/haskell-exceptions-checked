{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Control.Exception.Checked
    ( Caught
    , Checked(runChecked)
    , CheckedIO
    , Handler
    , Safe.MonadCatch
    , Safe.MonadMask(..)
    , Safe.MonadThrow
    , Proxy.Proxy
    , Throws
    , ThrowsAll
    , (<:>)
    , (<::>)
    , (<++>)
    , appendHandler
    , emptyHandler
    , unsafeRunChecked
    , throw
    , throws
    , throws'
    , throwsAll
    , throwsAll'
    , try
    , catch
    , handle
    , handles
    , handler
    , catches
    , finally
    , onException
    , withException
    , bracket
    , bracketOnError
    ) where


import qualified Control.Applicative        as App
import qualified Control.Monad              as Monad
import qualified Control.Monad.Fail         as Fail
import qualified Control.Monad.Fix          as Fix
import qualified Control.Monad.Zip          as Zip
import qualified Data.Functor.Classes       as Functor
import qualified Data.Kind                  as Kind
import qualified Data.Proxy                 as Proxy

import qualified Control.Exception.Safe     as Safe
import qualified Control.Monad.Cont.Class   as Cont
import qualified Control.Monad.Error.Class  as Error
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Class  as State
import qualified Control.Monad.Writer.Class as Writer


infixr 5 <:>, <::>
infixl 2 `catches`, `handles`, `finally`, `onException`, `withException`, <++>


newtype Checked l m a
    = Checked { runChecked :: m a }
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
        , (Error.MonadError e)
        , Zip.MonadZip
        , Fail.MonadFail )

type CheckedIO l a = Checked l IO a

newtype Handler m a es es' = Handler [Safe.Handler m a]

data Caught e l
data AnyException

class Allowed l
instance Allowed (Caught e l)
instance Allowed AnyException

class Allowed l => Throws e l
instance {-# OVERLAPS #-} Throws e (Caught e l)
instance {-# OVERLAPPABLE #-} Throws e l => Throws e (Caught e' l)
instance {-# OVERLAPPING #-} Throws e (Caught Safe.SomeException l)
instance {-# OVERLAPPING #-} Throws e AnyException

type family ToCaught a b where
    ToCaught (h : t) l = Caught h (ToCaught t l)
    ToCaught '[] l = l

type family Append as bs where
    Append (h : t) l = h : Append t l
    Append '[] l =  l

type family ThrowsAll as bs where
    ThrowsAll (e : es) l = (Throws e l, ThrowsAll es l)
    ThrowsAll '[] l = (() :: Kind.Constraint)


unsafeRunChecked
    :: Checked AnyException m a
    -> m a
unsafeRunChecked = runChecked

throw
    :: (Throws e l, Safe.Exception e, Safe.MonadThrow m)
    => e
    -> Checked l m a
throw = Checked . Safe.throw

throws
    :: forall e m l a . (Safe.Exception e, Throws e l, Safe.MonadThrow m)
    => m a
    -> Checked l m a
throws = Checked

throwsAll
    :: forall e m l a . (ThrowsAll e l, Safe.MonadThrow m)
    => m a
    -> Checked l m a
throwsAll = Checked

throws'
    :: (Safe.Exception e, Throws e l, Safe.MonadThrow m)
    => Proxy.Proxy e
    -> m a
    -> Checked l m a
throws' _ = Checked

throwsAll'
    :: (ThrowsAll e l, Safe.MonadThrow m)
    => Proxy.Proxy e
    -> m a
    -> Checked l m a
throwsAll' _ = Checked

try
    :: (Safe.Exception e, Safe.MonadCatch m)
    => Checked (Caught e l) m a
    -> Checked l m (Either e a)
try = Checked . Safe.try . runChecked

catch
    :: (Safe.Exception e, Safe.MonadCatch m)
    => Checked (Caught e l) m a
    -> (e -> Checked l m a)
    -> Checked l m a
catch f g =
    Checked $ Safe.catch (runChecked f) (runChecked . g)

handle
    :: (Safe.Exception e, Safe.MonadCatch m)
    => (e -> Checked l m a)
    -> Checked (Caught e l) m a
    -> Checked l m a
handle = flip catch

catches
    :: Safe.MonadCatch m
    => Checked (ToCaught l l') m a
    -> Handler m a l l'
    -> Checked l' m a
catches f (Handler handlers) =
    Checked $ Safe.catches (runChecked f) handlers

handler
    :: Safe.Exception e
    => (e -> Checked l m a)
    -> Handler m a '[e] l
handler f = Handler [Safe.Handler $ runChecked . f]

emptyHandler :: Handler m a '[] es
emptyHandler = Handler []

appendHandler
    :: Handler m a es' es
    -> Handler m a es'' es
    -> Handler m a (Append es' es'') es
appendHandler (Handler hs) (Handler hs') = Handler $ hs ++ hs'

(<++>)
    :: Handler m a es' es
    -> Handler m a es'' es
    -> Handler m a (Append es' es'') es
(<++>) = appendHandler

(<:>)
    :: Safe.Exception e
    => (e -> Checked es m a)
    -> Handler m a es'' es
    -> Handler m a (e : es'') es
f <:> (Handler hs) = Handler $ Safe.Handler (runChecked . f) : hs

(<::>)
    :: (Safe.Exception e, Safe.Exception e')
    => (e -> Checked es m a)
    -> (e' -> Checked es m a)
    -> Handler m a '[e, e'] es
f <::> g = f <:> handler g

handles
    :: Safe.MonadCatch m
    => Handler m a l l'
    -> Checked (ToCaught l l') m a
    -> Checked l' m a
handles = flip catches

finally
    :: Safe.MonadMask m
    => Checked l m a
    -> Checked l m b
    -> Checked l m a
finally thing after =
    Checked $ Safe.finally (runChecked thing) (runChecked after)

onException
    :: Safe.MonadMask m
    => Checked l m a
    -> Checked l m b
    -> Checked l m a
onException thing after =
    Checked $ Safe.onException (runChecked thing) (runChecked after)

withException
    :: (Safe.Exception e, Safe.MonadMask m)
    => Checked l m a
    -> (e -> Checked l m b)
    -> Checked l m a
withException thing after =
    Checked $ Safe.withException (runChecked thing) (runChecked . after)

bracket
    :: Safe.MonadMask m
    => Checked l m a
    -> (a -> Checked l m b)
    -> (a -> Checked l m c)
    -> Checked l m c
bracket before after thing =
    Checked $ Safe.bracket (runChecked before) (runChecked . after) (runChecked . thing)

bracketOnError
    :: Safe.MonadMask m
    => Checked l m a
    -> (a -> Checked l m b)
    -> (a -> Checked l m c)
    -> Checked l m c
bracketOnError before after thing =
    Checked $ Safe.bracketOnError (runChecked before) (runChecked . after) (runChecked . thing)
