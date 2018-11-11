# exceptions-checked: Statically Checked Exceptions

[![Release Branch Build Status](https://img.shields.io/travis/shajra/exceptions-checked/release.svg?label=release)](https://travis-ci.org/shajra/exceptions-checked)
[![Candidate Branch Build Status](https://img.shields.io/travis/shajra/exceptions-checked/candidate.svg?label=candidate)](https://travis-ci.org/shajra/exceptions-checked)

This package provides an API to statically check exceptions at the type-level.
Think of it like checked exceptions in Java, but with better ergonomics. People
sometimes claim that checked exceptions are a failed experiment. This module is
an attempt to prove the contrary.

Though there are enough differences to warrant a separate package, this work is
heavily derived from Pepe Iborra's
[control-monad-exception][control-monad-exception] package and supporting paper
[“Explicitly typed exceptions for Haskell”][control-monad-exception-paper].

Some features include:

* delegation of exception handling to the [safe-exceptions][safe-exceptions]
  library (instead of `Control.Exception`) for improved handling of synchronous
  versus asynchronous exceptions.

* an ergonomic API including not just a basic `throw` and `catch`, but most
  of the handling offered by `safe-exceptions` (`catches`, `finally`,
  `bracket`, `onException`, and so forth).

## Motivation

We generally want types to help us avoid defects, but what can we do about
types like `IO` that threaten to throw a myriad of exceptions? With types like
`Maybe` and `Either`, we can use the type-checker to assure that we've
exhaustively covered all cases with our pattern matching. But this can lead to
at least two problems:

* a split world view of errors as plain values versus exceptions in `IO`

* non-extensible error types.

### A split world

To deal with the composition of `Either` and `IO`, people commonly use
`ExceptT` or `MonadError`, but now we have to think about handling errors
through two APIs: `Control.Exception` and `Control.Monad.Except`. Migrating
from thrown exceptions in `IO` to “error” data types in `Either` or `ExceptT`
can feel tedious.

Also, if we want to implement things like `onFailure`. Do we want to call our
action when there's an exception thrown? Or upon returning a `Left`? Or both?
We can certainly write functions for all possibilities, but it would be nicer
if we didn't have this complexity at all.

### Non-extensible errors

We have further boilerplate with `ExceptT` and `MonadError` every time we
handle a possible error condition. We end up defining data types for every
combination of errors we might have at any given moment. For instance, if we
have an error type like

```
data MyError = NetworkFailed | MsgMalformed
```

and we're given a `ExceptT MyError m a`, what should we return if we handle the
network exception, but still have the possibility of a malformed message? It
seems we need a new data type, because pattern matching over `MyError` forces
us to deal with both cases.

Also, note that `mtl`'s `MonadError` type class gets in the way because the
only way we can move from `MonadError ErrorA` to `MonadError ErrorB` is by
running a concrete transformer stack, which defeats the polymorphism we may
desire by using `mtl`, specifically not committing to any specific transformer
stack before we're ready.

Fans of [lens][lens] may use `Control.Lens.Prism`s and make “Has” type classes,
maybe with Template Haskell using `Control.Lens.TH.makeClassyPrisms`, but even
this non-trivial solution doesn't really help us solve the problem we have of
needing to define data types for when conditions are handled within a program.
Otherwise, we can't discharge the “Has” constraint.

It would be nice if Haskell had row polymorphism, which would naturally solve
this problem. Still, we can employ heterogenous lists to represent an
extensible error type. A variety of libraries can assist with this, [for
example `haskus-utils-variant`][haskus-utils-variant-article].

Building beyond heterogenous lists, we may end up with something like
[extensible-effects][extensible-effects], which maintains an expressive
type-level list of “effects,” one of which can capture which errors have been
thrown and which are not yet handled.

Putting aside debates about performance and lawfulness when considering
`extensible-effects` versus `mtl`, it seems clear that the `extensible-effects`
library pretty well provides us a library for extensible errors. Unfortunately,
we still have a split-world between errors in its `Eff` type and exceptions in
the underlying base (often `IO`). If we want to call things like `finally` or
`onException`, we have to use the `Lift` effect, which provides `MonadBase`,
`MonadBaseControl`, and `MonadIO` instances.

## Our solution

Pepe Iborra's [control-monad-exception][control-monad-exception] package seems
to address the “split-world” problem, while also providing extensible errors.
With it, we can leave our exceptions within `IO`, and just annotate which
exceptions have been thrown and caught with a `Throws` constraint.

`control-monad-exception` wraps our computation with a `Checked` data type.
Some have [tried to avoid this wrapping][safe-exceptions-checked], but seem to
[run into issues][safe-exceptions-checked-issue] with less type inference,
limitations of the API, and possible idiosyncrasies with `forkIO`.

For that reason, in this package we stick with the basic approach of
`control-monad-exception`, but we have a different approach to the API design,
and also choose to delegate exception management calls to the `safe-exception`
package rather than `base`'s `Control.Exception`.

## Using the library candidates

This library is not yet officially released on Hackage, though
[candidates are being published to Hackage][exceptions-checked-candidates].

Candidates in Hackage are not completely implemented, and there's not yet a
standard workflow for them.  For now, we're just using them as a sanity check
of the upload and to review documentation.   That said, we republish candidates
under the same version number, which mutates them.

Tags on GitHub won't change, and we won't force-push on either the “candidate”
or “release” branch (though “user/\*” have no such guarantees).  So we
recommend you get candidates directly from GitHub.  This can be done with both
Cabal and Stack.

### Pulling in candidates with Cabal

If you're using a recent release of Cabal, you can put a
`source-repository-package` stanza in your `cabal.project` file.

Tags of candidates can be found on GitHub.  For instance, to use the
“candidate/0.0.1-rc1” candidate, you can include the following in
`cabal.project`:

```
source-repository-package
    type: git
    location: https://github.com/shajra/exceptions-checked
    tag: candidate/0.0.1-rc1
```

You can then use the `exceptions-checked` package in your Cabal file as usual.

### Pulling in candidates with Stack

Alternatively for Stack, to use the “candidate/0.0.1-rc1” candidate, you can
put the following in your `stack.yaml` file:

```yaml
extra-deps:
- github: shajra/exceptions-checked
  commit: candidate/0.0.1-rc1
```

You can then use the `exceptions-checked` package in your Cabal file as usual.


[exceptions-checked-candidates]: https://hackage.haskell.org/package/exception-checked/candidates/
[control-monad-exception]: https://hackage.haskell.org/package/control-monad-exception
[control-monad-exception-paper]: https://dl.acm.org/citation.cfm?id=2127644
[extensible-effects]: https://hackage.haskell.org/package/extensible-effects
[haskus-utils-variant-article]: http://www.sylvain-henry.info/home/posts/2018-11-04-trouble-with-typed-errors.html
[lens]: https://hackage.haskell.org/package/lens
[safe-exceptions-checked]: https://www.well-typed.com/blog/2015/07/checked-exceptions/
[safe-exceptions-checked-issue]: https://github.com/mitchellwrosen/safe-exceptions-checked/issues/1
[safe-exceptions]: https://hackage.haskell.org/package/safe-exceptions
