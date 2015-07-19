dejafu [![Build Status][build-status]][build-log]
======

Concurrency is nice, deadlocks and race conditions not so much. The
`Par` monad family, as defined in [abstract-par][] provides
deterministic parallelism, but sometimes we can tolerate a bit of
nondeterminism.

This package provides a class of monads for potentially
nondeterministic concurrency, with an interface very much in the
spirit of `Par`, but slightly more relaxed. Specifically,
`MonadConc`'s `IVar` equivalent, `CVar`s, can be written to multiple
times.

The documentation of the latest developmental version is
[available online][docs].

`MonadConc` and `IO`
--------------------

The intention of the `MonadConc` class is to provide concurrency where
any apparent nondeterminism arises purely from the scheduling
behaviour. To put it another way, a given computation, parametrised
with a fixed set of scheduling decisions, is deterministic. This
assumption is used by the testing functionality provided by
Control.Monad.Conc.SCT.

Whilst this assumption may not hold in general when `IO` is involved,
you should strive to produce test cases where it does.

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

[build-status]: http://ci.barrucadu.co.uk/job/dejafu/badge/icon?style=plastic
[build-log]:    http://ci.barrucadu.co.uk/job/dejafu/
[docs]:         https://barrucadu.github.io/dejafu
[abstract-par]: https://hackage.haskell.org/package/abstract-par/docs/Control-Monad-Par-Class.html
