async-dejafu
============

The [async][] library provides a higher-level interface over threads,
allowing users to conveniently run IO operations asynchronously and
wait for their results. This package is a reimplementation of async
using the `MonadConc` abstraction from [concurrency][], providing
easy-to-use asynchronous operaitons within an easily-testable
framework.

This package itself is tested with [dejafu][].

When these functions are used in an IO context, the behaviour should
appear identical to the original async package.

The documentation of the latest developmental version is
[available online][docs].

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

[docs]:        https://docs.barrucadu.co.uk/async-dejafu
[async]:       https://hackage.haskell.org/package/async
[concurrency]: https://hackage.haskell.org/package/concurrency
[dejafu]:      https://hackage.haskell.org/package/dejafu
