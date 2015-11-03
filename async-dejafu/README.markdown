async-dejafu [![Build Status][build-status]][build-log]
============

The [async][] library provides a higher-level interface over threads,
allowing users to conveniently run IO operations asynchronously and
wait for their results. This package is a reimplementation of async
using the `MonadConc` abstraction from [dejafu][], providing
easy-to-use asynchronous operaitons within an easily-testable
framework.

When these functions are used in an IO context, the behaviour should
appear identical to the original async package.

The documentation of the latest developmental version is
[available online][docs].

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

[build-status]: http://ci.barrucadu.co.uk/job/(dejafu)/job/async-dejafu/badge/icon?style=plastic
[build-log]:    http://ci.barrucadu.co.uk/job/(dejafu)/job/async-dejafu/
[docs]:         https://barrucadu.github.io/dejafu/async
[async]:        https://hackage.haskell.org/package/async
[dejafu]:       https://hackage.haskell.org/package/dejafu
