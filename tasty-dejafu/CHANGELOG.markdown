Release Notes
=============

This project is versioned according to the [Package Versioning Policy](https://pvp.haskell.org), the
*de facto* standard Haskell versioning scheme.

- [0.4.0.0](#0.4.0.0) [2017-02-21]
- [0.3.0.2](#0.3.0.2) [2016-09-10]
- [0.3.0.1](#0.3.0.1) [2016-05-26]
- [0.3.0.0](#0.3.0.0) [2016-04-28]
- [0.1.1.0](#0.1.1.0) [2016-04-03]
- [0.2.0.0](#0.2.0.0) [2015-12-01]


unreleased
----------

### Miscellaneous

- There is now a changelog.


---------------------------------------------------------------------------------------------------


0.4.0.0
-------

- **Date:** 2017-02-21
- **Hackage:** [https://hackage.haskell.org/package/tasty-dejafu-0.4.0.0](https://hackage.haskell.org/package/tasty-dejafu-0.4.0.0)
- **Git Tag:** [tasty-dejafu-0.4.0.0](https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.4.0.0)

### Test.Tasty.DejaFu

- All the functions which did take a `Bounds` now take a `Way` instead and support random scheduling
  as well.
- The `Way` type from dejafu is now re-exported.
- The `IsOption` instance (and so corresponding command-line argument) for `Bounds` is gone.
- A new `IsOption` instance for `Way` (and so corresponding command-line argument):
    - "systematically": systematic testing with the default bounds.
    - "randomly": 100 executions with a fixed random seed.

### Miscellaneous

- The minimum supported version of dejafu has been increased to 0.5 (from 0.2)


---------------------------------------------------------------------------------------------------


0.3.0.2
-------

- **Date:** 2016-09-10
- **Hackage:** [https://hackage.haskell.org/package/tasty-dejafu-0.3.0.2](https://hackage.haskell.org/package/tasty-dejafu-0.3.0.2)
- **Git Tag:** [tasty-dejafu-0.3.0.2](https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.3.0.2)

### Miscellaneous

- Now supports concurrency 1.0.0.0 and dejafu 0.4.0.0


---------------------------------------------------------------------------------------------------


0.3.0.1
-------

- **Date:** 2016-05-26
- **Hackage:** [https://hackage.haskell.org/package/tasty-dejafu-0.3.0.1](https://hackage.haskell.org/package/tasty-dejafu-0.3.0.1)
- **Git Tag:** [tasty-dejafu-0.3.0.1](https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.3.0.1)

### Miscellaneous

- Now supports GHC 8.


---------------------------------------------------------------------------------------------------


0.3.0.0
-------

- **Date:** 2016-04-28
- **Hackage:** [https://hackage.haskell.org/package/tasty-dejafu-0.3.0.0](https://hackage.haskell.org/package/tasty-dejafu-0.3.0.0)
- **Git Tag:** [tasty-dejafu-0.3.0.0](https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.3.0.0)

### Test.Tasty.DejaFu

- New `IsTest` instances for `ConcST t (Maybe String)` and `ConcIO (Maybe String)`, with a `Just
  String` result being a test failure with an error message.
- The `Bounds` type from dejafu is now re-exported.
- New `IsOption` instances for `Bounds` and `MemType`.
- New command-line parameter to set the `MemType` parameter:
    - "sc": sequential consistency.
    - "tso": total store order.
    - "pso": partial store order.

### Miscellaneous

- Now supports dejafu 0.2 (again).


---------------------------------------------------------------------------------------------------


0.1.1.0
-------

**This version was misnumbered! It should have been 0.2.1.0!**

- **Date:** 2016-04-03
- **Hackage:** never pushed to hackage, whoops!
- **Git Tag:** [tasty-dejafu-0.1.1.0](https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.1.1.0)

### Miscellaneous

- Now supports dejafu 0.3, but drops support for dejafu 0.2.


---------------------------------------------------------------------------------------------------


0.2.0.0
-------

- **Date:** 2015-12-01
- **Hackage:** [https://hackage.haskell.org/package/tasty-dejafu-0.2.0.0](https://hackage.haskell.org/package/tasty-dejafu-0.2.0.0)
- **Git Tag:** [0.2.0.0](https://github.com/barrucadu/dejafu/releases/tag/0.2.0.0)

Initial release. Go read the API docs.
