Release Notes
=============

All notable changes to this project will be documented in this file.

This project is versioned according to the [Package Versioning Policy](https://pvp.haskell.org), the
*de facto* standard Haskell versioning scheme.


unreleased
----------

### Test.Tasty.DejaFu

- New `testDejafusDiscard` function (#195).


---------------------------------------------------------------------------------------------------


1.0.0.1
-------

- **Date**    2018-01-09
- **Git tag** [tasty-dejafu-1.0.0.1][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-1.0.0.1

### Miscellaneous

- Bump upper bound of tasty to 1.1.

[tasty-dejafu-1.0.0.1]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-1.0.0.1


---------------------------------------------------------------------------------------------------


1.0.0.0
-------

- **Date**    2017-12-23
- **Git tag** [tasty-dejafu-1.0.0.0][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-1.0.0.0

### Test.Tasty.DejaFu

- The `ConcST` functions have been removed and replaced by the `ConcIO` functions.
- The `IsTest` instance for `ConcST t (Maybe String)` is gone.
- All test functions are generalised to take a `ProPredicate`.
- All test functions now take the action to test as the last parameter.

### Miscellaneous

- The minimum supported version of dejafu is now 1.0.0.0.

[tasty-dejafu-1.0.0.0]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-1.0.0.0


---------------------------------------------------------------------------------------------------


0.7.1.1
-------

- **Date**    2017-11-30
- **Git tag** [tasty-dejafu-0.7.1.1][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.7.1.1

### Test.Tasty.DejaFu

- Fix a missing `@since` annotation.

[tasty-dejafu-0.7.1.1]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.7.1.1


---------------------------------------------------------------------------------------------------


0.7.1.0
-------

- **Date**    2017-11-30
- **Git tag** [tasty-dejafu-0.7.1.0][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.7.1.0

### Test.Tasty.DejaFu

- A new `testPropertyFor` function for checking refinement properties with a custom number of seed
  values and variable assignments.

[tasty-dejafu-0.7.1.0]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.7.1.0


---------------------------------------------------------------------------------------------------


0.7.0.3
-------

- **Date**    2017-11-02
- **Git tag** [tasty-dejafu-0.7.0.3][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.7.0.3

### Miscellaneous

- tasty-0.12 support

[tasty-dejafu-0.7.0.3]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.7.0.3


---------------------------------------------------------------------------------------------------


0.7.0.2
-------

- **Date**    2017-10-11
- **Git tag** [tasty-dejafu-0.7.0.2][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.7.0.2

### Miscellaneous

- dejafu-0.9 support

[tasty-dejafu-0.7.0.2]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.7.0.2


---------------------------------------------------------------------------------------------------


0.7.0.1
-------

- **Date**    2017-09-26
- **Git tag** [tasty-dejafu-0.7.0.1][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.7.0.1

### Miscellaneous

- dejafu-0.8 support

[tasty-dejafu-0.7.0.1]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.7.0.1


---------------------------------------------------------------------------------------------------


0.7.0.0
-------

- **Date**    2017-08-10
- **Git tag** [tasty-dejafu-0.7.0.0][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.6.0.0

### Test.Tasty.DejaFu

- Two new functions: `testDejafuDiscard` and `testDejafuDiscardIO`, allowing you to selectively
  discard results or traces.
- The `Discard` type and `defaultDiscarder` function from dejafu is now re-exported.

### Miscellaneous

- Lower version bound on dejafu raised to 0.7.1.0.

[tasty-dejafu-0.7.0.0]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.7.0.0


---------------------------------------------------------------------------------------------------


0.6.0.0
-------

- **Date**    2017-04-08
- **Git tag** [tasty-dejafu-0.6.0.0][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.6.0.0

### Test.Tasty.DejaFu

- The refinement property testing functionality of dejafu is exposed in the new `testProperty`
  function, and re-exported values.
- Due to changes in dejafu, the `Way` type is now abstract and exposes smart constructor functions:
    - `systematically`, corresponding to the old `Systematically`.
    - `randomly`, corresponding to the old `Randomly`.
    - `uniformly`, a new uniform random (as opposed to weighted random) scheduler.
    - `swarmy`, corresponding to the old `Randomly` and specifying how many executions to use the
      same weights for.
- The `defaultWay`, `defaultMemType`, and `defaultBounds` values are all now re-exported.

### Miscellaneous

- Only dejafu 0.7 is supported.

[tasty-dejafu-0.6.0.0]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.6.0.0


---------------------------------------------------------------------------------------------------


0.5.0.0
-------

- **Date**    2017-04-08
- **Git tag** [tasty-dejafu-0.5.0.0][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.5.0.0

### Test.Tasty.DejaFu

- Due to changes in dejafu, the `Way` type no longer takes a parameter; it is now a GADT.

### Miscellaneous

- There is now a changelog.
- Every definition and instance now has a Haddock "@since" annotation.
- Only dejafu 0.6 is supported.

[tasty-dejafu-0.5.0.0]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.5.0.0


---------------------------------------------------------------------------------------------------


0.4.0.0
-------

- **Date**    2017-02-21
- **Git tag** [tasty-dejafu-0.4.0.0][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.4.0.0

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

[tasty-dejafu-0.4.0.0]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.4.0.0


---------------------------------------------------------------------------------------------------


0.3.0.2
-------

- **Date**    2016-09-10
- **Git tag** [tasty-dejafu-0.3.0.2][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.3.0.2

### Miscellaneous

- Now supports concurrency 1.0.0.0 and dejafu 0.4.0.0

[tasty-dejafu-0.3.0.2]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.3.0.2


---------------------------------------------------------------------------------------------------


0.3.0.1
-------

- **Date**    2016-05-26
- **Git tag** [tasty-dejafu-0.3.0.1][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.3.0.1

### Miscellaneous

- Now supports GHC 8.

[tasty-dejafu-0.3.0.1]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.3.0.1


---------------------------------------------------------------------------------------------------


0.3.0.0
-------

- **Date**    2016-04-28
- **Git tag** [tasty-dejafu-0.3.0.0][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.3.0.0

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

[tasty-dejafu-0.3.0.0]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.3.0.0


---------------------------------------------------------------------------------------------------


0.1.1.0
-------

- **Date**    2016-04-03
- **Git tag** [tasty-dejafu-0.1.1.0][]
- **This version was never pushed to hackage, whoops!**

**This version was misnumbered! It should have been 0.2.1.0!**

### Miscellaneous

- Now supports dejafu 0.3, but drops support for dejafu 0.2.

[tasty-dejafu-0.1.1.0]: https://github.com/barrucadu/dejafu/releases/tag/tasty-dejafu-0.1.1.0


---------------------------------------------------------------------------------------------------


0.2.0.0
-------

- **Date**    2015-12-01
- **Git tag**  [0.2.0.0][]
- **Hackage** https://hackage.haskell.org/package/tasty-dejafu-0.2.0.0

Initial release. Go read the API docs.

[0.2.0.0]: https://github.com/barrucadu/dejafu/releases/tag/0.2.0.0
