Release Notes
=============

All notable changes to this project will be documented in this file.

This project is versioned according to the [Package Versioning Policy](https://pvp.haskell.org), the
*de facto* standard Haskell versioning scheme.


0.7.0.0 [2017-08-10] (git tag: [hunit-dejafu-0.7.0.0][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.7.0.0

### Test.HUnit.DejaFu

- Two new functions: `testDejafuDiscard` and `testDejafuDiscardIO`, allowing you to selectively
  discard results or traces.
- The `Discard` type and `defaultDiscarder` function from dejafu is now re-exported.

### Miscellaneous

- Lower version bound on dejafu raised to 0.7.1.0.

[hunit-dejafu-0.7.0.0]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.7.0.0


---------------------------------------------------------------------------------------------------


0.6.0.0 [2017-06-07] (git tag: [hunit-dejafu-0.6.0.0][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.6.0.0

### Test.HUnit.DejaFu

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

[hunit-dejafu-0.6.0.0]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.6.0.0


---------------------------------------------------------------------------------------------------


0.5.0.0 [2017-04-08] (git tag: [hunit-dejafu-0.5.0.0][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.5.0.0

### Test.HUnit.DejaFu

- Due to changes in dejafu, the `Way` type no longer takes a parameter; it is now a GADT.

### Miscellaneous

- There is now a changelog.
- Every definition and instance now has a Haddock "@since" annotation.
- Only dejafu 0.6 is supported.

[hunit-dejafu-0.5.0.0]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.5.0.0


---------------------------------------------------------------------------------------------------


0.4.0.1 [2017-03-20] (git tag: [hunit-dejafu-0.4.0.1][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.4.0.1

### Miscellaneous

- Now supports HUnit 1.6.

[hunit-dejafu-0.4.0.1]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.4.0.1


---------------------------------------------------------------------------------------------------


0.4.0.0 [2017-02-21] (git tag: [hunit-dejafu-0.4.0.0][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.4.0.0

### Test.HUnit.DejaFu

- All the functions which did take a `Bounds` now take a `Way` instead and support random scheduling
  as well.
- The `Way` type from dejafu is now re-exported.

### Miscellaneous

- The minimum supported version of dejafu has been increased to 0.5 (from 0.2)

[hunit-dejafu-0.4.0.0]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.4.0.0


---------------------------------------------------------------------------------------------------


0.3.0.3 [2016-10-22] (git tag: [hunit-dejafu-0.3.0.3][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.3.0.3

### Miscellaneous

- Now supports HUnit 1.4 and 1.5.

[hunit-dejafu-0.3.0.3]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.3.0.3


---------------------------------------------------------------------------------------------------


0.3.0.2 [2016-09-10] (git tag: [hunit-dejafu-0.3.0.2][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.3.0.2

### Miscellaneous

- Now supports concurrency 1.0.0.0 and dejafu 0.4.0.0

[hunit-dejafu-0.3.0.2]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.3.0.2


---------------------------------------------------------------------------------------------------


0.3.0.1 [2016-05-26] (git tag: [hunit-dejafu-0.3.0.1][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.3.0.1

### Miscellaneous

- Now supports GHC 8.

[hunit-dejafu-0.3.0.1]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.3.0.1


---------------------------------------------------------------------------------------------------


0.3.0.0 [2016-04-28] (git tag: [hunit-dejafu-0.3.0.0][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.3.0.0

### Test.HUnit.DejaFu

- New `Assertable` and `Testable` instances for `ConcST t ()` and `ConcIO ()`.
- The `Bounds` type from dejafu is now re-exported.

### Miscellaneous

- Now supports dejafu 0.2 (again).

[hunit-dejafu-0.3.0.0]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.3.0.0


---------------------------------------------------------------------------------------------------


0.2.1.0 [2016-04-03] (git tag: [hunit-dejafu-0.2.1.0][])
-------

**This version was never pushed to hackage, whoops!**

### Miscellaneous

- Now supports dejafu 0.3, but drops support for dejafu 0.2.

[hunit-dejafu-0.2.1.0]: https://github.com/barrucadu/dejafu/releases/tag/hunit-dejafu-0.2.1.0


---------------------------------------------------------------------------------------------------


0.2.0.0 [2015-12-01] (git tag: [0.2.0.0][])
-------

https://hackage.haskell.org/package/hunit-dejafu-0.2.0.0

Initial release. Go read the API docs.

[0.2.0.0]: https://github.com/barrucadu/dejafu/releases/tag/0.2.0.0
