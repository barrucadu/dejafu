Change Log
==========

All notable changes to projects in this repository will be documented
in the change logs. Furthermore, these projects are versioned
according to the [Package Versioning Policy][pvp], the *de facto*
standard Haskell versioning scheme.

[pvp]: https://pvp.haskell.org

Each package has its own change log:

- [concurrency/CHANGELOG.markdown][concurrency]
- [dejafu/CHANGELOG.markdown][dejafu]
- [hunit-dejafu/CHANGELOG.markdown][hunit-dejafu]
- [tasty-dejafu/CHANGELOG.markdown][tasty-dejafu]

[concurrency]:  https://github.com/barrucadu/dejafu/blob/master/concurrency/CHANGELOG.markdown
[dejafu]:       https://github.com/barrucadu/dejafu/blob/master/dejafu/CHANGELOG.markdown
[hunit-dejafu]: https://github.com/barrucadu/dejafu/blob/master/hunit-dejafu/CHANGELOG.markdown
[tasty-dejafu]: https://github.com/barrucadu/dejafu/blob/master/tasty-dejafu/CHANGELOG.markdown

Changes to purely internal APIs are not included. Changes to internal
APIs which are exposed through non-internal modules are detailed as if
the change were made in the exposing module (eg, visible additions to
Test.DejaFu.STM.Internal, such as a new typeclass instance, will show
up as if they were added to Test.DejaFu.STM).

Additions may be breaking changes as far as the PVP is concerned. For
example, the addition of new data constructors, or new typeclass
member functions.
