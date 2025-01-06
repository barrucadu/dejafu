Supported GHC Versions
======================

Déjà Fu supports the latest four GHC releases, at least.  For testing purposes,
we use Stackage snapshots as a proxy for GHC versions.  The currently supported
versions are:

| GHC | Stackage | base |
| --- | -------- | ---- |
| 9.8 | LTS 23.0 | 4.19.2.0 |
| 9.6 | LTS 22.0 | 4.18.1.0 |
| 9.4 | LTS 21.0 | 4.17.0.0 |
| 9.2 | LTS 20.0 | 4.16.0.0 |
| 9.0 | LTS 19.0 | 4.15.0.0 |
| 8.1 | LTS 17.0 | 4.14.1.0 |
| 8.8 | LTS 15.0 | 4.13.0.0 |
| 8.6 | LTS 14.0 | 4.12.0.0 |
| 8.4 | LTS 12.0 | 4.11.0.0 |
| 8.2 | LTS 10.0 | 4.10.1.0 |

In practice, we may *compile with* older versions of GHC, but keeping them
working is not a priority.


Adding new GHC releases
-----------------------

When a new version of GHC is released, we need to make some changes for
everything to go smoothly.  In general, these changes should only cause a
**patch level version bump**.

1. Bump the upper bound of [base][] and set up any needed conditional
   compilation
2. Add the GHC and base versions to the table.
3. Remove any unsupported versions from the table.
4. Make a patch release.

A new GHC release won't get a Stackage snapshot for little while.  When it
does:

1. Add the snapshot to the GitHub Actions configuration.
2. Update the resolver in the stack.yaml.
3. Put the snapshot in the table.


Dropping old GHC releases
-------------------------

When we want to drop an unsupported version of GHC, we need to bump the version
bound on [base][] to preclude it.  This is a backwards-incompatible change which
causes a **major version bump**.

1. Remove the dropped GHC version from the GitHub Actions configuration.
2. Bump the lower bound of [base][].
3. Look through the other dependencies.  Some may not work with our new lower
   bound on [base][], so we should bump those too.
4. Remove any now-irrelevant conditional compilation (mostly CPP, but there may
   also be some cabal file bits).
5. Make whatever change required the bump.
6. Make a major release.

GHC versions shouldn't be dropped just because we can, but here are some good
reasons to do it:

- We want to bump the lower bounds of a dependency to a version which doesn't
  support that GHC.
- We want to add a new dependency which doesn't support that GHC.
- The conditional compilation needed to keep that GHC working is getting
  confusing.

[base]: https://hackage.haskell.org/package/base
