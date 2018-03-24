Supported GHC Versions
======================

Déjà Fu supports the latest four GHC releases, after GHC 8.0.  For
testing purposes, we use Stackage LTSes as a proxy for GHC versions.
The currently supported versions are:

.. csv-table::
   :header: "GHC", "Stackage", "base"

   "8.4",  "",         "4.11.0.0"
   "8.2",  "LTS 10.0", "4.10.1.0"
   "8.0",  "LTS 9.0",  "4.9.1.0"
   "7.10", "LTS 6.0",  "4.8.2.0"

In practice, we may *compile with* older versions of GHC, but keeping
them working is not a priority.


Adding new GHC releases
-----------------------

When a new version of GHC is released, we need to make some changes
for everything to go smoothly.  In general, these changes should only
cause a **patch level version bump**.

1. Bump the upper bound of :hackage:`base` and set up any needed
   conditional compilation
2. Add the GHC and base versions to the table.
3. Remove any unsupported versions from the table.
4. Make a patch release.

A new GHC release won't get a Stackage LTS for little while.  When it
does:

1. Add the LTS to the Travis script.
2. Update the resolver in the stack.yaml.
3. Put the LTS in the table.


Dropping old GHC releases
-------------------------

When we want to drop an unsupported version of GHC, we need to bump
the version bound on :hackage:`base` to preclude it.  This is a
backwards-incompatible change which causes a **major version bump**.

1. Remove the dropped GHC version from the Travis script.
2. Bump the lower bound of :hackage:`base`.
3. Look through the other dependencies.  Some may not work with our
   new lower bound on :hackage:`base`, so we should bump those too.
4. Remove any now-irrelevant conditional compilation (mostly CPP, but
   there may also be some cabal file bits).
5. Make whatever change required the bump.
6. Make a major release.

GHC versions shouldn't be dropped just because we can, but here are
some good reasons to do it:

* We want to bump the lower bounds of a dependency to a version which
  doesn't support that GHC.
* We want to add a new dependency which doesn't support that GHC.
* The conditional compilation needed to keep that GHC working is
  getting confusing.
