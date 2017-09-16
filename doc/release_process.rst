Release Process
===============

1. Figure out what the next version number is.  See the PVP_ page if
   unsure.

2. Update version numbers in the relevant cabal files:

   * Update the ``version`` field
   * Update the ``tag`` in the ``source-repository`` block

3. Fill in all ``@since unreleased`` Haddock comments with the
   relevant version number.

4. Update version numbers in the tables in the README and the Getting
   Started page.

5. Ensure the relevant CHANGELOG files have all the entries they
   should.

6. Add the release information to the relevant CHANGELOG files:

   * Change the ``unreleased`` title to the version number
   * Add the current date
   * Add the git tag name and a link to where it will be on github
   * Add the Hackage URL

7. Commit.

8. Push to GitHub and wait for Travis to confirm everything is OK.  If
   it's not OK, fix what is broken before continuing.

9. Tag.  Tags are in the form ``<package>-<version>``, and the message
   is the changelog entry.

10. Push tags to GitHub.

11. Upload updated packages to Hackage.

See the dejafu-0.7.1.3 `commit`__ and `tag`__ if any of this is
unclear.

.. _PVP: https://pvp.haskell.org/
.. __: https://github.com/barrucadu/dejafu/commit/44181e6018f1ffdfba2c7a71f6a2adfa314cc49d
.. __: https://github.com/barrucadu/dejafu/releases/tag/dejafu-0.7.1.3
