dpor
====

A generic implementation of dynamic partial-order reduction (DPOR) for
testing arbitrary models of concurrency.

We can characterise the state of a concurrent computation by
considering the ordering of dependent events. This is a partial order:
independent events can be performed in any order without affecting the
result. DPOR is a technique for computing these partial orders at
run-time, and only testing one total order for each partial
order. This cuts down the amount of work to be done significantly. In
particular, this package implemented bounded partial-order reduction,
which is a further optimisation. Only schedules within some *bound*
are considered.

- DPOR with no schedule bounding is **complete**, it *will* find all
  distinct executions!

- DPOR with schedule bounding is **incomplete**, it will only find all
  distinct executions *within the bound*!

The documentation of the latest developmental version is
[available online][docs]. If you want a concrete example of using this
library have a look at dejafu itself, in particular the
Test.DejaFu.SCT module.

**Caution:** The fundamental assumption behind DPOR is that the *only*
source of nondeterminism in your program is the scheduler. Or, to put
it another way, if you execute the same program with the same schedule
twice, you get the same result. If you are using this library in
combination with something which performs I/O, be *very* certain that
this is the case!

For details on the algorithm, albeit presented in a very imperative
way, see *Bounded partial-order reduction*, K. Coons, M. Musuvathi,
and K. McKinley (2013), available at
http://research.microsoft.com/pubs/202164/bpor-oopsla-2013.pdf

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

[docs]: https://docs.barrucadu.co.uk/dpor
