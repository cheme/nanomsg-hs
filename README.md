This library provides Haskell bindings to nanomsg C library (http://nanomsg.org).

Current Status
--------------

This library is currently in *alpha* stages.

Version 0.0.1 - Initial release

Installation
------------

As usual for Haskell packages this software is installed best via Cabal
(http://www.haskell.org/cabal). In addition to GHC it depends on nanomsg
of course, and on c2hs.

Notes
-----

Nanomsg-hs, provides an API for simple usage with Haskell and with a Socket Type abstraction similar to zeromq3-haskell. The Haskell typing is used to reduce in use error occurence.

The API may not be suited to advanced usage, in term of performance, or library customization. In this case the lower level C binding could be used.

Examples
--------

The examples folder contains some simple tests, mostly in dev test.

Bugs
----

If you find any bugs or other shortcomings I would greatly appreciate a bug
report, via http://github.com/cheme/nanomsg-hs/issues

