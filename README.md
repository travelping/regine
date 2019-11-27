regine - modular Erlang process registry
========================================
[![Build Status][travis badge]][travis]
[![Coverage Status][coveralls badge]][coveralls]
[![Erlang Versions][erlang version badge]][travis]

This is process registry similar to [gproc][1]. The difference is that gproc complete handles the
logic of mapping key to pids and vice versa. Regine on the other hand only deals with the
association of key to pids. The reverse direction and some other mapping functions have to
be supplied by the developer through a callback module.

This the developer to full flexibility to implement mapping models that to not fall into the
one key, one map model.

BUILDING
--------

Using rebar3:

    # rebar3 compile


[1]: https://github.com/uwiger/gproc

<!-- Badges -->
[travis]: https://travis-ci.com/travelping/regine
[travis badge]: https://img.shields.io/travis/com/travelping/regine/master.svg?style=flat-square
[coveralls]: https://coveralls.io/github/travelping/regine
[coveralls badge]: https://img.shields.io/coveralls/travelping/regine/master.svg?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-R19.1%20to%2022.1-blue.svg?style=flat-square
