-module(ebe).

-export([start/0, stop/0, failure/0]).

start() ->
    ok.

failure() ->
    {error, this_is_a_failure}.

stop() ->
    ok.


% EOF
