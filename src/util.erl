-module(util).
-export([pp/1]).

-spec pp(any()) -> string().
pp(X) ->
  A = io_lib:format("~p", [X]),
  lists:flatten(A).
