%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Jeno Jakab
%%% @doc
%%%  Http Json file reader
%%% @end
%%% @contributors
%%%  Jeno Jakab
%%%-------------------------------------------------------------------
-module(jsonreader).

-export([start/0]).
-export([stop/1]).
-export([loop/0]).
-export([fetch_data_from_json/1]).

-import(util, [pp/1]).

-type url() :: string().
-type reason() :: any().
-type result() :: any().

% API
-spec start() -> pid().
start() ->
  spawn(?MODULE, loop, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
  Pid ! {self(), terminate},
  ok.

% Internals
-spec loop() -> ok.
loop() ->
  receive
    {From, {fetch, Url}} ->
        Result = fetch_data_from_json(Url),
        From ! {self(), {result, Result}};
    {_From, terminate} ->
      ok;
    _ ->
      loop()
  end.

-spec fetch_data_from_json(url()) -> {ok, result()} |
                                     {error, reason()}.
fetch_data_from_json(Url) ->
  case httpc:request(Url) of
    {error, Reason} ->
      {error, Reason};
    {ok, Result} ->
      {_, _, Body} = Result,
      try
        Bin = list_to_binary(Body),
        Data = jsx:decode(Bin, [return_maps]),
        {ok, Data}
      catch
        _ ->
          Err = io_lib:format("Invalid json XXX in the content of url: ~s", [Url]),
          {error, Err};
        _:Err2 ->
          {error, Err2}
      end;
    _ ->
      {error, "Unknown error from httpc:request()"}
  end.
