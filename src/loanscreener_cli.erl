%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Jeno Jakab
%%% @doc
%%%  Loan screener CLI module
%%% @end
%%% @contributors
%%%  Jeno Jakab
%%%-------------------------------------------------------------------

-module(loanscreener_cli).

-export([main/1]).

-export([usage/0, usage/1]).
-export([run_screener/1]).

-type iospec() :: standard_error | standard_io.

-import(util, [pp/1]).

% API
-spec main(list()) -> no_return().
main(["--help"]) ->
  usage(),
  erlang:halt(0);
main(["-help"]) ->
  usage(),
  erlang:halt(0);
main([]) ->
  usage(),
  erlang:halt(0);
main(Argv) when (length(Argv) < 3) ->
  io:format(standard_error, "~nInsufficient number of command-line parameters ...> see <help>~n~n", []),
  usage(standard_error),
  erlang:halt(1);
main(Argv) ->
  run_screener(Argv),
  erlang:halt(0).

% Internals
-spec usage() -> no_return().
usage() ->
  usage(standard_io).

-spec usage(iospec()) -> no_return().
usage(IoSpec) ->
	io:format(IoSpec, "
  Loan screener
  =============

   The program takes url-s of marketdata.json, loandata.json and creditpolicy.json as input
   and processes the loandata and its share position data for detecting if the loan is on default.

   For all loans found on default, there would be a loan alert generated.

   The program outpui is the loan alert array in json format on the STDOUT.
   If any error occured the error is reported on the STDERR.

   Usage: loanscreener <url-of-marketdata-json> <url-of-loandata-json> <url-of-creditpolicy-json>
          loanscreener http://ws.jenojakab.com/files/marketdata.json \\
                        http://ws.jenojakab.com/files/loandata.json \\
                        http://ws.jenojakab.com/files/creditpolicy.json

   Options:
   --help        Prints this help
   ~n
   ", []).


 -spec run_screener(list()) -> string().
run_screener(Argv) ->
  Urls = {
    lists:nth(1, Argv),
    lists:nth(2, Argv),
    lists:nth(3, Argv)
  },
  try
      application:start(inets),
      Data = loanscreener:load_data(Urls),
      LoanAlerts = loanscreener:screen(Data),
      loanscreener:create_report(LoanAlerts)
  catch
    error:Error ->
      io:format(standard_error, "~s~n", [Error]),
      pp(Error);
    _:_ ->
      Err = "Unknown error occured",
      io:format(standard_error, "~s~n", [Err]),
      pp(Err)
  end
  .
