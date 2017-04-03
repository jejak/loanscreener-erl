-module(loanscreener).

-export([load_data/1]).
-export([screen/1]).
-export([create_report/1]).
-export([receive_data/3]).
-export([convert_md_to_map/2]).
-export([validate_data/1]).
-export([validate_cp/1]).
-export([validate_mde/1]).
-export([validate_pos/1]).
-export([validate_loan/2]).
-export([calc_collateral/5]).
-export([default_check/3]).
-export([eligibility_check/3]).
-export([screen_loans/4]).

-type url() :: string().

-import(util, [pp/1]).

% API functions

-spec load_data({url(), url(), url()}) -> {map(), list(), map()}.
load_data({UrlMarketDataJson, UrlLoanDataJson, UrlCreditPolicyJson}) ->
  PidMd = jsonreader:start(),
  PidLd = jsonreader:start(),
  PidCp = jsonreader:start(),
  PidMd ! {self(), {fetch, UrlMarketDataJson}},
  PidLd ! {self(), {fetch, UrlLoanDataJson}},
  PidCp ! {self(), {fetch, UrlCreditPolicyJson}},
  Md = receive_data(PidMd, "market data", UrlMarketDataJson),
  Ld = receive_data(PidLd, "loan data", UrlLoanDataJson),
  Cp = receive_data(PidCp, "credit policy data", UrlCreditPolicyJson),
  MdMap = convert_md_to_map(Md, #{}),
  _Data = {
    MdMap,
    Ld,
    Cp
    }.

-spec screen({map(), list(), map()}) -> list().
screen({Md, Ld, Cp}) ->
  validate_data({Md, Ld, Cp}),
  _LoansAlerts = screen_loans(Md, Cp, Ld, []).

-spec create_report(list()) -> string().
create_report(LoansAlerts) ->
  try
    Bin = jsx:encode(LoansAlerts),
    Json = binary_to_list(Bin),
    io:format(standard_io, "~s", [Json]),
    Json
  catch
    _:_ ->
      Err = "Error during generating loan alert json-report!",
      io:format(standard_error, "~s~n", [pp(Err)]),
      pp(Err)
  end
  .

%% -------------
%% Internals
%% -------------

-spec receive_data(pid(), string(), url()) -> any().
receive_data(Pid, TypeStr, Url) ->
  receive
    {Pid, {result, {ok, Data}}} ->
      Data;
    {Pid, {result, {error, Error}}} ->
      Err = io_lib:format("Cannot access ~s [url:~s] Error:~s ~n", [pp(TypeStr), pp(Url), pp(Error)]),
      erlang:error(Err)
    after 50000 ->
      Err2 = io_lib:format("Cannot access ~s: jsonreader not responding [url:~s] ~n", [pp(TypeStr), pp(Url)]),
      erlang:error(Err2)
  end.

-spec convert_md_to_map(list(), map()) -> map().
convert_md_to_map([], MdMapAcc) ->
  MdMapAcc;
convert_md_to_map([Md|Mds], MdMapAcc) ->
  NewMdMapAcc = case maps:is_key(<<"id">>, Md) of
    true ->
      MdMapAcc#{maps:get(<<"id">>, Md) => Md};
    _ ->
      MdMapAcc
  end,
  convert_md_to_map(Mds, NewMdMapAcc).

-spec validate_data({map(), list(), map()}) -> true.
validate_data({Md, Ld, Cp}) ->
  case is_map(Md) of
    false -> erlang:error("Market data is not valid: -not map-");
    true -> true
  end,
  case is_list(Ld) of
    false -> erlang:error("Loan data is not valid: -not array-");
    true -> true
  end,
  case is_map(Cp) of
    false -> erlang:error("Creditpolicy data is not valid: -not map-");
    true -> true
  end,
  true.

-spec validate_cp(map()) -> boolean().
validate_cp(Cp) ->
  _Valid =
    maps:is_key(<<"currency">>, Cp) andalso
    maps:is_key(<<"price">>, Cp).

-spec validate_mde(map()) -> boolean().
validate_mde(Mde) ->
  _Valid =
    maps:is_key(<<"id">>, Mde) andalso
    maps:is_key(<<"currency">>, Mde) andalso
    maps:is_key(<<"price">>, Mde).

-spec validate_pos(map()) -> boolean().
validate_pos(Pos) ->
  _Valid =
    maps:is_key(<<"id">>, Pos) andalso
    maps:is_key(<<"quantity">>, Pos).

-spec validate_loan(map(), map()) -> boolean().
validate_loan(Loan, Cp) ->
  ValidFlds =
    is_map(Loan)
    andalso maps:is_key(<<"creditpolicy">>, Loan)
    andalso maps:is_key(<<"positions">>, Loan)
    andalso maps:is_key(<<"amount">>, Loan)
    ,
  Valid =
    case ValidFlds of
      false -> false;
      true ->
        CpId = maps:get(<<"creditpolicy">>, Loan),
        maps:is_key(CpId, Cp)
    end,
  Valid.

-spec calc_collateral(map(), map(), list(), number(), number()) -> {number(), number()}.
calc_collateral(_Md, _Cp, [], EligPosisAcc, ColValAcc) ->
  {EligPosisAcc, ColValAcc};
calc_collateral(Md, Cp, [Position|Positions], EligPosisAcc, ColValAcc) ->
  {NewEligPosisAcc, NewColValAcc} =
    case eligibility_check(Md, Cp, Position) of
      true ->
        PoId = maps:get(<<"id">>, Position),
        PoQtty = maps:get(<<"quantity">>, Position),
        Mde = maps:get(PoId, Md),
        MdePric = maps:get(<<"price">>, Mde),
        {
          (EligPosisAcc + 1),
          (ColValAcc + PoQtty*MdePric)
        };
      false ->
        {
          EligPosisAcc,
          ColValAcc
        }
    end,
  calc_collateral(Md, Cp, Positions, NewEligPosisAcc, NewColValAcc).

-spec default_check(map(), map(), map()) -> {true, number()} | false.
default_check(Md, Cp, Loan) ->
  Amount = maps:get(<<"amount">>, Loan),
  Positions = maps:get(<<"positions">>, Loan),
  {EligiblePositions, CollateralValue} = calc_collateral(Md, Cp, Positions, 0, 0),
  case (CollateralValue < Amount) of
    true when (EligiblePositions > 0) ->
      {true, CollateralValue};
    _ ->
      false
  end.

-spec eligibility_check(map(), map(), map()) -> boolean().
eligibility_check(Md, Cp, Position) ->
  CpCcy = maps:get(<<"currency">>, Cp),
  case validate_pos(Position) of
    false ->
      false;
    true ->
      PoId = maps:get(<<"id">>, Position),
      case maps:is_key(PoId, Md) of
        false ->
          false;
        true ->
          Mde = maps:get(PoId, Md),
          CpCcy = maps:get(<<"currency">>, Cp),
          PoCcy = maps:get(<<"currency">>, Mde),
          MdePric = maps:get(<<"price">>, Mde),
          CpPric = maps:get(<<"price">>, Cp),
          _Eligible =
            (CpCcy =:= PoCcy) andalso
            (MdePric >= CpPric)
      end
  end.

-spec screen_loans(map(), map(), list(), list()) -> list().
screen_loans(_Md, _Cp, [], LoansAlertsAcc) ->
  lists:reverse(  LoansAlertsAcc);
screen_loans(Md, Cp, [Loan | Loans], LoanAlertsAcc) ->
  NewLoanAlertsAcc =
    case validate_loan(Loan, Cp) of
      true ->
        LcpId = maps:get(<<"creditpolicy">>, Loan),
        Lcp = maps:get(LcpId, Cp),
        case default_check(Md, Lcp, Loan) of
          {true, CollateralValue} ->
            Newalert = #{
              <<"id">> => maps:get(<<"id">>, Loan),
              <<"creditpolicy">> => maps:get(<<"creditpolicy">>, Loan),
              <<"amount">> => maps:get(<<"amount">>, Loan),
              <<"eligible_collateral">> => CollateralValue
            },
            [Newalert|LoanAlertsAcc];
          _ ->
            LoanAlertsAcc
        end;
      false ->
        LoanAlertsAcc
    end,
  screen_loans(Md, Cp, Loans, NewLoanAlertsAcc).
