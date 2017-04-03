-module(loanscreener_test).

-include_lib("eunit/include/eunit.hrl").

-import(util, [pp/1]).

-define(URL_MD, "http://example.com/marketdata.json").
-define(URL_LD, "http://example.com/loandata.json").
-define(URL_CP, "http://example.com/creditpolicy.json").

loanscreen_test_() ->
  {
    foreach,
    fun() ->
      meck:new(httpc)
    end,
    fun(_) ->
      meck:unload(httpc)
    end,
    [
      {
        "Two loans one position not eligible by cp threshold test - should return one loan alert",
        fun() ->
          setup_stubs(md(), ld(), cp()),
          Alerts = run_screener(),
          ?assert(is_list(Alerts)),
          ?assertEqual(1, length(Alerts)),
          verify_alerts(Alerts),
          ExpCollateralValue = (17.2000 * 97002),
          ?assertEqual(ExpCollateralValue, maps:get(<<"eligible_collateral">>, lists:nth(1, Alerts)))
        end
      },
      {
        "Two loans one position not eligible by cp threshold test - should return two loan alerts",
        fun() ->
          Ld = ld(),
          NewLd = [maps:put(<<"amount">>, 480023, lists:nth(1, Ld))] ++ lists:nthtail(1, Ld),
          setup_stubs(md(), NewLd, cp()),
          Alerts = run_screener(),
          ?assert(is_list(Alerts)),
          ?assertEqual(2, length(Alerts)),
          verify_alerts(Alerts),
          ExpCollateralValue1 = (3.7800 * 81188),
          ?assertEqual(ExpCollateralValue1, maps:get(<<"eligible_collateral">>, lists:nth(1, Alerts))),
          ExpCollateralValue2 = (17.2000 * 97002),
          ?assertEqual(ExpCollateralValue2, maps:get(<<"eligible_collateral">>, lists:nth(2, Alerts)))
        end
      },
      {
        "Two loans two positions are not eligible by cp currency and threshold - should return one loan alert",
        fun() ->
          Md = md(),
          NewMd = lists:sublist(Md, 2) ++ [maps:put(<<"currency">>, <<"EUR">>, lists:nth(3, Md))],
          Ld = ld(),
          NewLd = [maps:put(<<"amount">>, 480023, lists:nth(1, Ld))] ++ lists:nthtail(1, Ld),
          setup_stubs(NewMd, NewLd, cp()),
          Alerts = run_screener(),
          ?assert(is_list(Alerts)),
          ?assertEqual(1, length(Alerts)),
          verify_alerts(Alerts),
          ExpCollateralValue = (17.2000 * 97002),
          ?assertEqual(ExpCollateralValue, maps:get(<<"eligible_collateral">>, lists:nth(1, Alerts)))
        end
      },
      {
        "Sample data from files test - should return two loan alerts",
        fun() ->
          Md = md_from_jsonfile(),
          Ld = ld_from_jsonfile(),
          Cp = cp_from_jsonfile(),
          setup_stubs2(Md, Ld, Cp),
          Alerts = run_screener(),
          ?assert(is_list(Alerts)),
          ?assertEqual(2, length(Alerts)),
          verify_alerts(Alerts)
        end
      }
    ]
  }.

test_urls() ->
  [?URL_MD, ?URL_LD, ?URL_CP].

verify_alerts(Alerts) ->
  lists:foreach(
    fun(Alert) ->
      ?assert(is_map(Alert)),
      ?assert(maps:is_key(<<"id">>, Alert)),
      ?assert(maps:is_key(<<"creditpolicy">>, Alert)),
      ?assert(maps:is_key(<<"amount">>, Alert)),
      ?assert(maps:is_key(<<"eligible_collateral">>, Alert))
    end,
    Alerts).

run_screener() ->
  List = loanscreener_cli:run_screener(test_urls()),
  Bin = list_to_binary(List),
  jsx:decode(Bin, [return_maps]).

setup_stubs(Md, Ld, Cp) ->
  meck:expect(httpc, request,
    fun(Url) ->
      case Url of
        ?URL_MD ->
          {ok, {{200, "OK"}, [], body(Md)}};
        ?URL_LD ->
          {ok, {{200, "OK"}, [], body(Ld)}};
        ?URL_CP ->
          {ok, {{200, "OK"}, [], body(Cp)}}
      end
    end).

setup_stubs2(Md, Ld, Cp) ->
  meck:expect(httpc, request,
    fun(Url) ->
      case Url of
        ?URL_MD ->
          {ok, {{200, "OK"}, [], Md}};
        ?URL_LD ->
          {ok, {{200, "OK"}, [], Ld}};
        ?URL_CP ->
          {ok, {{200, "OK"}, [], Cp}}
      end
    end).

body(Data) ->
  binary_to_list(jsx:encode(Data)).

md() ->
  [
    #{
      <<"currency">> => <<"USD">>,
      <<"ticker">> => <<"FTR">>,
      <<"exchange">> => <<"USNASD">>,
      <<"id">> => <<"US35906A1088">>,
      <<"price">> => 3.6500,
      <<"name">> => <<"Frontier Communications Corp">>
    },
    #{
      <<"currency">> => <<"USD">>,
      <<"ticker">> => <<"UNIB">>,
      <<"exchange">> => <<"USOTC">>,
      <<"id">> => <<"US9140901052">>,
      <<"price">> => 17.2000,
      <<"name">> => <<"University Bancorp Inc. (MI)">>
    },
    #{
      <<"currency">> => <<"USD">>,
      <<"ticker">> => <<"ELMD">>,
      <<"exchange">> => <<"USAMEX">>,
      <<"id">> => <<"US2854091087">>,
      <<"price">> => 3.7800,
      <<"name">> => <<"Electromed Inc.">>
    }
  ].

ld() ->
  [
    #{
      <<"amount">> => 10,  % 480023,
      <<"creditpolicy">> => <<"policy1">>,
      <<"id">> => <<"loan1">>,
      <<"positions">> => [
        #{
          <<"id">> => <<"US2854091087">>,
          <<"quantity">> => 81188
        }
      ]
    },
    #{
      <<"amount">> => 2533492,
      <<"creditpolicy">> => <<"policy2">>,
      <<"id">> => <<"loan0">>,
      <<"positions">> => [
        #{
          <<"id">> => <<"US35906A1088">>,
          <<"quantity">> => 4736
        },
        #{
          <<"id">> => <<"US9140901052">>,
          <<"quantity">> => 97002
        }
      ]
    }
  ].

cp() ->
  #{
    <<"policy1">> => #{
      <<"currency">> => <<"USD">>,
      <<"price">> => 1.00
    },
    <<"policy2">> => #{
      <<"currency">> => <<"USD">>,
      <<"price">> => 10.00
    }
  }.

md_from_jsonfile() ->
  {ok, Data} = file:read_file("./data/marketdata.json"),
  binary_to_list(Data).

ld_from_jsonfile() ->
  {ok, Data} = file:read_file("./data/loandata.json"),
  binary_to_list(Data).

cp_from_jsonfile() ->
  {ok, Data} = file:read_file("./data/creditpolicy.json"),
  binary_to_list(Data).
