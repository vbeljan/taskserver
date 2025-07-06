%%%-------------------------------------------------------------------
%%% @author Vanja Beljan <vanja.beljan@gmx.com>
%%% @copyright (C) 2025, Vanja Beljan
%%% @doc
%%%
%%% @end
%%% Created :  3 Jul 2025 by Vanja Beljan <vanja.beljan@gmx.com>
%%%-------------------------------------------------------------------
-module(taskserver_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    application:ensure_all_started(taskserver),
    application:ensure_all_started(katt),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [test1,
     test2,
     test3,
     test4].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test1(doc) ->
    ["Basic task case as described in the document"];

test1(Config) when is_list(Config) ->
    common_run("request1.apib").

test2(doc) ->
    ["Rearrange order of input tasks to verify we get the same result"];

test2(Config) when is_list(Config) ->
    common_run("request2.apib").

test3(doc) ->
    ["Test getting script output"];

test3(Config) when is_list(Config) ->
    common_run("request3.apib").

test4(doc) ->
    ["Test rearranging a more complex example"];

test4(Config) when is_list(Config) ->
    common_run("request4.apib").

common_run(ApibFile) ->
    PrivDir = code:priv_dir(taskserver),
    TestFile = filename:join([PrivDir, ApibFile]),
    {Result, _, _, _, _} = katt:run(TestFile, [{hostname, "localhost"}, {port, "8080"}]),
    ?assertEqual(pass, Result).
