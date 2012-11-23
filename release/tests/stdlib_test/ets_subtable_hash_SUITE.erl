-module(ets_subtable_hash_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([basic_operations_test/1]).

-include_lib("test_server/include/test_server.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic_operations_test].

groups() -> 
    [].

init_per_suite(Config) ->
    erts_debug:set_internal_state(available_internal_state, true),
    Config.

end_per_suite(_Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false).

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

basic_operations_test(suite) ->
    [];
basic_operations_test(doc) ->
    ["Test basic functionality of the subtable_hash table type"];
basic_operations_test(Config) when is_list(Config) ->
    ets:new(name, [subtable_hash]),
    io:format("CREATED ~n"),
    ok.
