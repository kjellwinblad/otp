-module(ets_generic_interface_SUITE).

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
    %CREATE
    Table = ets:new(name, [generic_interface]),
    %INSERT
    ets:insert(Table, {5, value5}),
    ets:insert(Table, {2, value2}),
    ets:insert(Table, {1, value1}),
    ets:insert(Table, {4, value4}),    
    ets:insert(Table, {3, value3}),
    %LOOKUP
    [{1, value1}] = ets:lookup(Table, 1),
    [{2, value2}] = ets:lookup(Table, 2),
    [{3, value3}] = ets:lookup(Table, 3),
    [{4, value4}] = ets:lookup(Table, 4),
    [{5, value5}] = ets:lookup(Table, 5),
    %DELETE
    ets:delete(Table, 5),
    [] = ets:lookup(Table, 5),
    %DELETE TABLE
    ets:delete(Table),
    ok.
