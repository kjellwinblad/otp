%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

-module(phash2_properties).

-compile(export_all).

-proptest(eqc).
-proptest([proper]).

-ifndef(PROPER).
-define(EQC,true).
-endif.

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).
-endif.

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Properties --------------------------------------------------------
%%--------------------------------------------------------------------

prop_phash2_same_with_same_input() ->
    ?FORALL(T, any(), erlang:phash2(T) =:= erlang:phash2(T)).

prop_phash2_same_in_different_versions(DifferntVersionNode) ->
    ?FORALL(T, any(),
            erlang:phash2(T) =:= rpc:call(DifferntVersionNode,erlang,phash2,[T])).

prop_phash2_same_in_different_versions_with_long_input(DifferntVersionNode) ->
    ?FORALL(T, any(),
            begin
                BigTerm = lists:duplicate(10000, T),
                RpcRes = rpc:call(DifferntVersionNode,erlang,phash2,[BigTerm]),
                LocalRes = erlang:phash2(BigTerm),
                RpcRes =:= LocalRes
            end).

%%--------------------------------------------------------------------
%% Generators  -------------------------------------------------------
%%--------------------------------------------------------------------

