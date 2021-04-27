#!/usr/bin/env escript
-module(trace_tools).
-author("yimo").

-export([main/1]).

main([FilePath, LineInt]) ->

    Base = filename:basename(FilePath),
    true = (length(Base) - 4 > 0),
    Module = lists:sublist(Base, length(Base) - 4),
    Base = Module ++ ".erl",
    {Function, _, Body} = find(new_function(FilePath), list_to_integer(LineInt)),
    Cmd = io_lib:format("recon_trace:calls({~p,~p,fun" ++ token_to_code(Body, []) ++ "-> return_trace() end},100,[{scope,local}]).", [list_to_atom(Module), Function]),
    os:cmd("echo \"" ++ lists:flatten(Cmd) ++ "\" | pbcopy").

token_to_code([{Symbol, _} | Token], Acc) when is_atom(Symbol) ->
    token_to_code(Token, [atom_to_list(Symbol) | Acc]);
token_to_code([{_, _, Arg} | Token], Acc) ->
    token_to_code(Token, [atom_to_list(Arg) | Acc]);
token_to_code([], Acc) ->
    lists:concat(lists:reverse(Acc)).



find([{F, L1, B} | _Rest], Line) when L1 > Line ->
    {F, L1, B};
find([{F, L1, B}, {_, L2, _} | _Rest], Line) when L1 =< Line andalso Line < L2 ->
    {F, L1, B};
find([Last], _Line) ->
    Last;
find([_ | Rest], Line) ->
    find(Rest, Line).


to_token(File) when 1 =:= 1 ->
    {ok, Code} = file:read_file(File),
    {ok, Token, _} = erl_scan:string(binary_to_list(Code)),
    Token.

new_function(File) ->
    lists:reverse(p_new_function(to_token(File), [])).
p_new_function([{Split, _}, {atom, L, F} | R], Acc) when Split =:= 'dot' orelse Split =:= ';' ->
    {Function, Rest} = parse_function(R, []),
    p_new_function(Rest, [{F, L, Function} | Acc]);
p_new_function([_ | Rest], Acc) ->
    p_new_function(Rest, Acc);
p_new_function([], Acc) ->
    Acc.

parse_function([{'->', _} | Rest], Func) ->
    {lists:reverse(Func), Rest};
parse_function([Any | Rest], Acc) ->
    parse_function(Rest, [Any | Acc]).
