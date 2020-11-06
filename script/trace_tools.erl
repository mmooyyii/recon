#!/usr/bin/env escript
-module(trace_tools).
-author("yimo").

-export([main/1]).

%% 因为我懒得搞include的解析了,麻烦您把除了本项目include外的依赖路径写在这里
%% 另外请把本脚本的路径也写进去
-define(Include, [

]).

main([FilePath, LineInt]) ->
    {unix, darwin} = os:type(),
    c:c(trace_tools_parse),
    Line = list_to_integer(LineInt),
    Ast = ast(FilePath),
    Module = lists:foldl(fun({attribute, _, module, M}, _) -> M;(_, M) -> M end, '_', Ast),
    FunctionMap = lists:reverse(parse_function(Ast, [])),
    case search_function(Line, FunctionMap) of
        null -> ok;
        Function ->
            Cmd = recon_trace_format(Module, Function),
            os:cmd("echo \"" ++ Cmd ++ "\" | pbcopy"),
            ok
    end.

search_function(Line, [{Function1, Line1}, {_Function2, Line2} | _]) when Line1 =< Line andalso Line =< Line2 ->
    Function1;
search_function(_Line, [{Function, _}]) ->
    Function;
search_function(Line, [_ | Rest]) ->
    search_function(Line, Rest);
search_function(_Line, []) ->
    null.

parse_function([{function, Line, Function, _Args, _Body} | Rest], Acc) ->
    case proplists:is_defined(Function, Acc) of
        true -> parse_function(Rest, Acc);
        false -> parse_function(Rest, [{Function, Line} | Acc])
    end;
parse_function([_ | Rest], Acc) ->
    parse_function(Rest, Acc);
parse_function([], Acc) ->
    Acc.
recon_trace_format(Module, Function) ->
    Tmp = io_lib:format("recon_trace:calls({~p,~p,fun(_)-> return_trace() end},100,[{scope,local}]).", [Module, Function]),
    lists:flatten(Tmp).

ast(Path) ->
    erlang:register(?MODULE, self()),
    io:format("~p~n", [include(Path) ++ [{parse_transform, trace_tools_parse}]]),
    c:c(Path, include(Path) ++ [{parse_transform, trace_tools_parse}]),
    receive AST -> AST after 1000 -> throw(compile_error) end.

include(Path) ->
    ProjectDir = to_project_dir(Path),
    I = lists:map(fun(Dep) -> {i, filename:join([ProjectDir, Dep])} end, ?Include),
    [{i, filename:join([ProjectDir, "include"])}] ++ I.

to_project_dir(Path) ->
    case filename:basename(Path) of
        <<>> -> throw({path_error, Path});
        [] -> throw({path_error, Path});
        "src" -> filename:dirname(Path);
        "test" -> filename:dirname(Path);
        _ -> to_project_dir(filename:dirname(Path))
    end.
