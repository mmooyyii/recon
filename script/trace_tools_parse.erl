-module(trace_tools_parse).
-author("yimo").

%% API
-export([parse_transform/2]).


parse_transform(AST, _Option) ->
    trace_tools ! AST,
    AST.