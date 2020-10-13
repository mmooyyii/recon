-module(recon_io_buffer).
-author("yimo").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-export([stop/1]).
-define(SERVER, ?MODULE).


%% 直接使用IoServer会导致背压式(Back Pressure)的调用，所以这里需要做一个IoServer的buffer，
%% 在高压力的情况下，解除recon_trace:calls,防止对生产环境造成影响


-define(MaxQueueLen, 200).
-record(recon_io_server_state, {
    io,
    remote_nodes,
    queue
}).

start_link(Nodes) ->
    gen_server:start_link(?MODULE, [Nodes], []).

init([Nodes]) ->
    erlang:send_after(100, self(), io),
    {ok, #recon_io_server_state{
        io = group_leader(),
        remote_nodes = Nodes,
        queue = {0, queue:new()}
    }}.

handle_call(stop, From, State = #recon_io_server_state{}) ->
    gen_server:reply(From, ok),
    {stop, normal, State}.

handle_cast(_Request, State = #recon_io_server_state{}) ->
    {noreply, State}.

handle_info({io_request, From, Mref, M}, State = #recon_io_server_state{
    remote_nodes = Nodes,
    queue = {L, Queue}
}) ->
    case L > ?MaxQueueLen of
        false ->
            From ! {io_reply, Mref, ok},
            {noreply, State#recon_io_server_state{
                queue = {L + 1, queue:in({io_request, self(), Mref, M}, Queue)}
            }};
        true ->
            rpc:multicall(Nodes, recon_trace, clear, [], 5000),
            {stop, warning, State}
    end;

handle_info(io, State = #recon_io_server_state{io = Io, queue = {_, Queue}}) ->
    lists:foreach(fun(Msg) -> Io ! Msg end, queue:to_list(Queue)),
    erlang:send_after(100, self(), io),
    {noreply, State#recon_io_server_state{queue = {0, queue:new()}}};

handle_info(_, State) ->
    {noreply, State}.

terminate(warning, #recon_io_server_state{io = Io, queue = {_, Queue}}) ->
    lists:foreach(fun(Msg) -> Io ! Msg end, queue:to_list(Queue)),
    io:format("Quit: high benchmark~n"),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State = #recon_io_server_state{}, _Extra) ->
    {ok, State}.

stop(Pid) ->
    gen_server:call(Pid, stop).
