-module(cell).

-define(SERVER, {global, ?MODULE}).

-behaviour(gen_server).

%% API
-export([start/2, calculate_next/1, next/1, status/1, cell_pid/2, rand_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {siblings, status, next_status, x, row, y, col}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start({X, Row}, {Y, Col}) ->
  gen_server:start({local, list_to_atom(lists:concat([?MODULE, X, Y]))}, ?MODULE, [{X, Row}, {Y, Col}], []).

calculate_next(Pid) ->
  gen_server:call(Pid, calculate_next).

next(Pid) ->
  gen_server:call(Pid, next).

status(Pid) ->
  gen_server:call(Pid, status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([{X, Row}, {Y, Col}]) ->
  Status = rand_status(),
  {ok, #state{status = Status, x = X, row = Row, y = Y, col = Col}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(calculate_next, _From, State) ->
  State1 = case State#state.siblings of
    undefined ->
      init_siblings(State);
    _ ->
      State
  end,

  Status = lists:foldl(fun(Sibling, Acc) ->
        status(Sibling) + Acc
    end, 0, State1#state.siblings),

  Reply = if
    Status < 2 ->
      0;
    Status == 2 ->
      State#state.status;
    Status == 3 ->
      1;
    Status > 3 ->
      0
  end,
  {reply, Reply, State1#state{next_status = Reply}};

handle_call(next, _From, State) ->
  NextStatus = State#state.next_status,
  if
    State#state.x == 0 ->
      io:format("~n", []);
    State#state.status == 1 ->
      io:format(".", []);
    State#state.status == 0 ->
      io:format(" ", []);
    true ->
      ok
  end,
  {reply, ok, State#state{status = NextStatus}};

handle_call(status, _From, State) ->
  {reply, State#state.status, State};

handle_call(state, _From, State) ->
  {reply, State,  State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

cell_name(X, Y) ->
  list_to_atom(lists:concat([?MODULE, X, Y])).

cell_pid(X, Y) ->
  whereis(cell_name(X, Y)).

init_siblings(State) ->
  X = State#state.x, Y = State#state.y, Row = State#state.row, Col = State#state.col,
  L = (X + Row - 1) rem Row,
  R = (X + Row + 1) rem Row,
  T = (Y + Col - 1) rem Col,
  B = (Y + Col + 1) rem Col,
  Siblings = [cell_pid(L, T), cell_pid(L, Y), cell_pid(L, B),
              cell_pid(X, T),                 cell_pid(X, B),
              cell_pid(R, T), cell_pid(R, Y), cell_pid(R, B)],
  State#state{siblings = Siblings}.

rand_status() ->
  random:seed(erlang:now()),
  lists:nth(5, float_to_list(random:uniform())) rem 2.

