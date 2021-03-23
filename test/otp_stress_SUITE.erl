-module(otp_stress_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

%%--------------------------------------------------------------------

-include_lib("common_test/include/ct_event.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_h_common.hrl").

-ifdef(SAVE_STATS).
  -define(COLLECT_STATS(_All_,_Procs_),
          ct:pal("~p",[stats(_All_,_Procs_)])).
-else.
  -define(COLLECT_STATS(_All_,_Procs__), ok).
-endif.

-define(TEST_DURATION, 120). % seconds

%%--------------------------------------------------------------------

all() ->
  [
  ].

%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{minutes,3}},
     {ct_hooks,[logger_test_lib]}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(Case, Config) ->
  try
    ?MODULE:Case(cleanup, Config)
  catch
    error:undef -> ok
  end,
  ok.

%%--------------------------------------------------------------------

%% Cascading failure that produce gen_server and proc_lib reports -
%% how many of the produced log events are actually written to a log
%% with logger_std_h file handler.
std_handler(Config) ->
  File = "/tmp/json.log",
  _ = file:delete(File),
  LoggerCfg =
    #{ config => #{type => {file, File}}
     , formatter => {ejsonlog, #{}}
     },
  {ok,_,Node} =
    logger_test_lib:setup(Config,
          [{logger,
            [{handler,default,logger_std_h, LoggerCfg}]
           }]),
  cascade({Node,{logger_backend,log_allowed,2},[]},
          {Node,{logger_std_h,write,4},[{default,logger_std_h_default}]},
          fun otp_cascading/0).

%% cascade(ProducerInfo,ConsumerInfo,TestFun)
cascade({PNode,PMFA,_PStatProcs},{CNode,CMFA,_CStatProcs},TestFun) ->
    Tab  = ets:new(counter,[set,public]),
    ets:insert(Tab,{producer,0}),
    ets:insert(Tab,{consumer,0}),
    dbg:tracer(process,{fun tracer/2,{Tab,PNode,CNode}}),
    dbg:n(PNode),
    dbg:n(CNode),
    dbg:cn(node()),
    dbg:p(all,[call,arity]),
    dbg:tpl(PMFA,[]),
    dbg:tpl(CMFA,[]),


    Pid = rpc:call(CNode,?MODULE,wrap_test,[PNode,TestFun]),
    MRef = erlang:monitor(process,Pid),
    TO = ?TEST_DURATION*1000,
    receive {'DOWN',MRef,_,_,Reason} ->
            ct:fail({remote_pid_down,Reason})
    after TO ->
            All = ets:lookup_element(Tab,producer,2),
            Written = ets:lookup_element(Tab,consumer,2),
            dbg:stop_clear(),
            ?COLLECT_STATS(All,
                           [{PNode,P,Id} || {Id,P} <- _PStatProcs] ++
                               [{CNode,P,Id} || {Id,P} <- _CStatProcs]),
            Ratio = Written/All * 100,
            ct_event:notify(#event{name = benchmark_data,
                                   data = [{value,Ratio}]}),
            {comment,io_lib:format("~p % (~p written, ~p produced)",
                                   [round(Ratio),Written,All])}
    end.

do_fun(Fun) ->
    reset(),
    Fun().

wrap_test(Fun) ->
    wrap_test(node(),Fun).
wrap_test(Node,Fun) ->
    reset(),
    group_leader(whereis(user),self()),
    rpc:call(Node,?MODULE,do_fun,[Fun]).

reset() ->
    reset([logger_std_h_default, logger_disk_log_h_default, logger_proxy]).
reset([P|Ps]) ->
    is_pid(whereis(P)) andalso logger_olp:reset(P),
    reset(Ps);
reset([]) ->
    ok.

tracer({trace,_,call,{?MODULE,producer,_}},{Tab,_PNode,_CNode}=S) ->
    ets:update_counter(Tab,producer,1),
    S;
tracer({trace,Pid,call,{logger_backend,log_allowed,_}},{Tab,PNode,_CNode}=S) when node(Pid)=:=PNode ->
    ets:update_counter(Tab,producer,1),
    S;
tracer({trace,_,call,{?MODULE,log,_}},{Tab,_PNode,_CNode}=S) ->
    ets:update_counter(Tab,consumer,1),
    S;
tracer({trace,_,call,{_,write,_}},{Tab,_PNode,_CNode}=S) ->
    ets:update_counter(Tab,consumer,1),
    S;
tracer(_,S) ->
    S.

%%%-----------------------------------------------------------------
%%% Create a supervisor tree with processes that crash repeatedly,
%%% causing a lot of supervisor reports and crashreports
otp_cascading() ->
    {ok,Pid} = supervisor:start_link({local,otp_super}, ?MODULE, [otp_super]),
    unlink(Pid),
    Pid.

otp_server_sup() ->
    supervisor:start_link({local,otp_server_sup},?MODULE,[otp_server_sup]).

otp_client_sup(N) ->
    supervisor:start_link({local,otp_client_sup},?MODULE,[otp_client_sup,N]).

otp_server() ->
    gen_server:start_link({local,otp_server},?MODULE,[otp_server],[]).

otp_client() ->
    gen_server:start_link(?MODULE,[otp_client],[]).

init([otp_super]) ->
    {ok, {{one_for_one, 200, 10},
            [{client_sup,
                    {?MODULE, otp_client_sup, [10000]},
                    permanent, 1000, supervisor, [?MODULE]},
             {server_sup,
                    {?MODULE, otp_server_sup, []},
                    permanent, 1000, supervisor, [?MODULE]}
            ]}};
init([otp_server_sup]) ->
    {ok, {{one_for_one, 2, 10},
            [{server,
                    {?MODULE, otp_server, []},
                    permanent, 1000, worker, [?MODULE]}
            ]}};
init([otp_client_sup,N]) ->
    spawn(fun() ->
                  [supervisor:start_child(otp_client_sup,[])
                   || _ <- lists:seq(1,N)]
          end),
    {ok, {{simple_one_for_one, N*10, 1},
            [{client,
                    {?MODULE, otp_client, []},
                    permanent, 1000, worker, [?MODULE]}
            ]}};
init([otp_server]) ->
    {ok, server, 100};
init([otp_client]) ->
    {ok, client,1}.

handle_info(timeout, client) ->
    true = is_pid(whereis(otp_server)),
    {noreply,client,1};
handle_info(timeout, server) ->
    exit(self(), some_error).
