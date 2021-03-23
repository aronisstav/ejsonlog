-module(ejsonlog_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

%%--------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%%--------------------------------------------------------------------

all() ->
  [ test
  , system_test
  ].

%%--------------------------------------------------------------------

suite() ->
  [{timetrap, {seconds, 3}}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

%%--------------------------------------------------------------------

test(Config) ->
  Id = ?FUNCTION_NAME,
  setup_log(Id, Config),
  Log = #{test => test},
  ?LOG_NOTICE(Log),
  check_it_was_logged(Id, Log, Config),
  ok.

%%--------------------------------------------------------------------

system_test(Config) ->
  Id = ?FUNCTION_NAME,
  setup_log(Id, Config),
  OldCfg = logger:get_primary_config(),
  logger:set_primary_config(level, info),
  application:start(sasl),
  application:stop(sasl),
  logger:set_primary_config(OldCfg),
  check_sth_was_logged(Id, Config),
  ok.

%%--------------------------------------------------------------------

setup_log(Id, Config) ->
  LogFile = get_logfile_name(Id, Config),
  LogConfig =
    #{ config =>
         #{ file => LogFile
          }
     , formatter => {ejsonlog, #{}}
     },
  ?assertEqual(ok, logger:add_handler(my_std_h, logger_std_h, LogConfig)),
  ok.

check_it_was_logged(Id, Term, Config) ->
  Bin = get_log(Id, Config),
  Decoded = decode(Bin),
  Match = decode(encode(Term)),
  ?assertMatch(#{<<"data">> := Match}, Decoded).

check_sth_was_logged(Id, Config) ->
  Bin = get_log(Id, Config),
  ?assertNotEqual(<<"">>, Bin).

get_log(Id, Config) ->
  logger:remove_handler(my_std_h),
  LogFile = get_logfile_name(Id, Config),
  {ok, Bin} = file:read_file(LogFile),
  ct:log("Log content: ~p", [Bin]),
  Bin.

get_logfile_name(Id, Config) ->
  PrivDir = ?config(priv_dir, Config),
  Filename = atom_to_list(Id) ++ ".txt",
  filename:join(PrivDir, Filename).

encode(Term) ->
  jiffy:encode(Term).

decode(Bin) ->
  jiffy:decode(Bin, [return_maps]).
