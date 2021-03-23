-module(ejsonlog).

-export([format/2]).

%%--------------------------------------------------------------------

-type config() ::
        #{ cb_fun := fun((logger:log_event()) -> term())
         , time_offset := integer()
         , time_designator := char()
         }.

%%--------------------------------------------------------------------

-spec format(LogEvent, Config) -> unicode:chardata() when
    LogEvent :: logger:log_event(),
    Config :: config().
format(Map = #{msg := {report, #{label := {error_logger, _}, format := Format, args := Terms}}}, UsrConfig) ->
  format(Map#{msg := {report, #{text => txt(Format, Terms)}}}, UsrConfig);
format(#{level:=Level, msg:={report, Msg}, meta:=Meta}, UsrConfig) when is_map(Msg) ->
  Config = apply_defaults(UsrConfig),
  NewMeta = format_meta(Meta, Config),
  NewMsg = maps:merge(NewMeta, #{level => Level, data => Msg}),
  format_log(NewMsg, Config);
format(Map = #{msg := {report, KeyVal}}, UsrConfig) when is_list(KeyVal) ->
  format(Map#{msg := {report, maps:from_list(KeyVal)}}, UsrConfig);
format(Map = #{msg := {string, String}}, UsrConfig) ->
  format(Map#{msg := {report, #{text => txt(String, [])}}}, UsrConfig);
format(Map = #{msg := {Format, Terms}}, UsrConfig) ->
  format(Map#{msg := {report, #{text => txt(Format, Terms)}}}, UsrConfig).

%%--------------------------------------------------------------------

apply_defaults(UsrConfig) ->
  Defaults =
    #{ cb_fun => fun(LogEvent) -> jiffy:encode(LogEvent) end
     , time_offset => 0
     , time_designator => $T
     },
  maps:merge(Defaults, UsrConfig).

format_log(Data, Config) ->
  #{ cb_fun := CbFun
   } = Config,
  Sanitized = sanitize(Data, Config),
  try
    Bin = CbFun(Sanitized),
    <<Bin/binary,$\n>>
  catch
    C:R:S ->
      ErrorLog =
        #{ error => txt("~s", ["JSON conversion failed"])
         , info =>
             #{ class => C
              , reason => txt(R)
              , stack => [txt(F) || F <- S]
              }
         , input => txt(Data)
         },
      EBin = CbFun(ErrorLog),
      <<EBin/binary,$\n>>
  end.

format_meta(Meta, Config) ->
  Map =
    fun(Key, Value) ->
        format_meta_field(Key, Value, Config)
    end,
  maps:map(Map, Meta).

format_meta_field(mfa, {M, F, A}, _Config) ->
  #{module => M, function => F, arity => A};
format_meta_field(time, Time, Config) when is_integer(Time) ->
  #{ time_offset := O
   , time_designator := D
   } = Config,
  Options =
    [ {unit, microsecond}
    , {offset, O}
    , {time_designator, D}
    ],
  txt("~s", [calendar:system_time_to_rfc3339(Time, Options)]);
format_meta_field(_, Value, _Config) ->
  Value.

txt(Term) ->
  txt("~p", [Term]).

txt(Format, Args) ->
  unicode:characters_to_binary(io_lib:format(Format, Args)).

sanitize(Term, Config) when is_map(Term) ->
  Map = fun(_Key, Value) -> sanitize(Value, Config) end,
  maps:map(Map, Term);
sanitize(Term, Config) when is_list(Term) ->
  case lists:all(fun(X) -> is_tuple(X) andalso tuple_size(X) =:= 2 end, Term) of
    true -> sanitize(maps:from_list(Term), Config);
    false ->
      case lists:all(fun is_integer/1, Term) of
        true -> txt("~s", [Term]);
        false -> [sanitize(L, Config) || L <- Term]
      end
  end;
sanitize(Term, Config) when is_tuple(Term) ->
  sanitize(erlang:tuple_to_list(Term), Config);
sanitize(Term, _Config) when is_pid(Term) ->
  txt(Term);
sanitize(Term, _Config) when is_function(Term) ->
  txt(Term);
sanitize(Term, _Config) ->
  Term.
