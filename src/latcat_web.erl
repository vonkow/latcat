%% @author Caz vonKow <caz@vonkow.com>
%% @copyright 2012+ Caz vonKow <caz@vonkow.com>

%% @doc Web server for latcat.

-module(latcat_web).
-author("Caz vonKow <caz@vonkow.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    latcat_db:init(),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
					"search/box/" ++ BoxString ->
						Box = string:tokens(BoxString, "/"), % [Top, Left, Bottom, Right] 
						Res = latcat_logic:search({box, Box}),
						Req:respond({200, [{"Content-Type", "application/json"}], [mochijson2:encode(Res)]});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                Data = Req:parse_post(),
                case Path of
					"add" ->
						latcat_logic:add_post(Data),
						Req:respond({200, [], [<<"OK\n">>]});
                    "delete" ->
                        latcat_logic:delete_note(Data),
                        Req:respond({200, [], [<<"OK\n">>]});
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
