%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc latcat.

-module(latcat).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the latcat server.
start() ->
    latcat_deps:ensure(),
    ensure_started(crypto),
    application:start(latcat).


%% @spec stop() -> ok
%% @doc Stop the latcat server.
stop() ->
    application:stop(latcat).
