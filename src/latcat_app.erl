%% @author Mochi Media <dev@mochimedia.com>
%% @copyright latcat Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the latcat application.

-module(latcat_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for latcat.
start(_Type, _StartArgs) ->
    latcat_deps:ensure(),
    latcat_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for latcat.
stop(_State) ->
    ok.
