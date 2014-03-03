-module(test1_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    lager:start() ,
    lager:error("lager app is start ~p", [time()]),
    application:start(test1). 

start(_StartType, _StartArgs) ->
    test1_sup:start_link().

stop(_State) ->
    ok.
