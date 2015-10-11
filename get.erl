-module(get).
-compile(export_all).

%% {ok,{{"HTTP/1.1",200,"OK"},
%%      [{"access-control-allow-origin","*"}],
%%      "{\"info_text\":\"This is a battleship game to show how to use erlrest\"}"}}

do_get() ->
    inets:start(),
    {ok, {{_HttpVersion,
           _Status,
           _Msg},
          _Headers,
          Body}} = httpc:request("http://localhost:28251/battleship/info/"),
    io:format("HTTP Body: ~p~n", [Body]).
