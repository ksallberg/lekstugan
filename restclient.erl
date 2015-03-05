-module(restclient).
-export([main/0]).

main() ->
    inets:start(),
    Method      = get,
    Header      = [],
    Type        = "application/json",
    HTTPOptions = [],
    Options     = [],
    R = httpc:request(get,
                      {"http://localhost:28251/battleship/info/", []},
                      [],
                      [{sync, false}]),
%    R           = httpc:request(Method,
%                                {URL, Header, Type, Body},
%                                HTTPOptions,
%                                Options),
    {ok, RequestId} = R,
    receive
        {http,{Ref, {_Status, _Headers, Body}}} ->
            io:format("R: ~p~n", [Body]),
            X = jiffy:decode(Body),
            io:format("HEJ! ~p~n", [X]),
            ok;
        OP ->
            io:format("mismatch ~p~n", [OP])
    after 500 ->
        error
    end.
