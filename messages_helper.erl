-module(messages_helper).
-export([help_me/0]).

help_me() ->
    io:format("Being nice doing some helping.~n"),
    here_is_a_help_atom_hope_youre_happy.
