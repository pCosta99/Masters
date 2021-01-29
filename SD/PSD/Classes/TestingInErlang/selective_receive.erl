-module(selective_receive).
-export([important/0, semi/0,normal/0]).

important() ->
    receive
        {Priority, _} when Priority > 10 -> [{Priority,high} | important()];
        {Priority, _} when Priority > 5 -> [semihigh | important()]
        after 0 -> []
    end.

semi() ->
    receive
        {Priority, _} when Priority > 3 -> [semi | semi()]
        after 0 -> normal()
    end.

normal() ->
    receive
        {_, _} -> [low | normal()]
        after 0 -> []
    end.