-module(math1).
-export([factorial/1]).
factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).