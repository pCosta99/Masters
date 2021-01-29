% Module defining a queue.

-module(test).
-export([create/0, enqueue/2, dequeue/1,createOP/0, enqueueOP/2, dequeueOP/1]).

% Naive approach.
create() -> [].

enqueue(Queue, Item) -> Queue ++ [Item].

dequeue([]) -> empty;
dequeue([Head|_]) -> Head.

% Smart approach.
% Use two lists.

createOP() -> {[],[]}.

enqueueOP({In,Out},Item) -> {[Item|In],Out}.

dequeueOP({[],[]}) -> empty;
dequeueOP({In,[]}) -> dequeueOP({[],lists:reverse(In)});
dequeueOP({In,[H|T]}) -> {{In,T},H}.