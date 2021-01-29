# Distributed Systems Fundamentals

# Project - Distributed Key-Value Store

In this project we designed an algorithm to create a distributed key-value store API in Java.

Worked with:
- Completable Futures
- Asynchronous sockets

The API offers two methods only, those being write and read.

It guarantees that:
- When a client performs a read following a write operation (on the same objects) he will see what he wrote.
- In the end of concurrent write operations on the same objects the same version is present along all of them.
- Concurrent reads of items being modified in that moment do not observe a partial read.

Our solution was vaguely inspired in 2PC and, in retrospective, it would be the best strategy to have adopted.
