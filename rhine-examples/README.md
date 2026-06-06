# README
--------

In this package, you can find several working Rhine examples,
which you can run and toy around with.

## The examples

### `HelloWorld`

Outputs the string `"Hello World!"` on the console every 100 milliseconds.

### `Demonstration`

This example demonstrates Rhine's scheduling capabilities by running several clocks in parallel, and sequentially.
On two clocks ticking every 0.5 and 1.2 seconds, respectively,
a message is created each.
On a third clock, ticking every second,
all messages are collected and output on the console.

To see how clock safety is enforced in Rhine via the type checker,
you can uncomment the last (deliberately incorrect) line in this example,
and witness the type error.
