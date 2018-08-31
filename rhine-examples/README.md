# README
--------

In this package, you can find several working Rhine examples,
which you can run and toy around with.

## How to run

### `cabal`

Navigate to this directory, and then execute:

```
cabal sandbox init
cabal install --only-dependencies
```

To run the example `ExampleName`, just execute `cabal run ExampleName`.

### `stack`

For initialisation, and upon every change, execute `stack build`.
To run the example `ExampleName`, just execute `stack exec ExampleName`.

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

### `EventClock`

This shows how Rhine's `Event` clock can be used,
