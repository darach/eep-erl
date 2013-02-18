# **eep.erl**

> Embedding Event Processing for Erlang

## Status

Experimental.

## Overview

**eep.erl** is a small lightweight subset of Complex Event Processing (CEP) that adds aggregate functions and windowed stream operations to Erlang. It is a straight forward port of [**eep.js**](http://github.com/darach/eep-js/) to Erlang. To understand the motivation, then, read the introduction to **eep.js**. If you prefer PHP, then Ian Barber has ported **eep.js** adding a React PHP edition to the **eep.star** family. Get Ian's [**eep.php**](http://github.com/ianbarber/eep-php) there.

This version is different. It uses processes and message passing and callbacks are provided via OTP's gen_event behaviour. It isn't designed for speed but it is relatively fast for a pure Erlang library.

## Simple Event Processing

There are a number of excellent projects for monitoring/metrics systems already available in Erlang, such as Boundary's excellent [folsom](http://github.com/boundary/folsom). If you need a monitoring or metrics system use those. They do what they say on the tin. If you want lower level building blocks, **eep.erl** gives you four different types of windowed event processing with pluggable aggregate functions.

## Getting Started

Include the header.

```
include_lib("eep_erl/include/eep_erl.hrl").
```

Create a tumbling window process.

```
P = eep_window_tumbling(eep_stats_count, 4).
```

Add an event handler to capture window emissions.

```
P ! {add_handler, eep_emit_trace, []).
```

You can provide your own handler's by implementing **gen_event**

Update the contents of the window with interesting events.

```
P ! {push, 1},
P ! {push, 10},
P ! {push, 100},
P ! {push, 1000}.
```

The window will emit results once the window size (4 here) has been reached.
In the case of a tumbling window the count is reset or zeroed when window closes.

A sliding window, on the other hand, will result in an emission on every event subsequent
to the first emission.

You might use a sliding window as follows.

```
P = eep_window_sliding(eep_stats_sum, 4),
P ! {add_handler, eep_emit_trace, []),
[ P ! {push X} || X <- lists:seq(1,24)].
```

Periodic windows and monotonic windows are created similarly:

```
%% periodic
OneMilli = eep_window_periodic:start(eep_stats_sum, 1),
OneHunMillis = eep_window_periodic:start(eep_stats_avg, 100),
OneSec - eep_window_periodic:start(eep_stats_vars, 1000),

%% monotonic
Mono = eep_window_monotonic(eep_stats_stdevs, eep_clock_count, 1).
```

But usage differs. For clock driven windows a clock tick needs to be
provided / donated by the user. The window will emit results on or after
the anniversary of a significant clock event.

```
main(_Args) ->
  P = eep_window_monotonic:start(eep_stats_count,eep_clock_count,1000),
  P ! {add_handler, eep_emit_trace, []},
  main_loop(P,1).

main_loop(Window,Count) ->
  Window ! {push, 1},
  Window ! tick,
  main_loop(Window,Count+1).
```

## Statistics package

Numerically stable statistics functions ship with the library. These functions
can be used with any of the 4 provided window types.

**eep.erl** ships with:

1. Count - Counts all the things in a window
2. Sum - Adds (the value of) all the things in a window
3. Min - Gets the minimum value'd thing in a window
4. Max - Gets the maximum value'd thing in a window
5. Mean - Gets the statistical mean
6. Variance - Gets the sample variance
7. Standard deviation - Gets the standard deviation


## Noop package

Sometimes the ingress of an event or triggering a close on a window is useful enough information in its own right. So the 'noop' aggregate function supports exactly that.

## Roll your own functions

To implement your own functions, just implement the **eep_aggregate.erl** behaviour.
You can roll your own clock for monotonic windows by implementing the **eep_clock.erl** behvaiour.

## TODO

1. Consider replacing processes with OTP/gen_server.
2. Separate out a library (no process / message passing overhead, emeddable) from existing process oriented windows.
3. Consider a simpler solution than gen_event for wiring up emitted events.
4. Add performance tests. Indicatively 600-700K messages per second to a window process.
5. Explore rewriting performance critical sections and aggregate functions in C, exposed as NIFs.
6. Steal (**beam.js**)[http://github.com/darach/beam-js/] beams and pipes but make it erlangy somehow.

## Enjoy!
