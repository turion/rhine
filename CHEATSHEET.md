# `rhine` cheatsheet

The most common operators, functions and types,
with short examples and explanations.
If you miss something, feel free to open an issue or pull request.

## Types

`m` is always the monad in which side effects can take place at every step.

| Type                   | Long name               | Explanation                                                                                    |
-----------------------------------------------------------------------------------------------------------------------------------------------------
| `ClSF m cl a b`        | Clocked Signal Function | Input signal `a` at rate `cl`, output signal `b` at the same rate                              |
| `BehaviourF m td a b`  | Behaviour function      | A signal function that can run at any rate or clock.                                           |
| `ResBuf m cl1 cl2 a b` | Resampling buffer       | Input signal `a` at rate `cl1`, output signal `b` at rate `cl2`                                |
| `SN m cl a b`          | Signal network          | Process data internally at rate `cl`. Input `a` at rate `In cl`, and data `b` at rate `Out cl` |
| `Rhine m cl a b`       | Rhine                   | Combination of a signal network and a type-matching clock value.                               |

## Combinators

### General naming guideline

* `@` means roughly "at this rate"
* `>` and `-` are in the direction of data flow

### Common combinators in action

#### Sequential signal composition
```
clsf @@ cl >-- resBuf -@- schedule --> rh

clsf    -- This is a clocked signal function.
     @@    -- Run the clocked signal function on...
        cl    -- ...this clock. Together, they form a "rhine".
           >--    -- We're resampling the output...
               resBuf    -- ...with this buffer...
                      -@-    -- at the following schedule:
                          schedule    -- It decides when the two signal networks are activated.
                                   -->    -- The output of the resampling is forwarded to...
                                       rh    -- ...another rhine.
```

#### Data-parallel signal composition

```
sn1 **** sn2

sn1    -- This is a signal network with input `a` and output `b`, on some clock `cl`. (It is not a rhine, it has no clock value.)
    **** sn2
```

#### Time-parallel signal composition
```
rh1 ||@ schedule @|| rh2

rh1    -- A rhine that inputs some data `a` and outputs some data `b`, on some clock `cl1`.
    ||@    -- We are going to execute the first rhine in parallel with another one.
        schedule    -- This schedule decides when which clock is activated.
                 @||    -- And now the other rhine:
                     rh2    -- Here it is. It also inputs `a` and outputs `b`, but on another clock, e.g. `cl2`.




## Common functions
