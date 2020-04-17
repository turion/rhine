
##### Parallel rhine composition
To compose two `Rhine`s in parallel we need a shared `Schedule` between those two.
To share a `Schedule` between two `Rhine`s we need to first construct a `RhineParallelAndSchedule` out of one `Rhine` and one `Schedule`.
To do this we can use the constructor `RhineParallelAndSchedule` or the aliases `||@` or `++@`.
Then we need to combine that resulting `RhineParallelAndSchedule` with another `Rhine` using `@||` or `@++`.

###### Relation between |||| and @||, ++++ and @++
`@||` is defined using `||||`.
This is the implementation:

```
RhineParallelAndSchedule (Rhine sn1 clL) schedule @|| (Rhine sn2 clR)
  = Rhine (sn1 |||| sn2) (ParallelClock clL clR schedule)
```

`@++` is also defined the same way except it replaces `||||` with `++++`.
This is the implementation:

```
RhineParallelAndSchedule (Rhine sn1 clL) schedule @|| (Rhine sn2 clR)
  = Rhine (sn1 ++++ sn2) (ParallelClock clL clR schedule)
```
