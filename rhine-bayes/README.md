# README

This package connects [`rhine`](https://hackage.haskell.org/package/rhine) to the [`monad-bayes`](hackage.haskell.org/package/monad-bayes) library for probabilistic programming and inference.
It provides:

* Some standard stochastic processes such as Brownian Motion and Levý processes
* A particle filter inference method called Sequential Monte Carlo

This allows you to do interactive probabilistic (i.e. involving randomness) programs,
and at the same time perform online inference, or realtime machine learning.
An example for this is given in `rhine-bayes/app/Main.hs`,
where inference is performed both on simulated values as well as external input given by the user.
