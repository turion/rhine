# Contributing guidelines

## Contributions

* Contributions and issues are warmly welcome.
  Even beginners can contribute!
  Just look out for issues tagged as
  ["good first issue"](https://github.com/turion/rhine/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).
* Support requests are also very welcome, as github issues.
* If you have written an application using Rhine,
  you are welcome to ask for a code review here, as an issue.

## Development model

### Version numbering

* The big version numbers (i.e. `N.n.*.*`) follow the
  [Dunai](https://github.com/ivanperez-keera/dunai) version numbering,
  since every API change in Dunai can result in an API change for Rhine.
* The small version numbers (i.e. `*.*.n.n`) conform to the
  [Haskell Package Versioning Policy](https://pvp.haskell.org/).

### GHC, cabal, stack

* Rhine should be compilable with `cabal` and `stack`.
* Rhine does not make an effort to support old GHC versions.
  Currently, Rhine builds on GHC versions 8.0, 8.2 and 8.4,
  but this may change in the future.
* The default GHC version is the one that comes with the stackage resolver,
  specified in `stack.yaml`. This is currently 8.2, but may change.
  Rhine aims to build without warnings for the default version,
  but not necessarily for all supported versions.

### Code quality

* All functions and modules should be documented.
* All components such as clocks, schedules and resampling buffers
  should at least have an interactive test that displays their usage.

### Branching model

* At the moment, the `master` branch should always build,
  in all supported configurations.
* All new contributions go to the `develop` branch.
* New releases are prepared and tested on `release-N.n.n.n`-branches,
  and tagged when merged and uploaded to hackage.
* Rhine aims to follow this model:
  http://nvie.com/posts/a-successful-git-branching-model/

## Contributors

Thanks go to:

* https://github.com/ivanperez-keera/
* https://github.com/ggreif/
* https://github.com/fphh/
* https://github.com/alexpeits/
