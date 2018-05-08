# Contributing guidelines

## Contributions

* Contributions and issues are in general warmly welcome.
* Support requests are also very welcome, as github issues.

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
  Currently, Rhine builds on GHC versions 7.10, 8.0 and 8.2,
  but this may change in the future.

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
* https://github.com/ggreif
* https://github.com/fphh/
