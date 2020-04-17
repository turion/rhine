# Contributing guidelines

## Contributions

* Contributions and issues are warmly welcome.
  Even beginners can contribute!
  Just look out for issues tagged as
  ["good first issue"](https://github.com/turion/rhine/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).
* Support requests are also very welcome, as github issues.
* If you have written an application using Rhine,
  you are welcome to ask for a code review here, as an issue.
* Bindings to other frameworks and backends are always a good idea to contribute.

## Development model

### Version numbering

* The big version numbers (i.e. `N.n.*.*`) usually follow the
  [Dunai](https://github.com/ivanperez-keera/dunai) version numbering,
  since every API change in Dunai can result in an API change for Rhine.
* The small version numbers (i.e. `*.*.n.n`) conform to the
  [Haskell Package Versioning Policy](https://pvp.haskell.org/).

### GHC, cabal, stack, nix

* Rhine should be compilable with `cabal`, `nix` and `stack`.
  If in doubt, use `cabal` and `nix`.
* Rhine does not make an effort to support old GHC versions,
  but it does make an effort to support _new_ versions.
  Check `.travis.yml` for supported GHC versions.
* The default GHC version is the newest officially released version.
* Rhine aims to be available through [`stackage`](https://www.stackage.org/package/rhine) and [`nixpkgs`/Nixos](https://github.com/NixOS/nixpkgs/).
  If you find that it is not available through these channels,
  it's a bug and you are warmly welcome to report or even fix it.

### Code quality

* All functions and modules should be documented.
* All components such as clocks, schedules and resampling buffers
  should at least have an interactive test that displays their usage.

### Branching model

* At the moment, the `master` branch should always build,
  in all supported configurations.
* All new contributions go through a PR onto the `master` branch.
* New releases correspond to tags on the `master` branch when uploaded to hackage.

### Release checklist

1. Optional: Create release-vx.x.x.x branch
2. Bump versions (including tags)
3. If possible, bump to latest stackage lts
4. Check whether we could support a newer GHC (see below) and
   deprecate old GHCs in case they become a nuisance.
   Rule of thumb: If you'd have to use CPP precompiler blocks, rather deprecate old GHC.
5. Diff with last version and edit `*/ChangeLog.md`
6. If release branch: Create pull request on github onto master
7. Wait for Travis to complete, fixing all build failures.
   Possibly repeat steps 4. and 5.
8. Upload to hackage.
9. Create tag `vx.x.x.x`.
10. If applicable: Merge release branch into master.

### GHC changes checklist

In a pull request, do the following:

1. Add new GHC to `.travis.yml`, remove old ones
2. Update currently supported and default GHCs in this document

## Contributors

Thanks go to:

* https://github.com/ivanperez-keera/
* https://github.com/ggreif/
* https://github.com/fphh/
* https://github.com/SolviQorda/
* https://github.com/alexpeits/
* https://github.com/walseb/
* https://github.com/smunix/
