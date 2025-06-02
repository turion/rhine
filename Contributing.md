# Contributing guidelines

## Contributions

* Contributions and issues are warmly welcome.
  Even beginners can contribute!
  Just look out for issues tagged as
  ["good first issue"](https://github.com/turion/rhine/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22).
* Support requests are also very welcome, as [Github discussions](https://github.com/turion/rhine/discussions).
* If you have written an application using Rhine,
  you are welcome to ask for a code review here, as a [Github discussion](https://github.com/turion/rhine/discussions).
* Bindings to other frameworks and backends are always a good idea to contribute.

## Development model

### Version numbering

Rhine conforms to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

### GHC, cabal, nix

* Rhine should be compilable with any regular Haskell installation using `cabal`, and also with `nix` (including flakes) & `nixpkgs`.
  If in doubt, use `cabal` and `nix` (preferably flakes with `nix develop`).
* Rhine does not make an effort to support old GHC versions,
  but it does make an effort to support _new_ versions.
  Check `rhine/rhine.cabal` for supported GHC versions.
* The default GHC version is the `nixpkgs` default version.
* Rhine aims to be available through [`stackage`](https://www.stackage.org/package/rhine) and [`nixpkgs`/Nixos](https://github.com/NixOS/nixpkgs/).
  If you find that it is not available through these channels,
  it's a bug and you are warmly welcome to report or even fix it.

### Code quality

* All functions and modules should be documented.
* All components such as clocks, schedules and resampling buffers
  should at least have an interactive application that shows their usage and can be tested by a user.
  This test application should reside in `rhine-examples`
  (for core clocks in the `rhine` package)
  or in a backend package (like `rhine-gloss`, `rhine-terminal`, ...)

### Branching model

* At the moment, the `master` branch should always build,
  in all supported configurations.
* All new contributions go through a PR onto the `master` branch.
* New releases correspond to tags on the `master` branch when uploaded to hackage.

### Release checklist

1. Create release-vx.x.x.x branch
2. Bump versions of `rhine-*` packages (including tags)
3. If possible, bump to latest stackage lts
4. Check whether we could support a newer GHC (see below) and
   deprecate old GHCs in case they become a nuisance.
   Rule of thumb: If you'd have to use CPP precompiler blocks, rather deprecate old GHC.
5. Diff with last version and edit `*/ChangeLog.md`
6. Create pull request on github onto master
7. Wait for CI to complete, fixing all build failures.
   Possibly repeat steps 4. and 5.
8. Upload to hackage.
9. Create tag `vx.x.x.x`.
10. Merge release branch into master.

### GHC changes checklist

In a pull request, do the following:

1. Add new GHC to `tested-with` in `rhine/rhine.cabal`
2. Edit `supportedGhcs` in `flake.nix`

## Library layout

The repository is split into these packages:

* `rhine`: The main library
* `rhine-gloss`: A backend library connecting `rhine` to [`gloss`](http://hackage.haskell.org/package/gloss)
* `rhine-bayes`: A backend library connecting `rhine` to [`monad-bayes`](https://hackage.haskell.org/package/monad-bayes)
* `rhine-terminal`: A backend library connecting `rhine` to [`terminal`](https://hackage.haskell.org/package/terminal)
* `rhine-examples`: A zoo of examples using the main library

If you have a contribution, and you are wondering where it should go, you can ask these questions to find the right place:

* Is it portable (all OSes), has no external (non-Haskell) dependencies, and can be universally used with all backends? __`rhine`__.
* Is it an example application, or maybe a tutorial? __`rhine-examples`__.
* Does it concern an existing backend? __`rhine-BACKEND`__ (where you replace `BACKEND` with the name of the backend).
* Does it add a new backend? Ideally, create a separate repository called `rhine-BACKEND`, and link to it from the main `README.md`. If it's a backend to some library that is well-established in the Haskell community, and you're willing to maintain it and update it whenever there are breaking changes, we can talk about merging it into the main repository, though.

### Backend libraries

Core backend libraries (i.e. those that live in this repo here) have the same version numbers as the main library,
and will always be up to date with that version of the main library.
(There is no support for non-matching versions of a backend library and the main library.)

Contributed backend libraries are of course free to adopt whatever versioning schema they want.

#### Backend library addition checklist

* Add to `flake.nix`
* Add to `README.md`
* Add to `Contributing.md`

## Contributors

Thanks go to:

* https://github.com/ivanperez-keera/
* https://github.com/ggreif/
* https://github.com/fphh/
* https://github.com/SolviQorda/
* https://github.com/alexpeits/
* https://github.com/walseb/
* https://github.com/smunix/
* https://github.com/freckletonj
* https://github.com/jmatsushita
* https://github.com/miguel-negrao
* https://github.com/reubenharry/
