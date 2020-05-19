settings@{}:
let
  localPackages = import ../. settings;
in
localPackages.rhine-examples
