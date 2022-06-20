let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat-src = fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
  };
  flake-compat = import flake-compat-src { src =  ./.; };
in
flake-compat.shellNix
