{ mkDerivation, base, random, rhine, stdenv, transformers }:
mkDerivation {
  pname = "rhine-examples";
  version = "0.6.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base random rhine transformers ];
  license = stdenv.lib.licenses.bsd3;
}
