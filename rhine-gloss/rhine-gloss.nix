{ mkDerivation, base, dunai, gloss, rhine, stdenv }:
mkDerivation {
  pname = "rhine-gloss";
  version = "0.6.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base dunai gloss rhine ];
  executableHaskellDepends = [ base ];
  description = "Gloss backend for Rhine";
  license = stdenv.lib.licenses.bsd3;
}
