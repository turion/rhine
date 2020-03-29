{ mkDerivation, base, MonadRandom, simple-affine-space, stdenv
, transformers, transformers-base
}:
mkDerivation {
  pname = "dunai";
  version = "0.6.0";
  sha256 = "b6afcb509bda033a9484e684d67602748ff13d68793c5a1394087792c4109c03";
  libraryHaskellDepends = [
    base MonadRandom simple-affine-space transformers transformers-base
  ];
  doCheck = false;
  homepage = "https://github.com/ivanperez-keera/dunai";
  description = "Generalised reactive framework supporting classic, arrowized and monadic FRP";
  license = stdenv.lib.licenses.bsd3;
}
