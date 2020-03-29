{ mkDerivation, base, containers, deepseq, dunai, free, MonadRandom
, random, stdenv, time, transformers, vector-sized
}:
mkDerivation {
  pname = "rhine";
  version = "0.6.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq dunai free MonadRandom random time
    transformers vector-sized
  ];
  description = "Functional Reactive Programming with type-level clocks";
  license = stdenv.lib.licenses.bsd3;
}
