{ mkDerivation, attoparsec, base, free, recursion-schemes, stdenv
}:
mkDerivation {
  pname = "testLang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base free recursion-schemes
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
