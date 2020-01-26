{ mkDerivation, base, comonad, containers, free, haskeline, parsec
, pretty, recursion-schemes, stdenv, text
}:
mkDerivation {
  pname = "testLang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base comonad containers free haskeline parsec pretty
    recursion-schemes text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
