{ mkDerivation, base, aeson, servant, containers, ghcjs-base,  miso, stdenv }:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso aeson servant containers ghcjs-base ];
  description = "First miso app";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
