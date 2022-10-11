{ mkDerivation, base, lib, shakespeare, text, yesod-core
, yesod-websockets
}:
mkDerivation {
  pname = "yesod-autoreload";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base shakespeare text yesod-core yesod-websockets
  ];
  executableHaskellDepends = [ base yesod-core ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/yesod-autoreload#readme";
  description = "Auto-reload a yesod app during development";
  license = lib.licenses.mit;
}