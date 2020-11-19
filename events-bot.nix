{ mkDerivation, base, base64-bytestring, bytestring
, discord-haskell, gogol, gogol-apps-calendar, hpack, http-client
, http-client-tls, lens, mtl, resourcet, servant, servant-client
, servant-server, stdenv, text, time
}:
mkDerivation {
  pname = "events-bot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base64-bytestring bytestring discord-haskell gogol
    gogol-apps-calendar http-client http-client-tls lens mtl resourcet
    servant servant-client servant-server text time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base base64-bytestring bytestring discord-haskell gogol
    gogol-apps-calendar http-client http-client-tls lens mtl resourcet
    servant servant-client servant-server text time
  ];
  testHaskellDepends = [
    base base64-bytestring bytestring discord-haskell gogol
    gogol-apps-calendar http-client http-client-tls lens mtl resourcet
    servant servant-client servant-server text time
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/events-bot#readme";
  license = stdenv.lib.licenses.bsd3;
}
