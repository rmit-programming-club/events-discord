{ mkDerivation, base, hspec, megaparsec, parser-combinators, stdenv
, text, time
}:
mkDerivation {
  pname = "rrule";
  version = "0.1.0.0";
  src = 
    let
      pkgs = import <nixpkgs> {};
    in
      pkgs.fetchFromGitHub {
        owner = "Hazelfire";
        repo = "rrule";
        rev = "5485872705ea8a9c77eb8d593f016b8c72f8167e";
        sha256 = "078g6ppp08bslk14rh6bjvbq40g0ykxrndams17axa0f14g8smdn";
        fetchSubmodules = false;
      };
  sha256 = "5450819976b20f4b472772bc88706e316ea6e1f5ea338518f39a65b777cea900";
  libraryHaskellDepends = [
    base megaparsec parser-combinators text time
  ];
  testHaskellDepends = [ base hspec text ];
  homepage = "https://github.com/mitchellvitez/rrule#readme";
  description = "Recurrence rule parser and formatter";
  license = stdenv.lib.licenses.bsd3;
}
