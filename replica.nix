{ mkDerivation, aeson, async, base, bytestring, chronos
, co-log-core, containers, cryptonite, Diff, file-embed, hex-text
, http-media, http-types, lib, psqueues, QuickCheck
, quickcheck-instances, resourcet, stm, template-haskell, text
, torsor, wai, wai-websockets, websockets
}:
mkDerivation {
  pname = "replica";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring chronos co-log-core containers
    cryptonite Diff file-embed hex-text http-media http-types psqueues
    resourcet stm template-haskell text torsor wai wai-websockets
    websockets
  ];
  testHaskellDepends = [
    aeson async base bytestring chronos co-log-core containers
    cryptonite Diff file-embed hex-text http-media http-types psqueues
    QuickCheck quickcheck-instances resourcet stm template-haskell text
    torsor wai wai-websockets websockets
  ];
  homepage = "https://github.com/https://github.com/pkamenarsky/replica#readme";
  description = "Remote virtual DOM library";
  license = lib.licenses.bsd3;
}
