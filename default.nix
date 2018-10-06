{ mkDerivation, aeson, base, base64-bytestring, blaze-html
, bytestring, case-insensitive, cereal, containers, cryptonite
, directory, http-api-data, http-client, http-client-tls, lens
, memory, money-sync-service, protolude, regex-posix, safecopy
, servant, servant-client, stdenv, text, time, unix
}:
mkDerivation {
  pname = "money-ihaskell";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring blaze-html bytestring case-insensitive
    cereal containers cryptonite directory http-api-data http-client
    http-client-tls lens memory money-sync-service protolude
    regex-posix safecopy servant servant-client text time unix
  ];
  license = stdenv.lib.licenses.bsd3;
}
