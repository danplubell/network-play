module X509 where

import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as LBS
import           Data.Either
import           Data.PEM
import           System.Process

import           Data.X509
import           Data.X509.CertificateStore

-- The certificates that I used for testing were not in the keychains assumed to be used in MacOS
-- System.X509.CertificateStore assumes that there are only checks two specific locations.
-- A hacked up version that will load the list of known keychains instead of a predefined list
getKeyChains:: IO [FilePath]
getKeyChains = do
    (_, Just hout, _, ph) <- createProcess (proc "security" [ "list-keychains"]) {std_out = CreatePipe}
    content <- LBS.hGetContents hout
    let pathsraw = BSC.lines $ LBS.toStrict content
        paths = map (BSC.unpack.fst.BSC.breakSubstring (BSC.pack  "\"").snd.BSC.breakSubstring (BSC.pack "/")) pathsraw
    _ <- paths `seq` waitForProcess ph
    return paths
listInKeyChains:: [FilePath]-> IO [SignedCertificate]
listInKeyChains keyChains = do
   (_,Just hout, _ , ph) <- createProcess (proc "security" ("find-certificate": "-pa": keyChains)) {std_out = CreatePipe}
   pems <- either error id. pemParseLBS <$> LBS.hGetContents hout
   let targets = rights $ map (decodeSignedCertificate . pemContent) $ filter ((== "CERTIFICATE").pemName) pems
   _ <- targets `seq` waitForProcess ph
   return targets

getSystemCertificateStore::IO CertificateStore
getSystemCertificateStore = do
  keyChains <- getKeyChains
  makeCertificateStore <$> listInKeyChains keyChains



