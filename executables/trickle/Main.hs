{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude.Unicode

import Library

import Control.Applicative
import Control.Exception
import Control.Lens qualified as Lens
import Control.Monad
import Control.Monad.Trans.Writer.CPS
import Data.BEncode (BEncode)
import Data.BEncode qualified as BEncode
import Data.Bifunctor
import Data.ByteString qualified as ByteArray
import Data.ByteString.Lazy qualified as Bytes
import Data.List.NonEmpty qualified as NonEmptyList
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Word
import Net.IPv4 qualified as IPv4
import Net.IPv6 qualified as IPv6
import Network.HTTP.Client
import Network.HTTP.Types
import Numeric.Natural
import Options.Generic
import System.Entropy (getEntropy)
import Text.Read

Lens.makePrisms ''BEncode

data Options
  = Encode {info_hash ∷ Text}
  | Announce {tracker ∷ Text, info_hash ∷ Text, ip ∷ Maybe Text, port ∷ Maybe Word16, detailed ∷ Bool}
  deriving (Show, Generic)
instance ParseRecord Options
data EncodeOptions = EncodeOptions {info_hash ∷ Text} deriving (Show, Generic)
data AnnounceOptions = AnnounceOptions {tracker ∷ Text, info_hash ∷ Text, ip ∷ Maybe Text, port ∷ Maybe Word16, detailed ∷ Bool} deriving (Show, Generic)

data Exceptions
  = CannotDecodeResponseBody {responseBody ∷ Text}
  | CannotParseInfoHash {info_hash ∷ Text}
  deriving (Show, Generic)
instance Exception Exceptions

main ∷ IO ()
main = do
  options ← getRecord @IO @Options "trickle — a handheld BitTorrent tracker diagnostic tool"
  case options of
    Encode {..} → encodeCommand EncodeOptions {..}
    Announce {..} → announceCommand AnnounceOptions {..}

encodeCommand ∷ EncodeOptions → IO ()
encodeCommand EncodeOptions {..} = do
  encodedInfoHash ← encodeInfoHash info_hash
  Text.putStrLn do (Text.decodeUtf8Lenient ∘ urlEncode True) encodedInfoHash

announceCommand ∷ AnnounceOptions → IO ()
announceCommand AnnounceOptions {..} = do
  entropy ← getEntropy 13
  manager ← newManager defaultManagerSettings
  infoHashByteArray ← encodeInfoHash info_hash
  let query = (fmap (bimap Text.encodeUtf8 Just) ∘ ($ []) ∘ execWriter) do
        write ("info_hash", infoHashByteArray)
        write ("peer_id", Text.encodeUtf8 "trickle" <> entropy)
        write ("ip", maybe "0.0.0.0" Text.encodeUtf8 ip)
        write ("port", maybe "8999" (Text.encodeUtf8 ∘ Text.pack ∘ show) port)
        write ("downloaded", "0")
        write ("left", "0")
        write ("uploaded", "0")
        write ("event", "started")

  defaultRequestWithHost ← (parseRequest ∘ Text.unpack) tracker
  let request =
        defaultRequestWithHost
          { queryString = renderQuery True query
          , requestHeaders = [("user-agent", "trickle")]
          }

  response ← httpLbs request manager
  maybeDecodedResponseBody ← catch @ErrorCall
    do evaluate (BEncode.bRead response.responseBody)
    do const (pure Nothing)
  outOfMaybe
    maybeDecodedResponseBody
    do throwIO CannotDecodeResponseBody {responseBody = showTruncatedBytes 1024 response.responseBody}
    \decodedResponseBody → do
      let ipv4Peers = viewIPv4Peers decodedResponseBody
      let ipv6Peers = viewIPv6Peers decodedResponseBody
      let peers = (fmap ∘ first) (either IPv4.encode IPv6.encode) ((fmap ∘ first) Left ipv4Peers ++ ipv6Peers)
      printTabulated peers

viewIPv4Peers ∷ BEncode → [(IPv4.IPv4, Word16)]
viewIPv4Peers =
  id
    ∘ catMaybes
    ∘ maybe [] parseIPv4AddressAndPortList
    ∘ fmap Bytes.toStrict
    ∘ Lens.preview (_BDict ∘ Lens.ix "peers" ∘ _BString)

viewIPv6Peers ∷ BEncode → [(Either IPv4.IPv4 IPv6.IPv6, Word16)]
viewIPv6Peers =
  id
    ∘ catMaybes
    ∘ maybe [] parseIPv6AddressAndPortList
    ∘ fmap Bytes.toStrict
    ∘ Lens.preview (_BDict ∘ Lens.ix "peers6" ∘ _BString)

printTabulated ∷ [(Text, Word16)] → IO ()
printTabulated peers = outOfMaybe
  do NonEmptyList.nonEmpty peers
  do pure ()
  \nonEmptyPeers →
    let widthOfAddress = (maximum ∘ fmap (textLength ∘ fst)) nonEmptyPeers + 1
    in  forM_ peers \(address, port) → Text.putStrLn
          do rightPad widthOfAddress address <> (Text.pack ∘ show) port

encodeInfoHash ∷ Text → IO ByteArray.ByteString
encodeInfoHash info_hash = do
  infoHashNumber ← maybe
    do throwIO CannotParseInfoHash {..}
    do pure
    do
      Nothing
        <|> (readMaybe @Natural ∘ Text.unpack) info_hash
        <|> (readMaybe @Natural ∘ Text.unpack) ("0x" <> info_hash)
  pure do (ByteArray.pack ∘ bytesOfNaturalMostSignificantFirst) infoHashNumber
