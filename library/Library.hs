module Library where

import Prelude.Unicode

import Control.Monad.Trans.Writer.CPS
import Data.Bitraversable
import Data.ByteString qualified as ByteArray
import Data.ByteString.Lazy qualified as Bytes
import Data.Function
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word
import Net.IPv4
import Net.IPv6
import Numeric.Natural
import Options.Generic

bytesOfNaturalLeastSignificantFirst ∷ Natural → [Word8]
bytesOfNaturalLeastSignificantFirst = \case
  0 → [0]
  n → flip fix n \recurse → \case
    0 → []
    natural → let (quotient, remainder) = natural `divMod` 256 in fromIntegral remainder : recurse quotient

caching ∷ (α → β) → ((β, α) → γ) → α → γ
caching cache function input = function (cache input, input)

bytesOfNaturalMostSignificantFirst ∷ Natural → [Word8]
bytesOfNaturalMostSignificantFirst = caching ((+ 1) ∘ floor @Double ∘ logBase 256 ∘ fromIntegral) work
 where
  work ∷ (Word, Natural) → [Word8]
  work = fix \recurse → \case
    (0, _) → []
    (numberOfDigits, number) →
      let
        magnitudeOfMostSignificantBit = 256 ^ (numberOfDigits − 1)
        valueOfMostSignificantBit = fromIntegral (number `div` magnitudeOfMostSignificantBit)
      in
        valueOfMostSignificantBit : recurse (numberOfDigits − 1, number − valueOfMostSignificantBit × magnitudeOfMostSignificantBit)

naturalFromBytesLeastSignificantFirst ∷ [Word8] → Natural
naturalFromBytesLeastSignificantFirst = List.foldl' (\natural word → fromIntegral word + natural × 256) 0

naturalFromBytesMostSignificantFirst ∷ [Word8] → Natural
naturalFromBytesMostSignificantFirst = List.foldl' (\natural word → fromIntegral word + natural × 256) 0

write ∷ (Monad monad) ⇒ anything → WriterT ([anything] → [anything]) monad ()
write item = tell (++ [item])

parseIPv4AddressAndPortList ∷ ByteArray.ByteString → [Maybe (IPv4, Word16)]
parseIPv4AddressAndPortList = fix \recurse byteArray →
  if
    | ByteArray.length byteArray ≥ 6 →
        let
          (addressAndPort, remainder) = ByteArray.splitAt 6 byteArray
          (address, port) = ByteArray.splitAt 4 addressAndPort
        in
          bisequence (parseIPv4Address address, (Just ∘ parsePort) port) : recurse remainder
    | otherwise → []

parseIPv6AddressAndPortList ∷ ByteArray.ByteString → [Maybe (Either IPv4 IPv6, Word16)]
parseIPv6AddressAndPortList = fix \recurse byteArray →
  if
    | ByteArray.length byteArray ≥ 18 →
        let
          (addressAndPort, remainder) = ByteArray.splitAt 18 byteArray
          (address, port) = ByteArray.splitAt 16 addressAndPort
        in
          bisequence (parseIPAddress address, (Just ∘ parsePort) port) : recurse remainder
    | otherwise → []

parsePort ∷ ByteArray.ByteString → Word16
parsePort = fromIntegral ∘ naturalFromBytesMostSignificantFirst ∘ ByteArray.unpack

parseIPv4Address ∷ ByteArray.ByteString → Maybe IPv4
parseIPv4Address byteArray
  | ByteArray.length byteArray ≡ 4 = Just do (IPv4 ∘ fromIntegral ∘ naturalFromBytesMostSignificantFirst ∘ ByteArray.unpack) byteArray
  | otherwise = Nothing

parseIPv6Address ∷ ByteArray.ByteString → Maybe IPv6
parseIPv6Address byteArray
  | ByteArray.length byteArray ≡ 16 = Just do (IPv6 ∘ fromIntegral ∘ naturalFromBytesMostSignificantFirst ∘ ByteArray.unpack) byteArray
  | otherwise = Nothing

parseIPAddress ∷ ByteArray.ByteString → Maybe (Either IPv4 IPv6)
parseIPAddress byteArray
  | ByteArray.length byteArray ≡ 16 =
      if
        | True
            ∧ ByteArray.take 10 byteArray ≡ ByteArray.replicate 10 0
            ∧ (ByteArray.drop 10 ∘ ByteArray.take 12) byteArray ≡ ByteArray.replicate 2 255 →
            fmap Left do parseIPv4Address (ByteArray.drop 12 byteArray)
        | otherwise → fmap Right do parseIPv6Address byteArray
  | otherwise = Nothing

rightPad ∷ Word → Text → Text
rightPad desiredLength inputText
  | actualLength > desiredLength = inputText
  | otherwise = inputText <> textReplicate (desiredLength − actualLength) " "
 where
  actualLength = textLength inputText ∷ Word

textLength ∷ (Num number) ⇒ Text → number
textLength = fromIntegral ∘ Text.length

textReplicate ∷ (Integral integral) ⇒ integral → Text → Text
textReplicate = Text.replicate ∘ fromIntegral

bytesTake ∷ (Integral integral) ⇒ integral → Bytes.ByteString → Bytes.ByteString
bytesTake = Bytes.take ∘ fromIntegral

outOfMaybe ∷ Maybe input → output → (input → output) → output
outOfMaybe = (flip ∘ fmap flip) maybe

showTruncatedBytes ∷ Word → Bytes.ByteString → Text
showTruncatedBytes desiredLength bytes =
  let truncatedBytes = bytesTake desiredLength bytes
  in  (Text.decodeUtf8Lenient ∘ Bytes.toStrict) truncatedBytes <> if truncatedBytes ≡ bytes then Text.empty else "… (and so on)"
