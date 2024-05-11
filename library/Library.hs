module Library where

import Prelude.Unicode

import Control.Applicative
import Data.ByteString qualified as ByteArray
import Data.ByteString.Lazy qualified as Bytes
import Data.Function
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Types
import Numeric.Natural
import Options.Generic
import Text.Read

bytesOfNaturalLeastSignificantFirst ∷ Natural → [Word8]
bytesOfNaturalLeastSignificantFirst = \case
  0 → [0]
  n → flip fix n \recurse → \case
    0 → []
    natural → let (quotient, remainder) = natural `divMod` 256 in fromIntegral remainder : recurse quotient

caching ∷ (α → β) → ((β, α) → γ) → α → γ
caching cache function input = function (cache input, input)

bytesOfNaturalMostSignificantFirst ∷ Natural → [Word8]
bytesOfNaturalMostSignificantFirst = caching ((+ 1) ∘ floor ∘ logBase 256 ∘ fromIntegral) work
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
