module Main where

import Prelude.Unicode

import Library

import Control.Applicative
import Control.Exception
import Data.ByteString qualified as ByteArray
import Data.ByteString.Lazy qualified as Bytes
import Data.Function
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Types
import Numeric.Natural
import Options.Generic
import Text.Read

data Options = Options
  { tracker ∷ Text
  , info_hash ∷ Text
  }
  deriving (Generic)

instance ParseRecord Options

main ∷ IO ()
main = do
  Options {..} ← getRecord @IO @Options "trickle"
  manager ← newManager defaultManagerSettings
  infoHashNumber ← maybe (throwIO @SomeException undefined) pure do
    Nothing
      <|> (readMaybe @Natural ∘ Text.unpack) info_hash
      <|> (readMaybe @Natural ∘ Text.unpack) ("0x" <> info_hash)
  let infoHashByteArray = (ByteArray.pack ∘ bytesOfNaturalMostSignificantFirst) infoHashNumber
  let query = [(Text.encodeUtf8 "info_hash", Just infoHashByteArray)]
  defaultRequestWithHost ← (parseRequest ∘ Text.unpack) tracker
  let request =
        defaultRequestWithHost
          { queryString = renderQuery True query
          , path = "/announce"
          }
          ∷ Request

  print infoHashNumber
  print request
  parseRequest "http://[200:1e2f:e608:eb3a:2bf:1e62:87ba:e2f7]/announce?info_hash=C%24%F09Zx%3C%5C%1Cb%81%3B%04%AF%15%DB%7FUp~"
  response ← httpLbs request manager
  print response
