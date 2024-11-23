module Bodywash.Config where

import Data.Text (Text)
import Data.Word (Word64)
import Dhall (FromDhall, Generic, ToDhall, Natural)
import Dhall qualified
import Prelude

data Config = Config
    { discordToken :: Text
    , channelId :: Word64
    , maxAllowedMessages :: Natural
    , maxMessageLifetime :: Natural
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromDhall, ToDhall)

getConfig :: IO Config
getConfig = Dhall.inputFile (Dhall.auto @Config) "/etc/bodywash/config.dhall"
