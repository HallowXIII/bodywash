{-# OPTIONS_GHC -Wno-orphans #-}

module Bodywash.State where

import Bodywash.Config
import Data.Either (fromRight)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Discord (DiscordHandler, restCall)
import Discord.Requests (ChannelRequest (GetChannelMessages), MessageTiming (BeforeMessage, LatestMessages))
import Discord.Types
    ( ChannelId
    , DiscordId (DiscordId)
    , Message (messageId, messageTimestamp)
    , Snowflake (Snowflake)
    )
import GHC.Generics (Generic)
import System.Random (RandomGen (..), StdGen, getStdGen)
import Prelude

deriving newtype instance Num Snowflake

deriving newtype instance Num (DiscordId a)

data State = State
    { stdGen :: StdGen
    , channelId :: ChannelId
    , messages :: Seq Message
    , maxAllowedMessages :: Int
    , maxMessageLifetime :: Int
    }
    deriving stock (Generic)

liftRandomGen :: (StdGen -> (b, StdGen)) -> State -> (b, State)
liftRandomGen f st = let (a, g) = f st.stdGen in (a, st{stdGen = g})

instance RandomGen State where
    genWord8 = liftRandomGen genWord8
    genWord16 = liftRandomGen genWord16
    genWord32 = liftRandomGen genWord32
    genWord64 = liftRandomGen genWord64
    genWord32R = liftRandomGen . genWord32R
    genWord64R = liftRandomGen . genWord64R
    genShortByteString = liftRandomGen . genShortByteString
    split st = let (g1, g2) = split st.stdGen in (st{stdGen = g1}, st{stdGen = g2})

getAllMessages :: ChannelId -> MessageTiming -> DiscordHandler [Message]
getAllMessages channelId timing = do
    messages <- fromRight [] <$> restCall (GetChannelMessages channelId (100, timing))
    remainingMessages <-
        if null messages
            then pure []
            else getAllMessages channelId . BeforeMessage . messageId $ last messages
    pure $ messages <> remainingMessages

getInitialState :: Config -> DiscordHandler State
getInitialState Config{..} = do
    stdGen <- getStdGen
    messages' <- Seq.fromList <$> getAllMessages (fromIntegral channelId) LatestMessages
    pure
        State
            { stdGen
            , channelId = DiscordId . Snowflake $ channelId
            , messages = Seq.reverse $ Seq.sortOn messageTimestamp messages'
            , maxAllowedMessages = fromIntegral maxAllowedMessages
            , maxMessageLifetime = fromIntegral maxMessageLifetime
            }
