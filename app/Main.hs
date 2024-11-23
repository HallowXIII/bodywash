{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Bodywash.Config
import Bodywash.State
import Control.Lens.Operators ((%=), (.=))
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Data.Foldable (Foldable (toList))
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Text.IO qualified as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Discord (DiscordHandler, RestCallErrorCode, def, restCall)
import Discord.Requests (ChannelRequest (..))
import Discord.Types (ChannelId, Event (..), Message (..), MessageId)
import FRP.Rhine
import FRP.Rhine.Discord
import Prelude
import Control.Monad (unless)

handleEventsSF :: ClSF DiscordHandler DiscordEventClock State State
handleEventsSF = proc st -> do
  time <- absoluteS -< ()
  event <- tagS -< ()
  arrMCl (uncurry handleEvent) -< ((time, event), st)

handleEvent :: (Time DiscordEventClock, Tag DiscordEventClock) -> State -> DiscordHandler State
handleEvent (_, event) = State.execStateT do
  st <- State.get
  case event of
    MessageCreate message | message.messageChannelId == st.channelId -> addMessage message
    MessageDelete channelId messageId | channelId == st.channelId -> removeMessages [messageId]
    MessageDeleteBulk channelId messageIds | channelId == st.channelId -> removeMessages messageIds
    _ -> pure ()
  where
    addMessage :: Message -> StateT State DiscordHandler ()
    addMessage message = #messages %= (message :<|)
    removeMessages :: [MessageId] -> StateT State DiscordHandler ()
    removeMessages messageIds = #messages %= Seq.filter \msg -> messageId msg `notElem` messageIds

handleLogSF :: ClSF DiscordHandler DiscordLogClock State State
handleLogSF = returnA

type DiscordMillisecond n = HoistClock IO DiscordHandler (Millisecond n)

simRh :: Rhine DiscordHandler (DiscordMillisecond 1000) State State
simRh = simSF @@ ioClock waitClock

simSF :: (Diff (Time cl) ~ Double) => ClSF DiscordHandler cl State State
simSF = proc model -> do
  dt <- sinceLastS -< ()
  arrMCl (uncurry sim) -< (dt, model)

secondsSinceTimestamp :: UTCTime -> UTCTime -> Int
secondsSinceTimestamp now utct = floor . nominalDiffTimeToSeconds $ diffUTCTime now utct

msgInfo :: UTCTime -> Message -> (Int, MessageId)
msgInfo now msg =
  (secondsSinceTimestamp now $ messageTimestamp msg, messageId msg)

deleteMessageCall :: ChannelId -> [MessageId] -> DiscordHandler (Either RestCallErrorCode ())
deleteMessageCall _ [] = pure $ Right ()
deleteMessageCall chanId [msgId] = restCall $ DeleteMessage (chanId, msgId)
deleteMessageCall chanId msgs = restCall $ BulkDeleteMessage (chanId, msgs)

sim :: Double -> State -> DiscordHandler State
sim _ = State.execStateT do
  State {..} <- State.get
  now <- liftIO getCurrentTime
  let (aliveMessages, decayedMessages) = Seq.splitAt maxAllowedMessages messages
      (decayedMessages', aliveMessages') = Seq.partition (\msg -> secondsSinceTimestamp now (messageTimestamp msg) > maxMessageLifetime) aliveMessages
      finalDecayedMessages = decayedMessages >< decayedMessages'
  liftIO $ unless (null finalDecayedMessages) do
    putStrLn ("DecayedMessages: " ++ show (fmap (msgInfo now) finalDecayedMessages))
    putStrLn ("AliveMessages: " ++ show (fmap (msgInfo now) aliveMessages'))
  #messages .= aliveMessages
  void . State.lift . deleteMessageCall channelId $ toList (fmap messageId finalDecayedMessages)

main :: IO ()
main = do
  config <- getConfig
  Text.putStrLn =<< flowDiscord config.discordToken def (getInitialState config) handleEventsSF handleLogSF simRh
