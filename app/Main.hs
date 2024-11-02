{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Bodywhash.Config
import Bodywhash.State
import Control.Lens.Operators
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Data.Foldable (Foldable (toList))
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Random (sampleState)
import Data.Random.Distribution.Binomial
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text.IO qualified as Text
import Data.Tuple qualified as Tuple
import Discord (DiscordHandler, def, restCall)
import Discord.Requests (ChannelRequest (BulkDeleteMessage))
import Discord.Types (Event (..), Message (..), MessageId)
import FRP.Rhine
import FRP.Rhine.Discord
import Prelude

handleEventsSF :: ClSF DiscordHandler DiscordEventClock State State
handleEventsSF = proc st -> do
    time <- absoluteS -< ()
    event <- tagS -< ()
    arrMCl (uncurry handleEvent) -< ((time, event), st)

handleEvent :: (Time DiscordEventClock, Tag DiscordEventClock) -> State -> DiscordHandler State
handleEvent (_, event) = State.execStateT do
    st <- State.get
    case event of
        MessageCreate message | message.messageChannelId == st.channelId -> addMessage message.messageId
        MessageDelete channelId messageId | channelId == st.channelId -> removeMessages [messageId]
        MessageDeleteBulk channelId messageIds | channelId == st.channelId -> removeMessages messageIds
        _ -> pure ()
  where
    addMessage :: MessageId -> StateT State DiscordHandler ()
    addMessage message = #messages %= (message :<|)
    removeMessages :: [MessageId] -> StateT State DiscordHandler ()
    removeMessages messageIds = #messages %= Seq.filter (not . (`elem` messageIds))

handleLogSF :: ClSF DiscordHandler DiscordLogClock State State
handleLogSF = returnA

type DiscordMillisecond n = HoistClock IO DiscordHandler (Millisecond n)

simRh :: Rhine DiscordHandler (DiscordMillisecond 1000) State State
simRh = simSF @@ ioClock waitClock

simSF :: (Diff (Time cl) ~ Double) => ClSF DiscordHandler cl State State
simSF = proc model -> do
    dt <- sinceLastS -< ()
    arrMCl (uncurry sim) -< (dt, model)

sim :: Double -> State -> DiscordHandler State
sim dt = State.execStateT do
    State{..} <- State.get
    let k = messageHalfLifeSeconds / dt
        n = length messages
        p = 2 ** (-1 / k)
        q = 1 - p
    dn <- sampleState $ Binomial n q
    (decayedMessages, aliveMessages) <- pure . Tuple.swap $ Seq.splitAt dn messages
    #messages .= aliveMessages
    void . State.lift . restCall $ BulkDeleteMessage (channelId, toList decayedMessages)

main :: IO ()
main = do
    config <- getConfig
    Text.putStrLn =<< flowDiscord config.discordToken def (getInitialState config) handleEventsSF handleLogSF simRh
