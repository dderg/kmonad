{-|
Module      : KMonad.Model.Sluice
Description : The component that provides pausing functionality
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

For certain KMonad operations we need to be able to pause and resume processing
of events. This component provides the ability to temporarily pause processing,
and then resume processing and return all events that were caught while paused.

-}
module KMonad.Model.Sluice
  ( Sluice
  , mkSluice
  , block
  , unblock
  , pull
  )
where

import KMonad.Prelude

import KMonad.Model.Action (WrappedEvent(..))
import KMonad.Model.EventSrc

--------------------------------------------------------------------------------
-- $env

-- | The 'Sluice' environment.
--
-- NOTE: 'Sluice' has no internal multithreading, i.e. its 'pull' action will
-- never be interrupted, therefore we can simply use 'IORef' and sidestep all
-- the STM complications.
data Sluice = Sluice
  { eventSrc  :: EventSrc WrappedEvent IO -- ^ Where we get our 'WrappedEvent's from
  , _blocked  :: IORef Int                -- ^ How many locks have been applied to the sluice
  , _blockBuf :: IORef [WrappedEvent]     -- ^ Internal buffer to store events while closed
  }
makeLenses ''Sluice

-- | Create a new 'Sluice' environment
mkSluice' :: MonadUnliftIO m => EventSrc WrappedEvent m -> m Sluice
mkSluice' s = withRunInIO $ \u -> do
  bld <- newIORef 0
  buf <- newIORef []
  pure $ Sluice (unliftESrc u s) bld buf

-- | Create a new 'Sluice' environment, but do so in a ContT context
mkSluice :: MonadUnliftIO m => EventSrc WrappedEvent m -> ContT r m Sluice
mkSluice = lift . mkSluice'


--------------------------------------------------------------------------------
-- $op

-- | Increase the block-count by 1
block :: HasLogFunc e => Sluice -> RIO e ()
block s = do
  modifyIORef (s^.blocked) (+1)
  readIORef (s^.blocked) >>= \n ->
    logDebug $ "Block level set to: " <> display n

-- | Set the Sluice to unblocked mode, return a list of all the stored events
-- that should be rerun, in the correct order (head was first-in, etc).
unblock :: HasLogFunc e => Sluice -> RIO e [WrappedEvent]
unblock s = do
  modifyIORef' (s^.blocked) (\n -> n - 1)
  readIORef (s^.blocked) >>= \case
    0 -> do
      es <- readIORef (s^.blockBuf)
      writeIORef (s^.blockBuf) []
      logDebug $ "Unblocking input stream, " <>
        if null es
        then "no stored events"
        else "rerunning " <> display (length es) <> " stored events"
      pure $ reverse es
    n -> do
      logDebug $ "Block level set to: " <> display n
      pure []


--------------------------------------------------------------------------------
-- $loop

-- | Try to read from the Sluice, if we are blocked, store the event internally
-- and return Nothing. If we are unblocked, return Just the WrappedEvent.
pull :: HasLogFunc e => Sluice -> EventSrc WrappedEvent (RIO e)
pull s@Sluice{eventSrc = EventSrc{tryESrc, postESrc}} = EventSrc
  { tryESrc = tryESrc
  , postESrc = liftIO . postESrc >=> maybe (pure Nothing) step
  }
 where
  step we = do
    readIORef (s^.blocked) >>= \case
      0 -> pure $ Just we
      _ -> do
        modifyIORef' (s^.blockBuf) (we:)
        readIORef (s^.blockBuf) >>= \es ->
          logDebug $ "Storing event, buffer size: " <> display (length es)
        pure Nothing
