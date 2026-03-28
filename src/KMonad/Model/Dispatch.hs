{-
Module      : KMonad.Model.Dispatch
Description : Component for async reading.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

The 'Dispatch' component of the app-loop solves the following problem: we might
at some point during execution be in the following situation:
- We have set our processing to held
- There is a timer running that might unhold at any point
- We are awaiting a key from the OS

This means we need to be able to:
1. Await events from some kind of rerun buffer
2. Await events from the OS
3. Await timer signals
4. Do all of these things without ever entering a race-condition where we lose
   an event because multiple things happen at exactly the same time.

The Dispatch component provides the ability to read events from some IO action
while at the same time providing a method to write events into the Dispatch,
sending them to the head of the read-queue, while guaranteeing that no events
ever get lost.

In the sequencing of components, the 'Dispatch' occurs first, which means that
it reads directly from the KeySource. Any component after the 'Dispatch' need
not worry about whether an event is being rerun or not, it simply treats all
events as equal.

-}
module KMonad.Model.Dispatch
  ( -- $env
    Dispatch
  , mkDispatch

    -- $op
  , pull
  , rerun
  )
where

import KMonad.Prelude
import KMonad.Keyboard
import KMonad.Model.Action (WrappedEvent(..))
import KMonad.Model.EventSrc

import RIO.Seq (Seq(..), (><))
import qualified RIO.Seq  as Seq
import qualified RIO.Text as T

import Data.Unique

--------------------------------------------------------------------------------
-- $env

-- | The 'Dispatch' environment
data Dispatch = Dispatch
  { eventSrc  :: EventSrc KeyEvent IO     -- ^ How to read 1 key event from the OS
  , _rerunBuf :: TVar (Seq WrappedEvent)  -- ^ Buffer for rerunning wrapped events
  , _injectTmr :: TMVar Unique            -- ^ Shared timer signal channel
  }
makeLenses ''Dispatch

-- | Create a new 'Dispatch' environment
mkDispatch' :: MonadUnliftIO m => TMVar Unique -> EventSrc KeyEvent m -> m Dispatch
mkDispatch' tmr s = withRunInIO $ \u -> do
  rrb <- newTVarIO Seq.empty
  pure $ Dispatch (unliftESrc u s) rrb tmr

-- | Create a new 'Dispatch' environment in a 'ContT' environment
mkDispatch :: MonadUnliftIO m => TMVar Unique -> EventSrc KeyEvent m -> ContT r m Dispatch
mkDispatch tmr = lift . mkDispatch' tmr

--------------------------------------------------------------------------------
-- $op

-- | Return the next event. Precedence:
-- 1. Timer signals (WrappedTag)
-- 2. Items from the rerun buffer (WrappedEvent)
-- 3. New items from the OS (wrapped as WrappedKeyEvent)
pull :: (HasLogFunc e) => Dispatch -> EventSrc WrappedEvent (RIO e)
pull d@Dispatch{eventSrc = EventSrc{tryESrc, postESrc}} = EventSrc
  { tryESrc =   (Left . Left  <$> takeTMVar (d^.injectTmr))
        `orElse` (Left . Right <$> popRerun)
        `orElse` (Right        <$> tryESrc)
  , postESrc = \case
    -- Timer signal: wrap as WrappedTag
    Left (Left tag) -> do
      logDebug $ "\n" <> display (T.replicate 80 "-")
              <> "\nTimer signal: " <> display (hashUnique tag)
      pure $ Just (WrappedTag tag)

    -- Rerun event: already wrapped
    Left (Right we) -> do
      case we of
        WrappedKeyEvent e ->
          logDebug $ "\n" <> display (T.replicate 80 "-")
                  <> "\nRerunning event: " <> display e
        WrappedTag tag ->
          logDebug $ "\n" <> display (T.replicate 80 "-")
                  <> "\nRerunning timer: " <> display (hashUnique tag)
      pure $ Just we

    -- OS event: wrap as WrappedKeyEvent
    Right e -> liftIO (postESrc e) >>= \case
      Nothing -> pure Nothing
      Just ke -> pure $ Just (WrappedKeyEvent ke)
  }
  where
    popRerun = readTVar (d^.rerunBuf) >>= \case
      Seq.Empty -> retrySTM
      (e :<| b) -> do
        writeTVar (d^.rerunBuf) b
        pure e

-- | Add a list of wrapped events to be rerun.
rerun :: (HasLogFunc e) => Dispatch -> [WrappedEvent] -> RIO e ()
rerun d es = atomically $ modifyTVar (d^.rerunBuf) (>< Seq.fromList es)
