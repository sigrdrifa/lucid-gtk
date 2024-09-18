{- | Represents GUI application state that is passed around
between functions.
-}
module AppState (
  AppState (..),
  AppStateLoaded (..),
  newAppState,
) where

import Control.Concurrent (MVar, newEmptyMVar)

data AppStateLoaded where
  AppStateLoaded :: {loadedSuccess :: Bool} -> AppStateLoaded

data AppState where
  AppState :: {loaded :: MVar AppStateLoaded} -> AppState

-- | Creates a new empty appState
newAppState :: IO AppState
newAppState = do
  AppState <$> newEmptyMVar
