{-# LANGUAGE
OverloadedStrings,
TypeFamilies,
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude
-- Lenses
import Lens.Micro.Platform
-- Monads
import Control.Monad.IO.Class
-- Web
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Web.Spock.Lucid
import Lucid hiding (for_)
-- acid-state
import Data.Acid as Acid
-- IO
import System.IO

-- Local
import Types


data ServerState = ServerState {
  _db :: DB }

type DB = AcidState GlobalState

-- | Update something in the database.
dbUpdate :: (MonadIO m, HasSpock m, SpockState m ~ ServerState,
             EventState event ~ GlobalState, UpdateEvent event)
         => event -> m (EventResult event)
dbUpdate x = do
  db <- _db <$> Spock.getState
  liftIO $ do
    Acid.update db SetDirty
    Acid.update db x

-- | Read something from the database.
dbQuery :: (MonadIO m, HasSpock m, SpockState m ~ ServerState,
            EventState event ~ GlobalState, QueryEvent event)
        => event -> m (EventResult event)
dbQuery x = do
  db <- _db <$> Spock.getState
  liftIO $ Acid.query db x

-- | Like 'createCheckpoint', but doesn't create a checkpoint if there were
-- no changes made.
createCheckpoint' :: MonadIO m => DB -> m ()
createCheckpoint' db = liftIO $ do
  wasDirty <- Acid.update db UnsetDirty
  when wasDirty $ do
    createArchive db
    createCheckpoint db

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let emptyState = GlobalState {
        _users = [],
        _games = [],
        _dirty = True }
  let prepare = openLocalStateFrom "state/" sampleState
      finalise db = do
        createCheckpoint' db
        closeAcidState db
  bracket prepare finalise $ \db -> do
    let serverState = ServerState {
          _db = db }
    let spockConfig = defaultSpockCfg () PCNoDatabase serverState
    runSpock 7070 $ spock spockConfig $ do
      Spock.get root $ do
        s <- dbQuery GetGlobalState
        lucid $ do
          h1_ "Hat"
          h2_ "Available games"
          ul_ $ for_ (s ^. games) $ \game ->
            li_ $ mkLink (toHtml (game^.title))
                         ("game/" <> uidToText (game^.uid))
      Spock.get ("game" <//> var) $ \gameId -> do
        game <- dbQuery (GetGame gameId)
        lucid $ do
          h1_ (toHtml (game^.title))
          p_ $ do
            toHtml $ "game begins: " ++ show (game^.begins)

mkLink :: Monad m => HtmlT m a -> Url -> HtmlT m a
mkLink x src = a_ [href_ src] x
