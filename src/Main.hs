{-# LANGUAGE
OverloadedStrings,
QuasiQuotes,
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
import Control.Monad.Random
-- Text
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import NeatInterpolation
-- Web
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Web.Spock.Lucid
import Lucid hiding (for_)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
-- acid-state
import Data.Acid as Acid
-- IO
import System.IO
import System.Directory
-- Passwords
import Crypto.Scrypt


-- Local
import Types
import Utils
import qualified JS
import JS (JS(..), allJSFunctions)


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
  do b <- doesDirectoryExist "state"
     when b $ removeDirectoryRecursive "state"
  let prepare = openLocalStateFrom "state/" sampleState
      finalise db = do
        createCheckpoint' db
        closeAcidState db
  bracket prepare finalise $ \db -> do
    let serverState = ServerState {
          _db = db }
    let spockConfig = defaultSpockCfg Nothing PCNoDatabase serverState
    runSpock 7070 $ spock spockConfig $ do
      middleware (staticPolicy (addBase "static"))
      Spock.get "/js.js" $ do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        Spock.bytes $ T.encodeUtf8 (fromJS allJSFunctions)
      Spock.get root $ do
        s <- dbQuery GetGlobalState
        sess <- readSession
        lucidIO $ wrapPage sess s "Hat" $ do
          h1_ "Hat"
          h2_ "Available games"
          ul_ $ for_ (s ^. games) $ \game ->
            li_ $ mkLink (toHtml (game^.title))
                         ("/game/" <> uidToText (game^.uid))
      Spock.get ("game" <//> var) $ \gameId -> do
        s <- dbQuery GetGlobalState
        sess <- readSession
        game <- dbQuery (GetGame gameId)
        creator <- dbQuery (GetUser (game^.createdBy))
        lucidIO $ wrapPage sess s ((game^.title) <> " | Hat") $ do
          h1_ (toHtml (game^.title))
          when (game^.ended) $ do
            p_ $ strong_ "This game already ended."
          ul_ $ do
            li_ $ do "game begins at "
                     toHtml (show (game^.begins))
            li_ $ do "created by "
                     mkLink (toHtml (creator^.name))
                            ("/user/" <> creator^.nick)
      Spock.get ("user" <//> var) $ \nick' -> do
        s <- dbQuery GetGlobalState
        sess <- readSession
        user <- dbQuery (GetUserByNick nick')
        lucidIO $ wrapPage sess s ((user^.name) <> " | Hat") $ do
          h1_ $ toHtml $ user^.name <> " (aka " <> user^.nick <> ")"
      Spock.get "admin" $ do
        s <- dbQuery GetGlobalState
        sess <- readSession
        lucidIO $ wrapPage sess s "Admin | Hat" $ do
          h1_ "Admin stuff"
          h2_ "List of users"
          ul_ $ for_ (s^.users) $ \user -> li_ $ do
            mkLink (toHtml (user^.name))
                   ("/user/" <> user^.nick)
      Spock.get "login" $ do
        s <- dbQuery GetGlobalState
        sess <- readSession
        lucidIO $ wrapPage sess s "Login | Hat" $ do
          errorId <- randomLongUid
          let formSubmitHandler formNode =
                JS.tryLogin (JS.selectUid errorId, formNode)
          form_ [onFormSubmit formSubmitHandler] $ do
            input_ [type_ "text", name_ "nick", placeholder_ "Username"]
            input_ [type_ "password", name_ "pass", placeholder_ "Password"]
            input_ [type_ "submit", value_ "Log in"]
          div_ [uid_ errorId, style_ "display:none"] ""
      Spock.post "login" $ do
        nick' <- param' "nick"
        pass' <- Pass . T.encodeUtf8 <$> param' "pass"
        mbUser <- dbQuery (GetUserByNick' nick')
        case mbUser of
          Nothing   -> json (False, "User not found" :: Text)
          Just user -> case verifyPass' pass' (user^.pass) of
            False -> json (False, "Incorrect password" :: Text)
            True  -> do writeSession (Just (user^.uid))
                        json (True, "" :: Text)
      Spock.post "logout" $ do
        writeSession Nothing

wrapPage
  :: (MonadIO m, MonadRandom m)
  => Maybe (Uid User)
  -> GlobalState
  -> Text                              -- ^ Page title
  -> HtmlT m ()
  -> HtmlT m ()
wrapPage sess gs pageTitle page = doctypehtml_ $ do
  head_ $ do
    title_ (toHtml pageTitle)
    meta_ [name_ "viewport",
           content_ "width=device-width, initial-scale=1.0, user-scalable=yes"]
    -- Report all Javascript errors with alerts
    script_ [text|
      window.onerror = function (msg, url, lineNo, columnNo, error) {
        alert("Error in "+url+" at "+lineNo+":"+columnNo+": "+msg+
              "\n\n"+
              "========== Please report it! =========="+
              "\n\n"+
              "https://github.com/neongreen/hat/issues");
        return false; };
      |]
    includeJS "/jquery-2.2.0.min.js"
    -- See Note [autosize]
    includeJS "/autosize-3.0.15.min.js"
    onPageLoad (JS "autosize($('textarea'));")
    includeCSS "/normalize.css"
    includeCSS "/milligram.min.css"
    includeCSS "/css.css"
    includeCSS "/loader.css"
    -- Include definitions of all Javascript functions that we have defined
    -- in this file. (This isn't an actual file, so don't look for it in the
    -- static folder.)
    includeJS "/js.js"

  body_ $ do
    script_ $ fromJS $ JS.createAjaxIndicator ()
    div_ [id_ "main"] $ do
      case sess of
        Nothing -> do
          mkLink "Log in" "/login"
        Just u  -> do
          let nick' = gs ^. userById u . nick
          mkLink (toHtml nick') ("/user/" <> nick')
          a_ [href_ "#", onclick_ (fromJS (JS.logout ()) <> "return false;")]
            "Log out"
      page
    div_ [id_ "footer"] $ do
      mapM_ (div_ [class_ "footer-item"]) $
        [ do "made by "
             mkLink "Artyom" "https://artyom.me"
        , do mkLink "source" "https://github.com/neongreen/hat"
             "/"
             mkLink "issue tracker" "https://github.com/neongreen/hat/issues"
        ]

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ format "{} return false;" [f (JS "this")]

onPageLoad :: Monad m => JS -> HtmlT m ()
onPageLoad js = script_ $ format "$(document).ready(function(){{}});" [js]

mkLink :: Monad m => HtmlT m a -> Url -> HtmlT m a
mkLink x src = a_ [href_ src] x
