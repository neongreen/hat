{-# LANGUAGE
OverloadedStrings,
QuasiQuotes,
TypeFamilies,
TupleSections,
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Monads
import Control.Monad.IO.Class
import Control.Monad.Random
-- Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import NeatInterpolation
-- Lists
import Data.List.Index
-- Vector
import qualified Data.Vector.Unboxed as U
-- Containers
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
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
          for_ sess $ \u -> do
            case (S.member u (game^.players), game^.wordReq, game^.ended) of
              -- the game is on but the user isn't participating
              (False, _, False) ->
                p_ "You aren't participating in this game yet."
              -- the game has ended
              (False, _, True) ->
                p_ "You didn't participate in this game."
              -- the game is on, the user doesn't have to propose anything
              (_, Nothing, False) ->
                p_ "You don't have to propose words for this game."
              -- the game has ended, the user didn't have to propose anything
              (_, Nothing, True) ->
                return ()
              -- the game is on, the user has to propose words
              (_, Just req, False) -> do
                case req^.userWords.at u of
                  Nothing -> p_ "You can propose words for this game."
                  Just ws -> do
                    p_ "Your proposed words for the game are:"
                    for_ ws $ \w -> li_ (toHtml w)
              -- the game has ended, the user had to propose words
              (_, Just req, True) -> do
                case req^.userWords.at u of
                  Nothing -> p_ "You didn't propose words for this game."
                  Just ws -> do
                    p_ "Your proposed words for the game were:"
                    for_ ws $ \w -> li_ (toHtml w)
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
    includeJS "/jquery.js"
    -- See Note [autosize]
    includeJS "/autosize.js"
    onPageLoad (JS "autosize($('textarea'));")
    includeCSS "/normalize.css"
    includeCSS "/milligram.css"
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
        , do mkLink "DDRaniki 2016" "https://vk.com/ddraniki"
        , do mkLink "source" "https://github.com/neongreen/hat"
             " / "
             mkLink "issue tracker" "https://github.com/neongreen/hat/issues"
        ]

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ format "{} return false;" [f (JS "this")]

onPageLoad :: Monad m => JS -> HtmlT m ()
onPageLoad js = script_ $ format "$(document).ready(function(){{}});" [js]

mkLink :: Monad m => HtmlT m a -> Url -> HtmlT m a
mkLink x src = a_ [href_ src] x

rateSolution
  :: Int                  -- ^ Player count
  -> U.Vector (Int, Int)  -- ^ Past games
  -> U.Vector (Int, Int)  -- ^ Solution
  -> [(Int, Double)]
rateSolution pc past future =
  sort . map rate . IM.elems $
  U.ifoldl (\mp turn (a, b) -> mp & ix a %~ (turn:) & ix b %~ (turn:))
           (IM.fromList (map (, []) [0..pc-1]))
           (past <> future)
  where
    rate ls =
      let ds = zipWith (-) ls (tail ls)
      in if null ds
           then (0, 0)
           else (minimum ds, fromIntegral (sum ds) / fromIntegral (length ds))

printRating :: [(Int, Double)] -> IO ()
printRating xs = mapM_ (\(a,s) -> printf "%d/%.2f " a s) xs >> putStrLn ""

-- https://en.wikipedia.org/wiki/Simulated_annealing
findSchedule
  :: Int                          -- ^ Player count
  -> U.Vector (Int, Int)          -- ^ Past games
  -> Int                          -- ^ Iterations
  -> IO (U.Vector (Int, Int))     -- ^ Solution
findSchedule pc past kmax =
  if U.null future then return past
    else go (future, rfuture) (future, rfuture) kmax
  where
    future = U.fromList $
      [(x, y) | x <- [0..pc-1], y <- [0..pc-1], x/=y] \\ U.toList past
    rfuture = rateSolution pc past future
    rate :: [(Int, Double)] -> Double
    rate xs = sum (imap (\i (m,s) -> 0.9^^i * fromIntegral m * s) xs)
            * fromIntegral (fst (head xs))^(2::Int)
    p e e' t = if e' > e then 1 else exp (-(rate e - rate e')/t)
    go _       (sbest, _)      0 = return sbest
    go (s, rs) (sbest, rsbest) k = do
      s' <- swap2 s
      let t = 0.9999^^(kmax-k)
          rs' = rateSolution pc past s'
      rnd <- randomIO
      let (sbest', rsbest')
            | rs' > rsbest = (s', rs')
            | otherwise    = (sbest, rsbest)
      if p rs rs' t >= rnd
        then go (s', rs') (sbest', rsbest') (k-1)
        else go (s , rs ) (sbest', rsbest') (k-1)

swap2 :: U.Unbox a => U.Vector a -> IO (U.Vector a)
swap2 xs = do
  let len = U.length xs
  i <- randomRIO (0, len-1)
  j <- randomRIO (0, len-1)
  return (U.unsafeUpd xs [(i, U.unsafeIndex xs j), (j, U.unsafeIndex xs i)])
