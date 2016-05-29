{-# LANGUAGE
OverloadedStrings,
DataKinds,
QuasiQuotes,
TypeFamilies,
TupleSections,
FlexibleContexts,
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
import Data.List.Split (chunksOf)
-- Vector
import qualified Data.Vector.Unboxed as U
-- Randomness
import System.Random.Shuffle
-- Containers
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
-- Web
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Web.Spock.Lucid
import Lucid hiding (for_)
import qualified Lucid
import Network.Wai.Middleware.Static (staticPolicy, addBase)
-- acid-state
import Data.Acid as Acid
-- IO
import System.IO
-- Time
import Data.Time
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

gameVar :: Path '[Uid Game]
gameVar = "game" <//> var

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let prepare = openLocalStateFrom "state/" sampleState
      finalise db = do
        createCheckpoint' db
        closeAcidState db
  bracket prepare finalise $ \db -> do
    let serverState = ServerState {
          _db = db }
    let spockConfig = (defaultSpockCfg Nothing PCNoDatabase serverState) {
          spc_sessionCfg = (defaultSessionCfg Nothing) {
            sc_housekeepingInterval = 10, -- seconds
            sc_sessionTTL = 86400*30,     -- month
            sc_persistCfg = Just SessionPersistCfg {
              spc_load = Acid.query db GetSessions,
              -- this will be called every housekeeping_Interval
              spc_store = \ss -> do
                ssOld <- Acid.query db GetSessions
                when (ss /= ssOld) $ do
                  Acid.update db SetDirty
                  Acid.update db (SetSessions ss) } } }
    runSpock 7070 $ spock spockConfig $ do
      middleware (staticPolicy (addBase "static"))
      Spock.get "/js.js" $ do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        Spock.bytes $ T.encodeUtf8 (fromJS allJSFunctions)
      Spock.get root $ do
        s <- dbQuery GetGlobalState
        sess <- readSession
        lucidIO $ wrapPage sess s "Hat" $ do
          h2_ "Available games"
          ul_ $ for_ (s ^. games) $ \game ->
            li_ $ mkLink (toHtml (game^.title))
                         ("/game/" <> uidToText (game^.uid))
      Spock.post (gameVar <//> "players" <//> "add-self") $ \gameId -> do
        sess <- readSession
        game <- dbQuery (GetGame gameId)
        now <- liftIO getCurrentTime
        u <- case sess of
          Nothing -> jsonFail "You're not logged in"
          Just u  -> return u
        when (u `S.member` (game^.players)) $
          jsonFail "You're already registered for this game"
        when (now > game^.begins) $
          jsonFail "This game has already begun"
        dbUpdate (AddPlayer gameId u)
        jsonSuccess
      Spock.post (gameVar <//> "players" <//> "remove-self") $ \gameId -> do
        sess <- readSession
        game <- dbQuery (GetGame gameId)
        now <- liftIO getCurrentTime
        u <- case sess of
          Nothing -> jsonFail "You're not logged in"
          Just u  -> return u
        when (u `S.notMember` (game^.players)) $
          jsonFail "You aren't registered for this game"
        when (now > game^.begins) $
          jsonFail "The game has already begun, you can't unregister now"
        dbUpdate (RemovePlayer gameId u)
        jsonSuccess
      Spock.get (gameVar <//> "words" <//> "printable") $ \gameId -> do
        game <- dbQuery (GetGame gameId)
        currentAdmin <- isAdmin
        lucidIO $ do
          if not currentAdmin
            then p_ "Only admins can see the words"
            else case game^.wordReq of
              Nothing -> p_ "In this game players don't submit words"
              Just req -> do
                ws <- chunksOf 3 <$>
                  shuffleM (concatMap S.toList (req^..userWords.each))
                table_ $ for_ ws $ \wg -> tr_ $ do
                  for_ wg $ \w -> td_ (toHtml w)
                style_ [text|
                  table {
                    border-collapse: collapse;
                    border-spacing: 0; }
                  td {
                      text-align: center;
                      width: 5.5cm;
                      height: 1cm;
                      max-height: 1cm;
                      border: 1pt dotted #eee;
                      padding: 0.2cm 0.5cm; }
                  |]
      Spock.post (gameVar <//> "words" <//> "submit") $ \gameId -> do
        sess <- readSession
        game <- dbQuery (GetGame gameId)
        ws <- T.words <$> param' "words"
        u <- case sess of
          Just u  -> return u
          Nothing -> jsonFormFail "words" "You're not logged in"
        req <- case game^.wordReq of
          Just req -> return req
          Nothing  -> jsonFormFail "words"
            "Words aren't required for this game"
        when (u `S.notMember` (game^.players)) $
          jsonFormFail "words" "You aren't participating in this game"
        when (isJust (req^.userWords.at u)) $
          jsonFormFail "words" "You have already submitted words"
        when (length (ordNub ws) < length ws) $
          jsonFormFail "words" "The list contains duplicates"
        when (any (T.any (== ',')) ws) $
          jsonFormFail "words" "Punctuation isn't allowed"
        when (null ws) $
          jsonFormFail "words" "You haven't entered any words"
        when (length ws < req^.wordsPerUser) $
          jsonFormFail "words" $
            format "You entered {} word{} out of {}"
              (length ws, if length ws == 1 then "" else "s" :: Text,
               req^.wordsPerUser)
        when (length ws > req^.wordsPerUser) $
          jsonFormFail "words" $
            format "You entered {} words, that's too many"
              [length ws]
        -- All checks passed
        dbUpdate (SetWords gameId u ws)
        jsonSuccess
      Spock.get (gameVar) $ \gameId ->
        gamePage gameId
      Spock.get ("user" <//> var) $ \nick' -> do
        s <- dbQuery GetGlobalState
        sess <- readSession
        user <- dbQuery (GetUserByNick nick')
        currentAdmin <- isAdmin
        lucidIO $ wrapPage sess s ((user^.name) <> " | Hat") $ do
          h2_ $ toHtml $ user^.name <> " (aka " <> user^.nick <> ")"
          when (currentAdmin && not (user^.admin)) $
            button "Make admin" $
              JS.makeAdmin [nick']
          when (user^.admin) $
            p_ "One of the admins."
      Spock.post ("user" <//> var <//> "make-admin") $ \nick' -> do
        currentAdmin <- isAdmin
        when currentAdmin $ do
          user <- dbQuery (GetUserByNick nick')
          dbUpdate (SetAdmin (user^.uid) True)
      Spock.get "admin" $ do
        s <- dbQuery GetGlobalState
        sess <- readSession
        currentAdmin <- isAdmin
        lucidIO $ wrapPage sess s "Admin | Hat" $ do
          h2_ "Admin stuff"
          if not currentAdmin then
            p_ "You're not an admin."
          else do
            let (admins, ordinaryUsers) = partition (view admin) (s^.users)
            h3_ "Admins"
            p_ $ sequence_ $ intersperse ", " $ map userLink admins
            h3_ "Users"
            p_ $ sequence_ $ intersperse ", " $ map userLink ordinaryUsers
      Spock.get "login" $ do
        s <- dbQuery GetGlobalState
        sess <- readSession
        lucidIO $ wrapPage sess s "Log in | Hat" $ do
          let formSubmitHandler formNode =
                JS.tryLogin [formNode]
          form_ [onFormSubmit formSubmitHandler] $ do
            label_ [Lucid.for_ "nick"]
              "Username"
            input_ [type_ "text", name_ "nick"]
            label_ [Lucid.for_ "pass"]
              "Password"
            input_ [type_ "password", name_ "pass"]
            input_ [type_ "submit", value_ "Log in"]
      Spock.post "login" $ do
        nick' <- param' "nick"
        pass' <- Pass . T.encodeUtf8 <$> param' "pass"
        mbUser <- dbQuery (GetUserByNick' nick')
        case mbUser of
          Nothing   -> jsonFormFail "nick" "User not found"
          Just user -> case verifyPass' pass' (user^.pass) of
            False -> jsonFormFail "pass" "Incorrect password"
            True  -> do writeSession (Just (user^.uid))
                        jsonSuccess
      Spock.get "signup" $ do
        s <- dbQuery GetGlobalState
        sess <- readSession
        lucidIO $ wrapPage sess s "Sign up | Hat" $ do
          case sess of
            Just _ -> p_ "You are already logged in."
            Nothing -> do
              let formSubmitHandler formNode =
                    JS.trySignup [formNode]
              form_ [onFormSubmit formSubmitHandler] $ do
                fieldset_ $ do
                  label_ [Lucid.for_ "name"]
                    "Name"
                  input_ [type_ "text", name_ "name"]
                  label_ [Lucid.for_ "nick"]
                    "Nickname (letters, digits, _-.)"
                  input_ [type_ "text", name_ "nick"]
                fieldset_ $ do
                  label_ [Lucid.for_ "pass"]
                    "Password"
                  input_ [type_ "password", name_ "pass"]
                  label_ [Lucid.for_ "pass2"]
                    "Password again"
                  input_ [type_ "password", name_ "pass2"]
                fieldset_ $ do
                  label_ [Lucid.for_ "email"]
                    "Email (just in case)"
                  input_ [type_ "email", name_ "email"]
                input_ [type_ "submit", value_ "Sign up"]
                a_ [class_ "button button-clear",
                    href_ "http://imgur.com/r/blep/hvk4r7x",
                    target_ "_blank"]
                  "Show me cats with stuck-out tongues instead"
      Spock.post "signup" $ do
        nick'  <- param' "nick"
        name'  <- param' "name"
        pass'  <- param' "pass"
        pass2' <- param' "pass2"
        email' <- param' "email"
        -- Validating the form
        -- name
        when (T.null name') $
          jsonFormFail "name" "Can't be empty"
        -- nick
        when (T.null nick') $
          jsonFormFail "nick" "Can't be empty"
        mbUser <- dbQuery (GetUserByNick' nick')
        when (isJust mbUser) $
          jsonFormFail "nick" "This nickname is taken"
        unless (T.all (\c -> isAlphaNum c || c `elem` ['.','-','_']) nick') $
          jsonFormFail "nick" "Contains forbidden characters"
        -- passwords
        when (T.null pass') $
          jsonFormFail "pass" "Can't be empty"
        when (pass' /= pass2') $
          jsonFormFail "pass" "Passwords don't match"
        -- email
        when (T.null email') $
          jsonFormFail "email" "Can't be empty"
        -- Success
        uid' <- randomShortUid
        encPass <- liftIO $ encryptPassIO' (Pass (T.encodeUtf8 pass'))
        now <- liftIO getCurrentTime
        user <- dbUpdate (AddUser uid' nick' name' encPass email' now)
        writeSession (Just (user^.uid))
        jsonSuccess
      Spock.post "logout" $ do
        writeSession Nothing

wrapPage
  :: (MonadIO m, MonadRandom m)
  => Session
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
    div_ [id_ "header"] $ do
      a_ [class_ "logo", href_ "/"]
        "Hat"
      span_ [class_ "float-right"] $ do
        case sess of
          Nothing -> do
            a_ [class_ "header-nav", href_ "/signup"]
              "Sign up"
            a_ [class_ "header-nav", href_ "/login"]
              "Log in"
          Just u -> do
            let user = gs ^. userById u
            when (user^.admin) $
              a_ [class_ "header-nav", href_ "/admin"]
                "Admin stuff"
            a_ [class_ "header-nav", href_ ("/user/" <> user^.nick)]
              (toHtml (user^.name))
            a_ [class_ "header-nav", href_ "#",
                onclick_ (fromJS (JS.logout ()) <> "return false;")]
              "Log out"
    div_ [id_ "main"] $ do
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

gamePage
  :: Uid Game
  -> SpockActionCtx ctx conn Session ServerState ()
gamePage gameId = do
  s <- dbQuery GetGlobalState
  sess <- readSession
  game <- dbQuery (GetGame gameId)
  creator <- dbQuery (GetUser (game^.createdBy))
  players' <- mapM (dbQuery . GetUser) $ S.toList (game^.players)
  currentAdmin <- isAdmin
  lucidIO $ wrapPage sess s (game^.title <> " | Hat") $ do
    h2_ (toHtml (game^.title))
    when (game^.ended) $ do
      p_ $ strong_ "This game has already ended."
    ul_ $ do
      li_ $ do "Game begins at "
               toHtml (show (game^.begins))
      li_ $ do "Created by "
               userLink creator
      li_ $ do "Registered players: "
               if null players'
                 then "none"
                 else sequence_ $ intersperse ", " (map userLink players')
      let mbUw = game^?wordReq._Just.userWords
      case mbUw of
        Nothing -> return ()
        Just uw -> li_ $ do
          "Players who haven't submitted words: "
          case filter (\p -> (p^.uid) `M.notMember` uw) players' of
            [] -> "none"
            xs -> sequence_ $ intersperse ", " (map userLink xs)

    for_ sess $ \u -> do
      -- the game is on, the user has to propose words
      let wordsNeeded req = case req^.userWords.at u of
            Just ws -> do
              p_ "Your proposed words for the game are:"
              blockquote_ (toHtml (T.unwords (S.toList ws)))
            Nothing -> do
              let formSubmitHandler formNode =
                    JS.submitWords (gameId, formNode)
              form_ [onFormSubmit formSubmitHandler] $ do
                label_ [Lucid.for_ "words"]
                  "Your proposed words for this game"
                let plh = format "{} space-separated words" [req^.wordsPerUser]
                textarea_ [name_ "words", placeholder_ plh] ""
                input_ [type_ "submit", value_ "Submit"]
      -- the game has ended, the user had to propose words
      let wordsWereNeeded req = case req^.userWords.at u of
            Nothing ->
              p_ "You didn't propose words for this game."
            Just ws -> do
              p_ "Your proposed words for the game were:"
              for_ ws $ \w -> li_ (toHtml w)

      case (S.member u (game^.players), game^.wordReq, game^.ended) of
        (False, _, False) -> do
          errorId <- randomLongUid
          button "Register" $
            JS.addPlayerSelf (JS.selectUid errorId, game^.uid)
          div_ [uid_ errorId, style_ "display:none"] ""
        (False, _, True) ->
          p_ "You didn't participate in this game."
        (_, Nothing, False) -> do
          errorId <- randomLongUid
          button "Unregister" $
            JS.removePlayerSelf (JS.selectUid errorId, game^.uid)
          div_ [uid_ errorId, style_ "display:none"] ""
          p_ ""
          p_ "You don't have to propose words for this game."
        (_, Nothing, True) ->
          return ()  -- game has ended, nobody asked for words
        (_, Just req, False) -> do
          errorId <- randomLongUid
          button "Unregister" $
            JS.removePlayerSelf (JS.selectUid errorId, game^.uid)
          div_ [uid_ errorId, style_ "display:none"] ""
          p_ ""
          wordsNeeded req
        (_, Just req, True) -> wordsWereNeeded req

    when (currentAdmin && isJust (game^.wordReq)) $
      a_ [class_ "button",
          href_ (format "/game/{}/words/printable" [game^.uid])]
        "Show submitted words"

isAdmin :: SpockActionCtx ctx conn Session ServerState Bool
isAdmin = do
  sess <- readSession
  case sess of
    Nothing -> return False
    Just u  -> view admin <$> dbQuery (GetUser u)

userLink :: Monad m => User -> HtmlT m ()
userLink user = mkLink (toHtml (user^.name))
                       ("/user/" <> user^.nick)

button :: Monad m => Text -> JS -> HtmlT m ()
button caption js = button_ [onClick js] (toHtml caption)

emptySpan :: Monad m => Text -> HtmlT m ()
emptySpan w = span_ [style_ ("margin-left:" <> w)] mempty

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ format "{} return false;" [f (JS "this")]

onPageLoad :: Monad m => JS -> HtmlT m ()
onPageLoad js = script_ $ format "$(document).ready(function(){{}});" [js]

onClick :: JS -> Attribute
onClick (JS js) = onclick_ js

mkLink :: Monad m => HtmlT m a -> Url -> HtmlT m a
mkLink x src = a_ [href_ src] x

jsonFail :: MonadIO m => Text -> ActionCtxT ctx m a
jsonFail x = json (False, x)

jsonFormFail :: MonadIO m => Text -> Text -> ActionCtxT ctx m a
jsonFormFail field err = json (False, field, err)

jsonSuccess :: MonadIO m => ActionCtxT ctx m a
jsonSuccess = json (True, "" :: Text)

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
