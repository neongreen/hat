{-# LANGUAGE
OverloadedStrings,
DataKinds,
QuasiQuotes,
TypeFamilies,
TupleSections,
FlexibleContexts,
ScopedTypeVariables,
MultiWayIf,
NoImplicitPrelude
  #-}


module Main (main) where


-- General
import BasePrelude
-- Lenses
import Lens.Micro.Platform hiding ((&))
-- Monads
import Control.Monad.IO.Class
-- Text
import qualified Data.Text.All as T
import Data.Text.All (Text)
import NeatInterpolation
-- Lists
import Data.List.Index
import Data.List.Split
-- Randomness
import Control.Monad.Random
import System.Random.Shuffle
-- Containers
import qualified Data.Set as S
import qualified Data.Map as M
-- Web
import Web.Spock hiding (head, get, text)
import qualified Web.Spock as Spock
import Web.Spock.Lucid
import Lucid hiding (for_)
import qualified Lucid
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.HTTP.Types.Status
-- acid-state
import Data.Acid as Acid
-- IO
import System.IO
-- Time
import Data.Time
-- Passwords
import Crypto.Scrypt
-- Concurrency
import qualified SlaveThread as Slave


-- Local
import DB
import Utils
import qualified JS
import JS (JS(..), allJSFunctions)
import Schedule


data ServerState = ServerState {
  _db :: DB }

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

gamePhaseRoomVars :: Path '[Uid Game, Int, Int]
gamePhaseRoomVars = gameVar <//> var <//> var

roundVars :: Path '[Uid User, Uid User]
roundVars = "round" <//> var <//> var

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let prepare = openLocalStateFrom "state/" emptyState
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
    -- The command-line interface thread
    Slave.fork $ forever $ do
      putStr "> "
      cmd <- getLine
      putStrLn ""
      execCommand db cmd
      putStrLn ""
    -- Thread to update all partial schedules; crude, but works
    Slave.fork $ forever $ do
      updatedAny <- newIORef False
      games' <- view games <$> Acid.query db GetGlobalState
      for_ games' $ \game' ->
        ifor_ (game'^..currentPhase._Just.rooms.each) $ \i room ->
          case room^.schedule of
            ScheduleDone{} -> return ()
            ScheduleCalculating ps -> do
              writeIORef updatedAny True
              -- It's possible that the room will be changed while the
              -- computation is going. To prevent this, AdvanceSchedule will
              -- only update the schedule if it matches the one we were
              -- advancing.
              ps' <- advancePartialSchedule 1000 ps
              Acid.update db (AdvanceSchedule (game'^.uid) (i+1) ps ps')
              when (isRight ps') $
                T.putStrLn $ T.format
                  "finished calculating schedule for game {}, room #{}"
                  (game'^.uid, i+1)
      upd <- readIORef updatedAny
      when (not upd) $ threadDelay 50000

    runSpockNoBanner 7070 $ spock spockConfig $ do
      middleware (staticPolicy (addBase "static"))
      Spock.get "/js.js" $ do
        setHeader "Content-Type" "application/javascript; charset=utf-8"
        Spock.bytes $ T.encodeUtf8 (fromJS allJSFunctions)
      Spock.get root $ do
        s <- dbQuery GetGlobalState
        sess <- readSession
        lucidIO $ wrapPage sess s "Hat" $ do
          h2_ "Available games"
          ul_ $ for_ (s^.games) $ \g ->
            li_ $ mkLink (toHtml (g^.title))
                         ("/game/" <> uidToText (g^.uid))
      gameMethods
      userMethods

      Spock.get "admin" $ do
        s <- dbQuery GetGlobalState
        sess <- readSession
        currentAdmin <- isAdmin
        lucidIO $ wrapPage sess s "Admin | Hat" $ do
          h2_ "Admin stuff"
          if not currentAdmin
            then p_ "You're not an admin."
            else do
              let (admins, ordinaryUsers) = partition (view admin) (s^.users)
              h3_ "Admins"
              p_ $ list $ map userLink admins
              h3_ "Users"
              p_ $ list $ map userLink ordinaryUsers

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
                buttonLink "Show me cats with stuck-out tongues instead"
                  [class_ "button-clear", target_ "_blank"]
                  "http://imgur.com/r/blep/hvk4r7x"

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
    includeJS  "/magnific-popup.js"
    includeCSS "/magnific-popup.css"
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

{-
How preregistration works:

  * during preregistration, anyone can register, unregister, etc
  * when preregistration ends, people can still add words and unregister but can't register
-}

gameMethods :: SpockM ctx Session ServerState ()
gameMethods = do
  Spock.get (gameVar) $ \gameId ->
    gamePage gameId

  Spock.get (gameVar <//> var <//> var) $ \gameId phaseNum roomNum ->
    roomPage gameId phaseNum roomNum

  Spock.post (gameVar <//> "begin") $ \gameId -> do
    currentAdmin <- isAdmin
    game' <- dbQuery (GetGame gameId)
    when (not currentAdmin) $
      jsonFail "You're not an admin"
    when (game'^.begun) $
      jsonFail "The game has already begun"
    dbUpdate (SetGameBegun gameId True)
    jsonSuccess

  Spock.post (gameVar <//> "begin-next-phase") $ \gameId -> do
    currentAdmin <- isAdmin
    game' <- dbQuery (GetGame gameId)
    when (not currentAdmin) $
      jsonFail "You're not an admin"
    when (game'^.ended) $
      jsonFail "The game has already ended"
    when (not (game'^.begun)) $
      jsonFail "The game hasn't yet begun"
    when (isJust (game'^.currentPhase)) $
      jsonFail "A phase is already in progress"
    case game'^.groups of
      Nothing -> jsonFail "The players haven't been divided into groups"
      Just gs -> do
        rooms' <- for gs $ \gr -> do
          sch <- case precomputedSchedules ^? ix (length gr) of
            -- TODO: fix
            Nothing -> error "no known schedule"
            Just ss -> uniform ss
          return Room {
            _roomPlayers = gr,
            _roomAbsentees = mempty,
            _roomTable = M.fromList [
                ((a, b), if a==b then RoundImpossible else RoundNotYetPlayed)
                | a <- gr, b <- gr ],
            _roomPastGames = [],
            _roomSchedule = ScheduleDone sch }
        let phase' = Phase {
              _phaseRooms = rooms' }
        dbUpdate (SetGameCurrentPhase gameId phase')
        jsonSuccess

  Spock.post (gameVar <//> "generate-groups") $ \gameId -> do
    currentAdmin <- isAdmin
    num <- param' "num"
    game' <- dbQuery (GetGame gameId)
    when (not currentAdmin) $
      jsonFail "You're not an admin"
    when (not (game'^.begun)) $
      jsonFail "The game has not yet begun"
    when (game'^.ended) $
      jsonFail "The game has already ended"
    when (num < 1) $
      jsonFail "The number of groups is less than 1"
    let pls = S.size (game'^.players)
    when (num > pls) $
      jsonFail "There are more groups than players"
    let gs = concatMap (\(n, p) -> replicate n p) (calcBreak num pls)
    ps <- shuffleM (S.toList (game'^.players))
    let grouped = splitPlaces gs ps
    dbUpdate (SetGameGroups gameId (Just grouped))

  Spock.post (gameVar <//> "players" <//> "add-self") $ \gameId -> do
    sess <- readSession
    game' <- dbQuery (GetGame gameId)
    now <- liftIO getCurrentTime
    u <- case sess of
      Nothing -> jsonFail "You're not logged in"
      Just u  -> return u
    when (u `S.member` (game'^.players)) $
      jsonFail "You're already registered for this game"
    when (game'^.begun) $
      jsonFail "This game has already begun"
    when (now > game'^.registerUntil) $
      jsonFail "The pre-registration period has ended"
    dbUpdate (AddPlayer gameId u)
    jsonSuccess

  Spock.post (gameVar <//> "players" <//> "remove-self") $ \gameId -> do
    sess <- readSession
    game' <- dbQuery (GetGame gameId)
    u <- case sess of
      Nothing -> jsonFail "You're not logged in"
      Just u  -> return u
    when (u `S.notMember` (game'^.players)) $
      jsonFail "You aren't registered for this game"
    when (game'^.begun) $
      jsonFail "The game has already begun, you can't unregister now"
    dbUpdate (RemovePlayer gameId u)
    jsonSuccess

  Spock.get (gameVar <//> "words" <//> "printable") $ \gameId -> do
    game' <- dbQuery (GetGame gameId)
    currentAdmin <- isAdmin
    lucidIO $ do
      if not currentAdmin
        then p_ "Only admins can see the words"
        else case game'^.wordReq of
          Nothing -> p_ "In this game players don't submit words"
          Just req -> do
            ws <- chunksOf 3 <$>
              shuffleM (concatMap S.toList (req^..submitted.each))
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
                border: 1pt dotted #eee;
                padding: 0.2cm 0.5cm; }
              |]

  Spock.post (gameVar <//> "words" <//> "submit") $ \gameId -> do
    sess <- readSession
    game' <- dbQuery (GetGame gameId)
    ws <- T.words <$> param' "words"
    u <- case sess of
      Just u  -> return u
      Nothing -> jsonFormFail "words" "You're not logged in"
    req <- case game'^.wordReq of
      Just req -> return req
      Nothing  -> jsonFormFail "words"
        "Words aren't required for this game"
    when (u `S.notMember` (game'^.players)) $
      jsonFormFail "words" "You aren't participating in this game"
    when (isJust (req^.submitted.at u)) $
      jsonFormFail "words" "You have already submitted words"
    when (length (ordNub ws) < length ws) $
      jsonFormFail "words" "The list contains duplicates"
    when (any (T.any (== ',')) ws) $
      jsonFormFail "words" "Punctuation isn't allowed"
    when (null ws) $
      jsonFormFail "words" "You haven't entered any words"
    when (length ws < req^.perUser) $
      jsonFormFail "words" $
        T.format "You entered {} {} out of {}"
          (length ws, plural (length ws) "word", req^.perUser)
    when (length ws > req^.perUser) $
      jsonFormFail "words" $
        T.format "You entered {} {}, that's too many"
          (length ws, plural (length ws) "word")
    -- All checks passed
    dbUpdate (SetWords gameId u ws)
    jsonSuccess

  Spock.post (gamePhaseRoomVars <//> roundVars <//> "set") $
    \gameId phaseNum roomNum namerId guesserId -> do
    score'          <- param' "score"
    namerPenalty'   <- abs <$> param' "namer-penalty"
    guesserPenalty' <- abs <$> param' "guesser-penalty"
    discards'       <- abs <$> param' "discards"
    (_, _, _, room) <-
      getGamePhaseRoom gameId phaseNum roomNum
    let res = RoundPlayed {
          _roundScore = score',
          _roundNamerPenalty = namerPenalty',
          _roundGuesserPenalty = guesserPenalty',
          _roundDiscards = discards' }
    when (namerId == guesserId) $
      fail "a player can't play with themself"
    when (namerId `notElem` room^.players) $
      fail "the namer isn't playing in this room"
    when (guesserId `notElem` room^.players) $
      fail "the guesser isn't playing in this room"
    dbUpdate (SetRoundResults gameId phaseNum roomNum (namerId, guesserId) res)

  Spock.post (gamePhaseRoomVars <//> roundVars <//> "clear") $
    \gameId phaseNum roomNum namerId guesserId -> do
    (_, _, _, room) <-
      getGamePhaseRoom gameId phaseNum roomNum
    let res = RoundNotYetPlayed
    when (namerId == guesserId) $
      fail "a player can't play with themself"
    when (namerId `notElem` room^.players) $
      fail "the namer isn't playing in this room"
    when (guesserId `notElem` room^.players) $
      fail "the guesser isn't playing in this room"
    dbUpdate (SetRoundResults gameId phaseNum roomNum (namerId, guesserId) res)

  Spock.post (gamePhaseRoomVars <//> "player" <//> var <//> "absent") $
    \gameId phaseNum roomNum playerId -> do
    val <- (== ("true" :: Text)) <$> param' "val"
    (_, _, _, room) <-
      getGamePhaseRoom gameId phaseNum roomNum
    when (playerId `notElem` room^.players) $
      fail "the player isn't playing in this room"
    dbUpdate (SetAbsent gameId phaseNum roomNum playerId val)

gamePage
  :: Uid Game
  -> SpockActionCtx ctx conn Session ServerState ()
gamePage gameId = do
  s <- dbQuery GetGlobalState
  sess <- readSession
  game' <- dbQuery (GetGame gameId)
  creator <- dbQuery (GetUser (game'^.createdBy))
  players' <- mapM (dbQuery . GetUser) $ S.toList (game'^.players)
  groups' <- (_Just.each.each) (dbQuery . GetUser) $ game'^.groups
  currentAdmin <- isAdmin
  lucidIO $ wrapPage sess s (game'^.title <> " | Hat") $ do
    h2_ (toHtml (game'^.title))
    when (game'^.ended) $ do
      p_ $ strong_ "This game has already ended."
    now <- liftIO getCurrentTime
    ul_ $ do
      li_ $ do "Created by "
               userLink creator
      when (now < game'^.registerUntil && not (game'^.begun)) $
        li_ $ do "Pre-registration ends at "
                 toHtml (show (game'^.registerUntil))
      li_ $ do "Registered players: "
               if null players'
                 then "none"
                 else list (map userLink players')
      let mbUw = game'^?wordReq._Just.submitted
      case mbUw of
        Nothing -> return ()
        Just uw -> li_ $ do
          "Players who haven't submitted words: "
          case filter (\p -> (p^.uid) `M.notMember` uw) players' of
            [] -> "none"
            xs -> list (map userLink xs)

    for_ sess $ \u -> do
      -- the game is on, the user has to propose words
      let wordsNeeded req = case req^.submitted.at u of
            Just ws -> do
              p_ "Your proposed words for the game are:"
              blockquote_ (toHtml (T.unwords (S.toList ws)))
            Nothing -> do
              let formSubmitHandler formNode =
                    JS.submitWords (gameId, formNode)
              form_ [onFormSubmit formSubmitHandler] $ do
                label_ [Lucid.for_ "words"]
                  "Your proposed words for this game"
                let plh = T.format "{} space-separated words"
                            [req^.perUser]
                textarea_ [name_ "words", placeholder_ plh] ""
                input_ [type_ "submit", value_ "Submit"]
      -- the game has ended, the user had to propose words
      let wordsWereNeeded req = case req^.submitted.at u of
            Nothing ->
              p_ "You didn't propose words for this game."
            Just ws -> do
              p_ "Your proposed words for the game were:"
              ul_ $ for_ ws $ \w -> li_ (toHtml w)

      case (S.member u (game'^.players), game'^.wordReq, game'^.ended) of
        (False, _, False) -> do
          errorId <- randomLongUid
          button "Register" [] $
            JS.addPlayerSelf (JS.selectUid errorId, gameId)
          div_ [uid_ errorId, style_ "display:none"] ""
        (False, _, True) ->
          p_ "You didn't participate in this game."
        (_, Nothing, False) -> do
          errorId <- randomLongUid
          button "Unregister" [] $
            JS.removePlayerSelf (JS.selectUid errorId, gameId)
          div_ [uid_ errorId, style_ "display:none"] ""
          p_ ""
          p_ "You don't have to propose words for this game."
        (_, Nothing, True) ->
          return ()  -- game has ended, nobody asked for words
        (_, Just req, False) -> do
          errorId <- randomLongUid
          button "Unregister" [] $
            JS.removePlayerSelf (JS.selectUid errorId, gameId)
          div_ [uid_ errorId, style_ "display:none"] ""
          p_ ""
          wordsNeeded req
        (_, Just req, True) -> wordsWereNeeded req

    when currentAdmin $ do
      h3_ "Admin things"
      when (isJust (game'^.wordReq)) $ do
        buttonLink "Show submitted words" []
          (T.format "/game/{}/words/printable" [gameId])
        emptySpan "1rem"
      when (not (game'^.begun)) $
        button "End the preregistration phase" [] $
          JS.endPreregistration [gameId]
      p_ ""
      when (game'^.begun) $ do
        -- show past phases
        ifor_ (game'^.pastPhases) $ \i p -> do
          h4_ $ toHtml $ T.format "Phase {}" [i+1]
          let roomCount = length (p^._1.rooms)
          p_ $ toHtml $ T.format "There {} {} {}."
                          (plural roomCount "was", roomCount,
                           plural roomCount "room")
        unless (game'^.ended) $ case game'^.currentPhase of
          -- if there's a phase in progress, show it
          Just p -> do
            let pNum = length (game'^.pastPhases) + 1
            h4_ $ toHtml $ T.format "Phase #{}" [pNum]
            p_ $ do
              "Rooms: "
              let roomLink i = mkLink (toHtml (T.show i))
                    (T.format "/game/{}/{}/{}" (gameId, pNum, i))
              list $ map roomLink [1..length (p^.rooms)]
          -- if there isn't, choose players to advance to the next phase
          Nothing -> do
            let pls = S.size (game'^.players)
            p_ $ do
              toHtml $ T.format "There {} {} {}. "
                (plural pls "is", pls, plural pls "player")
              "How many groups do you want?"
            -- the least number of groups such that no group has 7 people
            let defGroups = fromMaybe 1 $
                  fmap (sum . map fst) $
                  find (all (< 7) . map snd) (map (`calcBreak` pls) [1..])
            numId <- randomLongUid
            input_ [uid_ numId, type_ "number",
                    value_ (T.show $ maybe defGroups length groups'),
                    style_ "width: 30%; margin-right: 1em;"]
            button "Generate" [] $
              JS.generateGroups (gameId, JS.selectUid numId)
            case groups' of
              Nothing -> return ()
              Just gs -> do
                ol_ $ do
                  for_ gs $ \g -> li_ $ do
                    toHtml $ T.format "{} {}: "
                      (length g, plural (length g) "player")
                    list (map userLink g)
                button "Begin next phase" [] $
                  JS.beginNextPhase [gameId]

roomPage
  :: Uid Game
  -> Int        -- ^ Phase
  -> Int        -- ^ Room number
  -> SpockActionCtx ctx conn Session ServerState ()
roomPage gameId phaseNum roomNum = do
  s <- dbQuery GetGlobalState
  sess <- readSession
  (game', _, _, room) <- getGamePhaseRoom gameId phaseNum roomNum
  players' <- mapM (dbQuery . GetUser) (room^.players)
  let pageTitle = T.format "{}: phase #{}, room #{}"
                           (game'^.title, phaseNum, roomNum)
  lucidIO $ wrapPage sess s (pageTitle <> " | Hat") $ do
    -- Header
    h2_ $ do
      mkLink (toHtml (game'^.title)) (T.format "/game/{}" [game'^.uid])
      toHtml $ T.format ": phase #{}, room #{}" (phaseNum, roomNum)

    -- Helper: round table cell
    let roundCell px py = do
          let roundRes = room^?!table.ix (py^.uid, px^.uid)
          let handler = JS.showRoundEditPopup
                          (gameId, phaseNum, roomNum,
                           py^.uid, px^.uid,
                           fromMaybe 0 (roundRes^?score),
                           fromMaybe 0 (roundRes^?namerPenalty),
                           fromMaybe 0 (roundRes^?guesserPenalty),
                           fromMaybe 0 (roundRes^?discards))
          case roundRes of
            RoundNotYetPlayed ->
              td_ [onClick handler, class_ "not-yet-played"] ""
            RoundPlayed sc _ _ _ ->
              td_ [onClick handler, class_ "played"] $ toHtml (T.show sc)
            RoundImpossible ->
              td_ [class_ "impossible"] ""

    -- Helpers: penalties & totals
    let penaltiesRow = tr_ [class_ "penalties"] $ do
          td_ "Penalty"
          for_ players' $ \p -> do
            let penalty = sum . catMaybes $ do
                  ((a, b), r) <- M.toList (room^.table)
                  [guard (p^.uid == a) *> r^?discards,
                   guard (p^.uid == a) *> r^?namerPenalty,
                   guard (p^.uid == b) *> r^?guesserPenalty ]
            td_ $ when (penalty /= 0) $ toHtml ("−" <> T.show penalty)
    let totalsRow = tr_ [class_ "totals"] $ do
          td_ "Total"
          for_ players' $ \p -> do
            let total = sum . catMaybes $ do
                  ((a, b), r) <- M.toList (room^.table)
                  [guard (p^.uid == a)         *> r^?discards.to negate,
                   guard (p^.uid == a)         *> r^?namerPenalty.to negate,
                   guard (p^.uid == b)         *> r^?guesserPenalty.to negate,
                   guard (p^.uid `elem` [a,b]) *> r^?score ]
            td_ $ toHtml (T.show total)

    -- Actually generate the table
    table_ [class_ "roomtable"] $ do
      for_ [-1 .. length players' - 1] $ \y -> tr_ $
        for_ [-1 .. length players' - 1] $ \x -> do
          -- x = guesser, y = namer
          let px = players' !! x
              py = players' !! y
              xAbsent = px^.uid `elem` room^.absentees
              yAbsent = py^.uid `elem` room^.absentees
          withP (x /= -1 && xAbsent || y /= -1 && yAbsent)
            [class_ " absent "] $
            case (x, y) of
              (-1, -1) -> td_ [class_ "header-corner"] ""
              -- left column: namers
              (-1,  _) -> td_ [class_ "header-left"] $ do
                if yAbsent
                  then imgButton "mark as present" "/media-play.svg"
                                 [class_ "absent-button"] $
                         JS.setAbsent (gameId, phaseNum, roomNum,
                                       py^.uid, False)
                  else imgButton "mark as absent" "/media-pause.svg"
                                 [class_ "absent-button"] $
                         JS.setAbsent (gameId, phaseNum, roomNum,
                                       py^.uid, True)
                userLink py
              -- upper row: guessers
              ( _, -1) -> td_ [class_ "header-top"] (userLink px)
              ( _,  _) -> roundCell px py
      penaltiesRow
      totalsRow

    -- Next rounds
    case room^.schedule of
      ScheduleCalculating ps -> p_ $ do
        let iLeft = ps^.schIterationsTotal - ps^.schIterationsLeft
            iTotal = ps^.schIterationsTotal
        toHtml $ T.format
          "The schedule is being calculated ({} out of {} iterations,\
          \ or {}%). It shouldn't take more than 10 seconds. You can\
          \ refresh the page to see the progress."
          (iLeft, iTotal,
           T.fixed 0 (100*fromIntegral iLeft/fromIntegral iTotal :: Double))
      ScheduleDone sch -> do
        ol_ [class_ "future-rounds",
             start_ (T.show (length (room^.pastGames) + 1))] $
          for_ (sch^..each) $ \(namerPos, guesserPos) -> li_ $ do
            let namer   = players' !! namerPos
                guesser = players' !! guesserPos
            userLink namer >> " plays with " >> userLink guesser

getGamePhaseRoom
  :: (MonadIO m, HasSpock (ActionCtxT ctx m),
      SpockState (ActionCtxT ctx m) ~ ServerState)
  => Uid Game -> Int -> Int
  -> ActionCtxT ctx m (Game, Phase, Maybe PhaseResults, Room)
getGamePhaseRoom gameId phaseNum roomNum = do
  game' <- dbQuery (GetGame gameId)
  let mbPhase
        | 1 <= phaseNum && phaseNum <= length (game'^.pastPhases) =
            Just (Left (game'^?!pastPhases.ix (phaseNum-1)))
        | phaseNum == length (game'^.pastPhases) + 1 =
            Right <$> (game'^.currentPhase)
        | otherwise =
            Nothing
  (phase', mbRes) <- case mbPhase of
    Nothing -> do
      setStatus status404
      Spock.text "No phase with such number"
    Just (Left (p, res)) -> return (p, Just res)
    Just (Right p)       -> return (p, Nothing)
  room <- case phase'^?rooms.ix (roomNum-1) of
    Nothing -> do
      setStatus status404
      Spock.text "No room with such number"
    Just room -> return room
  return (game', phase', mbRes, room)

userMethods :: SpockM ctx Session ServerState ()
userMethods = do
  Spock.get ("user" <//> var) $ \nick' -> do
    s <- dbQuery GetGlobalState
    sess <- readSession
    user <- dbQuery (GetUserByNick nick')
    currentAdmin <- isAdmin
    lucidIO $ wrapPage sess s ((user^.name) <> " | Hat") $ do
      h2_ $ toHtml $ user^.name <> " (aka " <> user^.nick <> ")"
      when (currentAdmin && not (user^.admin)) $
        button "Make admin" [] $
          JS.makeAdmin [nick']
      when (user^.admin) $
        p_ "One of the admins."

  Spock.post ("user" <//> var <//> "make-admin") $ \nick' -> do
    currentAdmin <- isAdmin
    when currentAdmin $ do
      user <- dbQuery (GetUserByNick nick')
      dbUpdate (SetAdmin (user^.uid) True)

isAdmin :: SpockActionCtx ctx conn Session ServerState Bool
isAdmin = do
  sess <- readSession
  case sess of
    Nothing -> return False
    Just u  -> view admin <$> dbQuery (GetUser u)

userLink :: Monad m => User -> HtmlT m ()
userLink user = mkLink (toHtml (user^.name))
                       ("/user/" <> user^.nick)

button :: Monad m => Text -> [Attribute] -> JS -> HtmlT m ()
button caption attrs js =
  button_ (onClick js : attrs) (toHtml caption)

buttonLink :: Monad m => Text -> [Attribute] -> Url -> HtmlT m ()
buttonLink caption attrs link =
  a_ (href_ link : class_ " button " : attrs) (toHtml caption)

emptySpan :: Monad m => Text -> HtmlT m ()
emptySpan w = span_ [style_ ("margin-left:" <> w)] mempty

onFormSubmit :: (JS -> JS) -> Attribute
onFormSubmit f = onsubmit_ $ T.format "{} return false;" [f (JS "this")]

onPageLoad :: Monad m => JS -> HtmlT m ()
onPageLoad js = script_ $ T.format "$(document).ready(function(){{}});" [js]

onClick :: JS -> Attribute
onClick (JS js) = onclick_ js

mkLink :: Monad m => HtmlT m a -> Url -> HtmlT m a
mkLink x src = a_ [href_ src] x

imgButton :: Monad m => Text -> Url -> [Attribute] -> JS -> HtmlT m ()
imgButton alt src attrs (JS handler) =
  a_ [href_ "#", onclick_ (handler <> "return false;")]
     (img_ (src_ src : alt_ alt : title_ alt : attrs))

list :: Monad m => [HtmlT m a] -> HtmlT m ()
list = sequence_ . intersperse ", " . map void

jsonFail :: MonadIO m => Text -> ActionCtxT ctx m a
jsonFail x = json (False, x)

jsonFormFail :: MonadIO m => Text -> Text -> ActionCtxT ctx m a
jsonFormFail field err = json (False, field, err)

jsonSuccess :: MonadIO m => ActionCtxT ctx m a
jsonSuccess = json (True, "" :: Text)

calcBreak
  :: Int               -- ^ Number of groups
  -> Int               -- ^ Number of people
  -> [(Int, Int)]      -- ^ (groups, people in group)
calcBreak x n =
  let (people, extra) = divMod n x
  in  if extra == 0 then [(x, people)]
                    else [(extra, people+1), (x-extra, people)]

withP :: With a => Bool -> [Attribute] -> a -> a
withP False _  x = x
withP True  as x = with x as
