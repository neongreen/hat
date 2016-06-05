{-# LANGUAGE
TemplateHaskell,
TypeFamilies,
TupleSections,
MultiParamTypeClasses,
FunctionalDependencies,
FlexibleInstances,
OverloadedStrings,
GeneralizedNewtypeDeriving,
FlexibleContexts,
RankNTypes,
NoImplicitPrelude
  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module DB
(
  -- * Types
  Session,
  User(..),
    nick,
    name,
    email,
    pass,
    admin,
  WordReq(..),
    userWords,
    wordsPerUser,
  Game(..),
    title,
    createdBy,
    registerUntil,
    begun,
    ended,
    players,
    wordReq,
    groups,
  GlobalState(..),
    games,
    users,
    sessions,
    dirty,
  DB,

  -- * Common lenses
  uid,
  created,

  -- * Stuff
  sampleState,
  userById,
  execCommand,

  -- * Methods
  GetGlobalState(..),
  GetSessions(..), SetSessions(..),
  AddUser(..),
  AddPlayer(..), RemovePlayer(..),
  GetUser(..),
  GetUserByNick(..), GetUserByNick'(..),
  SetAdmin(..),
  GetGame(..),
  SetGameBegun(..),
  SetGameGroups(..),
  SetWords(..),
  SetDirty(..), UnsetDirty(..),
)
where


-- General
import BasePrelude
-- Monads
import Control.Monad.State
-- Lenses
import Lens.Micro.Platform
-- Containers
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
-- Text
import qualified Data.Text.All as T
import Data.Text.All (Text)
-- Time
import Data.Time
-- Web
import Web.Spock (SessionId)
-- acid-state
import Data.Acid as Acid
import Data.SafeCopy
-- Passwords
import Crypto.Scrypt
-- Command-line parsing
import Options.Applicative.Simple
-- Exception handling
import Control.Exception.Enclosed

-- Local
import Utils



data User = User {
  _userUid :: Uid User,
  _userNick :: Text,
  _userName :: Text,
  _userEmail :: Text,
  _userCreated :: UTCTime,
  _userPass :: EncryptedPass,
  _userAdmin :: Bool }
  deriving (Show)

deriveSafeCopySimple 0 'base ''User
makeFields ''User

type Session = Maybe (Uid User)

data WordReq = WordReq {
  _wordReqUserWords :: Map (Uid User) (Set Text),
  _wordReqWordsPerUser :: Int }
  deriving (Show)

deriveSafeCopySimple 0 'base ''WordReq
makeFields ''WordReq

data Game = Game {
  _gameUid :: Uid Game,
  _gameTitle :: Text,
  _gameCreatedBy :: Uid User,
  _gameWordReq :: Maybe WordReq,
  _gameRegisterUntil :: UTCTime,
  _gameBegun :: Bool,
  _gameEnded :: Bool,
  _gamePlayers :: Set (Uid User),
  _gameGroups :: Maybe [[Uid User]] }
  deriving (Show)

deriveSafeCopySimple 0 'base ''Game
makeFields ''Game

data GlobalState = GlobalState {
  _users    :: [User],
  _games    :: [Game],
  _sessions :: [(SessionId, UTCTime, Session)],
  _dirty    :: Bool }
  deriving (Show)

deriveSafeCopySimple 0 'base ''GlobalState
makeLenses ''GlobalState

type DB = AcidState GlobalState

hasUid :: HasUid a (Uid u) => Uid u -> a -> Bool
hasUid u x = x^.uid == u

userById :: Uid User -> Lens' GlobalState User
userById uid' = singular $
  users.each . filtered (hasUid uid') `failing`
  error ("userById: couldn't find user with uid " ++
         T.unpack (uidToText uid'))

userByNick :: Text -> Lens' GlobalState User
userByNick nick' = singular $
  userByNick' nick' `failing`
  error ("userById: couldn't find user with nick " ++
         T.unpack nick')

userByNick' :: Text -> Traversal' GlobalState User
userByNick' nick' = users.each . filtered ((== nick') . view nick)

gameById :: Uid Game -> Lens' GlobalState Game
gameById uid' = singular $
  games.each . filtered (hasUid uid') `failing`
  error ("gameById: couldn't find game with uid " ++
         T.unpack (uidToText uid'))

sampleState :: GlobalState
sampleState = GlobalState {
  _users = [
      User {
          _userUid = "user-cooler-100",
          _userNick = "cooler",
          _userName = "Mr. Cooler",
          _userCreated = read "2016-05-20 12:20:06 UTC",
          _userEmail = "cooler@gmail.com",
          _userPass = encryptPass' salt (Pass "password"),
          _userAdmin = True } ],
  _games = [
      Game {
          _gameUid = "game-awesome-100",
          _gameTitle = "Awesome game",
          _gameCreatedBy = "user-cooler-100",
          _gameWordReq = Just $ WordReq {
              _wordReqUserWords = mempty,
              _wordReqWordsPerUser = 10 },
          _gameRegisterUntil = read "2016-06-03 12:20:06 UTC",
          _gameBegun = False,
          _gameEnded = False,
          _gamePlayers = S.fromList ["user-cooler-100"],
          _gameGroups = Nothing },
      Game {
          _gameUid = "game-boring-200",
          _gameTitle = "Boring game",
          _gameCreatedBy = "user-cooler-100",
          _gameWordReq = Nothing,
          _gameRegisterUntil = read "2016-05-25 12:20:06 UTC",
          _gameBegun = True,
          _gameEnded = True,
          _gamePlayers = mempty,
          _gameGroups = Nothing } ],
  _sessions = [],
  _dirty = True }
  where
    salt = Salt "*xxxPxxxxxx)xHL#nx~z2xPxxxxvxxx#"

-- | A useful lens operator that modifies something and returns the old value.
(<<.=) :: MonadState s m => LensLike ((,) a) s s a b -> b -> m a
(<<.=) l b = state (l (,b))
{-# INLINE (<<.=) #-}
infix 4 <<.=

getGlobalState :: Acid.Query GlobalState GlobalState
getGlobalState = view id

getSessions :: Acid.Query GlobalState [(SessionId, UTCTime, Session)]
getSessions = view sessions

setSessions :: [(SessionId, UTCTime, Session)] -> Acid.Update GlobalState ()
setSessions x = sessions .= x

addUser
  :: Uid User       -- ^ New user's uid
  -> Text           -- ^ Nick
  -> Text           -- ^ Name
  -> EncryptedPass  -- ^ Pass
  -> Text           -- ^ Email
  -> UTCTime        -- ^ Creation time
  -> Acid.Update GlobalState User
addUser uid' nick' name' pass' email' now = do
  let user = User {
        _userUid = uid',
        _userNick = nick',
        _userName = name',
        _userEmail = email',
        _userCreated = now,
        _userPass = pass',
        _userAdmin = False }
  users %= (user:)
  return user

addPlayer :: Uid Game -> Uid User -> Acid.Update GlobalState ()
addPlayer gameId userId = do
  gameById gameId . players %= S.insert userId
  gameById gameId . groups .= Nothing

removePlayer :: Uid Game -> Uid User -> Acid.Update GlobalState ()
removePlayer gameId userId = do
  gameById gameId . players %= S.delete userId
  gameById gameId . wordReq . _Just . userWords . at userId .= Nothing
  gameById gameId . groups .= Nothing

getUser :: Uid User -> Acid.Query GlobalState User
getUser uid' = view (userById uid')

getUserByNick :: Text -> Acid.Query GlobalState User
getUserByNick nick' = view (userByNick nick')

getUserByNick' :: Text -> Acid.Query GlobalState (Maybe User)
getUserByNick' nick' = preview (userByNick' nick')

setAdmin :: Uid User -> Bool -> Acid.Update GlobalState ()
setAdmin userId adm = userById userId . admin .= adm

getGame :: Uid Game -> Acid.Query GlobalState Game
getGame uid' = view (gameById uid')

setGameBegun :: Uid Game -> Bool -> Acid.Update GlobalState ()
setGameBegun gameId val = gameById gameId . begun .= val

setGameGroups :: Uid Game -> Maybe [[Uid User]] -> Acid.Update GlobalState ()
setGameGroups gameId val = gameById gameId . groups .= val

setWords :: Uid Game -> Uid User -> [Text] -> Acid.Update GlobalState ()
setWords gameId userId ws =
  gameById gameId.wordReq._Just.userWords.at userId .= Just (S.fromList ws)

setDirty :: Acid.Update GlobalState ()
setDirty = dirty .= True

unsetDirty :: Acid.Update GlobalState Bool
unsetDirty = dirty <<.= False

makeAcidic ''GlobalState [
  'getGlobalState,
  'getSessions, 'setSessions,
  'addUser,
  'addPlayer, 'removePlayer,
  'getUser,
  'getUserByNick, 'getUserByNick',
  'setAdmin,
  'getGame,
  'setGameBegun,
  'setGameGroups,
  'setWords,
  'setDirty, 'unsetDirty
  ]

execCommand :: DB -> String -> IO ()
execCommand db s = do
  let res = execParserPure (prefs showHelpOnError) parserInfo (breakArgs s)
  case res of
    Success ((), io) -> catchAny io $ \e ->
      if isJust (fromException e :: Maybe ExitCode)
        then throwIO e
        else print e
    Failure f -> do
      let (msg, _) = renderFailure f ""
      putStrLn msg
    CompletionInvoked _ -> error "completion invoked"
  where
    breakArgs "" = []
    breakArgs (x:xs)
      | x == '"'  = let (l, r) = break (== '"') xs
                    in read (x:l++"\"") : breakArgs (drop 1 r)
      | isSpace x = breakArgs xs
      | otherwise = let (l, r) = break (\c -> isSpace c || c == '"') xs
                    in (x:l) : breakArgs r

    userArg = argument
      (do input <- T.pack <$> str
          return $ case T.stripPrefix "id:" input of
            Nothing -> Right input
            Just x  -> Left (Uid x))
      (metavar "(NICK|id:ID)")

    findUser = either (Acid.query db . GetUser)
                      (Acid.query db . GetUserByNick)

    addCommand' cmd descr act = addCommand cmd descr (const act) (pure ())

    parserInfo = info parser mempty
    parser = simpleParser (pure ()) $ do

      addCommand' "exit"
        "Stop the server"
        (exitSuccess)

      addCommand' "sessions"
        "Print open sessions"
        (do ss <- Acid.query db GetSessions
            for_ ss $ \(_, time, content) -> do
              printf "  * %s: %s\n" (show time) (show content)
        )

      addCommand "user"
        "Show information about a user"
        (\u -> do
            user <- findUser u
            printf "%s (%s)\n" (user^.name) (user^.nick)
            printf "\n"
            printf "  * uid       %s\n" (uidToText (user^.uid))
            printf "  * email     %s\n" (user^.email)
            printf "  * created   %s\n" (show (user^.created))
            printf "  * admin     %s\n" (show (user^.admin))
        )
        userArg

      addCommand' "users"
        "List users"
        (do us <- view users <$> Acid.query db GetGlobalState
            for_ us $ \u ->
              printf "  * %s (%s)\n" (u^.name) (u^.nick)
        )

      addCommand "users.add"
        "Create a new user"
        (\(nick', name', pass', email') -> do
            uid' <- randomShortUid
            now  <- getCurrentTime
            encPass <- encryptPassIO' (Pass (T.encodeUtf8 pass'))
            Acid.update db $
              AddUser uid' nick' name' encPass email' now
            printf "uid: %s\n" (uidToText uid')
        )
        (fmap (each %~ T.pack) $
           (,,,) <$> strArgument (metavar "NICK")
                 <*> strArgument (metavar "NAME")
                 <*> strArgument (metavar "PASS")
                 <*> strArgument (metavar "EMAIL"))

      addCommand' "games"
        "List games"
        (do gs <- view games <$> Acid.query db GetGlobalState
            for_ gs $ \game ->
              printf "  * %s (%s)\n" (game^.title) (uidToText (game^.uid))
        )

      addCommand "game.reg"
        "Register a user for the game"
        (\(g, u) -> do
            user <- findUser u
            Acid.update db (AddPlayer g (user^.uid))
        )
        ((,) <$> (Uid . T.pack <$> strArgument (metavar "GAME"))
             <*> userArg)
