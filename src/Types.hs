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


module Types
(
  -- * Types
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
    begins,
    ended,
    players,
    wordReq,
  GlobalState(..),
    games,
    users,
    dirty,

  -- * Common lenses
  uid,
  created,

  -- * Stuff
  sampleState,
  userById,

  -- * Methods
  GetGlobalState(..),
  GetUser(..),
  GetUserByNick(..), GetUserByNick'(..),
  GetGame(..),
  SetWords(..),
  SetDirty(..),
  UnsetDirty(..),
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
import qualified Data.Text as T
import Data.Text (Text)
-- Time
import Data.Time
-- acid-state
import Data.Acid as Acid
import Data.SafeCopy
-- Passwords
import Crypto.Scrypt

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
  _gameBegins :: UTCTime,
  _gameEnded :: Bool,
  _gamePlayers :: Set (Uid User) }
  deriving (Show)

deriveSafeCopySimple 0 'base ''Game
makeFields ''Game

data GlobalState = GlobalState {
  _users :: [User],
  _games :: [Game],
  _dirty :: Bool }
  deriving (Show)

deriveSafeCopySimple 0 'base ''GlobalState
makeLenses ''GlobalState

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
          _gameBegins = read "2016-06-03 12:20:06 UTC",
          _gameEnded = False,
          _gamePlayers = S.fromList ["user-cooler-100"] },
      Game {
          _gameUid = "game-boring-200",
          _gameTitle = "Boring game",
          _gameCreatedBy = "user-cooler-100",
          _gameWordReq = Nothing,
          _gameBegins = read "2016-05-25 12:20:06 UTC",
          _gameEnded = True,
          _gamePlayers = mempty } ],
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

getUser :: Uid User -> Acid.Query GlobalState User
getUser uid' = view (userById uid')

getUserByNick :: Text -> Acid.Query GlobalState User
getUserByNick nick' = view (userByNick nick')

getUserByNick' :: Text -> Acid.Query GlobalState (Maybe User)
getUserByNick' nick' = preview (userByNick' nick')

getGame :: Uid Game -> Acid.Query GlobalState Game
getGame uid' = view (gameById uid')

setWords :: Uid Game -> Uid User -> [Text] -> Acid.Update GlobalState ()
setWords gameId userId ws =
  gameById gameId.wordReq._Just.userWords.at userId .= Just (S.fromList ws)

setDirty :: Acid.Update GlobalState ()
setDirty = dirty .= True

unsetDirty :: Acid.Update GlobalState Bool
unsetDirty = dirty <<.= False

makeAcidic ''GlobalState [
  'getGlobalState,
  'getUser,
  'getUserByNick, 'getUserByNick',
  'getGame,
  'setWords,
  'setDirty, 'unsetDirty
  ]
