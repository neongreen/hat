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
  -- * Common types
  Uid(..),
  Url,

  -- * Specific types
  User(..),
    nick,
    name,
    email,
    pass,
    admin,
  Game(..),
    title,
    createdBy,
    begins,
    ended,
    players,
  GlobalState(..),
    games,
    users,
    dirty,

  -- * Common lenses
  uid,
  created,

  -- * Stuff
  sampleState,

  -- * Methods
  GetGlobalState(..),
  GetUser(..),
  GetUserByNick(..),
  GetGame(..),
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
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- Time
import Data.Time
-- Web
import Web.PathPieces
-- acid-state
import Data.Acid as Acid
import Data.SafeCopy
-- Passwords
import Crypto.Scrypt


deriveSafeCopySimple 0 'base ''EncryptedPass

newtype Uid a = Uid {uidToText :: Text}
  deriving (Eq, Ord, Show, PathPiece)

deriveSafeCopySimple 0 'base ''Uid

type Url = Text

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

data Game = Game {
  _gameUid :: Uid Game,
  _gameTitle :: Text,
  _gameCreatedBy :: Uid User,
  _gameBegins :: UTCTime,
  _gameEnded :: Bool,
  _gamePlayers :: [Uid User] }
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
  users.each . filtered ((== nick') . view nick) `failing`
  error ("userById: couldn't find user with nick " ++
         T.unpack nick')

gameById :: Uid Game -> Lens' GlobalState Game
gameById uid' = singular $
  games.each . filtered (hasUid uid') `failing`
  error ("gameById: couldn't find game with uid " ++
         T.unpack (uidToText uid'))

sampleState :: GlobalState
sampleState = GlobalState {
  _users = [
      User {
          _userUid = Uid "user-cooler-100",
          _userNick = "cooler",
          _userName = "Mr. Cooler",
          _userCreated = read "2016-05-20 12:20:06 UTC",
          _userEmail = "cooler@gmail.com",
          _userPass = encryptPass' salt (Pass "password"),
          _userAdmin = True } ],
  _games = [
      Game {
          _gameUid = Uid "game100",
          _gameTitle = "Awesome game",
          _gameCreatedBy = Uid "user-cooler-100",
          _gameBegins = read "2016-06-03 12:20:06 UTC",
          _gameEnded = False,
          _gamePlayers = [] } ],
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

getGame :: Uid Game -> Acid.Query GlobalState Game
getGame uid' = view (gameById uid')

setDirty :: Acid.Update GlobalState ()
setDirty = dirty .= True

unsetDirty :: Acid.Update GlobalState Bool
unsetDirty = dirty <<.= False

makeAcidic ''GlobalState [
  'getGlobalState,
  'getUser,
  'getUserByNick,
  'getGame,
  'setDirty, 'unsetDirty
  ]
