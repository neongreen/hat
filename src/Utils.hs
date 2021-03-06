{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
GeneralizedNewtypeDeriving,
FlexibleContexts,
FlexibleInstances,
TypeFamilies,
RecordWildCards,
NoImplicitPrelude
  #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Utils
(
  -- * Lists
  ordNub,

  -- * 'Eq'
  equating,

  -- * Text
  plural,

  -- * URLs
  Url,
  makeSlug,
  
  -- * UID
  Uid(..),
  Node,
  randomShortUid,
  randomLongUid,
  uid_,

  -- * Lucid
  includeJS,
  includeCSS,

  -- * Instances
  -- ** 'MonadRandom' for 'HtmlT'
  -- ** 'MonadRandom' for 'ActionCtxT'
  -- ** 'SafeCopy' for 'EncryptedPass'
)
where


-- General
import BasePrelude
-- Monads and monad transformers
import Control.Monad.Trans
import Control.Monad.Random
-- Hashable (needed for Uid)
import Data.Hashable
-- Containers
import qualified Data.Set as S
-- Text
import Data.Text.All (Text)
import qualified Data.Text.All as T
-- Web
import Lucid
import Web.Spock
import Web.PathPieces
-- acid-state
import Data.SafeCopy
-- Passwords
import Crypto.Scrypt


ordNub :: Ord a => [a] -> [a]
ordNub = go mempty
  where
    go _ [] = []
    go s (x:xs) | x `S.member` s = go s xs
                | otherwise      = x : go (S.insert x s) xs

equating :: Eq b => (a -> b) -> (a -> a -> Bool)
equating f = (==) `on` f

plural :: Int -> Text -> Text
plural 1 x = x
plural _ "is" = "are"
plural _ "was" = "were"
plural _ x = x <> "s"

type Url = Text

-- | Make text suitable for inclusion into an URL (by turning spaces into
-- hyphens and so on)
makeSlug :: Text -> Text
makeSlug =
  T.intercalate "-" . T.words .
  T.filter (\c -> isLetter c || isDigit c || c == ' ' || c == '-') .
  T.toLower .
  T.map (\x -> if x == '_' || x == '/' then '-' else x)

-- | Unique id, used for many things – categories, items, and anchor ids.
newtype Uid a = Uid {uidToText :: Text}
  deriving (Eq, Ord, Show, PathPiece, T.Buildable, Hashable)

deriveSafeCopySimple 0 'base ''Uid

instance IsString (Uid a) where
  fromString = Uid . T.pack

randomText :: MonadRandom m => Int -> m Text
randomText n = do
  -- We don't want the 1st char to be a digit. Just in case (I don't really
  -- have a good reason). Maybe to prevent Javascript from doing automatic
  -- conversions or something (tho it should never happen).
  x <- getRandomR ('a', 'z')
  let randomChar = do
        i <- getRandomR (0, 35)
        return $ if i < 10 then toEnum (fromEnum '0' + i)
                           else toEnum (fromEnum 'a' + i - 10)
  xs <- replicateM (n-1) randomChar
  return (T.pack (x:xs))

randomLongUid :: MonadRandom m => m (Uid a)
randomLongUid = Uid <$> randomText 12

-- These are only used for items and categories (because their uids can occur
-- in links and so they should look a bit nicer).
randomShortUid :: MonadRandom m => m (Uid a)
randomShortUid = Uid <$> randomText 8

-- | A marker for Uids that would be used with HTML nodes
data Node

uid_ :: Uid Node -> Attribute
uid_ = id_ . uidToText

includeJS :: Monad m => Url -> HtmlT m ()
includeJS url = with (script_ "") [src_ url]

includeCSS :: Monad m => Url -> HtmlT m ()
includeCSS url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

instance MonadRandom m => MonadRandom (HtmlT m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

instance MonadRandom (ActionCtxT a (WebStateM b c d)) where
  getRandom = liftIO getRandom
  getRandoms = liftIO getRandoms
  getRandomR = liftIO . getRandomR
  getRandomRs = liftIO . getRandomRs

deriveSafeCopySimple 0 'base ''EncryptedPass
