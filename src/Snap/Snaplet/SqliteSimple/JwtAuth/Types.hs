{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.SqliteSimple.JwtAuth.Types where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Snap
import           Web.JWT as JWT (Secret)

data SqliteJwt = SqliteJwt {
    siteSecret    :: JWT.Secret
  , sqliteJwtConn :: MVar Connection
  }

-- | User account
-- User ID and login name.
--
-- If you need to store additional fields for your user accounts, persist them
-- in your application SQL tables and key them by 'userId'.
data User = User {
    userId    :: Int     -- ^ The database ID of the user account
  , userLogin :: T.Text  -- ^ The login name
  } deriving (Eq, Show)

instance FromJSON User where
  parseJSON (Object v) = User <$> (v .: "id") <*> (v .: "login")
  parseJSON _          = mzero

instance ToJSON User where
  toJSON (User i l) = object [ "id" .= i, "login" .= l ]

data LoginParams = LoginParams {
    lpLogin :: T.Text
  , lpPass  :: T.Text
  }

instance FromJSON LoginParams where
  parseJSON (Object v) = LoginParams <$>
                         v .: "login" <*>
                         v .: "pass"
  parseJSON _          = mzero

-- | Types of errors that can happen on login or new user creation.
data AuthFailure =
    UnknownUser     -- ^ The login name does not exist.
  | DuplicateLogin  -- ^ The login name already exists.
  | WrongPassword   -- ^ Failed the password check.
  deriving (Eq, Show)

data HttpError = HttpError Int String

type H a b = Handler a SqliteJwt b
