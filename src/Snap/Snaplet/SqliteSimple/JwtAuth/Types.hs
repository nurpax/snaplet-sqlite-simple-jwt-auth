{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.SqliteSimple.JwtAuth.Types where

import           Control.Concurrent
import           Control.Monad
import qualified Crypto.BCrypt as BC
import           Data.Aeson
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Snap
import           Web.JWT as JWT (Secret)

data SqliteJwt = SqliteJwt {
    siteSecret    :: JWT.Secret
  , sqliteJwtConn :: MVar Connection
  , options       :: Options
  }

-- | Site configuration options
--
-- Tokens will expire after 'maxTokenExpiration' seconds.  The login handler
-- may be extended in the future to allow setting a lower expiration time.  In
-- that case, 'maxTokenExpiration' will be used as the upper limit for expiry.
-- Otherwise someone might modify login requests to set an infinitely long
-- expiration time and JWTs would never expire.
--
-- You can use the 'Snap.Snaplet.SqliteSimple.JwtAuth.defaults' value as your
-- basis, overriding individual fields as necessary.
data Options = Options {
    hashingPolicy      :: BC.HashingPolicy  -- ^ Which bcrypt hashing policy to use
  , signingKeyFilename :: String            -- ^ Where to save site JWT key
  , maxTokenExpiration :: Int               -- ^ Maximum expiration time for a JWT
  } deriving (Show)

-- | User ID and login name.
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
