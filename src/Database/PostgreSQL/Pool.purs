module Database.PostgreSQL.Pool
  ( defaultConfiguration
  , Database
  , idleCount
  , new
  , Configuration
  , parseURI
  , PGConnectionURI
  , Pool
  , totalCount
  , waitingCount
  ) where

import Prelude (bind, flip, pure, ($))

import Data.Either (hush)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.String.CodeUnits (singleton)
import Data.Traversable (foldMap)
import Effect (Effect)
import StringParser (runParser)
import StringParser.CodePoints (anyChar, char, string)
import StringParser.Combinators (many, manyTill)

-- | PostgreSQL connection pool.
foreign import data Pool :: Type

type Database = String

-- | Configuration which we actually pass to FFI.
type Configuration' =
  { user :: Nullable String
  , password :: Nullable String
  , host :: Nullable String
  , port :: Nullable Int
  , database :: String
  , max :: Nullable Int
  , idleTimeoutMillis :: Nullable Int
  }

-- | PostgreSQL connection pool configuration.
type Configuration =
  { database :: Database
  , host :: Maybe String
  , idleTimeoutMillis :: Maybe Int
  , max :: Maybe Int
  , password :: Maybe String
  , port :: Maybe Int
  , user :: Maybe String
  }

type PGConnectionURI = String

-- | Get the default pool configuration from postgres connection uri
-- | TODO:
-- | * Do we really want to keep parsing dependency to handle config string?
-- | * In such a case we should improve parsing (validate port etc.)
parseURI :: PGConnectionURI -> Maybe Configuration
parseURI uri =
  hush
    $ flip runParser uri do
        _ <- string "postgres://"
        user <- tillChar (char ':')
        password <- tillChar (char '@')
        host <- tillChar (char ':')
        port <- tillChar (char '/')
        database <- many anyChar
        pure
          { database: toStr database
          , host: Just $ toStr host
          , idleTimeoutMillis: Nothing
          , max: Nothing
          , password: Just $ toStr password
          , port: fromString $ toStr port
          , user: Just $ toStr user
          }
  where
  tillChar = manyTill anyChar

  toStr = foldMap singleton

defaultConfiguration :: Database -> Configuration
defaultConfiguration database =
  { database
  , host: Nothing
  , idleTimeoutMillis: Nothing
  , max: Nothing
  , password: Nothing
  , port: Nothing
  , user: Nothing
  }

foreign import ffiNew
  :: Configuration'
  -> Effect Pool

-- | Create a new connection pool.
new :: Configuration -> Effect Pool
new cfg = ffiNew $ cfg'
  where
  cfg' =
    { user: toNullable cfg.user
    , password: toNullable cfg.password
    , host: toNullable cfg.host
    , port: toNullable cfg.port
    , database: cfg.database
    , max: toNullable cfg.max
    , idleTimeoutMillis: toNullable cfg.idleTimeoutMillis
    }

foreign import totalCount :: Pool -> Effect Int

foreign import idleCount :: Pool -> Effect Int

foreign import waitingCount :: Pool -> Effect Int

