module Database.PostgreSQL
( module Row
, module Value
, PG
, PGError(..)
, PGErrorDetail
, Database
, PoolConfiguration
, Pool
, Connection
, Query(..)
, newPool
, withConnection
, withTransaction
, defaultPoolConfiguration
, command
, execute
, query
, scalar
, onIntegrityError
) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Except.Trans (ExceptT, except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Database.PostgreSQL.Row (class FromSQLRow, class ToSQLRow, Row0(..), Row1(..), Row10(..), Row11(..), Row12(..), Row13(..), Row14(..), Row15(..), Row16(..), Row17(..), Row18(..), Row19(..), Row2(..), Row3(..), Row4(..), Row5(..), Row6(..), Row7(..), Row8(..), Row9(..), fromSQLRow, toSQLRow) as Row
import Database.PostgreSQL.Row (class FromSQLRow, class ToSQLRow, Row0(..), Row1(..), fromSQLRow, toSQLRow)
import Database.PostgreSQL.Value (class FromSQLValue)
import Database.PostgreSQL.Value (class FromSQLValue, class ToSQLValue, fromSQLValue, instantFromString, instantToString, null, toSQLValue, unsafeIsBuffer) as Value
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign (Foreign)

type Database = String

-- | PostgreSQL computations run in the `PG` monad. It's just `Aff` stacked with
-- | `ExceptT` to provide error handling.
-- |
-- | Errors originating from database queries or connection to the database are
-- | modeled with the `PGError` type. Use `runExceptT` from
-- | `Control.Monad.Except.Trans` to turn a `PG a` action into `Aff (Either
-- | PGError a)`.
type PG a = ExceptT PGError Aff a

-- | PostgreSQL connection pool configuration.
type PoolConfiguration =
    { database :: Database
    , host :: Maybe String
    , idleTimeoutMillis :: Maybe Int
    , max :: Maybe Int
    , password :: Maybe String
    , port :: Maybe Int
    , user :: Maybe String
    }

defaultPoolConfiguration :: Database -> PoolConfiguration
defaultPoolConfiguration database =
    { database
    , host: Nothing
    , idleTimeoutMillis: Nothing
    , max: Nothing
    , password: Nothing
    , port: Nothing
    , user: Nothing
    }

-- | PostgreSQL connection pool.
foreign import data Pool :: Type

-- | PostgreSQL connection.
foreign import data Connection :: Type

-- | PostgreSQL query with parameter (`$1`, `$2`, …) and return types.
newtype Query i o = Query String

derive instance newtypeQuery :: Newtype (Query i o) _

-- | Create a new connection pool.
newPool :: PoolConfiguration -> Effect Pool
newPool cfg =
    ffiNewPool $ cfg'
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

-- | Configuration which we actually pass to FFI.
type PoolConfiguration' =
    { user :: Nullable String
    , password :: Nullable String
    , host :: Nullable String
    , port :: Nullable Int
    , database :: String
    , max :: Nullable Int
    , idleTimeoutMillis :: Nullable Int
    }

foreign import ffiNewPool
    :: PoolConfiguration'
    -> Effect Pool

-- | Run an action with a connection. The connection is released to the pool
-- | when the action returns.
withConnection
    :: ∀ a
     . Pool
    -> (Connection -> PG a)
    -> PG a
withConnection p k =
    except <=< lift $ bracket (connect p) cleanup run
    where
    cleanup (Left _) = pure unit
    cleanup (Right { done }) = liftEffect done

    run (Left err) = pure $ Left err
    run (Right { connection }) = runExceptT $ k connection

connect
    :: Pool
    -> Aff (Either PGError ConnectResult)
connect =
    fromEffectFnAff
    <<< ffiConnect
            { nullableLeft: toNullable <<< map Left <<< convertError
            , right: Right
            }

type ConnectResult =
    { connection :: Connection
    , done :: Effect Unit
    }

foreign import ffiConnect
    :: ∀ a
     . { nullableLeft :: Error -> Nullable (Either PGError ConnectResult)
       , right :: a -> Either PGError ConnectResult
       }
    -> Pool
    -> EffectFnAff (Either PGError ConnectResult)

-- | Run an action within a transaction. The transaction is committed if the
-- | action returns cleanly, and rolled back if the action throws (either a
-- | `PGError` or a JavaScript exception in the Aff context). If you want to
-- | change the transaction mode, issue a separate `SET TRANSACTION` statement
-- | within the transaction.
withTransaction
    :: ∀ a
     . Connection
    -> PG a
    -> PG a
withTransaction conn action =
    begin *> lift (try $ runExceptT action) >>= case _ of
        Left jsErr -> do
            rollback
            lift $ throwError jsErr
        Right (Left pgErr) -> do
            rollback
            throwError pgErr
        Right (Right value) -> do
            commit
            pure value

    where
    begin = execute conn (Query "BEGIN TRANSACTION") Row0
    commit = execute conn (Query "COMMIT TRANSACTION") Row0
    rollback = execute conn (Query "ROLLBACK TRANSACTION") Row0

-- | Execute a PostgreSQL query and discard its results.
execute
    :: ∀ i o
     . (ToSQLRow i)
    => Connection
    -> Query i o
    -> i
    -> PG Unit
execute conn (Query sql) values =
    void $ unsafeQuery conn sql (toSQLRow values)

-- | Execute a PostgreSQL query and return its results.
query
    :: ∀ i o
     . ToSQLRow i
    => FromSQLRow o
    => Connection
    -> Query i o
    -> i
    -> PG (Array o)
query conn (Query sql) values = do
    _.rows <$> unsafeQuery conn sql (toSQLRow values)
    >>= traverse (fromSQLRow >>> case _ of
          Right row -> pure row
          Left  msg -> throwError $ ConversionError msg)

-- | Execute a PostgreSQL query and return the first field of the first row in
-- | the result.
scalar
    :: ∀ i o
     . ToSQLRow i
    => FromSQLValue o
    => Connection
    -> Query i (Row1 o)
    -> i
    -> PG (Maybe o)
scalar conn sql values =
    query conn sql values
    <#> map (case _ of Row1 a -> a) <<< head

-- | Execute a PostgreSQL query and return its command tag value
-- | (how many rows were affected by the query). This may be useful
-- | for example with `DELETE` or `UPDATE` queries.
command
    :: ∀ i
     . ToSQLRow i
    => Connection
    -> Query i Int
    -> i
    -> PG Int
command conn (Query sql) values =
    _.rowCount <$> unsafeQuery conn sql (toSQLRow values)

type QueryResult =
    { rows :: Array (Array Foreign)
    , rowCount :: Int
    }

unsafeQuery
    :: Connection
    -> String
    -> Array Foreign
    -> PG QueryResult
unsafeQuery c s =
    except <=< lift <<< fromEffectFnAff <<< ffiUnsafeQuery p c s
    where
    p =
        { nullableLeft: toNullable <<< map Left <<< convertError
        , right: Right
        }

foreign import ffiUnsafeQuery
    :: { nullableLeft :: Error -> Nullable (Either PGError QueryResult)
       , right :: QueryResult -> Either PGError QueryResult
       }
    -> Connection
    -> String
    -> Array Foreign
    -> EffectFnAff (Either PGError QueryResult)

data PGError
    = ConnectionError String
    | ConversionError String
    | InternalError PGErrorDetail
    | OperationalError PGErrorDetail
    | ProgrammingError PGErrorDetail
    | IntegrityError PGErrorDetail
    | DataError PGErrorDetail
    | NotSupportedError PGErrorDetail
    | QueryCanceledError PGErrorDetail
    | TransactionRollbackError PGErrorDetail

derive instance eqPGError :: Eq PGError
derive instance genericPGError :: Generic PGError _

instance showPGError :: Show PGError where
    show = genericShow

type PGErrorDetail =
    { severity :: String
    , code :: String
    , message :: String
    , detail :: String
    , hint :: String
    , position :: String
    , internalPosition :: String
    , internalQuery :: String
    , where_ :: String
    , schema :: String
    , table :: String
    , column :: String
    , dataType :: String
    , constraint :: String
    , file :: String
    , line :: String
    , routine :: String
    }

foreign import ffiSQLState :: Error -> Nullable String
foreign import ffiErrorDetail :: Error -> PGErrorDetail

convertError :: Error -> Maybe PGError
convertError err =
    case toMaybe $ ffiSQLState err of
        Nothing -> Nothing
        Just sqlState -> Just $ convert sqlState $ ffiErrorDetail err

  where
      convert :: String -> PGErrorDetail -> PGError
      convert s =
          if prefix "0A" s then NotSupportedError
          else if prefix "20" s || prefix "21" s then ProgrammingError
          else if prefix "22" s then DataError
          else if prefix "23" s then IntegrityError
          else if prefix "24" s || prefix "25" s then InternalError
          else if prefix "26" s || prefix "27" s || prefix "28" s then OperationalError
          else if prefix "2B" s || prefix "2D" s || prefix "2F" s then InternalError
          else if prefix "34" s then OperationalError
          else if prefix "38" s || prefix "39" s || prefix "3B" s then InternalError
          else if prefix "3D" s || prefix "3F" s then ProgrammingError
          else if prefix "40" s then TransactionRollbackError
          else if prefix "42" s || prefix "44" s then ProgrammingError
          else if s == "57014" then QueryCanceledError
          else if prefix "5" s then OperationalError
          else if prefix "F" s then InternalError
          else if prefix "H" s then OperationalError
          else if prefix "P" s then InternalError
          else if prefix "X" s then InternalError
          else const $ ConnectionError s

      prefix :: String -> String -> Boolean
      prefix p =
          maybe false (_ == 0) <<< String.indexOf (Pattern p)

onIntegrityError :: forall a. PG a -> PG a -> PG a
onIntegrityError errorResult db =
    catchError db handleError
    where
    handleError e =
        case e of
            IntegrityError _ -> errorResult
            _ -> throwError e
