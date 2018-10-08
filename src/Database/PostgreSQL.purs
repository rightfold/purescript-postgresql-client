module Database.PostgreSQL
( module Row
, module Value
, PoolConfiguration
, Pool
, Connection
, Query(..)
, newPool
, withConnection
, withTransaction
, command
, execute
, query
, scalar
, unsafeQuery
) where

import Prelude

import Effect.Aff (Aff, bracket)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (head)
import Data.Either (Either(..))
import Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Database.PostgreSQL.Row (class FromSQLRow, class ToSQLRow, Row0(..), Row1(..), fromSQLRow, toSQLRow)
import Database.PostgreSQL.Row (class FromSQLRow, class ToSQLRow, Row0(..), Row1(..), Row10(..), Row11(..), Row12(..), Row13(..), Row14(..), Row15(..), Row16(..), Row17(..), Row18(..), Row19(..), Row2(..), Row3(..), Row4(..), Row5(..), Row6(..), Row7(..), Row8(..), Row9(..), fromSQLRow, toSQLRow) as Row
import Database.PostgreSQL.Value (class FromSQLValue)
import Database.PostgreSQL.Value (class FromSQLValue, class ToSQLValue, fromSQLValue, instantFromString, instantToString, null, toSQLValue, unsafeIsBuffer) as Value


-- | PostgreSQL connection pool configuration.
type PoolConfiguration =
    { user              :: String
    , password          :: String
    , host              :: String
    , port              :: Int
    , database          :: String
    , max               :: Int
    , idleTimeoutMillis :: Int
    }

-- | PostgreSQL connection pool.
foreign import data Pool :: Type

-- | PostgreSQL connection.
foreign import data Connection :: Type

-- | PostgreSQL query with parameter (`$1`, `$2`, …) and return types.
newtype Query i o = Query String

derive instance newtypeQuery :: Newtype (Query i o) _

-- | Create a new connection pool.
newPool :: PoolConfiguration -> Aff Pool
newPool = liftEffect <<< ffiNewPool

foreign import ffiNewPool
    :: PoolConfiguration
    -> Effect Pool

-- | Run an action with a connection. The connection is released to the pool
-- | when the action returns.
withConnection
    :: ∀ a
     . Pool
    -> (Connection -> Aff a)
    -> Aff a
withConnection p k =
  bracket
    (connect p)
    (liftEffect <<< _.done)
    (k <<< _.connection)

connect
    :: Pool
    -> Aff
      { connection :: Connection
      , done :: Effect Unit
      }
connect = fromEffectFnAff <<< ffiConnect

foreign import ffiConnect
  :: Pool
  -> EffectFnAff
      { connection :: Connection
      , done :: Effect Unit
      }

-- | Run an action within a transaction. The transaction is committed if the
-- | action returns, and rolled back when the action throws. If you want to
-- | change the transaction mode, issue a separate `SET TRANSACTION` statement
-- | within the transaction.
withTransaction
    :: ∀ a
     . Connection
    -> Aff a
    -> Aff a
withTransaction conn action =
    execute conn (Query "BEGIN TRANSACTION") Row0
    *> catchError (Right <$> action) (pure <<< Left) >>= case _ of
        Right a -> execute conn (Query "COMMIT TRANSACTION") Row0 $> a
        Left e -> execute conn (Query "ROLLBACK TRANSACTION") Row0 *> throwError e

-- | Execute a PostgreSQL query and discard its results.
execute
    :: ∀ i o
     . (ToSQLRow i)
    => Connection
    -> Query i o
    -> i
    -> Aff Unit
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
    -> Aff (Array o)
query conn (Query sql) values =
    unsafeQuery conn sql (toSQLRow values)
    >>= traverse (fromSQLRow >>> case _ of
          Right row -> pure row
          Left  msg -> throwError (error msg))

-- | Execute a PostgreSQL query and return the first field of the first row in
-- | the result.
scalar
    :: ∀ i o
     . ToSQLRow i
    => FromSQLValue o
    => Connection
    -> Query i (Row1 o)
    -> i
    -> Aff (Maybe o)
scalar conn sql values =
    query conn sql values
    <#> map (case _ of Row1 a -> a) <<< head

unsafeQuery
    :: Connection
    -> String
    -> Array Foreign
    -> Aff (Array (Array Foreign))
unsafeQuery c s = fromEffectFnAff <<< ffiUnsafeQuery c s

foreign import ffiUnsafeQuery
    :: Connection
    -> String
    -> Array Foreign
    -> EffectFnAff (Array (Array Foreign))

-- | Execute a PostgreSQL query and return its command tag value.
command
    :: ∀ i
     . ToSQLRow i
    => Connection
    -> Query i Int
    -> i
    -> Aff Int
command conn (Query sql) values =
    unsafeCommand conn sql (toSQLRow values)

unsafeCommand
    :: Connection
    -> String
    -> Array Foreign
    -> Aff Int
unsafeCommand c s = fromEffectFnAff <<< ffiUnsafeCommand c s

foreign import ffiUnsafeCommand
    :: Connection
    -> String
    -> Array Foreign
    -> EffectFnAff Int
