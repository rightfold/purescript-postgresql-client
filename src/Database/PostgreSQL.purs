module Database.PostgreSQL
( module Row
, module Value
, POSTGRESQL
, PoolConfiguration
, Pool
, Connection
, Query(..)
, newPool
, withConnection
, withTransaction
, execute
, query
, scalar
, unsafeQuery
) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (catchError, throwError, withResource)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Database.PostgreSQL.Row (class FromSQLRow, class ToSQLRow, Row0(..), Row1(..), fromSQLRow, toSQLRow)
import Database.PostgreSQL.Row as Row
import Database.PostgreSQL.Value (class FromSQLValue)
import Database.PostgreSQL.Value as Value
import Prelude

foreign import data POSTGRESQL :: Effect

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
newPool :: ∀ eff. PoolConfiguration -> Aff (postgreSQL :: POSTGRESQL | eff) Pool
newPool = liftEff <<< ffiNewPool

foreign import ffiNewPool
    :: ∀ eff
     . PoolConfiguration
    -> Eff (postgreSQL :: POSTGRESQL | eff) Pool

-- | Run an action with a connection. The connection is released to the pool
-- | when the action returns.
withConnection
    :: ∀ eff a
     . Pool
    -> (Connection -> Aff (postgreSQL :: POSTGRESQL | eff) a)
    -> Aff (postgreSQL :: POSTGRESQL | eff) a
withConnection p k =
  withResource (makeAff $ ffiConnect p)
               (liftEff <<< _.done)
               (k <<< _.connection)

foreign import ffiConnect
    :: ∀ eff
     . Pool
    -> (Error -> Eff (postgreSQL :: POSTGRESQL | eff) Unit)
    -> (    { connection :: Connection
            , done :: Eff (postgreSQL :: POSTGRESQL | eff) Unit
            }
         -> Eff (postgreSQL :: POSTGRESQL | eff) Unit
       )
    -> Eff (postgreSQL :: POSTGRESQL | eff) Unit

-- | Run an action within a transaction. The transaction is committed if the
-- | action returns, and rolled back when the action throws. If you want to
-- | change the transaction mode, issue a separate `SET TRANSACTION` statement
-- | within the transaction.
withTransaction
    :: ∀ eff a
     . Connection
    -> Aff (postgreSQL :: POSTGRESQL | eff) a
    -> Aff (postgreSQL :: POSTGRESQL | eff) a
withTransaction conn action =
    execute conn (Query "BEGIN TRANSACTION") Row0
    *> catchError (Right <$> action) (pure <<< Left) >>= case _ of
        Right a -> execute conn (Query "COMMIT TRANSACTION") Row0 $> a
        Left e -> execute conn (Query "ROLLBACK TRANSACTION") Row0 *> throwError e

-- | Execute a PostgreSQL query and discard its results.
execute
    :: ∀ i o eff
     . (ToSQLRow i)
    => Connection
    -> Query i o
    -> i
    -> Aff (postgreSQL :: POSTGRESQL | eff) Unit
execute conn (Query sql) values =
    void $ unsafeQuery conn sql (toSQLRow values)

-- | Execute a PostgreSQL query and return its results.
query
    :: ∀ i o eff
     . ToSQLRow i
    => FromSQLRow o
    => Connection
    -> Query i o
    -> i
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Array o)
query conn (Query sql) values =
    unsafeQuery conn sql (toSQLRow values)
    >>= traverse (fromSQLRow >>> case _ of
          Right row -> pure row
          Left  msg -> throwError (error msg))

-- | Execute a PostgreSQL query and return the first field of the first row in
-- | the result.
scalar
    :: ∀ i o eff
     . ToSQLRow i
    => FromSQLValue o
    => Connection
    -> Query i (Row1 o)
    -> i
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Maybe o)
scalar conn sql values =
    query conn sql values
    <#> map (case _ of Row1 a -> a) <<< head

unsafeQuery
  :: ∀ eff
   . Connection
  -> String
  -> Array Foreign
  -> Aff (postgreSQL :: POSTGRESQL | eff) (Array (Array Foreign))
unsafeQuery c s a = makeAff $ ffiUnsafeQuery c s a

foreign import ffiUnsafeQuery
    :: ∀ eff
     . Connection
    -> String
    -> Array Foreign
    -> (Error -> Eff (postgreSQL :: POSTGRESQL | eff) Unit)
    -> (Array (Array Foreign) -> Eff (postgreSQL :: POSTGRESQL | eff) Unit)
    -> Eff (postgreSQL :: POSTGRESQL | eff) Unit

fromRight :: ∀ a b. Either a b -> Maybe b
fromRight (Left _) = Nothing
fromRight (Right a) = Just a
