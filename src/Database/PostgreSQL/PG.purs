module Database.PostgreSQL.PG
( module Row
, module Value
, module PostgreSQL
, command
, execute
, query
, onIntegrityError
, scalar
, withConnection
, withTransaction
) where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Database.PostgreSQL (Connection, PGError(..), Pool, Query)
import Database.PostgreSQL (class FromSQLRow, class FromSQLValue, class ToSQLRow, class ToSQLValue, Connection, Database, PGError(..), PGErrorDetail, Pool, PoolConfiguration, Query(..), Row0(..), Row1(..), Row10(..), Row11(..), Row12(..), Row13(..), Row14(..), Row15(..), Row16(..), Row17(..), Row18(..), Row19(..), Row2(..), Row3(..), Row4(..), Row5(..), Row6(..), Row7(..), Row8(..), Row9(..), defaultPoolConfiguration, fromSQLRow, fromSQLValue, instantFromString, instantToString, newPool, null, toSQLRow, toSQLValue, unsafeIsBuffer) as PostgreSQL
import Database.PostgreSQL (command, execute, query, scalar, withConnection, withTransaction) as P
import Database.PostgreSQL.Row (class FromSQLRow, class ToSQLRow, Row0(..), Row1(..), Row10(..), Row11(..), Row12(..), Row13(..), Row14(..), Row15(..), Row16(..), Row17(..), Row18(..), Row19(..), Row2(..), Row3(..), Row4(..), Row5(..), Row6(..), Row7(..), Row8(..), Row9(..), fromSQLRow, toSQLRow) as Row
import Database.PostgreSQL.Row (class FromSQLRow, class ToSQLRow, Row1)
import Database.PostgreSQL.Value (class FromSQLValue)
import Database.PostgreSQL.Value (class FromSQLValue, class ToSQLValue, fromSQLValue, instantFromString, instantToString, null, toSQLValue, unsafeIsBuffer) as Value
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)

type Database = String

hoistAffEither :: forall a m. MonadAff m => MonadError PGError m => Aff (Either PGError a) -> m a
hoistAffEither m = liftAff m >>= either throwError pure

-- | Run an action with a connection. The connection is released to the pool
-- | when the action returns.
withConnection
    :: forall a m
     . MonadError PGError m
    => MonadAff m
    => Pool
    -> (m a -> Aff (Either PGError a))
    -> (Connection -> m a)
    -> m a
withConnection p f k = do
  res <- liftAff $ P.withConnection p case _ of
    Right conn -> f $ k conn
    Left pgErr -> pure $ Left pgErr
  either throwError pure res

-- | Run an action within a transaction. The transaction is committed if the
-- | action returns cleanly, and rolled back if the action throws (either a
-- | `PGError` or a JavaScript exception in PG context). If you want to
-- | change the transaction mode, issue a separate `SET TRANSACTION` statement
-- | within the transaction.
withTransaction
    :: forall a m
     . MonadAff m
    => MonadError PGError m
    => Connection
    -> (m a -> Aff (Either PGError a))
    -> m a
    -> m a
withTransaction conn f action = do
  res <- liftAff $ P.withTransaction conn (f action)
  either throwError pure $ join res

-- | Execute a PostgreSQL query and discard its results.
execute
    :: forall i o m
     . ToSQLRow i
    => MonadError PGError m
    => MonadAff m
    => Connection
    -> Query i o
    -> i
    -> m Unit
execute conn sql values = do
  err <- liftAff $ P.execute conn sql values
  maybe (pure unit) throwError err

-- | Execute a PostgreSQL query and return its results.
query
    :: forall i o m
     . ToSQLRow i
    => FromSQLRow o
    => MonadError PGError m
    => MonadAff m
    => Connection
    -> Query i o
    -> i
    -> m (Array o)
query conn sql = hoistAffEither <<< P.query conn sql

-- | Execute a PostgreSQL query and return the first field of the first row in
-- | the result.
scalar
    :: forall i o m
     . ToSQLRow i
    => FromSQLValue o
    => MonadError PGError m
    => MonadAff m
    => Connection
    -> Query i (Row1 o)
    -> i
    -> m (Maybe o)
scalar conn sql = hoistAffEither <<< P.scalar conn sql

-- | Execute a PostgreSQL query and return its command tag value
-- | (how many rows were affected by the query). This may be useful
-- | for example with `DELETE` or `UPDATE` queries.
command
    :: forall i m
     . ToSQLRow i
    => MonadError PGError m
    => MonadAff m
    => Connection
    -> Query i Int
    -> i
    -> m Int
command conn sql = hoistAffEither <<< P.command conn sql

onIntegrityError 
    :: forall a m
     . MonadError PGError m
    => m a
    -> m a
    -> m a
onIntegrityError errorResult db =
    catchError db handleError
    where
    handleError e =
        case e of
            IntegrityError _ -> errorResult
            _ -> throwError e
