module Database.PostgreSQL.PG
( module Row
, module Value
, module PostgreSQL
, PG
, command
, execute
, hoist
, hoistWith
, query
, onIntegrityError
, scalar
, withConnection
, withTransaction
) where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
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

-- | PostgreSQL computations run in the `PG` monad. It's just `Aff` stacked with
-- | `ExceptT` to provide error handling.
-- |
-- | Errors originating from database queries or connection to the database are
-- | modeled with the `PGError` type. Use `runExceptT` from
-- | `Control.Monad.Except.Trans` to turn a `PG a` action into `Aff (Either
-- | PGError a)`.
type PG a = ExceptT PGError Aff a

hoistWith :: forall e m. MonadAff m => MonadError e m => (PGError -> e) -> PG ~> m
hoistWith f m = do
  result <- liftAff $ runExceptT m
  case result of
    Right a -> pure a
    Left pgError -> throwError (f pgError)

hoist :: forall m. MonadAff m => MonadError PGError m => PG ~> m
hoist = hoistWith identity

-- | Run an action with a connection. The connection is released to the pool
-- | when the action returns.
withConnection
    :: forall a
     . Pool
    -> (Connection -> PG a)
    -> PG a
withConnection p k = ExceptT $ P.withConnection p case _ of
  Right conn -> runExceptT $ k conn
  Left pgErr -> pure (Left pgErr)

-- | Run an action within a transaction. The transaction is committed if the
-- | action returns cleanly, and rolled back if the action throws (either a
-- | `PGError` or a JavaScript exception in PG context). If you want to
-- | change the transaction mode, issue a separate `SET TRANSACTION` statement
-- | within the transaction.
withTransaction
    :: forall a
     . Connection
    -> PG a
    -> PG a
withTransaction conn action =
  ExceptT $ join <$> P.withTransaction conn (runExceptT action)

-- | Execute a PostgreSQL query and discard its results.
execute
    :: forall i o
     . (ToSQLRow i)
    => Connection
    -> Query i o
    -> i
    -> PG Unit
execute conn sql values = ExceptT $ P.execute conn sql values >>= case _ of
  Just pgErr -> pure (Left pgErr)
  Nothing -> pure (Right unit)

-- | Execute a PostgreSQL query and return its results.
query
    :: forall i o
     . ToSQLRow i
    => FromSQLRow o
    => Connection
    -> Query i o
    -> i
    -> PG (Array o)
query conn sql = ExceptT <<< P.query conn sql

-- | Execute a PostgreSQL query and return the first field of the first row in
-- | the result.
scalar
    :: forall i o
     . ToSQLRow i
    => FromSQLValue o
    => Connection
    -> Query i (Row1 o)
    -> i
    -> PG (Maybe o)
scalar conn sql = ExceptT <<< P.scalar conn sql

-- | Execute a PostgreSQL query and return its command tag value
-- | (how many rows were affected by the query). This may be useful
-- | for example with `DELETE` or `UPDATE` queries.
command
    :: forall i
     . ToSQLRow i
    => Connection
    -> Query i Int
    -> i
    -> PG Int
command conn sql = ExceptT <<< P.command conn sql

onIntegrityError :: forall a. PG a -> PG a -> PG a
onIntegrityError errorResult db =
    catchError db handleError
    where
    handleError e =
        case e of
            IntegrityError _ -> errorResult
            _ -> throwError e


