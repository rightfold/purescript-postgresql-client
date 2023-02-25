module Database.PostgreSQL.PG
  ( command
  , execute
  , onIntegrityError
  , query
  , scalar
  , withClient
  , withClientTransaction
  , withConnection
  , withTransaction
  ) where

import Prelude
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (class MonadError)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Profunctor (lcmap)
import Database.PostgreSQL.Aff (Client, Connection, PGError(..), Query, fromClient)
import Database.PostgreSQL.Aff (command, execute, query, scalar, withClient, withClientTransaction, withTransaction) as Aff
import Database.PostgreSQL.Pool (Pool)
import Database.PostgreSQL.Row (class FromSQLRow, class ToSQLRow, Row1)
import Database.PostgreSQL.Value (class FromSQLValue)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)

type PG a = Aff (Either PGError a)

hoistPG :: forall a m. MonadAff m => MonadError PGError m => PG a -> m a
hoistPG m = liftAff m >>= either throwError pure

withClient
  :: forall a m
   . MonadError PGError m
  => MonadAff m
  => (m a -> Aff (Either PGError a))
  -> Pool
  -> (Client -> m a)
  -> m a
withClient f p k = do
  res <-
    liftAff
      $ Aff.withClient p case _ of
          Right client -> f $ k client
          Left pgErr -> pure $ Left pgErr
  either throwError pure res

withConnection
  :: forall a m
   . MonadError PGError m
  => MonadAff m
  => (m a -> Aff (Either PGError a))
  -> Pool
  -> (Connection -> m a)
  -> m a
withConnection f p k = withClient f p (lcmap fromClient k)

withTransaction
  :: forall a m
   . MonadAff m
  => MonadError PGError m
  => (m a -> Aff (Either PGError a))
  -> Pool
  -> (Connection -> m a)
  -> m a
withTransaction f pool action = do
  res <-
    liftAff
      $ Aff.withTransaction pool \client -> do
          (f (action client))
  either throwError pure $ join res

withClientTransaction
  :: forall a m
   . MonadAff m
  => MonadError PGError m
  => (m a -> Aff (Either PGError a))
  -> Client
  -> m a
  -> m a
withClientTransaction f client action = do
  res <- liftAff $ Aff.withClientTransaction client (f action)
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
execute h sql values = do
  err <- liftAff $ Aff.execute h sql values
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
query h sql = hoistPG <<< Aff.query h sql

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
scalar h sql = hoistPG <<< Aff.scalar h sql

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
command h sql = hoistPG <<< Aff.command h sql

onIntegrityError
  :: forall a m
   . MonadError PGError m
  => m a
  -> m a
  -> m a
onIntegrityError errorResult db = catchError db handleError
  where
  handleError e = case e of
    IntegrityError _ -> errorResult
    _ -> throwError e
