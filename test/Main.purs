module Test.Main
  ( main
  ) where

import Prelude

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Error.Class (catchError, throwError, try)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Decimal as D
import Data.Foldable (all)
import Data.JSDate (toInstant)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Database.PostgreSQL (Connection, POSTGRESQL, PoolConfiguration, Query(Query), Row0(Row0), Row1(Row1), Row2(Row2), Row3(Row3), Row9(Row9), execute, newPool, query, scalar, withConnection, withTransaction)
import Math ((%))
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)
import Test.Unit (suite)
import Test.Unit as Test.Unit
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

withRollback
  :: ∀ eff
   .  Connection
  -> Aff (postgreSQL :: POSTGRESQL | eff) Unit
  -> Aff (postgreSQL :: POSTGRESQL | eff) Unit
withRollback conn action = do
  execute conn (Query "BEGIN TRANSACTION") Row0
  catchError (action >>= const rollback) (\e -> rollback >>= const (throwError e))
  where
  rollback = execute conn (Query "ROLLBACK") Row0

test
  :: ∀ eff
   . Connection
  -> String
  -> Aff ( postgreSQL :: POSTGRESQL | eff) Unit
  -> Test.Unit.TestSuite (postgreSQL :: POSTGRESQL | eff)
test conn t a = Test.Unit.test t (withRollback conn a)

now :: ∀ eff.  Eff (now :: NOW | eff) Instant
now = unsafePartial $ (fromJust <<< toInstant) <$> JSDate.now

main :: ∀ eff. Eff (assert :: ASSERT, avar :: AVAR, console :: CONSOLE, exception :: EXCEPTION, now :: NOW, postgreSQL :: POSTGRESQL, testOutput :: TESTOUTPUT | eff) Unit
main = void $ launchAff do
  pool <- newPool config
  withConnection pool \conn -> do
    execute conn (Query """
     CREATE TEMPORARY TABLE foods (
       name text NOT NULL,
       delicious boolean NOT NULL,
       price NUMERIC(4,2) NOT NULL,
       added TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
       PRIMARY KEY (name)
     )
    """) Row0

    liftEff $ runTest $ do
      suite "Postgresql client" $ do
        let
          testCount n = do
            count <- scalar conn (Query """
              SELECT count(*) = $1
              FROM foods
            """) (Row1 n)
            liftEff <<< assert $ count == Just true

        Test.Unit.test "transaction commit" $ do
          withTransaction conn do
            execute conn (Query """
              INSERT INTO foods (name, delicious, price)
              VALUES ($1, $2, $3)
            """) (Row3 "pork" true (D.fromString "8.30"))
            testCount 1
          testCount 1
          execute conn (Query """
            DELETE FROM foods
          """) Row0

        Test.Unit.test "transaction rollback" $ do
          _ <- try $ withTransaction conn do
            execute conn (Query """
              INSERT INTO foods (name, delicious, price)
              VALUES ($1, $2, $3)
            """) (Row3 "pork" true (D.fromString "8.30"))
            testCount 1
            throwError $ error "fail"
          testCount 0

        let
          insertFood =
            execute conn (Query """
              INSERT INTO foods (name, delicious, price)
              VALUES ($1, $2, $3), ($4, $5, $6), ($7, $8, $9)
            """) (Row9
                "pork" true (D.fromString "8.30")
                "sauerkraut" false (D.fromString "3.30")
                "rookworst" true (D.fromString "5.60"))
        test conn "select column subset" $ do
          insertFood
          names <- query conn (Query """
            SELECT name, delicious
            FROM foods
            WHERE delicious
            ORDER BY name ASC
          """) Row0
          liftEff <<< assert $ names == [Row2 "pork" true, Row2 "rookworst" true]

        test conn "select default instant value" $ do
          before <- liftEff $ (unwrap <<< unInstant) <$> now
          insertFood
          added <- query conn (Query """
            SELECT added
            FROM foods
          """) Row0
          after <- liftEff $ (unwrap <<< unInstant) <$> now
          -- | timestamps are fetched without milliseconds so we have to
          -- | round before value down
          liftEff <<< assert $ all
            (\(Row1 t) ->
              ( unwrap $ unInstant t) >= (before - before % 1000.0)
                && after >= (unwrap $ unInstant t))
            added

        test conn "select decimal" $ do
          insertFood
          sauerkrautPrice <- query conn (Query """
            SELECT price
            FROM foods
            WHERE NOT delicious
          """) Row0
          liftEff <<< assert $ sauerkrautPrice == [Row1 (D.fromString "3.30")]


config :: PoolConfiguration
config =
  { user: "postgres"
  , password: "lol123"
  , host: "127.0.0.1"
  , port: 5432
  , database: "purspg"
  , max: 10
  , idleTimeoutMillis: 1000
  }
