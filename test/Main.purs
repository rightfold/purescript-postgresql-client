module Test.Main
  ( main
  ) where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (zip)
import Data.Date (Date, canonicalDate)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Decimal as D
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Foldable (all, length)
import Data.JSDate (JSDate, jsdate, toInstant)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (Connection, PG, PGError(..), PoolConfiguration, Query(Query), Row0(Row0), Row1(Row1), Row2(Row2), Row3(Row3), Row9(Row9), command, defaultPoolConfiguration, execute, newPool, onIntegrityError, query, scalar, withConnection, withTransaction)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Foreign.Object (Object, fromFoldable)
import Math ((%))
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)
import Test.Example (run) as Example
import Test.Unit (TestSuite, suite)
import Test.Unit as Test.Unit
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)


withRollback
  ∷ Connection
  → PG Unit
  → PG Unit
withRollback conn action =
    begin *> action *> rollback
    where
        begin = execute conn (Query "BEGIN TRANSACTION") Row0
        rollback = execute conn (Query "ROLLBACK TRANSACTION") Row0

test
  ∷ Connection
  → String
  → PG Unit
  → TestSuite
test conn name action =
    Test.Unit.test name $ checkPGErrors $ withRollback conn action

transactionTest
  ∷ String
  → PG Unit
  → TestSuite
transactionTest name action =
    Test.Unit.test name $ checkPGErrors $ action

checkPGErrors :: PG Unit -> Aff Unit
checkPGErrors action = do
    runExceptT action >>= case _ of
        Left pgError -> liftEffect $ assert false
        Right _ -> pure unit

now ∷ Effect Instant
now = unsafePartial $ (fromJust <<< toInstant) <$> JSDate.now

date ∷ Int → Int → Int → Date
date y m d = unsafePartial $ fromJust $ canonicalDate <$> toEnum y <*> toEnum m <*> toEnum d

jsdate_ ∷ Number → Number → Number → Number → Number → Number → Number → JSDate
jsdate_ year month day hour minute second millisecond =
  jsdate { year, month, day, hour, minute, second, millisecond }

main ∷ Effect Unit
main = do
  void $ launchAff do
    -- Running guide from README
    --Example.run

    -- Actual test suite
    pool <- newPool config
    checkPGErrors $ withConnection pool \conn -> do
      execute conn (Query """
        CREATE TEMPORARY TABLE foods (
          name text NOT NULL,
          delicious boolean NOT NULL,
          price NUMERIC(4,2) NOT NULL,
          added TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (name)
        );
        CREATE TEMPORARY TABLE dates (
          date date NOT NULL
        );
        CREATE TEMPORARY TABLE timestamps (
          timestamp timestamptz NOT NULL
        );
        CREATE TEMPORARY TABLE jsons (
          json json NOT NULL,
          jsonb jsonb NOT NULL
        );
      """) Row0

      liftEffect $ runTest $ do
        suite "PostgreSQL client" $ do
          let
            testCount n = do
              count <- scalar conn (Query """
                SELECT count(*) = $1
                FROM foods
              """) (Row1 n)
              liftEffect <<< assert $ count == Just true

          transactionTest "transaction commit" do
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

          transactionTest "transaction rollback on PostgreSQL error" $ do
            _ <- try $ withTransaction conn do
              execute conn (Query """
                INSERT INTO foods (name, delicious, price)
                VALUES ($1, $2, $3)
              """) (Row3 "pork" true (D.fromString "8.30"))
              testCount 1

              -- invalid SQL query --> PGError is thrown
              execute conn (Query "foo bar") Row0

            -- transaction should've been rolled back
            testCount 0

          transactionTest "transaction rollback on JavaScript exception" $ do
            result <- lift $ try $ runExceptT $ withTransaction conn do
              execute conn (Query """
                INSERT INTO foods (name, delicious, price)
                VALUES ($1, $2, $3)
              """) (Row3 "pork" true (D.fromString "8.30"))
              testCount 1

              -- throw a JavaScript error
              lift $ throwError $ error "fail"

            -- make sure the JavaScript error was thrown
            liftEffect $ case result of
                Left jsErr -> assert (message jsErr == "fail")
                Right _ -> assert false

            -- transaction should've been rolled back
            testCount 0

          test conn "usage of rows represented by nested tuples" $ do
            execute conn (Query """
              INSERT INTO foods (name, delicious, price)
              VALUES ($1, $2, $3), ($4, $5, $6), ($7, $8, $9)
            """)
              ( ("pork" /\ true /\ (D.fromString "8.30"))
              /\ ("sauerkraut" /\ false /\ (D.fromString "3.30"))
              /\ ("rookworst" /\ true /\ (D.fromString "5.60")))
            names <- query conn (Query """
              SELECT name, delicious
              FROM foods
              WHERE delicious
              ORDER BY name ASC
            """) Row0
            liftEffect <<< assert $ names == ["pork" /\ true, "rookworst" /\ true]

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
            liftEffect <<< assert $ names == [Row2 "pork" true, Row2 "rookworst" true]

          test conn "delete returning columns subset" $ do
            insertFood
            deleted <- query conn (Query """
              DELETE FROM foods
              WHERE delicious
              RETURNING name, delicious
            """) Row0
            liftEffect <<< assert $ deleted == [Row2 "pork" true, Row2 "rookworst" true]

          test conn "delete returning command tag value" $ do
            insertFood
            deleted <- command conn (Query """
              DELETE FROM foods
              WHERE delicious
            """) Row0
            liftEffect <<< assert $ deleted == 2

          test conn "handling instant value" $ do
            before <- liftEffect $ (unwrap <<< unInstant) <$> now
            insertFood
            added <- query conn (Query """
              SELECT added
              FROM foods
            """) Row0
            after <- liftEffect $ (unwrap <<< unInstant) <$> now
            -- | timestamps are fetched without milliseconds so we have to
            -- | round before value down
            liftEffect <<< assert $ all
              (\(Row1 t) ->
                ( unwrap $ unInstant t) >= (before - before % 1000.0)
                  && after >= (unwrap $ unInstant t))
              added

          test conn "handling decimal value" $ do
            insertFood
            sauerkrautPrice <- query conn (Query """
              SELECT price
              FROM foods
              WHERE NOT delicious
            """) Row0
            liftEffect <<< assert $ sauerkrautPrice == [Row1 (D.fromString "3.30")]

          transactionTest "integrity error handling" $ do
            withRollback conn do
              result <- onIntegrityError (pure "integrity error was handled") do
                insertFood
                insertFood
                pure "integrity error was not handled"
              liftEffect $ assert $ result == "integrity error was handled"

          test conn "handling date value" $ do
            let
              d1 = date 2010 2 31
              d2 = date 2017 2 1
              d3 = date 2020 6 31

            execute conn (Query """
              INSERT INTO dates (date)
              VALUES ($1), ($2), ($3)
            """) (Row3 d1 d2 d3)

            (dates :: Array (Row1 Date)) <- query conn (Query """
              SELECT *
              FROM dates
              ORDER BY date ASC
            """) Row0
            lift $ equal 3 (length dates)
            liftEffect <<< assert $ all (\(Tuple (Row1 r) e) -> e == r) $ (zip dates [d1, d2, d3])

          test conn "handling json and jsonb value" $ do
            let jsonIn = fromFoldable [Tuple "a" 1, Tuple "a" 2, Tuple "2" 3]
            let expected = fromFoldable [Tuple "a" 2, Tuple "2" 3]

            execute conn (Query """
              INSERT INTO jsons (json, jsonb)
              VALUES ($1, $2)
            """) (Row2 jsonIn jsonIn)

            (js ∷ Array (Row2 (Object Int) (Object Int))) <- query conn (Query """SELECT * FROM JSONS""") Row0
            liftEffect $ assert $ all (\(Row2 j1 j2) → j1 == expected && expected == j2) js

          test conn "handling jsdate value" $ do
            let
              jsd1 = jsdate_ 2010.0 2.0 31.0 6.0 23.0 1.0 123.0
              jsd2 = jsdate_ 2017.0 2.0 1.0 12.0 59.0 42.0 999.0
              jsd3 = jsdate_ 2020.0 6.0 31.0 23.0 3.0 59.0 333.0

            execute conn (Query """
              INSERT INTO timestamps (timestamp)
              VALUES ($1), ($2), ($3)
            """) (Row3 jsd1 jsd2 jsd3)

            (timestamps :: Array (Row1 JSDate)) <- query conn (Query """
              SELECT *
              FROM timestamps
              ORDER BY timestamp ASC
            """) Row0
            lift $ equal 3 (length timestamps)
            liftEffect <<< assert $ all (\(Tuple (Row1 r) e) -> e == r) $ (zip timestamps [jsd1, jsd2, jsd3])

        suite "PostgreSQL connection errors" $ do
          let doNothing _ = pure unit

          Test.Unit.test "connection refused" do
            testPool <- newPool cannotConnectConfig
            runExceptT (withConnection testPool doNothing) >>= liftEffect <<< case _ of
              Left (ConnectionError cause) ->
                assert $ cause == "ECONNREFUSED"

              _ ->
                assert false

          Test.Unit.test "no such database" do
            testPool <- newPool noSuchDatabaseConfig
            runExceptT (withConnection testPool doNothing) >>= liftEffect <<< case _ of
              Left (ProgrammingError { code, message }) -> do
                assert $ code == "3D000"
                assert $ message == "database \"this-database-does-not-exist\" does not exist"

              _ ->
                assert false

config :: PoolConfiguration
config =
  { user: Nothing
  , password: Nothing
  , host: Nothing
  , port: Nothing
  , database: "purspg"
  , max: Nothing
  , idleTimeoutMillis: Just 1000
  }

noSuchDatabaseConfig :: PoolConfiguration
noSuchDatabaseConfig =
  config { database = "this-database-does-not-exist" }

cannotConnectConfig :: PoolConfiguration
cannotConnectConfig =
  config { host = Just "127.0.0.1", port = Just 45287 }
