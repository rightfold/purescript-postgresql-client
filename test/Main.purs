module Test.Main
  ( main
  ) where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError, try)
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
import Data.Time (Time(..))
import Database.PostgreSQL (POSTGRESQL, PoolConfiguration, Query(..), Row0(..), Row1(..), Row2(..), Row6(..), execute, newPool, query, scalar, withConnection, withTransaction)
import Database.PostgreSQL.Row (Row3(..))
import Test.Assert (ASSERT, assert, assert')


time :: Maybe Time
time = do
  h <- toEnum 1
  m <- toEnum 20
  s <- toEnum 30
  ms <- toEnum 500
  pure $ Time h m s ms

date :: Maybe Date
date = do
  y <- toEnum 2017
  m <- toEnum 2
  d <- toEnum 11
  pure $ canonicalDate y m d

dateTime :: Maybe DateTime
dateTime = DateTime <$> date <*> time

testDates :: ∀ eff. Eff (assert :: ASSERT, exception :: EXCEPTION, postgreSQL :: POSTGRESQL | eff) Unit
testDates = void $ launchAff do
  pool <- newPool config
  withConnection pool \conn -> do
    execute conn (Query """
      CREATE TEMPORARY TABLE times (
        datevalue date NOT NULL,
        tsvalue timestamp NOT NULL,
        tsvaluetz timestamptz NOT NULL
      )
    """) Row0

    execute conn (Query """
      INSERT INTO times (datevalue, tsvalue, tsvaluetz)
      VALUES ($1, $2, $3)
    """) (Row3 date dateTime dateTime)

    (times :: Array (Row3 Date DateTime DateTime)) <- query conn (Query """
      SELECT *
      FROM times
    """) Row0

    liftEff $ assert' "consistently retrieve date and datetime" $
      all (\(Row3 a b c) -> (Just a) == date && (Just b)  == dateTime && (Just c) == dateTime) times

    pure unit

testCommitAndRollback :: ∀ eff. Eff (assert :: ASSERT, exception :: EXCEPTION, postgreSQL :: POSTGRESQL | eff) Unit
testCommitAndRollback = void $ launchAff do
  pool <- newPool config
  withConnection pool \conn -> do
    execute conn (Query """
      CREATE TEMPORARY TABLE foods (
        name text NOT NULL,
        delicious boolean NOT NULL,
        PRIMARY KEY (name)
      )
    """) Row0

    execute conn (Query """
      INSERT INTO foods (name, delicious)
      VALUES ($1, $2), ($3, $4), ($5, $6)
    """) (Row6 "pork" true "sauerkraut" false "rookworst" true)

    names <- query conn (Query """
      SELECT name
      FROM foods
      WHERE delicious
      ORDER BY name ASC
    """) Row0
    liftEff <<< assert $ names == [Row1 "pork", Row1 "rookworst"]

    testTransactionCommit conn
    testTransactionRollback conn

    pure unit
  where
  testTransactionCommit conn = do
    deleteAll conn
    withTransaction conn do
      execute conn (Query """
        INSERT INTO foods (name, delicious)
        VALUES ($1, $2)
      """) (Row2 "pork" true)
      testCount conn 1
    testCount conn 1

  testTransactionRollback conn = do
    deleteAll conn
    _ <- try $ withTransaction conn do
      execute conn (Query """
        INSERT INTO foods (name, delicious)
        VALUES ($1, $2)
      """) (Row2 "pork" true)
      testCount conn 1
      throwError $ error "fail"
    testCount conn 0

  deleteAll conn =
    execute conn (Query """
      DELETE FROM foods
    """) Row0

  testCount conn n = do
    count <- scalar conn (Query """
      SELECT count(*) = $1
      FROM foods
    """) (Row1 n)
    liftEff <<< assert $ count == Just true

main :: ∀ eff. Eff (assert :: ASSERT, exception :: EXCEPTION, postgreSQL :: POSTGRESQL | eff) Unit
main = do
  void testDates
  void testCommitAndRollback

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

