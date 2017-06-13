module Test.Main
  ( main
  ) where

import Prelude (Unit, bind, discard, map, pure, unit, void, ($), (&&), (<$>), (<*>), (<<<), (==))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError, try)
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(Just))
import Data.Time (Time(..))
import Database.PostgreSQL (POSTGRESQL, PoolConfiguration, Query(..), Row0(..), Row1(..), Row2(..), Row6(..), execute, newPool, query, scalar, withConnection, withTransaction)
import Database.PostgreSQL.Row (Row3(Row3))
import Test.Assert (ASSERT, assert, assert')

main :: ∀ eff. Eff (assert :: ASSERT, exception :: EXCEPTION, postgreSQL :: POSTGRESQL | eff) Unit
main = do
  _ <- testCommitAndRollback
  _ <- testDates
  pure unit

date :: Maybe Date
date = do
  y <- toEnum 2017
  m <- toEnum 1
  d <- toEnum 1
  pure $ canonicalDate y m d

time :: Maybe Time
time = do
  h <- toEnum 4
  m <- toEnum 10
  s <- toEnum 30
  ms <- toEnum 500
  pure $ Time h m s ms

dateTime :: Maybe DateTime
dateTime = DateTime <$> date <*> time

testDates :: ∀ eff. Eff (assert :: ASSERT, exception :: EXCEPTION, postgreSQL :: POSTGRESQL | eff) Unit
testDates = void $ launchAff do
  pool <- newPool config  
  withConnection pool \conn -> do
    execute conn (Query """
      CREATE TEMPORARY TABLE times (
        datevalue date NOT NULL,
        timestampvalue timestamptz NOT NULL,
        timestampvalue_notz timestamp NOT NULL
      )
    """) Row0

    execute conn (Query """
      INSERT INTO times (datevalue, timestampvalue, timestampvalue_notz)
      VALUES ($1, $2, $3)
    """) (Row3 date dateTime dateTime)
    
    (times :: Array (Row3 Date DateTime DateTime)) <- query conn (Query """
      SELECT * FROM times
    """) (Row0)

    liftEff <<< assert' "retrieve consistent time data" $ (map (\(Row3 a b c) -> 
      date == Just a &&
      dateTime == Just b &&
      dateTime == Just c
    ) times) == [true]

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
