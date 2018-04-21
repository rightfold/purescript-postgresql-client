module Test.Main
  ( main
  ) where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError, try)
import Data.Decimal as D
import Data.Maybe (Maybe(..))
import Database.PostgreSQL (POSTGRESQL, PoolConfiguration, Query(Query), Row0(Row0), Row1(Row1), Row2(Row2), Row3(Row3), Row9(Row9), execute, newPool, query, scalar, withConnection, withTransaction)
import Test.Assert (ASSERT, assert)

main :: ∀ eff. Eff (assert :: ASSERT, exception :: EXCEPTION, postgreSQL :: POSTGRESQL | eff) Unit
main = void $ launchAff do
  pool <- newPool config
  withConnection pool \conn -> do
    execute conn (Query """
     CREATE TEMPORARY TABLE foods (
       name text NOT NULL,
       delicious boolean NOT NULL,
       price NUMERIC(4,2) NOT NULL,
       PRIMARY KEY (name)
     )
    """) Row0

    execute conn (Query """
      INSERT INTO foods (name, delicious, price)
      VALUES ($1, $2, $3), ($4, $5, $6), ($7, $8, $9)
    """) (Row9
        "pork" true (D.fromString "8.30")
        "sauerkraut" false (D.fromString "3.30")
        "rookworst" true (D.fromString "5.60"))

    names <- query conn (Query """
      SELECT name
      FROM foods
      WHERE delicious
      ORDER BY name ASC
    """) Row0

    liftEff <<< assert $ names == [Row1 "pork", Row1 "rookworst"]

    sour <- query conn (Query """
      SELECT name, price
      FROM foods
      WHERE NOT delicious
      ORDER BY name ASC
    """) Row0
    liftEff <<< assert $ sour == [Row2 "sauerkraut" (D.fromString "3.30")]

    testTransactionCommit conn
    testTransactionRollback conn

    pure unit
  where
  testTransactionCommit conn = do
    deleteAll conn
    withTransaction conn do
      execute conn (Query """
        INSERT INTO foods (name, delicious, price)
        VALUES ($1, $2, $3)
      """) (Row3 "pork" true (D.fromString "8.30"))
      testCount conn 1
    testCount conn 1

  testTransactionRollback conn = do
    deleteAll conn
    _ <- try $ withTransaction conn do
      execute conn (Query """
        INSERT INTO foods (name, delicious, price)
        VALUES ($1, $2, $3)
      """) (Row3 "pork" true (D.fromString "8.30"))
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
