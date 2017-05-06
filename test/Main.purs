module Test.Main
  ( main
  ) where

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL (POSTGRESQL, PoolConfiguration, Query(..), execute, newPool, query, withConnection)
import Prelude
import Test.Assert (ASSERT, assert)

main :: ∀ eff. Eff (assert :: ASSERT, exception :: EXCEPTION, postgreSQL :: POSTGRESQL | eff) Unit
main = void $ launchAff do
  pool <- newPool config
  withConnection pool \conn -> do
    execute conn (Query """
      CREATE TEMPORARY TABLE foods (
        name text NOT NULL,
        delicious boolean NOT NULL,
        PRIMARY KEY (name)
      )
    """) unit

    execute conn (Query """
      INSERT INTO foods (name, delicious)
      VALUES ($1, $2), ($3, $4), ($5, $6)
    """) ("pork" /\ true /\ "sauerkraut" /\ false /\ "rookworst" /\ true /\ unit)

    query conn (Query """
      SELECT name
      FROM foods
      WHERE delicious
      ORDER BY name ASC
    """) unit
      >>= liftEff <<< assert <<< (==) ["pork" /\ unit, "rookworst" /\ unit]

    pure unit

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
