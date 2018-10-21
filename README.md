# purescript-postgresql-client

purescript-postgresql-client is a PostgreSQL client library for PureScript.

## Install

To use this library, you need to add `pg` and `decimal.js` as an npm dependency. You can also
find first of them on [https://github.com/brianc/node-postgres][pg].

## Usage

This guide is a literate Purescript file which is extracted into testing module (using [`literate-purescript`](https://github.com/Thimoteus/literate-purescript)) so it is a little verbose.

``` purescript
module Test.Example where

import Prelude

import Database.PostgreSQL (defaultPoolConfiguration, execute, newPool, query, Query(Query), withConnection)
import Database.PostgreSQL.Row (Row0(Row0), Row3(Row3))
import Data.Decimal as Decimal
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.Assert (assert)

-- Our interaction with db is performed asynchronously in `Aff`
run ∷ Aff Unit
run = do

  -- We assume here that postgres is running on a standard local port together
  -- with `ident` authentication so configuration can be nearly empty.
  -- It requires only database name which we pass to `newPool` function.
  -- We setup also `idleTimeoutMillis` setting because this code
  -- would be run by our test suite and we want to finish quickly ;-)

  pool <- newPool ((defaultPoolConfiguration "purspg") { idleTimeoutMillis = Just 1000 })
  withConnection pool \conn -> do

    -- We can now create our temporary table which we are going to query in this example.
    -- `execute` ignores result value which is what we want in this case.
 
    execute conn (Query """
      CREATE TEMPORARY TABLE fruits (
        name text NOT NULL,
        delicious boolean NOT NULL,
        price NUMERIC(4,2) NOT NULL,
        added TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY (name)
      );
    """) Row0

    -- We can insert some data calling `execute` function with `INSERT` statement.
    -- Please notice that we are passing a tuple of arguments to this query
    -- using dedicated constructor (in this case `Row3`).
    -- This library provides types from `Row0` to `Row19` and they are wrappers which
    -- provides instances for automatic conversions from and to SQL values.

    execute conn (Query """
      INSERT INTO fruits (name, delicious, price)
      VALUES ($1, $2, $3)
    """) (Row3 "coconut" true (Decimal.fromString "8.30"))


    -- You can also use nested tuples instead of `Row*` types but this can be a bit more
    -- verbose. `/\` is just an alias for `Tuple` constructor.

    execute conn (Query """
      INSERT INTO fruits (name, delicious, price)
      VALUES ($1, $2, $3)
    """) ("lemon" /\ false /\ Decimal.fromString "3.30")

    -- Of course `Row*` typees and nested tuples can be also used when we are fetching
    -- data from db.

    names <- query conn (Query """
      SELECT name, delicious
      FROM fruits
      ORDER BY name ASC
    """) Row0
    liftEffect <<< assert $ names == ["coconut" /\ true, "lemon" /\ false]

```

## Testing

Currently tests are prepared to work with default and local setup for postgresql (ident authentication, standart port etc.).
If you think that we should add configuration layer for our test runner please open an issue.
To run suite:

  * prepare empty "purspg" database

  * `$ nmp install literate-purescript`

  * `$ ./bin/test.sh`


## Generating SQL Queries

The purspgpp preprocessor has been replaced by [sqltopurs], which is a code
generator instead of a preprocessor, and easier to use.

[sqltopurs]: https://github.com/rightfold/sqltopurs


