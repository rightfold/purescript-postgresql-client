# purescript-postgresql-client

purescript-postgresql-client is a PostgreSQL client library for PureScript based on `node-postgres`.

## Install

To use this library, you need to add [`pg`][pg] and [`decimal.js`][decimal.js] as an npm dependency. You can also
find first of them on [https://github.com/brianc/node-postgres](https://github.com/brianc/node-postgres).

## Usage

This guide is a literate Purescript file which is extracted into testing module (using [`literate-purescript`](https://github.com/Thimoteus/literate-purescript)) so it is a little verbose.

Let's start with imports.

```purescript
module Test.README where

import Prelude

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Database.PostgreSQL (Connection, fromPool, Pool, Query(Query), PGError)
import Database.PostgreSQL.PG (command, execute, query, withTransaction) as PG
import Database.PostgreSQL.Pool (new) as Pool
import Database.PostgreSQL.Row (Row0(Row0), Row3(Row3))
import Data.Decimal as Decimal
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Assert (assert)
import Test.Config (load) as Config
```

The whole API for interaction with PostgreSQL is performed asynchronously in `Aff`
(the only function which runs in plain `Effect` is `Pool.new`). Core library
functions usually results in somthing like `Aff (Either PGError a)` which can be easily
wrapped by user into `ExceptT` or any other custom monad stack. This base API is exposed by
`PostgreSQL.Aff` module.
To be honest we provide alternatives to functions in the `Database.PostgreSQL.PG` module that work on any stack `m` with `MonadError PGError m` and `MonadAff m`.
The module contains two functions `withClient` and `withTransaction` that require additional parameter - a transformation from a custom monad stack to `Aff (Either PGError a)`.
We are going to work with custom `AppM` type in this tutorial but please don't consider it as the only option
if you encounter any troubles integrating it into your own app monad stack.

```purescript
type AppM a = ExceptT PGError Aff a

withTransaction :: forall a. Pool -> (Connection -> AppM a) -> AppM a
withTransaction p = PG.withTransaction runExceptT p
```

Our tests runner reads the configuration for the current process environment or from
the _.env_ file (please check _.env-example_ for details).
We assume here that Postgres is running on a standard local port
with `ident` authentication so configuration can be nearly empty (`defaultConfiguration`).
It requires only database name which we pass to the `newPool` function.
Additionally we pass `idleTimeoutMillis` value because this code
is run by our test suite and we want to exit after its execution quickly ;-)


```purescript
run ∷ AppM Unit
run = do
  config ← liftAff $ Config.load
  pool ← liftEffect $ Pool.new config
```

We can now create our temporary table which we are going to query in this example.
`PG.execute` ignores result value which is what we want in this case.
The last `Row0` value indicates that this `Query` doesn't take any additional parameters.

Database quering functions like `execute` below can perform the action using the pool 
or the connection instance so they expect a value of type `Connection` (which is just
a wrapper around `Either` - `newtype Connection = Connection (Either Pool Client)`).

```purescript

  PG.execute (fromPool pool) (Query """
    CREATE TEMPORARY TABLE fruits (
      name text NOT NULL,
      delicious boolean NOT NULL,
      price NUMERIC(4,2) NOT NULL,
      added TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (name)
    );
  """) Row0
```

There is a `withTransaction` helper provided. You can wrap the whole
piece of interaction with database in it. It will rollback if any exception
is thrown during execution of a given `Aff` block. It excecutes `COMMIT`
in the other case.
We start our session with insert of some data. It is done by `PG.execute`
function with `INSERT` statement.
Please notice that we are passing a tuple of the arguments to this query
using dedicated constructor. In this case `Row3`.  This library provides types
from `Row0` to `Row19` and they are wrappers which provide instances for
automatic conversions from and to SQL values.
For details please investigate following classes `ToSQLRow`, `ToSQLValue`,
`FromSQLRow` and `FromSQLValue`.

```purescript
  withTransaction pool \h -> do
    PG.execute h (Query """
      INSERT INTO fruits (name, delicious, price)
      VALUES ($1, $2, $3)
    """) (Row3 "coconut" true (Decimal.fromString "8.30"))
```

We can also use nested tuples instead of `Row*` constructors. This can be a bit more
verbose but is not restricted to limited and constant number of arguments.
`/\` is just an alias for the `Tuple` constructor from `Data.Tuple.Nested`.

```purescript
    PG.execute h (Query """
      INSERT INTO fruits (name, delicious, price)
      VALUES ($1, $2, $3)
    """) ("lemon" /\ false /\ Decimal.fromString "3.30")
```

Of course `Row*` types and nested tuples can be also used when we are fetching
data from db.
`query` function processes db response and returns an `Array` of rows.

```purescript
  names <- PG.query (fromPool pool) (Query """
    SELECT name, delicious
    FROM fruits
    ORDER BY name ASC
  """) Row0
  liftEffect <<< assert $ names == ["coconut" /\ true, "lemon" /\ false]
```

There is also a `command` function at our disposal.
Some postgres SQL expressions return a "command tag" which carries
a value with a number of rows which were affected by a given query.
For example we can have: `DELETE rows`, `UPDATE rows`, `INSERT oid rows` etc.
This function should return `rows` value associated with given response.

```purescript
  deleted <- PG.command (fromPool pool) (Query """DELETE FROM fruits """) Row0
  liftEffect <<< assert $ deleted == 2
```

[pg]: https://www.npmjs.com/package/pg
[decimal.js]: https://www.npmjs.com/package/decimal.js

## Hacking

### Testing

Test database is read from the environment or loaded from _.env_ file. You can find _.env-example_ in the repo with some simple testing db setup.

