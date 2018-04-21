# purescript-postgresql-client

purescript-postgresql-client is a PostgreSQL client library for PureScript.

## NOTE: I WILL NO LONGER BE MAINTAINING THIS LIBRARY. DUE TO THE COUNTLESS PROBLEMS AND PITFALLS IN THE NODE.JS POSTGRESQL LIBRARY IT IS NO MORE FUN BUT JUST A MASSIVE PITA. IF ANYBODY WANTS TO VOLUNTEER, PLEASE LET ME KNOW, AND I WILL TRANSFER OWNERSHIP OF THE REPO. I THINK THE ONLY TWO REAL SOLUTIONS WOULD BE TO EITHER WRITE A NEW LIBPQ BINDING FOR NODE OR TO WRITE A PSC BACKEND FOR A DECENT PLATFORM (NOT NODE.JS).

To use this library, you need to add `pg` and `decimal.js` as an npm dependency. You can also
find this npm library on [https://github.com/brianc/node-postgres][pg].

The purspgpp preprocessor has been replaced by [sqltopurs], which is a code
generator instead of a preprocessor, and easier to use.

[sqltopurs]: https://github.com/rightfold/sqltopurs
