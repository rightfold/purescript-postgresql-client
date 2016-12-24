# purescript-postgresql-client

purescript-postgresql-client is a PostgreSQL client library for PureScript.
Included is a preprocessor, purspgpp, which finds embedded SQL queries in
PureScript source files and infers their types. Such queries can be written as
follows:

```purescript
userName = [query|
    SELECT first_name, last_name
    FROM users
    WHERE id = $1
|]
```

purspgpp will replace this by something like the following:

```purescript
(Query """
    SELECT first_name, last_name
    FROM users
    WHERE id = $1
""" :: Query (UUID × Unit) (String × String × Unit))
```

You can integrate purspgpp into your build system. For example, here is a
PowerShell script that executes it for all `.purspg` files:

```powershell
Get-ChildItem src -Recurse -Filter *.purspg `
| ForEach-Object {
    perl6 `
        bower_components/purescript-postgresql-client/purspgpp `
        "user=postgres password=lol123 dbname=nn" `
        "$($_.FullName)" `
        "$([IO.Path]::ChangeExtension($_.FullName, "purs"))"
    if (!$?) {
        Write-Error "Unable to preprocess $_"
    }
}
```
