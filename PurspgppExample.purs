module PurspgppExample where

f :: Query (String × Unit) (UUID × String × Unit)
f = [query|
    SELECT id, name
    FROM files
    WHERE author_id = $1
|]

g :: Query Unit (Boolean × Unit)
g = [query|SELECT pg_try_advisory_lock(3735928559)|]
