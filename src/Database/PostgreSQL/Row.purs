module Database.PostgreSQL.Row where

import Prelude

import Data.Array (uncons, (:))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Tuple (Tuple(..))
import Database.PostgreSQL.Value (class FromSQLValue, class ToSQLValue, fromSQLValue, toSQLValue)
import Foreign (Foreign)

-- | Convert things to SQL rows.
class ToSQLRow a where
    toSQLRow :: a -> Array Foreign

-- | Convert things from SQL rows.
class FromSQLRow a where
    fromSQLRow :: Array Foreign -> Either String a

instance toSQLRowForeignArray :: ToSQLRow (Array Foreign) where
  toSQLRow = identity

instance toSQLRowTupleOfTuples :: (ToSQLRow (Tuple a ta), ToSQLRow (Tuple b t)) => ToSQLRow (Tuple (Tuple a ta) (Tuple b t)) where
  toSQLRow (Tuple a t) = toSQLRow a <> toSQLRow t
else instance toSQLRowTuple :: (ToSQLValue a, ToSQLRow (Tuple b t)) => ToSQLRow (Tuple a (Tuple b t)) where
  toSQLRow (Tuple a t) = toSQLValue a : toSQLRow t
else instance toSQLRowTupleEnd :: (ToSQLValue a, ToSQLValue b) => ToSQLRow (Tuple a b) where
  toSQLRow (Tuple a b) = [ toSQLValue a, toSQLValue b ]

instance fromSQLRowTuple :: (FromSQLValue a, FromSQLRow (Tuple b t)) => FromSQLRow (Tuple a (Tuple b t)) where
  fromSQLRow r = do
    {head, tail} ← note "Expecting more fields in a row" $ uncons r
    Tuple <$> fromSQLValue head <*> fromSQLRow tail
else instance fromSQLRowTupleEnd :: (FromSQLValue a, FromSQLValue b) => FromSQLRow (Tuple a b) where
  fromSQLRow [a, b] = Tuple <$> fromSQLValue a <*> fromSQLValue b
  fromSQLRow _ = Left "Expecting exactly two more fields."

-- | A row with 0 fields.
data Row0 = Row0

derive instance eqRow0 :: Eq Row0

derive instance ordRow0 :: Ord Row0

instance showRow0 :: Show Row0 where
  show Row0 =
    "Row0"

instance fromSQLRowRow0 :: FromSQLRow Row0 where
  fromSQLRow [] =
    pure Row0
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 0."
    where n = Array.length xs

instance toSQLRowRow0 :: ToSQLRow Row0 where
  toSQLRow Row0 = []
-- | A row with 1 field.
data Row1 a = Row1 a

derive instance eqRow1 :: (Eq a) => Eq (Row1 a)

derive instance ordRow1 :: (Ord a) => Ord (Row1 a)

instance showRow1 :: (Show a) => Show (Row1 a) where
  show (Row1 a) =
    "(Row1 " <> show a <> ")"

instance fromSQLRowRow1 :: (FromSQLValue a) => FromSQLRow (Row1 a) where
  fromSQLRow [a] =
    pure Row1
    <*> fromSQLValue a
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 1."
    where n = Array.length xs

instance toSQLRowRow1 :: (ToSQLValue a) => ToSQLRow (Row1 a) where
  toSQLRow (Row1 a) =
    [toSQLValue a]
-- | A row with 2 fields.
data Row2 a b = Row2 a b

derive instance eqRow2 :: (Eq a, Eq b) => Eq (Row2 a b)

derive instance ordRow2 :: (Ord a, Ord b) => Ord (Row2 a b)

instance showRow2 :: (Show a, Show b) => Show (Row2 a b) where
  show (Row2 a b) =
    "(Row2 " <> show a <> " " <> show b <> ")"

instance fromSQLRowRow2 :: (FromSQLValue a, FromSQLValue b) => FromSQLRow (Row2 a b) where
  fromSQLRow [a, b] =
    pure Row2
    <*> fromSQLValue a
    <*> fromSQLValue b
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 2."
    where n = Array.length xs

instance toSQLRowRow2 :: (ToSQLValue a, ToSQLValue b) => ToSQLRow (Row2 a b) where
  toSQLRow (Row2 a b) =
    [toSQLValue a, toSQLValue b]
-- | A row with 3 fields.
data Row3 a b c = Row3 a b c

derive instance eqRow3 :: (Eq a, Eq b, Eq c) => Eq (Row3 a b c)

derive instance ordRow3 :: (Ord a, Ord b, Ord c) => Ord (Row3 a b c)

instance showRow3 :: (Show a, Show b, Show c) => Show (Row3 a b c) where
  show (Row3 a b c) =
    "(Row3 " <> show a <> " " <> show b <> " " <> show c <> ")"

instance fromSQLRowRow3 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c) => FromSQLRow (Row3 a b c) where
  fromSQLRow [a, b, c] =
    pure Row3
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 3."
    where n = Array.length xs

instance toSQLRowRow3 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c) => ToSQLRow (Row3 a b c) where
  toSQLRow (Row3 a b c) =
    [toSQLValue a, toSQLValue b, toSQLValue c]
-- | A row with 4 fields.
data Row4 a b c d = Row4 a b c d

derive instance eqRow4 :: (Eq a, Eq b, Eq c, Eq d) => Eq (Row4 a b c d)

derive instance ordRow4 :: (Ord a, Ord b, Ord c, Ord d) => Ord (Row4 a b c d)

instance showRow4 :: (Show a, Show b, Show c, Show d) => Show (Row4 a b c d) where
  show (Row4 a b c d) =
    "(Row4 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> ")"

instance fromSQLRowRow4 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d) => FromSQLRow (Row4 a b c d) where
  fromSQLRow [a, b, c, d] =
    pure Row4
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 4."
    where n = Array.length xs

instance toSQLRowRow4 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d) => ToSQLRow (Row4 a b c d) where
  toSQLRow (Row4 a b c d) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d]
-- | A row with 5 fields.
data Row5 a b c d e = Row5 a b c d e

derive instance eqRow5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (Row5 a b c d e)

derive instance ordRow5 :: (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (Row5 a b c d e)

instance showRow5 :: (Show a, Show b, Show c, Show d, Show e) => Show (Row5 a b c d e) where
  show (Row5 a b c d e) =
    "(Row5 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> ")"

instance fromSQLRowRow5 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e) => FromSQLRow (Row5 a b c d e) where
  fromSQLRow [a, b, c, d, e] =
    pure Row5
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 5."
    where n = Array.length xs

instance toSQLRowRow5 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e) => ToSQLRow (Row5 a b c d e) where
  toSQLRow (Row5 a b c d e) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e]
-- | A row with 6 fields.
data Row6 a b c d e f = Row6 a b c d e f

derive instance eqRow6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (Row6 a b c d e f)

derive instance ordRow6 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) => Ord (Row6 a b c d e f)

instance showRow6 :: (Show a, Show b, Show c, Show d, Show e, Show f) => Show (Row6 a b c d e f) where
  show (Row6 a b c d e f) =
    "(Row6 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> ")"

instance fromSQLRowRow6 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f) => FromSQLRow (Row6 a b c d e f) where
  fromSQLRow [a, b, c, d, e, f] =
    pure Row6
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 6."
    where n = Array.length xs

instance toSQLRowRow6 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f) => ToSQLRow (Row6 a b c d e f) where
  toSQLRow (Row6 a b c d e f) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f]
-- | A row with 7 fields.
data Row7 a b c d e f g = Row7 a b c d e f g

derive instance eqRow7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (Row7 a b c d e f g)

derive instance ordRow7 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g) => Ord (Row7 a b c d e f g)

instance showRow7 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (Row7 a b c d e f g) where
  show (Row7 a b c d e f g) =
    "(Row7 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> ")"

instance fromSQLRowRow7 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g) => FromSQLRow (Row7 a b c d e f g) where
  fromSQLRow [a, b, c, d, e, f, g] =
    pure Row7
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 7."
    where n = Array.length xs

instance toSQLRowRow7 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g) => ToSQLRow (Row7 a b c d e f g) where
  toSQLRow (Row7 a b c d e f g) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g]
-- | A row with 8 fields.
data Row8 a b c d e f g h = Row8 a b c d e f g h

derive instance eqRow8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq (Row8 a b c d e f g h)

derive instance ordRow8 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h) => Ord (Row8 a b c d e f g h)

instance showRow8 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (Row8 a b c d e f g h) where
  show (Row8 a b c d e f g h) =
    "(Row8 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> ")"

instance fromSQLRowRow8 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h) => FromSQLRow (Row8 a b c d e f g h) where
  fromSQLRow [a, b, c, d, e, f, g, h] =
    pure Row8
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 8."
    where n = Array.length xs

instance toSQLRowRow8 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h) => ToSQLRow (Row8 a b c d e f g h) where
  toSQLRow (Row8 a b c d e f g h) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h]
-- | A row with 9 fields.
data Row9 a b c d e f g h i = Row9 a b c d e f g h i

derive instance eqRow9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq (Row9 a b c d e f g h i)

derive instance ordRow9 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i) => Ord (Row9 a b c d e f g h i)

instance showRow9 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show (Row9 a b c d e f g h i) where
  show (Row9 a b c d e f g h i) =
    "(Row9 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> ")"

instance fromSQLRowRow9 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i) => FromSQLRow (Row9 a b c d e f g h i) where
  fromSQLRow [a, b, c, d, e, f, g, h, i] =
    pure Row9
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 9."
    where n = Array.length xs

instance toSQLRowRow9 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i) => ToSQLRow (Row9 a b c d e f g h i) where
  toSQLRow (Row9 a b c d e f g h i) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i]
-- | A row with 10 fields.
data Row10 a b c d e f g h i j = Row10 a b c d e f g h i j

derive instance eqRow10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq (Row10 a b c d e f g h i j)

derive instance ordRow10 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j) => Ord (Row10 a b c d e f g h i j)

instance showRow10 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (Row10 a b c d e f g h i j) where
  show (Row10 a b c d e f g h i j) =
    "(Row10 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> ")"

instance fromSQLRowRow10 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j) => FromSQLRow (Row10 a b c d e f g h i j) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j] =
    pure Row10
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 10."
    where n = Array.length xs

instance toSQLRowRow10 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j) => ToSQLRow (Row10 a b c d e f g h i j) where
  toSQLRow (Row10 a b c d e f g h i j) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j]
-- | A row with 11 fields.
data Row11 a b c d e f g h i j k = Row11 a b c d e f g h i j k

derive instance eqRow11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => Eq (Row11 a b c d e f g h i j k)

derive instance ordRow11 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k) => Ord (Row11 a b c d e f g h i j k)

instance showRow11 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) => Show (Row11 a b c d e f g h i j k) where
  show (Row11 a b c d e f g h i j k) =
    "(Row11 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> " " <> show k <> ")"

instance fromSQLRowRow11 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j, FromSQLValue k) => FromSQLRow (Row11 a b c d e f g h i j k) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j, k] =
    pure Row11
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
    <*> fromSQLValue k
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 11."
    where n = Array.length xs

instance toSQLRowRow11 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j, ToSQLValue k) => ToSQLRow (Row11 a b c d e f g h i j k) where
  toSQLRow (Row11 a b c d e f g h i j k) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j, toSQLValue k]
-- | A row with 12 fields.
data Row12 a b c d e f g h i j k l = Row12 a b c d e f g h i j k l

derive instance eqRow12 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => Eq (Row12 a b c d e f g h i j k l)

derive instance ordRow12 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l) => Ord (Row12 a b c d e f g h i j k l)

instance showRow12 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) => Show (Row12 a b c d e f g h i j k l) where
  show (Row12 a b c d e f g h i j k l) =
    "(Row12 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> " " <> show k <> " " <> show l <> ")"

instance fromSQLRowRow12 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j, FromSQLValue k, FromSQLValue l) => FromSQLRow (Row12 a b c d e f g h i j k l) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j, k, l] =
    pure Row12
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
    <*> fromSQLValue k
    <*> fromSQLValue l
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 12."
    where n = Array.length xs

instance toSQLRowRow12 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j, ToSQLValue k, ToSQLValue l) => ToSQLRow (Row12 a b c d e f g h i j k l) where
  toSQLRow (Row12 a b c d e f g h i j k l) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j, toSQLValue k, toSQLValue l]
-- | A row with 13 fields.
data Row13 a b c d e f g h i j k l m = Row13 a b c d e f g h i j k l m

derive instance eqRow13 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m) => Eq (Row13 a b c d e f g h i j k l m)

derive instance ordRow13 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m) => Ord (Row13 a b c d e f g h i j k l m)

instance showRow13 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) => Show (Row13 a b c d e f g h i j k l m) where
  show (Row13 a b c d e f g h i j k l m) =
    "(Row13 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> " " <> show k <> " " <> show l <> " " <> show m <> ")"

instance fromSQLRowRow13 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j, FromSQLValue k, FromSQLValue l, FromSQLValue m) => FromSQLRow (Row13 a b c d e f g h i j k l m) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j, k, l, m] =
    pure Row13
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
    <*> fromSQLValue k
    <*> fromSQLValue l
    <*> fromSQLValue m
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 13."
    where n = Array.length xs

instance toSQLRowRow13 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j, ToSQLValue k, ToSQLValue l, ToSQLValue m) => ToSQLRow (Row13 a b c d e f g h i j k l m) where
  toSQLRow (Row13 a b c d e f g h i j k l m) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j, toSQLValue k, toSQLValue l, toSQLValue m]
-- | A row with 14 fields.
data Row14 a b c d e f g h i j k l m n = Row14 a b c d e f g h i j k l m n

derive instance eqRow14 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n) => Eq (Row14 a b c d e f g h i j k l m n)

derive instance ordRow14 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n) => Ord (Row14 a b c d e f g h i j k l m n)

instance showRow14 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) => Show (Row14 a b c d e f g h i j k l m n) where
  show (Row14 a b c d e f g h i j k l m n) =
    "(Row14 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> " " <> show k <> " " <> show l <> " " <> show m <> " " <> show n <> ")"

instance fromSQLRowRow14 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j, FromSQLValue k, FromSQLValue l, FromSQLValue m, FromSQLValue n) => FromSQLRow (Row14 a b c d e f g h i j k l m n) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j, k, l, m, n] =
    pure Row14
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
    <*> fromSQLValue k
    <*> fromSQLValue l
    <*> fromSQLValue m
    <*> fromSQLValue n
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 14."
    where n = Array.length xs

instance toSQLRowRow14 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j, ToSQLValue k, ToSQLValue l, ToSQLValue m, ToSQLValue n) => ToSQLRow (Row14 a b c d e f g h i j k l m n) where
  toSQLRow (Row14 a b c d e f g h i j k l m n) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j, toSQLValue k, toSQLValue l, toSQLValue m, toSQLValue n]
-- | A row with 15 fields.
data Row15 a b c d e f g h i j k l m n o = Row15 a b c d e f g h i j k l m n o

derive instance eqRow15 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) => Eq (Row15 a b c d e f g h i j k l m n o)

derive instance ordRow15 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o) => Ord (Row15 a b c d e f g h i j k l m n o)

instance showRow15 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) => Show (Row15 a b c d e f g h i j k l m n o) where
  show (Row15 a b c d e f g h i j k l m n o) =
    "(Row15 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> " " <> show k <> " " <> show l <> " " <> show m <> " " <> show n <> " " <> show o <> ")"

instance fromSQLRowRow15 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j, FromSQLValue k, FromSQLValue l, FromSQLValue m, FromSQLValue n, FromSQLValue o) => FromSQLRow (Row15 a b c d e f g h i j k l m n o) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o] =
    pure Row15
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
    <*> fromSQLValue k
    <*> fromSQLValue l
    <*> fromSQLValue m
    <*> fromSQLValue n
    <*> fromSQLValue o
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 15."
    where n = Array.length xs

instance toSQLRowRow15 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j, ToSQLValue k, ToSQLValue l, ToSQLValue m, ToSQLValue n, ToSQLValue o) => ToSQLRow (Row15 a b c d e f g h i j k l m n o) where
  toSQLRow (Row15 a b c d e f g h i j k l m n o) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j, toSQLValue k, toSQLValue l, toSQLValue m, toSQLValue n, toSQLValue o]
-- | A row with 16 fields.
data Row16 a b c d e f g h i j k l m n o p = Row16 a b c d e f g h i j k l m n o p

derive instance eqRow16 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p) => Eq (Row16 a b c d e f g h i j k l m n o p)

derive instance ordRow16 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o, Ord p) => Ord (Row16 a b c d e f g h i j k l m n o p)

instance showRow16 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p) => Show (Row16 a b c d e f g h i j k l m n o p) where
  show (Row16 a b c d e f g h i j k l m n o p) =
    "(Row16 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> " " <> show k <> " " <> show l <> " " <> show m <> " " <> show n <> " " <> show o <> " " <> show p <> ")"

instance fromSQLRowRow16 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j, FromSQLValue k, FromSQLValue l, FromSQLValue m, FromSQLValue n, FromSQLValue o, FromSQLValue p) => FromSQLRow (Row16 a b c d e f g h i j k l m n o p) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] =
    pure Row16
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
    <*> fromSQLValue k
    <*> fromSQLValue l
    <*> fromSQLValue m
    <*> fromSQLValue n
    <*> fromSQLValue o
    <*> fromSQLValue p
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 16."
    where n = Array.length xs

instance toSQLRowRow16 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j, ToSQLValue k, ToSQLValue l, ToSQLValue m, ToSQLValue n, ToSQLValue o, ToSQLValue p) => ToSQLRow (Row16 a b c d e f g h i j k l m n o p) where
  toSQLRow (Row16 a b c d e f g h i j k l m n o p) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j, toSQLValue k, toSQLValue l, toSQLValue m, toSQLValue n, toSQLValue o, toSQLValue p]
-- | A row with 17 fields.
data Row17 a b c d e f g h i j k l m n o p q = Row17 a b c d e f g h i j k l m n o p q

derive instance eqRow17 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p, Eq q) => Eq (Row17 a b c d e f g h i j k l m n o p q)

derive instance ordRow17 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o, Ord p, Ord q) => Ord (Row17 a b c d e f g h i j k l m n o p q)

instance showRow17 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q) => Show (Row17 a b c d e f g h i j k l m n o p q) where
  show (Row17 a b c d e f g h i j k l m n o p q) =
    "(Row17 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> " " <> show k <> " " <> show l <> " " <> show m <> " " <> show n <> " " <> show o <> " " <> show p <> " " <> show q <> ")"

instance fromSQLRowRow17 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j, FromSQLValue k, FromSQLValue l, FromSQLValue m, FromSQLValue n, FromSQLValue o, FromSQLValue p, FromSQLValue q) => FromSQLRow (Row17 a b c d e f g h i j k l m n o p q) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q] =
    pure Row17
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
    <*> fromSQLValue k
    <*> fromSQLValue l
    <*> fromSQLValue m
    <*> fromSQLValue n
    <*> fromSQLValue o
    <*> fromSQLValue p
    <*> fromSQLValue q
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 17."
    where n = Array.length xs

instance toSQLRowRow17 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j, ToSQLValue k, ToSQLValue l, ToSQLValue m, ToSQLValue n, ToSQLValue o, ToSQLValue p, ToSQLValue q) => ToSQLRow (Row17 a b c d e f g h i j k l m n o p q) where
  toSQLRow (Row17 a b c d e f g h i j k l m n o p q) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j, toSQLValue k, toSQLValue l, toSQLValue m, toSQLValue n, toSQLValue o, toSQLValue p, toSQLValue q]
-- | A row with 18 fields.
data Row18 a b c d e f g h i j k l m n o p q r = Row18 a b c d e f g h i j k l m n o p q r

derive instance eqRow18 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p, Eq q, Eq r) => Eq (Row18 a b c d e f g h i j k l m n o p q r)

derive instance ordRow18 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o, Ord p, Ord q, Ord r) => Ord (Row18 a b c d e f g h i j k l m n o p q r)

instance showRow18 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r) => Show (Row18 a b c d e f g h i j k l m n o p q r) where
  show (Row18 a b c d e f g h i j k l m n o p q r) =
    "(Row18 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> " " <> show k <> " " <> show l <> " " <> show m <> " " <> show n <> " " <> show o <> " " <> show p <> " " <> show q <> " " <> show r <> ")"

instance fromSQLRowRow18 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j, FromSQLValue k, FromSQLValue l, FromSQLValue m, FromSQLValue n, FromSQLValue o, FromSQLValue p, FromSQLValue q, FromSQLValue r) => FromSQLRow (Row18 a b c d e f g h i j k l m n o p q r) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r] =
    pure Row18
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
    <*> fromSQLValue k
    <*> fromSQLValue l
    <*> fromSQLValue m
    <*> fromSQLValue n
    <*> fromSQLValue o
    <*> fromSQLValue p
    <*> fromSQLValue q
    <*> fromSQLValue r
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 18."
    where n = Array.length xs

instance toSQLRowRow18 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j, ToSQLValue k, ToSQLValue l, ToSQLValue m, ToSQLValue n, ToSQLValue o, ToSQLValue p, ToSQLValue q, ToSQLValue r) => ToSQLRow (Row18 a b c d e f g h i j k l m n o p q r) where
  toSQLRow (Row18 a b c d e f g h i j k l m n o p q r) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j, toSQLValue k, toSQLValue l, toSQLValue m, toSQLValue n, toSQLValue o, toSQLValue p, toSQLValue q, toSQLValue r]
-- | A row with 19 fields.
data Row19 a b c d e f g h i j k l m n o p q r s = Row19 a b c d e f g h i j k l m n o p q r s

derive instance eqRow19 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p, Eq q, Eq r, Eq s) => Eq (Row19 a b c d e f g h i j k l m n o p q r s)

derive instance ordRow19 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o, Ord p, Ord q, Ord r, Ord s) => Ord (Row19 a b c d e f g h i j k l m n o p q r s)

instance showRow19 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r, Show s) => Show (Row19 a b c d e f g h i j k l m n o p q r s) where
  show (Row19 a b c d e f g h i j k l m n o p q r s) =
    "(Row19 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> " " <> show k <> " " <> show l <> " " <> show m <> " " <> show n <> " " <> show o <> " " <> show p <> " " <> show q <> " " <> show r <> " " <> show s <> ")"

instance fromSQLRowRow19 :: (FromSQLValue a, FromSQLValue b, FromSQLValue c, FromSQLValue d, FromSQLValue e, FromSQLValue f, FromSQLValue g, FromSQLValue h, FromSQLValue i, FromSQLValue j, FromSQLValue k, FromSQLValue l, FromSQLValue m, FromSQLValue n, FromSQLValue o, FromSQLValue p, FromSQLValue q, FromSQLValue r, FromSQLValue s) => FromSQLRow (Row19 a b c d e f g h i j k l m n o p q r s) where
  fromSQLRow [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s] =
    pure Row19
    <*> fromSQLValue a
    <*> fromSQLValue b
    <*> fromSQLValue c
    <*> fromSQLValue d
    <*> fromSQLValue e
    <*> fromSQLValue f
    <*> fromSQLValue g
    <*> fromSQLValue h
    <*> fromSQLValue i
    <*> fromSQLValue j
    <*> fromSQLValue k
    <*> fromSQLValue l
    <*> fromSQLValue m
    <*> fromSQLValue n
    <*> fromSQLValue o
    <*> fromSQLValue p
    <*> fromSQLValue q
    <*> fromSQLValue r
    <*> fromSQLValue s
  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 19."
    where n = Array.length xs

instance toSQLRowRow19 :: (ToSQLValue a, ToSQLValue b, ToSQLValue c, ToSQLValue d, ToSQLValue e, ToSQLValue f, ToSQLValue g, ToSQLValue h, ToSQLValue i, ToSQLValue j, ToSQLValue k, ToSQLValue l, ToSQLValue m, ToSQLValue n, ToSQLValue o, ToSQLValue p, ToSQLValue q, ToSQLValue r, ToSQLValue s) => ToSQLRow (Row19 a b c d e f g h i j k l m n o p q r s) where
  toSQLRow (Row19 a b c d e f g h i j k l m n o p q r s) =
    [toSQLValue a, toSQLValue b, toSQLValue c, toSQLValue d, toSQLValue e, toSQLValue f, toSQLValue g, toSQLValue h, toSQLValue i, toSQLValue j, toSQLValue k, toSQLValue l, toSQLValue m, toSQLValue n, toSQLValue o, toSQLValue p, toSQLValue q, toSQLValue r, toSQLValue s]
