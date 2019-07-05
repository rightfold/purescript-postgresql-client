module Database.PostgreSQL.Value where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, except, runExcept, runExceptT)
import Data.Array (foldl)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.ByteString (ByteString)
import Data.Date (Date, canonicalDate, day, month, year)
import Data.DateTime.Instant (Instant, instant)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Either (Either(..), note)
import Data.Enum (fromEnum, toEnum)
import Data.Identity (Identity)
import Data.Int (fromString)
import Data.JSDate (JSDate)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), split)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence, traverse)
import Foreign (Foreign, ForeignError(..), MultipleErrors, isNull, readArray, readBoolean, readChar, readInt, readNumber, readString, renderForeignError, unsafeFromForeign, unsafeToForeign)
import Foreign.Internal (readObject)
import Foreign.Object (Object)

-- | Convert things to SQL values.
class ToSQLValue a where
    toSQLValue :: a -> Foreign

-- | Convert things from SQL values.
class FromSQLValue a where
    fromSQLValue :: Foreign -> Either String a

instance fromSQLValueBoolean :: FromSQLValue Boolean where
    fromSQLValue = lmap show <<< runExcept <<< readBoolean

else instance fromSQLValueChar :: FromSQLValue Char where
    fromSQLValue = lmap show <<< runExcept <<< readChar

else instance fromSQLValueInt :: FromSQLValue Int where
    fromSQLValue = lmap show <<< runExcept <<< readInt

else instance fromSQLValueNumber :: FromSQLValue Number where
    fromSQLValue = lmap show <<< runExcept <<< readNumber

else instance fromSQLValueString :: FromSQLValue String where
    fromSQLValue = lmap show <<< runExcept <<< readString

else instance fromSQLValueArray :: (FromSQLValue a) => FromSQLValue (Array a) where
    fromSQLValue = traverse fromSQLValue <=< lmap show <<< runExcept <<< readArray

else instance fromSQLValueList :: (FromSQLValue a) => FromSQLValue (List a) where
    fromSQLValue = map List.fromFoldable <<< traverse fromSQLValue <=< lmap show <<< runExcept <<< readArray

else instance fromSQLValueByteString :: FromSQLValue ByteString where
    fromSQLValue x
        | unsafeIsBuffer x = pure $ unsafeFromForeign x
        | otherwise = throwError "FromSQLValue ByteString: not a buffer"

else instance fromSQLValueInstant :: FromSQLValue Instant where
    fromSQLValue v = do
      t <- instantFromString Left Right v
      note ("Instant construction failed for given timestamp: " <> show t) $ instant (Milliseconds t)

else instance fromSQLValueDate :: FromSQLValue Date where
    fromSQLValue v = do
        s <- lmap show $ runExcept (readString v)
        let
            msg = "Date parsing failed for value: " <> s
        case split (Pattern "-") s of
            [y, m, d] -> do
                let
                  result = canonicalDate
                    <$> (toEnum =<< fromString y)
                    <*> (toEnum =<< fromString m)
                    <*> (toEnum =<< fromString d)
                note msg result
            _ -> Left msg

else instance fromSQLValueJSDate :: FromSQLValue JSDate where
    fromSQLValue = Right <<< unsafeFromForeign

else instance fromSQLValueMaybe :: (FromSQLValue a) => FromSQLValue (Maybe a) where
    fromSQLValue x | isNull x  = pure Nothing
                   | otherwise = Just <$> fromSQLValue x

else instance fromSQLValueForeign :: FromSQLValue Foreign where
    fromSQLValue = pure

else instance fromSQLValueObject ∷ FromSQLValue a ⇒ FromSQLValue (Object a) where
  fromSQLValue sql = lmap showErr $ unwrap $ runExceptT main
    where
    showErr ∷ MultipleErrors → String
    showErr e = foldl (\a x → a <> renderForeignError x <> " ") "" e
    main ∷ ExceptT MultipleErrors Identity (Object a)
    main = do
      objF ∷ Object Foreign <- readObject sql
      let eso = sequence $ map fromSQLValue objF
      let emo = lmap (singleton <<< ForeignError) eso
      except emo
else instance fromSQLValueDecimal :: FromSQLValue Decimal where
    fromSQLValue v = do
        s <- lmap show $ runExcept (readString v)
        note ("Decimal literal parsing failed: " <> s) (Decimal.fromString s)

newtypeFromSQLValue ∷ ∀ a b. Newtype a b ⇒ FromSQLValue b ⇒ Foreign → Either String a
newtypeFromSQLValue = map wrap <<< fromSQLValue

instance toSQLValueBoolean :: ToSQLValue Boolean where
    toSQLValue = unsafeToForeign

else instance toSQLValueChar :: ToSQLValue Char where
    toSQLValue = unsafeToForeign

else instance toSQLValueInt :: ToSQLValue Int where
    toSQLValue = unsafeToForeign

else instance toSQLValueNumber :: ToSQLValue Number where
    toSQLValue = unsafeToForeign

else instance toSQLValueString :: ToSQLValue String where
    toSQLValue = unsafeToForeign

else instance toSQLValueArray :: (ToSQLValue a) => ToSQLValue (Array a) where
    toSQLValue = unsafeToForeign <<< map toSQLValue

else instance toSQLValueList :: (ToSQLValue a) => ToSQLValue (List a) where
    toSQLValue = unsafeToForeign <<< Array.fromFoldable <<< map toSQLValue

else instance toSQLValueByteString :: ToSQLValue ByteString where
    toSQLValue = unsafeToForeign

else instance toSQLValueInstant :: ToSQLValue Instant where
    toSQLValue = instantToString

else instance toSQLValueDate :: ToSQLValue Date where
    toSQLValue date =
        let
            y = fromEnum $ year date
            m = fromEnum $ month date
            d = fromEnum $ day date
        in
            unsafeToForeign $ show y <> "-" <> show m <> "-" <> show d

else instance toSQLValueJSDate :: ToSQLValue JSDate where
    toSQLValue = unsafeToForeign

else instance toSQLValueMaybe :: (ToSQLValue a) => ToSQLValue (Maybe a) where
    toSQLValue Nothing = null
    toSQLValue (Just x) = toSQLValue x

else instance toSQLValueForeign :: ToSQLValue Foreign where
    toSQLValue = identity

else instance toSQLValueObject ∷ ToSQLValue a ⇒ ToSQLValue (Object a) where
  toSQLValue = unsafeToForeign

else instance toSQLValueDecimal :: ToSQLValue Decimal where
    toSQLValue = Decimal.toString >>> unsafeToForeign

newtypeToSQLValue ∷ ∀ a b. Newtype a b ⇒ ToSQLValue b ⇒ a → Foreign
newtypeToSQLValue = unwrap >>> toSQLValue

foreign import null :: Foreign
foreign import instantToString :: Instant -> Foreign
foreign import instantFromString :: (String -> Either String Number) -> (Number -> Either String Number) -> Foreign -> Either String Number
foreign import unsafeIsBuffer :: ∀ a. a -> Boolean
