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
import Data.Newtype (unwrap)
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

instance toSQLValueBoolean :: ToSQLValue Boolean where
    toSQLValue = unsafeToForeign

instance fromSQLValueBoolean :: FromSQLValue Boolean where
    fromSQLValue = lmap show <<< runExcept <<< readBoolean

instance toSQLValueChar :: ToSQLValue Char where
    toSQLValue = unsafeToForeign

instance fromSQLValueChar :: FromSQLValue Char where
    fromSQLValue = lmap show <<< runExcept <<< readChar

instance toSQLValueInt :: ToSQLValue Int where
    toSQLValue = unsafeToForeign

instance fromSQLValueInt :: FromSQLValue Int where
    fromSQLValue = lmap show <<< runExcept <<< readInt

instance toSQLValueNumber :: ToSQLValue Number where
    toSQLValue = unsafeToForeign

instance fromSQLValueNumber :: FromSQLValue Number where
    fromSQLValue = lmap show <<< runExcept <<< readNumber

instance toSQLValueString :: ToSQLValue String where
    toSQLValue = unsafeToForeign

instance fromSQLValueString :: FromSQLValue String where
    fromSQLValue = lmap show <<< runExcept <<< readString

instance toSQLValueArray :: (ToSQLValue a) => ToSQLValue (Array a) where
    toSQLValue = unsafeToForeign <<< map toSQLValue

instance fromSQLValueArray :: (FromSQLValue a) => FromSQLValue (Array a) where
    fromSQLValue = traverse fromSQLValue <=< lmap show <<< runExcept <<< readArray

instance toSQLValueList :: (ToSQLValue a) => ToSQLValue (List a) where
    toSQLValue = unsafeToForeign <<< Array.fromFoldable <<< map toSQLValue

instance fromSQLValueList :: (FromSQLValue a) => FromSQLValue (List a) where
    fromSQLValue = map List.fromFoldable <<< traverse fromSQLValue <=< lmap show <<< runExcept <<< readArray

instance toSQLValueByteString :: ToSQLValue ByteString where
    toSQLValue = unsafeToForeign

instance fromSQLValueByteString :: FromSQLValue ByteString where
    fromSQLValue x
        | unsafeIsBuffer x = pure $ unsafeFromForeign x
        | otherwise = throwError "FromSQLValue ByteString: not a buffer"

instance toSQLValueInstant :: ToSQLValue Instant where
    toSQLValue = instantToString

instance fromSQLValueInstant :: FromSQLValue Instant where
    fromSQLValue v = do
      t <- instantFromString Left Right v
      note ("Instant construction failed for given timestamp: " <> show t) $ instant (Milliseconds t)

instance toSQLValueDate :: ToSQLValue Date where
    toSQLValue date =
        let
            y = fromEnum $ year date
            m = fromEnum $ month date
            d = fromEnum $ day date
        in
            unsafeToForeign $ show y <> "-" <> show m <> "-" <> show d

instance fromSQLValueDate :: FromSQLValue Date where
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

instance toSQLValueJSDate :: ToSQLValue JSDate where
    toSQLValue = unsafeToForeign

instance fromSQLValueJSDate :: FromSQLValue JSDate where
    fromSQLValue = Right <<< unsafeFromForeign

instance toSQLValueMaybe :: (ToSQLValue a) => ToSQLValue (Maybe a) where
    toSQLValue Nothing = null
    toSQLValue (Just x) = toSQLValue x

instance fromSQLValueMaybe :: (FromSQLValue a) => FromSQLValue (Maybe a) where
    fromSQLValue x | isNull x  = pure Nothing
                   | otherwise = Just <$> fromSQLValue x

instance toSQLValueForeign :: ToSQLValue Foreign where
    toSQLValue = identity

instance fromSQLValueForeign :: FromSQLValue Foreign where
    fromSQLValue = pure

instance toSQLValueObject ∷ ToSQLValue a ⇒ ToSQLValue (Object a) where
  toSQLValue = unsafeToForeign

instance fromSQLValueObject ∷ FromSQLValue a ⇒ FromSQLValue (Object a) where
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

instance toSQLValueDecimal :: ToSQLValue Decimal where
    toSQLValue = Decimal.toString >>> unsafeToForeign

instance fromSQLValueDecimal :: FromSQLValue Decimal where
    fromSQLValue v = do
        s <- lmap show $ runExcept (readString v)
        note ("Decimal literal parsing failed: " <> s) (Decimal.fromString s)

foreign import null :: Foreign
foreign import instantToString :: Instant -> Foreign
foreign import instantFromString :: (String -> Either String Number) -> (Number -> Either String Number) -> Foreign -> Either String Number
foreign import unsafeIsBuffer :: ∀ a. a -> Boolean
