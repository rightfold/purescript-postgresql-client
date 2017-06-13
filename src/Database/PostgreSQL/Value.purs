module Database.PostgreSQL.Value where

import Control.Applicative (pure)
import Control.Bind ((<=<))
import Control.Category (id)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Boolean (otherwise)
import Data.ByteString (ByteString)
import Data.Date (day, month, year)
import Data.DateTime (Date, DateTime)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (foldMap)
import Data.Foreign (Foreign, isNull, readArray, readBoolean, readChar, readInt, readNumber, readString, toForeign, unsafeFromForeign)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Identity (Identity(..))
import Data.JSDate (JSDate, fromDateTime, readDate, toDate, toDateTime)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (traverse)
import Prelude (bind)

-- | Convert things to SQL values.
class ToSQLValue a where
    toSQLValue :: a -> Foreign

-- | Convert things from SQL values.
class FromSQLValue a where
    fromSQLValue :: Foreign -> Either String a

instance toSQLValueBoolean :: ToSQLValue Boolean where
    toSQLValue = toForeign

instance fromSQLValueBoolean :: FromSQLValue Boolean where
    fromSQLValue = lmap show <<< runExcept <<< readBoolean

instance toSQLValueChar :: ToSQLValue Char where
    toSQLValue = toForeign

instance fromSQLValueChar :: FromSQLValue Char where
    fromSQLValue = lmap show <<< runExcept <<< readChar

instance toSQLValueInt :: ToSQLValue Int where
    toSQLValue = toForeign

instance fromSQLValueInt :: FromSQLValue Int where
    fromSQLValue = lmap show <<< runExcept <<< readInt

instance toSQLValueNumber :: ToSQLValue Number where
    toSQLValue = toForeign

instance fromSQLValueNumber :: FromSQLValue Number where
    fromSQLValue = lmap show <<< runExcept <<< readNumber

instance toSQLValueString :: ToSQLValue String where
    toSQLValue = toForeign

instance fromSQLValueString :: FromSQLValue String where
    fromSQLValue = lmap show <<< runExcept <<< readString

instance toSQLValueArray :: (ToSQLValue a) => ToSQLValue (Array a) where
    toSQLValue = toForeign <<< map toSQLValue

instance fromSQLValueArray :: (FromSQLValue a) => FromSQLValue (Array a) where
    fromSQLValue = traverse fromSQLValue <=< lmap show <<< runExcept <<< readArray

instance toSQLValueList :: (ToSQLValue a) => ToSQLValue (List a) where
    toSQLValue = toForeign <<< Array.fromFoldable <<< map toSQLValue

instance fromSQLValueList :: (FromSQLValue a) => FromSQLValue (List a) where
    fromSQLValue = map List.fromFoldable <<< traverse fromSQLValue <=< lmap show <<< runExcept <<< readArray

instance toSQLValueByteString :: ToSQLValue ByteString where
    toSQLValue = toForeign

instance fromSQLValueByteString :: FromSQLValue ByteString where
    fromSQLValue x
        | unsafeIsBuffer x = pure $ unsafeFromForeign x
        | otherwise = throwError "FromSQLValue ByteString: not a buffer"

instance toSQLValueInstant :: ToSQLValue Instant where
    toSQLValue = instantToString

instance toSQLValueMaybe :: (ToSQLValue a) => ToSQLValue (Maybe a) where
    toSQLValue Nothing = null
    toSQLValue (Just x) = toSQLValue x

instance fromSQLValueMaybe :: (FromSQLValue a) => FromSQLValue (Maybe a) where
    fromSQLValue x | isNull x  = pure Nothing
                   | otherwise = Just <$> fromSQLValue x

instance toSQLValueDate :: ToSQLValue Date where
    toSQLValue date = 
        let yr = fromEnum $ year date
            mnth = fromEnum $ month date
            dy = fromEnum $ day date
        in
        toForeign $ (show yr)<>"-"<>(show mnth)<>"-"<>(show dy)

instance fromSQLValueDate :: FromSQLValue Date where
    fromSQLValue x = do
        let Identity(jsDateE) = runExceptT $ readDate x
        jsDate <- bimap (foldMap show) id jsDateE
        case toDate (compensateTZ jsDate) of
            Just date -> Right date
            Nothing -> Left "err"

instance toSQLValueDateTime :: ToSQLValue DateTime where
    toSQLValue date = toForeign $ fromDateTime date 

instance fromSQLValueDateTime :: FromSQLValue DateTime where
    fromSQLValue x = do
        let Identity(jsDateE) = runExceptT $ readDate x
        jsDate <- bimap (foldMap show) id jsDateE
        case toDateTime jsDate of
            Just date -> Right date
            Nothing -> Left "err"

instance toSQLValueForeign :: ToSQLValue Foreign where
    toSQLValue = id

instance fromSQLValueForeign :: FromSQLValue Foreign where
    fromSQLValue = pure

foreign import null :: Foreign
foreign import instantToString :: Instant -> Foreign
foreign import unsafeIsBuffer :: ∀ a. a -> Boolean

foreign import compensateTZ :: JSDate -> JSDate
