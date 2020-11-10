module Database.PostgreSQL.Aff
  ( DBHandle
  , PGError(..)
  , PGErrorDetail
  , Connection
  , ConnectResult
  , Query(..)
  , connect
  , withConnection
  , withConnectionTransaction
  , withDBHandle
  , withTransaction
  , command
  , execute
  , query
  , scalar
  ) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Profunctor (lcmap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Database.PostgreSQL.Pool (Pool)
import Database.PostgreSQL.Row (class FromSQLRow, class ToSQLRow, Row0(..), Row1(..), fromSQLRow, toSQLRow)
import Database.PostgreSQL.Value (class FromSQLValue)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

-- | PostgreSQL connection.
foreign import data Connection :: Type

-- | PostgreSQL query with parameter (`$1`, `$2`, …) and return types.
newtype Query i o
  = Query String

derive instance newtypeQuery :: Newtype (Query i o) _

-- | Run an action with a connection. The connection is released to the pool
-- | when the action returns.
withConnection ::
  forall a.
  Pool ->
  (Either PGError Connection -> Aff a) ->
  Aff a
withConnection p k = bracket (connect p) cleanup run
  where
  cleanup (Left _) = pure unit

  cleanup (Right { done }) = liftEffect done

  run (Left err) = k (Left err)

  run (Right { connection }) = k (Right connection)

-- | Trivial helper / shortcut which also wraps
-- | the connection to provide `DBHandle`.
withDBHandle ::
  forall a.
  Pool ->
  (Either PGError DBHandle -> Aff a) ->
  Aff a
withDBHandle p k = withConnection p (lcmap (map Right) k)

connect ::
  Pool ->
  Aff (Either PGError ConnectResult)
connect =
  fromEffectFnAff
    <<< ffiConnect
        { nullableLeft: toNullable <<< map Left <<< convertError
        , right: Right
        }

type ConnectResult
  = { connection :: Connection
    , done :: Effect Unit
    }

foreign import ffiConnect ::
  forall a.
  { nullableLeft :: Error -> Nullable (Either PGError ConnectResult)
  , right :: a -> Either PGError ConnectResult
  } ->
  Pool ->
  EffectFnAff (Either PGError ConnectResult)

-- | TODO: Provide docs
withTransaction ::
  forall a.
  Pool ->
  (DBHandle -> Aff a) ->
  Aff (Either PGError a)
withTransaction pool action =
  withConnection pool case _ of
    Right conn -> withConnectionTransaction conn do
      (action $ Right conn)
    Left err → pure $ Left err

-- | TODO: Outdated docs
-- | Run an action within a transaction. The transaction is committed if the
-- | action returns cleanly, and rolled back if the action throws (either a
-- | `PGError` or a JavaScript exception in the Aff context). If you want to
-- | change the transaction mode, issue a separate `SET TRANSACTION` statement
-- | within the transaction.
withConnectionTransaction ::
  forall a.
  Connection ->
  Aff a ->
  Aff (Either PGError a)
withConnectionTransaction conn action =
  begin
    >>= case _ of
        Nothing -> do
          a <-
            action
              `catchError`
                \jsErr -> do
                  void $ rollback
                  throwError jsErr
          commit
            >>= case _ of
                Just pgError -> pure (Left pgError)
                Nothing -> pure (Right a)
        Just pgError -> pure (Left pgError)
  where
  h = Right conn
  begin = execute h (Query "BEGIN TRANSACTION") Row0

  commit = execute h (Query "COMMIT TRANSACTION") Row0

  rollback = execute h (Query "ROLLBACK TRANSACTION") Row0

type DBHandle
  = Either Pool Connection

-- | APIs of the `Pool.query` and `Client.query` are the same.
-- | We can dse this polyformphis to simplify ffi.
foreign import data UntaggedDBHandle ∷ Type

-- | Execute a PostgreSQL query and discard its results.
execute ::
  forall i o.
  (ToSQLRow i) =>
  DBHandle ->
  Query i o ->
  i ->
  Aff (Maybe PGError)
execute h (Query sql) values = hush <<< either Right Left <$> unsafeQuery h sql (toSQLRow values)

-- | Execute a PostgreSQL query and return its results.
query ::
  forall i o.
  ToSQLRow i =>
  FromSQLRow o =>
  DBHandle ->
  Query i o ->
  i ->
  Aff (Either PGError (Array o))
query h (Query sql) values = do
  r <- unsafeQuery h sql (toSQLRow values)
  pure $ r >>= _.rows >>> traverse (fromSQLRow >>> lmap ConversionError)

-- | Execute a PostgreSQL query and return the first field of the first row in
-- | the result.
scalar ::
  forall i o.
  ToSQLRow i =>
  FromSQLValue o =>
  DBHandle ->
  Query i (Row1 o) ->
  i ->
  Aff (Either PGError (Maybe o))
scalar h sql values = query h sql values <#> map (head >>> map (case _ of Row1 a -> a))

-- | Execute a PostgreSQL query and return its command tag value
-- | (how many rows were affected by the query). This may be useful
-- | for example with `DELETE` or `UPDATE` queries.
command ::
  forall i.
  ToSQLRow i =>
  DBHandle ->
  Query i Int ->
  i ->
  Aff (Either PGError Int)
command h (Query sql) values = map _.rowCount <$> unsafeQuery h sql (toSQLRow values)

type QueryResult
  = { rows :: Array (Array Foreign)
    , rowCount :: Int
    }

unsafeQuery ::
  DBHandle ->
  String ->
  Array Foreign ->
  Aff (Either PGError QueryResult)
unsafeQuery c s = fromEffectFnAff <<< ffiUnsafeQuery p (toUntaggedHandler c) s
  where
  toUntaggedHandler ∷ DBHandle → UntaggedDBHandle
  toUntaggedHandler (Left pool) = unsafeCoerce pool

  toUntaggedHandler (Right conn) = unsafeCoerce conn

  p =
    { nullableLeft: toNullable <<< map Left <<< convertError
    , right: Right
    }

foreign import ffiUnsafeQuery ::
  { nullableLeft :: Error -> Nullable (Either PGError QueryResult)
  , right :: QueryResult -> Either PGError QueryResult
  } ->
  UntaggedDBHandle ->
  String ->
  Array Foreign ->
  EffectFnAff (Either PGError QueryResult)

data PGError
  = ConnectionError String
  | ConversionError String
  | InternalError PGErrorDetail
  | OperationalError PGErrorDetail
  | ProgrammingError PGErrorDetail
  | IntegrityError PGErrorDetail
  | DataError PGErrorDetail
  | NotSupportedError PGErrorDetail
  | QueryCanceledError PGErrorDetail
  | TransactionRollbackError PGErrorDetail

derive instance eqPGError :: Eq PGError

derive instance genericPGError :: Generic PGError _

instance showPGError :: Show PGError where
  show = genericShow

type PGErrorDetail
  = { severity :: String
    , code :: String
    , message :: String
    , detail :: String
    , hint :: String
    , position :: String
    , internalPosition :: String
    , internalQuery :: String
    , where_ :: String
    , schema :: String
    , table :: String
    , column :: String
    , dataType :: String
    , constraint :: String
    , file :: String
    , line :: String
    , routine :: String
    }

foreign import ffiSQLState :: Error -> Nullable String

foreign import ffiErrorDetail :: Error -> PGErrorDetail

convertError :: Error -> Maybe PGError
convertError err = case toMaybe $ ffiSQLState err of
  Nothing -> Nothing
  Just sqlState -> Just $ convert sqlState $ ffiErrorDetail err
  where
  convert :: String -> PGErrorDetail -> PGError
  convert s =
    if prefix "0A" s then
      NotSupportedError
    else
      if prefix "20" s || prefix "21" s then
        ProgrammingError
      else
        if prefix "22" s then
          DataError
        else
          if prefix "23" s then
            IntegrityError
          else
            if prefix "24" s || prefix "25" s then
              InternalError
            else
              if prefix "26" s || prefix "27" s || prefix "28" s then
                OperationalError
              else
                if prefix "2B" s || prefix "2D" s || prefix "2F" s then
                  InternalError
                else
                  if prefix "34" s then
                    OperationalError
                  else
                    if prefix "38" s || prefix "39" s || prefix "3B" s then
                      InternalError
                    else
                      if prefix "3D" s || prefix "3F" s then
                        ProgrammingError
                      else
                        if prefix "40" s then
                          TransactionRollbackError
                        else
                          if prefix "42" s || prefix "44" s then
                            ProgrammingError
                          else
                            if s == "57014" then
                              QueryCanceledError
                            else
                              if prefix "5" s then
                                OperationalError
                              else
                                if prefix "F" s then
                                  InternalError
                                else
                                  if prefix "H" s then
                                    OperationalError
                                  else
                                    if prefix "P" s then
                                      InternalError
                                    else
                                      if prefix "X" s then
                                        InternalError
                                      else
                                        const $ ConnectionError s

  prefix :: String -> String -> Boolean
  prefix p = maybe false (_ == 0) <<< String.indexOf (Pattern p)
