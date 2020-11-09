module Test.Config where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Map (fromFoldable) as Map
import Data.Newtype (un)
import Data.Validation.Semigroup (V(..))
import Database.PostgreSQL.PG (Configuration)
import Dotenv (loadFile) as DotEnv
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (toUnfoldable) as Object
import Node.Process (getEnv)
import Polyform.Batteries.Env (Env, Validator) as Env
import Polyform.Batteries.Env (MissingValue)
import Polyform.Batteries.Env.Validators (optional, required) as Env
import Polyform.Batteries.Int (IntExpected)
import Polyform.Batteries.Int (validator) as Int
import Polyform.Validator (runValidator)
import Type.Row (type (+))

validator
  ∷ ∀ err m
  . Monad m
  ⇒ Env.Validator m (IntExpected + MissingValue + err) Env.Env Configuration
validator = { database: _, host: _, idleTimeoutMillis: _, max: _, password: _, port: _, user: _ }
  <$> Env.required "PG_DB" identity
  <*> Env.optional "PG_HOST" identity
  <*> Env.optional "PG_IDLE_TIMEOUT_MILLISECONDS" Int.validator
  <*> Env.optional "PG_MAX" Int.validator
  <*> Env.optional "PG_PASSWORD" identity
  <*> Env.optional "PG_PORT" Int.validator
  <*> Env.optional "PG_USER" identity

load ∷ Aff Configuration
load = do
  void $ DotEnv.loadFile
  env ← liftEffect $ getEnv <#> (Object.toUnfoldable ∷ _ → Array _) >>> Map.fromFoldable
  runValidator validator env >>= un V >>> case _ of
    Left err → do
      throwError $ error "Configuration error. Please verify your environment and .env file."
    Right p → pure p


