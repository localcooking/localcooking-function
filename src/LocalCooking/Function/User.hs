{-# LANGUAGE
    NamedFieldPuns
  #-}

module LocalCooking.Function.User where

import LocalCooking.Function.System
  (SystemM, liftDb, TokenContexts (..), SystemEnv (..), getSystemEnv)
import LocalCooking.Function.System.AccessToken (lookupAccess)
import LocalCooking.Semantics.User (HasRole (..), UserExists (..))
import LocalCooking.Database.Schema (hasRole, StoredUserId)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import Control.Monad.IO.Class (liftIO)



getUserId :: AuthToken -> SystemM (UserExists StoredUserId)
getUserId authToken = do
  SystemEnv{systemEnvTokenContexts} <- getSystemEnv
  case systemEnvTokenContexts of
    TokenContexts{tokenContextAuth} ->
      let mToU mX = case mX of
            Nothing -> UserDoesntExist
            Just x -> UserExists x
      in  mToU <$> liftIO (lookupAccess tokenContextAuth authToken)


verifyRole :: UserRole
           -> AuthToken
           -> SystemM (StoredUserId -> a)
           -> SystemM (UserExists (HasRole a))
verifyRole role authToken fX = do
  mUserId <- getUserId authToken
  case mUserId of
    UserExists userId -> fmap UserExists $ do
      isEditor <- liftDb (hasRole userId role)
      if not isEditor
        then pure DoesntHaveRole
        else (\f -> HasRole $ f userId) <$> fX
    UserDoesntExist -> pure UserDoesntExist
