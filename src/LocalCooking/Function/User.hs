module LocalCooking.Function.User where

import LocalCooking.Function.System (getUserId, liftDb)
import LocalCooking.Semantics.User (HasRole (..), UserExists (..))
import LocalCooking.Database.Schema (hasRole)
import LocalCooking.Common.User.Role (UserRole)



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
    UserExists userId -> do
      isEditor $ liftDb $ hasRole userId Editor
      if not isEditor
        then pure (UserExists DoesntHaveRole)
        else (\f -> UserExists $ HasRole $ f userId) <$> fX
    _ -> pure mUserId
