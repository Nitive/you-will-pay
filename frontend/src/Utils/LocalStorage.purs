module Utils.LocalStorage where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM.WebStorage (DOM, STORAGE, getItem, getLocalStorage, setItem)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude (Unit, bind, pure, ($))

newtype LocalStorageContent = LocalStorageContent
  { userId :: Maybe Int
  }
derive instance genericLocalStorageContent :: Generic LocalStorageContent

defaultLocalStorageContent :: LocalStorageContent
defaultLocalStorageContent = LocalStorageContent
  { userId: Nothing
  }

data LocalStorageKey a = LocalStorageKey
derive instance genericLocalStorageKey :: Generic (LocalStorageKey a)

type LocalStorageEffects e = Eff (storage :: STORAGE, dom :: DOM | e)


getLocalStorageContent :: forall e. LocalStorageEffects e LocalStorageContent
getLocalStorageContent = do
  localStorage <- getLocalStorage
  content <- getItem localStorage LocalStorageKey
  pure $ fromMaybe defaultLocalStorageContent content


setLocalStorageContent :: forall e. LocalStorageContent -> LocalStorageEffects e Unit
setLocalStorageContent content = do
  localStorage <- getLocalStorage
  setItem localStorage LocalStorageKey content


updateLocalStorageContent :: forall e. (LocalStorageContent -> LocalStorageContent) -> LocalStorageEffects e Unit
updateLocalStorageContent update = do
  content <- liftEff getLocalStorageContent
  setLocalStorageContent (update content)
