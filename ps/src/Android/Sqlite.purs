module Android.Sqlite where

import Prelude

import Android.Internal.View (Context)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
  

foreign import data Db :: Type

foreign import _createDb :: EffectFn4 Context Int String String Db
createDb = runEffectFn4 _createDb

foreign import _execSql :: EffectFn3 Db String (Array String) Unit
execSql = runEffectFn3 _execSql

foreign import data Cursor :: Type

foreign import _querySql :: EffectFn3 Db String (Array String) Cursor
querySql = runEffectFn3 _querySql

foreign import _mapCursor :: ∀a. EffectFn2 Cursor (Cursor -> Effect a) (Array a)
mapCursor = runEffectFn2 _mapCursor

foreign import _columnAsString :: EffectFn2 Cursor String String
columnAsString = runEffectFn2 _columnAsString

foreign import _columnAsInt :: EffectFn2 Cursor String Int
columnAsInt = runEffectFn2 _columnAsInt