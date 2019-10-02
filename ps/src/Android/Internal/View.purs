module Android.Internal.View where

import Data.Function.Uncurried
import Effect.Uncurried
import Prelude

import Android.Attributes (EditType, Layout, Orientation(..), TextStyle)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Context :: Type

foreign import _addContentView :: EffectFn2 Context View Unit
addContentView a b = runEffectFn2 _addContentView a (toView b)

foreign import data WidgetId :: Type

foreign import _rId :: Fn2 Context String WidgetId
rId = runFn2 _rId

foreign import data DrawableId :: Type

foreign import _rDrawable :: Fn2 Context String DrawableId
rDrawable = runFn2 _rDrawable

--
-- 
-- View
--
--
foreign import data View :: Type

class IsView a where
    toView :: a -> View
    
instance isViewView :: IsView View where toView = identity

foreign import data TextView :: Type
instance textViewView :: IsView TextView where toView = unsafeCoerce

foreign import _setWidth :: EffectFn2 View Layout Unit
setWidth v = runEffectFn2 _setWidth (toView v)

foreign import _setHeight :: EffectFn2 View Layout Unit
setHeight v = runEffectFn2 _setHeight (toView v)

foreign import _textView :: EffectFn2 Context String TextView
textView :: Context -> String -> Effect TextView
textView = runEffectFn2 _textView

foreign import data Button :: Type
instance buttonView :: IsView Button where toView = unsafeCoerce
foreign import _button :: EffectFn2 Context String Button
button :: Context -> String -> Effect Button
button = runEffectFn2 _button

foreign import data EditText :: Type
instance isViewEditText :: IsView EditText where toView = unsafeCoerce
foreign import _editText :: EffectFn1 Context EditText
editText :: Context -> Effect EditText
editText = runEffectFn1 _editText

foreign import _setEditType :: EffectFn2 EditText EditType Unit
setEditType = runEffectFn2 _setEditType

foreign import _setSingleLine :: EffectFn2 EditText Boolean Unit
setSingleLine = runEffectFn2 _setSingleLine


foreign import _onSubmit :: EffectFn2 EditText (Effect Unit) Unit
onSubmit = runEffectFn2 _onSubmit

foreign import _onTextChange :: EffectFn2 EditText (String -> Effect Unit) Unit
onTextChange = runEffectFn2 _onTextChange

foreign import _removeOnTextChange :: EffectFn1 EditText Unit
removeOnTextChange = runEffectFn1 _removeOnTextChange


foreign import _onClick :: EffectFn2 View (Effect Unit) Unit
onClick :: forall a. IsView a => a -> Effect Unit -> Effect Unit
onClick a = runEffectFn2 _onClick (toView a)

foreign import data ImageView :: Type
instance imageViewView :: IsView ImageView where toView = unsafeCoerce

foreign import _imageView :: EffectFn2 Context DrawableId ImageView
imageView = runEffectFn2 _imageView

foreign import _setMargins :: EffectFn5 View Int Int Int Int Unit
setMargins v = runEffectFn5 _setMargins (toView v)

foreign import data Checkbox :: Type
instance isViewCheckbox :: IsView Checkbox where toView = unsafeCoerce
foreign import _checkbox :: EffectFn1 Context Checkbox
checkbox = runEffectFn1 _checkbox
foreign import _setChecked  :: EffectFn2 Checkbox Boolean Unit
setChecked = runEffectFn2 _setChecked

foreign import _onChecked :: EffectFn2 Checkbox (Boolean -> Effect Unit) Unit
onChecked = runEffectFn2 _onChecked

--
--
-- ViewGroup
--
--
foreign import data ViewGroup :: Type

class IsViewGroup a where
    toViewGroup :: a -> ViewGroup

foreign import _addView :: EffectFn2 ViewGroup View Unit
addView :: forall a b. IsViewGroup a => IsView b => a -> b -> Effect Unit
addView vg v = runEffectFn2 _addView (toViewGroup vg) (toView v)

foreign import _getView :: EffectFn2 ViewGroup Int View
getView vg int = runEffectFn2 _getView (toViewGroup vg) int

foreign import _replaceView :: EffectFn3 ViewGroup Int View Unit
replaceView vg indx v = runEffectFn3 _replaceView (toViewGroup vg) indx (toView v)

foreign import _removeView :: EffectFn2 ViewGroup Int Unit
removeView vg = runEffectFn2 _removeView (toViewGroup vg)

foreign import data LinearLayout :: Type

foreign import _linearLayout :: EffectFn1 Context LinearLayout
linearLayout ctx = runEffectFn1 _linearLayout ctx

foreign import _setOrientation :: EffectFn2 LinearLayout Boolean Unit 

setOrientation :: LinearLayout -> Orientation -> Effect Unit
setOrientation v Horizontal = runEffectFn2 _setOrientation v false
setOrientation v Vertical = runEffectFn2 _setOrientation v true

instance viewGroupLinearLayout :: IsViewGroup LinearLayout where toViewGroup = unsafeCoerce
instance layoutView :: IsView LinearLayout where toView = unsafeCoerce

foreign import data ScrollView :: Type

foreign import _scrollView :: EffectFn1 Context ScrollView
scrollView = runEffectFn1 _scrollView

instance viewGroupScrollView :: IsViewGroup ScrollView where toViewGroup = unsafeCoerce
instance viewScrollView :: IsView ScrollView where toView = unsafeCoerce

foreign import _emptyView :: EffectFn1 Context View
emptyView = runEffectFn1 _emptyView

--
--
-- TextView
--
--
class IsTextView a where
    toTextView :: a -> TextView

instance isTextViewButton :: IsTextView Button where toTextView = unsafeCoerce
instance isTextViewEditText :: IsTextView EditText where toTextView = unsafeCoerce
instance isTextViewTextView :: IsTextView TextView where toTextView = identity

foreign import _getText :: EffectFn1 TextView String
getText :: ∀a. IsTextView a => a -> Effect String
getText tv = runEffectFn1 _getText (toTextView tv)

foreign import _setTextSize :: EffectFn2 TextView Number Unit
setTextSize tv = runEffectFn2 _setTextSize (toTextView tv)

foreign import _setTextStyle :: EffectFn2 TextView TextStyle Unit
setTextStyle tv = runEffectFn2 _setTextStyle (toTextView tv)


--
--
-- SetText
--
--
class IsSetText a where
    setText :: a -> String -> Effect Unit

foreign import _setTextTextView :: EffectFn2 TextView String Unit
foreign import textToHtml :: String -> String

instance isSetTextTextView :: IsTextView a => IsSetText a where
    setText a = runEffectFn2 _setTextTextView (toTextView a)


--
--
-- RecyclerView
--
--
foreign import data RecyclerView :: Type -> Type

instance isViewRecyclerView :: IsView (RecyclerView a) where toView = unsafeCoerce

foreign import _recyclerView :: ∀a. EffectFn5 Context (Array a) (Array a) (Context -> Tuple (Effect View) a) (View -> Int -> a -> a -> Effect View) (RecyclerView a)
recyclerView = runEffectFn5 _recyclerView

foreign import _updateRecyclerView :: ∀a. EffectFn2 (RecyclerView a) (Array a) Unit
updateRecyclerView :: ∀a. (RecyclerView a) -> (Array a) -> Effect Unit
updateRecyclerView = runEffectFn2 _updateRecyclerView

foreign import _updateRecyclerViewBinder :: ∀a. EffectFn2 (RecyclerView a) (View -> Int -> a -> a -> Effect View) Unit
updateRecyclerViewBinder = runEffectFn2 _updateRecyclerViewBinder

--
--
-- ActionBar
--
--
foreign import data ActionBar :: Type

foreign import _getActionBar :: EffectFn1 Context ActionBar
foreign import _setActionBarTitle :: EffectFn2 ActionBar String Unit

instance actionBarView :: IsView ActionBar where toView = unsafeCoerce


setActionBarTitle = runEffectFn2 _setActionBarTitle
getActionBar = runEffectFn1 _getActionBar