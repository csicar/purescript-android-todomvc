module Android.Ui where

import Prelude

import Android.Attributes (EditType, Layout, TextStyle, Orientation(..), orientation, text, textSize)
import Control.Monad.Free (Free, hoistFree, liftF)
import Data.CatList (CatList)
import Data.Foldable (foldr)
import Data.Function as Function
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (log)
import Record as Record

type TouchEvents e r = {onClick :: Maybe e | r}

type LayoutParameter e r = {width :: Maybe Layout, height:: Maybe Layout | r}

type ButtonAttr e = TouchEvents e (textSize :: Maybe Number, text :: Maybe String, textStyle:: Maybe TextStyle)

type LinearLayoutAttr e = { orientation :: Maybe Orientation }

type EditTextAttr e = LayoutParameter e
  ( onTextChange :: Maybe (String -> e)
  , onSubmit :: Maybe e
  , text :: Maybe String
  , editType :: Maybe EditType
  , singleLine :: Maybe Boolean
  )

type CheckboxAttr e = 
  { checked :: Maybe Boolean
  , onChecked :: Maybe (Boolean -> e)
  }

type TextViewAttr e = LayoutParameter e
  ( text :: Maybe String
  , textSize :: Maybe Number
  , textStyle :: Maybe TextStyle
  )

data Ui e
  = Button (ButtonAttr e)
  | LinearLayout (LinearLayoutAttr e) (Array (Ui e))
  | EditText (EditTextAttr e)
  | Checkbox (CheckboxAttr e)
  | TextView (TextViewAttr e)
  | Empty

button :: ∀e. Array (ButtonAttr e -> ButtonAttr e) -> Ui e
button = foldr ($) {textSize : Nothing, text : Nothing, onClick : Nothing, textStyle: Nothing} >>> Button

linearLayout :: ∀e. Array (LinearLayoutAttr e -> LinearLayoutAttr e) -> Array (Ui e) -> Ui e
linearLayout attrs children = LinearLayout (foldr ($) {orientation : Nothing} attrs) children

editText :: ∀e. Array (EditTextAttr e -> EditTextAttr e) -> Ui e
editText = foldr ($) {text: Nothing, onTextChange: Nothing, width: Nothing, height: Nothing, editType: Nothing, singleLine: Nothing, onSubmit: Nothing} >>> EditText

checkbox :: ∀e. Array (CheckboxAttr e -> CheckboxAttr e) -> Ui e
checkbox = foldr ($) {checked : Nothing, onChecked: Nothing} >>> Checkbox

textView :: ∀e. Array (TextViewAttr e -> TextViewAttr e) -> Ui e
textView = foldr ($) {text: Nothing, textSize: Nothing, textStyle: Nothing, height: Nothing, width: Nothing} >>> TextView