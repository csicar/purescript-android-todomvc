module Android.Attributes where

import Prelude

import Data.Maybe (Maybe(..))

textSize :: ∀r. Number -> {textSize :: Maybe Number | r} -> {textSize :: Maybe Number | r}
textSize a r = r {textSize = Just a}

data TextStyle
  = Bold
  | Normal
  | Monospace
derive instance eqTextStyle :: Eq TextStyle

textStyle :: ∀r. TextStyle -> {textStyle :: Maybe TextStyle | r} -> {textStyle :: Maybe TextStyle | r}
textStyle a r = r {textStyle = Just a}

text :: ∀r. String -> {text :: Maybe String | r} -> {text :: Maybe String | r}
text a r = r {text = Just a}

data Orientation = Horizontal | Vertical
derive instance eqOrientation :: Eq Orientation

orientation :: ∀r. Orientation -> {orientation :: Maybe Orientation | r} -> {orientation :: Maybe Orientation | r}
orientation a r = r {orientation = Just a}

data Layout
  = MatchParent
  | WrapContent
derive instance eqLayout :: Eq Layout

width :: ∀r. Layout -> {width:: Maybe Layout | r} -> {width:: Maybe Layout | r}
width a r = r {width = Just a}

height :: ∀r. Layout -> {height:: Maybe Layout | r} -> {height:: Maybe Layout | r}
height a r = r {height = Just a}

checked :: ∀r. Boolean -> {checked :: Maybe Boolean | r} -> {checked :: Maybe Boolean | r}
checked b = _ {checked = Just b}

data EditType
  = ActionGo
  | ActionSearch
  | ActionSend
  | ActionDone
derive instance editTypeEq :: Eq EditType

editType :: ∀r. EditType -> {editType :: Maybe EditType | r} -> {editType :: Maybe EditType | r}
editType e = _ {editType = Just e}

singleLine :: ∀r. Boolean -> {singleLine :: Maybe Boolean | r} -> {singleLine :: Maybe Boolean | r}
singleLine e = _ {singleLine = Just e}

onClick :: ∀r e. e -> {onClick :: Maybe e | r} ->  {onClick :: Maybe e | r} 
onClick e = _ {onClick = Just e}

onTextChange :: ∀r e. e -> {onTextChange :: Maybe e | r} -> {onTextChange :: Maybe e | r}
onTextChange e = _ {onTextChange = Just e}

onSubmit :: ∀r e. e -> {onSubmit :: Maybe e | r} -> {onSubmit :: Maybe e | r}
onSubmit e = _ {onSubmit = Just e}

onChecked :: ∀r e. e -> {onChecked :: Maybe e | r} -> {onChecked :: Maybe e | r}
onChecked e = _ {onChecked = Just e}