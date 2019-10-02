module Android.Render where

import Prelude

import Android.Attributes (Layout, TextStyle, editType, onTextChange, orientation, singleLine, textSize, width)
import Android.Internal.View (class IsSetText, class IsTextView, class IsView, Context, View, addView, button, checkbox, editText, emptyView, getView, linearLayout, removeView, replaceView, setChecked, setEditType, setHeight, setOrientation, setSingleLine, setText, setTextSize, setTextStyle, setWidth, textView, toView, toViewGroup)
import Android.Internal.View as I
import Android.Ui (Ui(..))
import Control.Monad.ST as ST
import Data.Array (drop, dropEnd, fold, head, length, range, reverse)
import Data.Array (zipWith, zip)
import Data.Foldable (for_, sequence_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy, trace, traceM)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)

foreign import setContentView :: View -> Context -> Effect Unit

foreign import setContent :: I.ViewGroup -> View -> Effect Unit
foreign import getContent :: I.ViewGroup -> Effect View

maybeDo :: ∀a. (a -> Effect Unit) -> Maybe a -> Effect Unit
maybeDo _ (Nothing) = pure unit
maybeDo f (Just v) = void $ f v 

type EventHandler e = e -> Effect Unit

type SimpleApp state event = {view :: state -> Ui event, update :: state -> event -> state, initial:: state }

runUi :: ∀s e.  Context -> SimpleApp s e -> Effect Unit
runUi ctx app = do
  topLayout <- I.linearLayout ctx
  setContentView (toView topLayout) ctx
  runUi' ctx app (toViewGroup topLayout) Empty

runUi' :: ∀s e. Context -> SimpleApp s e -> I.ViewGroup -> Ui e -> Effect Unit
runUi' ctx {view, initial, update} topLayout oldUi = do
  log "runUi'"
  oldView <- getContent topLayout
  let newView = view initial
  patched <- diffUi
    ctx
    -- TODO: this probably a memory leak, as it clojures over oldUi
    (\ev -> do
      runUi' ctx {view, initial : update initial ev, update} topLayout newView
    ) 
    newView
    oldUi
    oldView
  setContent topLayout patched
  pure unit

indexed :: ∀a. Array a -> Array (Tuple Int a)
indexed arr = zip (range 0 $ length arr) arr

diffSetText :: ∀r v. IsSetText v => {text:: Maybe String | r} -> {text:: Maybe String | r} -> v -> Effect Unit
diffSetText new old view = when (new.text /= old.text) $ maybeDo (setText view) new.text

diffSetTextStyle :: ∀r v. IsTextView v => {textStyle :: Maybe TextStyle | r} -> {textStyle :: Maybe TextStyle | r} -> v -> Effect Unit
diffSetTextStyle new old view = when (new.textStyle /= old.textStyle) $ maybeDo (setTextStyle view) new.textStyle

diffWidth :: ∀r v. IsView v => {width:: Maybe Layout | r} -> {width:: Maybe Layout | r} -> v -> Effect Unit
diffWidth new old view = when (new.width /= old.width) $ maybeDo (setWidth view) new.width

diffHeight :: ∀r v. IsView v => {height:: Maybe Layout | r} -> {height:: Maybe Layout | r} -> v -> Effect Unit
diffHeight new old view = when (new.height /= old.height) $ maybeDo (setHeight view) new.height

diffUi :: ∀e. Context -> EventHandler e -> Ui e -> Ui e -> View -> Effect View
diffUi ctx handler (Button new) (Button old) view = do
  traceM "button"
  let btn = unsafeCoerce view :: I.Button
  diffSetText new old btn
  diffSetTextStyle new old btn
  maybeDo (I.onClick btn) (handler <$> new.onClick)
  pure view

diffUi ctx handler (Checkbox new) (Checkbox old) view = do
  traceM "checkbox"
  let checkbox = unsafeCoerce view :: I.Checkbox
  I.onChecked checkbox (\_ -> pure unit)
  when (new.checked /= old.checked) $
    maybeDo (setChecked checkbox) new.checked
  I.onChecked checkbox (fold $ liftA1 handler <$> new.onChecked)
  pure view

diffUi ctx handler (TextView new) (TextView old) view = do
  traceM "textView"
  let textView = unsafeCoerce view :: I.TextView
  diffSetText new old textView
  diffHeight new old textView
  diffWidth new old textView
  pure view

diffUi ctx handler (EditText new) (EditText old) view = do
  traceM "editText"
  let editText = unsafeCoerce view :: I.EditText
  -- make sure we remove the listener, to ensure setting the text does not trigger the listener
  I.removeOnTextChange editText
  diffSetText new old editText
  when (new.editType /= old.editType) $ 
    maybeDo (setEditType editText) new.editType
  when (new.singleLine /= old.singleLine) $
    maybeDo (setSingleLine editText) new.singleLine
  maybeDo (I.onSubmit editText) (handler <$> new.onSubmit)
  I.onTextChange editText $ fold $ liftA1 handler <$> new.onTextChange
  pure view

diffUi ctx handler (LinearLayout {orientation} children) (LinearLayout {orientation: orientation'} children') view = do
  traceM "linearLayout"
  let linearLayout = unsafeCoerce view :: I.LinearLayout
  when (orientation /= orientation') $ do
    maybeDo (setOrientation linearLayout) orientation
  -- diff items, that already existed
  _ <- sequence $ zipWith (\(index /\ a) a' -> do
        oldItem <- getView linearLayout index
        patched <- diffUi ctx handler a a' oldItem
        replaceView linearLayout index patched
        pure unit
    ) (indexed children) children'
  -- add items, that did not exists yet
  if (length children' < length children) then do
    let newElements = drop (length children') children
    for_ newElements $ \child -> do
      newChild <- renderUi ctx handler child
      linearLayout `addView` newChild
    pure view
  -- remove items, if new ui has less children
  else do
    let removeIndices = reverse $ drop (length children) $ indexed children'
    for_ removeIndices $ \(index /\ child) -> do
      linearLayout `removeView` index
    pure view

diffUi ctx handler (RecyclerView children) (RecyclerView old) view = do
  traceM "recyclerView"
  let rv = unsafeCoerce view :: I.RecyclerView (Ui e)
  I.updateRecyclerView rv children
  let bindView view index new old = diffUi ctx handler new old view
  I.updateRecyclerViewBinder rv bindView
  pure view

diffUi ctx handler new _ view = do
  traceM "renderNew"
  renderUi ctx handler new

renderUi :: ∀e. Context -> EventHandler e -> Ui e -> Effect View
renderUi ctx handler (Button {textSize, text, onClick, textStyle}) = do
  btn <- button ctx ""
  maybeDo (setText btn) text
  maybeDo (setTextSize btn) textSize
  maybeDo (I.onClick btn) (handler <$> onClick)
  maybeDo (setTextStyle btn) textStyle
  pure $ toView btn

renderUi ctx handler (LinearLayout {orientation} children) = do
  childViews <- traverse (renderUi ctx handler) children
  view <- linearLayout ctx
  maybeDo (setOrientation view) orientation
  for_ childViews $ \child  -> addView view child
  pure $ toView view

renderUi ctx handler (EditText {text, onTextChange, singleLine, editType}) = do
  view <- editText ctx
  traceM "create new"
  maybeDo (setText view) text
  maybeDo (setSingleLine view) singleLine
  maybeDo (setEditType view) editType
  maybeDo (I.onTextChange view) (liftA1 handler <$> onTextChange) 
  pure $ toView view 

renderUi ctx handler (Checkbox {checked, onChecked}) = do
  view <- I.checkbox ctx
  maybeDo (I.onChecked view) (liftA1 handler <$> onChecked)
  maybeDo (setChecked view) checked

  pure $ toView view
renderUi ctx handler (TextView {text, textSize, height, width, textStyle}) = do
  tv <- I.textView ctx ""
  maybeDo (setText tv) text
  maybeDo (setTextSize tv) textSize
  maybeDo (setWidth tv) width
  maybeDo (setHeight tv) width
  maybeDo (setTextStyle tv) textStyle
  pure $ toView tv

renderUi ctx handler (RecyclerView ls) = do
  let defaultUi = fromMaybe Empty $ head ls
  let createView ctx' = (renderUi ctx' handler defaultUi) /\ defaultUi
  let bindView view index new old = diffUi ctx handler new old view
  rv <- I.recyclerView ctx ls (ls <#> const defaultUi) createView bindView
  pure $ toView rv

renderUi ctx handler (Empty) = emptyView ctx