module Main where

import Android.Attributes
import Android.Ui
import Prelude

import Android.Internal.View (Context)
import Android.Render (runUi)
import Android.Sqlite (Db, columnAsInt, columnAsString, createDb, execSql, mapCursor, querySql)
import Data.Array (drop, filter, length, range, zip)
import Data.Array as Array
import Data.Array as F
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

type Model =
  { todos ∷ Array Todo
  , pending ∷ String
  , fresh ∷ Int
  , visibility ∷ Visibility
  }

data Visibility
  = All
  | Active
  | Completed

derive instance eqVisibility ∷ Eq Visibility

instance showVisibility ∷ Show Visibility where
  show = case _ of
    All → "All"
    Active → "Active"
    Completed → "Completed"

type Todo =
  { text ∷ String
  , completed ∷ Boolean
  , editing ∷ Boolean
  , id ∷ Int
  }

initialModel ∷ Model
initialModel =
  { todos: 
    [ {text: "Buy Milk", completed: false, editing: false, id: 1}
    , {text: "else stuff", completed: true, editing: false, id: 2}
    ]
  , pending: ""
  , fresh: 3
  , visibility: All
  }

data Action
  = None
  | UpdatePending String
  | AddTodo
  | UpdateTodo Int String
  | ToggleTodo Int Boolean
  | EditingTodo Int Boolean
  | DeleteTodo Int
  | DeleteCompleted
  | ToggleAll Boolean
  | ChangeVisibility Visibility

newTodo ∷ String → Int → Todo
newTodo = { text: _, id: _, completed: false, editing: false }

modifyWhere ∷ forall f a. Functor f ⇒ (a → Boolean) → (a → a) → f a → f a
modifyWhere pred mod = map (\a → if pred a then mod a else a)


update :: Db -> Model -> Action -> Effect Model
update db model = case _ of
  None -> pure model
  UpdatePending pending -> pure $ model {pending = pending}
  AddTodo -> if model.pending == "" 
      then pure model
      else do 
        let new = newTodo model.pending model.fresh
        _ <- execSql db "INSERT INTO todos (id, text, completed) VALUES (?, ?, ?)" [show new.id, new.text, show 1]
        pure $ model {pending = "", fresh = model.fresh + 1, todos = Array.snoc model.todos new}
  UpdateTodo todo text -> pure $ model { todos = model.todos 
    # modifyWhere (eq todo <<< _.id) 
    _ { text = text } }
  ToggleTodo todo checked ->
    pure $ model { todos = model.todos 
    # modifyWhere (eq todo <<< _.id)
    _ { completed = checked } }
  EditingTodo todo editing -> pure $ model { todos = model.todos 
    # modifyWhere (eq todo <<< _.id)
    _ { editing = editing }
    }
  DeleteTodo todo -> pure $ model { todos = model.todos #  Array.filter (not eq todo <<< _.id) }
  DeleteCompleted -> pure $ model { todos = model.todos # Array.filter (not _.completed)}
  ToggleAll checked -> pure $ model { todos = model.todos <#> _ { completed = checked } }
  ChangeVisibility visibility -> pure $ model { visibility  = visibility }

-- update s Clicked = s { todos = s.todos <> [{title: s.textFieldText, done: false}], textFieldText = "" }
-- update s (Change txt) = s {textFieldText = txt }
-- update s (CheckAll) = s { todos = s.todos <#> (_ {done = true})}
-- update s ToggleEdit = s { inEditMode = not s.inEditMode}
-- update s (Delete index) = s {todos = drop 1 s.todos }

indexed :: ∀a. Array a -> Array (Int /\ a)
indexed as = zip (range 0 $ length as) as

view :: Model -> Ui Action
view {todos, pending, visibility} = 
  let
    allCompleted =
      F.all _.completed todos
  in
  linearLayout [orientation Vertical] $
    [ linearLayout [orientation Horizontal]
      [ button [ text "▼", onClick (ToggleAll (not allCompleted)) ]
      , editText 
        [ text pending
        , onTextChange UpdatePending
        , editType ActionDone
        , singleLine true
        , onSubmit AddTodo
        ]
      ]
    , (viewTodos visibility todos) 
    , viewControls visibility todos
    ]

viewTodos :: Visibility -> Array Todo -> Ui Action
viewTodos visibility todos =
  let
    filteredTodos = spy "filteredTodos" $ case visibility of
      All →
        todos
      Active →
        Array.filter (not _.completed) todos
      Completed →
        Array.filter _.completed todos

    allCompleted =
      F.all _.completed todos
  in
    -- linearLayout 
    --   [orientation Vertical]
    --   (filteredTodos <#> viewTodo)
    recyclerView $ filteredTodos <#> viewTodo

viewTodo :: Todo -> Ui Action
viewTodo todo =
  linearLayout [orientation Horizontal]
    [ checkbox [checked todo.completed, onChecked (ToggleTodo todo.id)]
    , if todo.editing 
      then editText 
        [ text todo.text
        , onTextChange (UpdateTodo todo.id)
        , onSubmit (EditingTodo todo.id false)
        , editType ActionDone
        , singleLine true
        ] 
      else textView 
        [ text todo.text
        ]
    , button [ text "⨯", onClick (DeleteTodo todo.id) ]
    , button [ text "✎", onClick (EditingTodo todo.id true)]
    ]

viewControls :: Visibility -> Array Todo -> Ui Action
viewControls visibility todos =
  let
    lenCompleted =
      todos
        # Array.filter _.completed
        # Array.length

    lenLeft =
      Array.length todos - lenCompleted
  in
    linearLayout [orientation Horizontal]
    [ textView [text $ "Items left: " <> show lenLeft]
    , viewVisibility All visibility
    , viewVisibility Active visibility
    , viewVisibility Completed visibility
    , viewClear lenCompleted 
    ]

viewClear :: Int -> Ui Action
viewClear len =
  button 
    [ text $ "Clear completed (" <> show len <> ")"
    , onClick DeleteCompleted
    ]

viewVisibility :: Visibility -> Visibility -> Ui Action
viewVisibility v vCurrent =
  button [text (show v), onClick (ChangeVisibility v), textStyle (if v == vCurrent then Bold else Normal)]

main :: Context -> Effect Unit
main ctx = do
  db <- createDb ctx 1 "todos" "CREATE TABLE todos (id INTEGER, text TEXT, completed INTEGER)"
  -- _ <- execSql db "INSERT INTO test (id, text) VALUES (1, \"asd\")"
  -- _ <- execSql db "INSERT INTO test (id, text) VALUES (1, \"from db!!!\")"
  cursor <- querySql db "SELECT * FROM todos" []
  res <- mapCursor cursor $ \c -> do
    id <- columnAsInt c "id"
    text <- columnAsString c "text"
    completed <- columnAsInt c "completed"
    pure {completed: if completed == 0 then false else true, id, text, editing: false}
  -- logShow res
  runUi ctx {initial: initialModel {todos = res}, update: update db, view}