module Jobsearch where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json exposing ((:=))
import Json.Encode as Encode exposing (..)
import Http exposing (..) 
import Task exposing (..)
import Signal exposing (Signal, Address)
import StartApp

app =
  StartApp.start {init = init, update = update, view = view, inputs = []}

  

type alias Model = {
    words: List String,
    query: String
}

type alias WordList =
  List String

initialModel : Model
initialModel = { words = [], query = " " }

init : (Model, Effects Action)
init =
  (initialModel, fetchWords)

type Action = NoOp
  | AddWord 
  | UpdateField String
  | FetchWordsResponse (Maybe (List String))
  | FetchWords

{-
model : Signal Model
model =
  Signal.foldp update initialModel actions.signal
  -}

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none)
    AddWord -> 
      ({ model | query = "" }, putWord model.query)
--      ({ model | words = model.words ++ [model.query] }, putWord model.word)
    UpdateField word ->
      ({ model | query = word }, Effects.none)
    FetchWordsResponse maybeWords ->
      ({ model | words = (Maybe.withDefault model.words maybeWords) }, Effects.none)
    FetchWords ->
      (model, fetchWords)

--main : Signal Html
--main =
--  Signal.map (view actions.address) model 
main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

actions : Signal.Mailbox Action
actions = 
  Signal.mailbox NoOp

view : Address Action -> Model -> Html
view address model =
  div [] [
    section [] [ 
      lazy2 wordEntry address model.query, 
      lazy2 wordList address model.words 
    ]
  ] 

wordEntry : Address Action -> String -> Html
wordEntry address word =
  div [] [
    input [ value word , on "input" targetValue (Signal.message address << UpdateField)] [],
    input [ type' "button", onClick address AddWord ] []
  ]

wordList : Address Action -> List String -> Html
wordList address words =
  div [] [
    ul [] (List.map wordItem words)
  ]


wordItem : String -> Html
wordItem word =
  li [] [text word]

{-
stringifyWord : String -> Body
stringifyWord word =
  let j = 
    Encode.encode 0 word
  in
    Http.string j 
 -}

putWord : String -> Effects Action
putWord word =
  let body = 
    Http.string (Encode.encode 0 (Encode.string word))

  in
    Http.post decodeWords "http://192.168.1.104:3003/api" body
      |> Task.toMaybe
      |> Task.map FetchWordsResponse
      |> Effects.task

fetchWords : Effects Action
fetchWords =
  Http.get decodeWords "http://192.168.1.104:3003/api"
    |> Task.toMaybe
    |> Task.map FetchWordsResponse
    |> Effects.task

decodeWords : Json.Decoder (List String)
decodeWords =
  Json.list Json.string
