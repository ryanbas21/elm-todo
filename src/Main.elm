module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, bool, decodeString, field, float, int, list, map, map3, nullable, string)



---- MODEL ----


type alias Id =
    Int


type alias Todo =
    { id : Id
    , todo : String
    , completed : String
    }


type alias Model =
    { todos : List Todo
    , value : String
    }


init : ( Model, Cmd Msg )
init =
    ( { value = ""
      , todos = []
      }
    , Http.get { url = "https://jsonplaceholder.typicode.com/todos", expect = Http.expectJson GotTodos todosDecoder }
    )



---- UPDATE ----


type Msg
    = Change String
    | Add String
    | Complete Id
    | Delete Id
    | GetTodos (Cmd Msg)
    | GotTodos (Result Http.Error (List Todo))


getId : List Todo -> Id
getId xs =
    case List.length xs == 0 of
        True ->
            1

        False ->
            List.length xs + 1


mapCompleted : Bool -> String
mapCompleted completed =
    case completed of
        True ->
            "complete"

        False ->
            ""


todoDecoder : Decoder Todo
todoDecoder =
    map3 Todo
        (field "id" int)
        (field "title" string)
        (field "completed" (map mapCompleted bool))


todosDecoder : Decoder (List Todo)
todosDecoder =
    list todoDecoder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTodos val ->
            ( model, Cmd.none )

        GotTodos val ->
            case val of
                Ok todos ->
                    ( { model | todos = todos }, Cmd.none )

                Err err ->
                    ( { model | todos = [] }, Cmd.none )

        Add val ->
            ( { model | todos = List.append [ { id = getId model.todos, todo = model.value, completed = "" } ] model.todos, value = "" }, Cmd.none )

        Change val ->
            ( { model | value = val }, Cmd.none )

        Delete val ->
            ( { model | todos = List.filter (\todo -> todo.id /= val) model.todos }, Cmd.none )

        Complete id ->
            ( { model
                | todos =
                    List.map
                        (\todo ->
                            if todo.id == id then
                                { todo
                                    | completed =
                                        if todo.completed == "completed" then
                                            ""

                                        else
                                            "completed"
                                }

                            else
                                todo
                        )
                        model.todos
              }
            , Cmd.none
            )



---- VIEW ----


todoList : Model -> Html Msg
todoList model =
    ul []
        (List.map
            (\item ->
                li [ class item.completed, onClick (Complete item.id) ]
                    [ text item.todo
                    , button [ onClick (Delete item.id) ] [ text "Delete" ]
                    ]
            )
            model.todos
        )


view : Model -> Html Msg
view model =
    div
        []
        [ todoList model
        , label [ for "todo-input" ] [ text "Add Todo:" ]
        , input [ value model.value, id "todo-input", type_ "text", onInput Change ] []
        , button [ onClick (Add model.value), type_ "Submit" ] [ text "Add" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
