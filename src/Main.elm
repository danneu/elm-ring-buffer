port module Main exposing (..)

import Html as Html exposing (Html, div, program, text)
import Html.Attributes as Ha
import Html.Events as He exposing (onClick, onInput, onSubmit)
import RingBuffer exposing (RingBuffer)


-- PORTS


port focus : () -> Cmd msg



-- MODEL


type alias Message =
    { id : Int
    , text : String
    }


type alias Model =
    { messages : RingBuffer Message
    , nextId : Int
    , text : String
    }


init : ( Model, Cmd Msg )
init =
    ( { nextId = 1
      , messages = RingBuffer.empty 10
      , text = ""
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = NoOp
    | SubmitMessage Direction
    | RemoveRight
    | RemoveLeft
    | RotateLeft
    | RotateRight
    | UpdateText String



-- UPDATE


type Direction
    = PushRight
    | PushLeft


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RemoveLeft ->
            let
                ( value, messages_ ) =
                    RingBuffer.popLeft model.messages
            in
            ( { model | messages = messages_ }
            , Cmd.none
            )

        RotateLeft ->
            ( { model | messages = RingBuffer.rotateLeft 1 model.messages }
            , Cmd.none
            )

        RotateRight ->
            ( { model | messages = RingBuffer.rotateRight 1 model.messages }
            , Cmd.none
            )

        RemoveRight ->
            let
                ( value, messages_ ) =
                    RingBuffer.pop model.messages
            in
            ( { model | messages = messages_ }
            , Cmd.none
            )

        SubmitMessage direction ->
            let
                id =
                    model.nextId

                message =
                    { id = id
                    , text = model.text
                    }

                model_ =
                    { model
                        | messages =
                            case direction of
                                PushRight ->
                                    RingBuffer.push message model.messages

                                PushLeft ->
                                    RingBuffer.pushLeft message model.messages
                        , nextId = id + 1
                        , text = ""
                    }
            in
            ( model_, focus () )

        UpdateText text ->
            ( { model | text = text }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


viewBuf : String -> RingBuffer a -> Html Msg
viewBuf msg buf =
    Html.div
        []
        [ Html.text <| msg ++ ": "
        , Html.text <| toString buf
        , Html.text <| " -- Length=" ++ (toString <| RingBuffer.length buf)
        , Html.text <| " -- toList=" ++ (toString <| RingBuffer.toList buf)
        , Html.hr [] []
        ]


viewMessage : Message -> Html Msg
viewMessage msg =
    Html.li
        []
        [ Html.text <| "<" ++ toString msg.id ++ "> "
        , Html.text msg.text
        ]


view : Model -> Html Msg
view model =
    Html.div
        [ Ha.style [ ( "border", "1px solid black" ) ] ]
        [ Html.text (toString model.messages)
        , Html.ol
            []
            (List.map viewMessage (RingBuffer.toList model.messages))
        , Html.form
            [ He.onSubmit (SubmitMessage PushRight)
            ]
            [ Html.input
                [ He.onInput UpdateText
                , Ha.value model.text
                , Ha.type_ "text"
                , Ha.autofocus True
                , Ha.required True
                ]
                []
            , Html.button
                [ onClick (SubmitMessage PushLeft)
                , Ha.type_ "button"
                ]
                [ Html.text "PushLeft " ]
            , Html.button
                [ onClick (SubmitMessage PushRight)
                , Ha.type_ "button"
                ]
                [ Html.text "PushRight " ]
            ]
        , Html.button
            [ onClick RemoveLeft ]
            [ Html.text "RemoveLeft " ]
        , Html.button
            [ onClick RemoveRight ]
            [ Html.text "RemoveRight " ]
        , Html.hr [] []
        , Html.button
            [ onClick RotateLeft ]
            [ Html.text "RotateLeft " ]
        , Html.button
            [ onClick RotateRight ]
            [ Html.text "RotateRight " ]
        , Html.p [] [ Html.text <| (++) "First: " <| toString <| RingBuffer.first model.messages ]
        , Html.p [] [ Html.text <| (++) "Last: " <| toString <| RingBuffer.last model.messages ]
        ]
