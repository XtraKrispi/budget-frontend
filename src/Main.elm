module Main exposing (..)

import Browser
import Html exposing (Html)
import Element
    exposing
        ( Element
        , Attribute
        , el
        , rgb
        , text
        , row
        , column
        , padding
        , paddingEach
        , spacing
        , spacingXY
        , spaceEvenly
        , mouseOver
        , centerX
        , height
        , width
        , px
        , fill
        )
import Element.Background as Background
import Element.Border as Border
import Element.Input exposing (button)
import Element.Font as Font
import Dict exposing (Dict)


---- MODEL ----


type Selected
    = Selected
    | NotSelected


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout []
        (mainLayout
            [ el [ paddingEach { top = 20, right = 0, bottom = 0, left = 20 } ] (text "Budget")
            , tabs
                (Dict.fromList
                    [ ( "Dashboard", NoOp )
                    , ( "Definitions", NoOp )
                    ]
                )
                "Dashboard"
            , mainContent
            ]
        )


mainLayout : List (Element Msg) -> Element Msg
mainLayout =
    column [ spacing 20, width fill ]


tabStyle : List (Attribute Msg)
tabStyle =
    [ Border.width 1
    , Border.rounded 5
    , Border.color (rgb 0.8 0.8 0.8)
    , padding 10
    , mouseOver
        [ Background.color (rgb 0 0 1)
        , Font.color (rgb 1 1 1)
        ]
    ]


tabStyleSelected : List (Attribute Msg)
tabStyleSelected =
    [ Border.width 1
    , Border.rounded 5
    , Border.color (rgb 0.8 0.8 0.8)
    , padding 10
    , Background.color (rgb 0 0 1)
    , Font.color (rgb 1 1 1)
    ]


tab : Selected -> String -> Msg -> Element Msg
tab s t m =
    let
        style =
            case s of
                Selected ->
                    tabStyleSelected

                NotSelected ->
                    tabStyle
    in
        button style
            { onPress = Just m
            , label = text t
            }


tabs : Dict String Msg -> String -> Element Msg
tabs ts selected =
    let
        tabs_ =
            ts
                |> Dict.toList
                |> List.map
                    (\( s, m ) ->
                        tab
                            (if s == selected then
                                Selected
                             else
                                NotSelected
                            )
                            s
                            m
                    )
    in
        row [ spacing 20, padding 20 ]
            tabs_


mainContent : Element Msg
mainContent =
    el [ Background.color (rgb 1 0 0) ] Element.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
