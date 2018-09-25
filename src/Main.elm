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


type Page
    = Dashboard
    | Definitions


type Selected
    = Selected
    | NotSelected


type alias Model =
    { currentPage : Page }


init : ( Model, Cmd Msg )
init =
    ( { currentPage = Dashboard }, Cmd.none )



---- UPDATE ----


type Msg
    = ChangePage Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            ( { model | currentPage = page }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout []
        (mainLayout
            [ el
                [ paddingEach
                    { top = 20
                    , right = 0
                    , bottom = 0
                    , left = 20
                    }
                ]
                (text "Budget")
            , tabs
                (Dict.fromList
                    [ ( pageTitle Dashboard, ChangePage Dashboard )
                    , ( pageTitle Definitions, ChangePage Definitions )
                    ]
                )
                model.currentPage
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


pages : Dict String Page
pages =
    Dict.fromList
        [ ( "Dashboard", Dashboard )
        , ( "Definitions", Definitions )
        ]


pageTitle : Page -> String
pageTitle page =
    Dict.toList pages
        |> List.filter (\( s, p ) -> p == page)
        |> List.map Tuple.first
        |> List.head
        |> Maybe.withDefault ""


pageFromTitle : Page -> String -> Page
pageFromTitle def title =
    Dict.get title pages
        |> Maybe.withDefault def


tab : Selected -> Page -> Msg -> Element Msg
tab s page m =
    case s of
        Selected ->
            button tabStyleSelected
                { onPress = Nothing
                , label = text (pageTitle page)
                }

        NotSelected ->
            button tabStyle
                { onPress = Just m
                , label = text (pageTitle page)
                }


tabs : Dict String Msg -> Page -> Element Msg
tabs ts selected =
    let
        tabs_ =
            ts
                |> Dict.toList
                |> List.map
                    (\( s, m ) ->
                        tab
                            (if s == (pageTitle selected) then
                                Selected
                             else
                                NotSelected
                            )
                            (pageFromTitle Dashboard s)
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
