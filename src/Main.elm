module Main exposing
    ( Model
    , Msg(..)
    , Page(..)
    , Selected(..)
    , init
    , main
    , mainContent
    , mainLayout
    , pageFromTitle
    , pageTitle
    , tab
    , tabStyle
    , tabStyleSelected
    , tabs
    , update
    , view
    )

import Browser
import Dict exposing (Dict)
import Element
    exposing
        ( Attribute
        , Element
        , alignRight
        , centerX
        , column
        , el
        , fill
        , fillPortion
        , height
        , minimum
        , mouseOver
        , padding
        , paddingEach
        , paddingXY
        , px
        , rgb
        , rgba
        , row
        , spaceEvenly
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)



---- MODEL ----


type alias Definition =
    { definitionId : Int
    }


type Page
    = Dashboard
    | Definitions


type Selected
    = Selected
    | NotSelected


type alias Model =
    { currentPage : Page
    , definitions : List Definition
    }


init : ( Model, Cmd Msg )
init =
    ( { currentPage = Definitions
      , definitions = []
      }
    , Cmd.none
    )



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
    Element.layout [ height fill, Font.family [ Font.typeface "Karla" ] ]
        (mainLayout
            [ el
                [ paddingEach
                    { top = 20
                    , right = 0
                    , bottom = 0
                    , left = 20
                    }
                , Background.color (rgb 1 1 1)
                , width fill
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 10
                    , blur = 10
                    , color = rgb 0.85 0.85 0.85
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
            , mainContent model
            ]
        )


mainLayout : List (Element Msg) -> Element Msg
mainLayout =
    column [ spacing 20, width fill, height fill, Background.gradient { angle = 0, steps = [ rgb 1 1 1, rgb 0.88 0.88 0.88 ] } ]


tabStyle : List (Attribute Msg)
tabStyle =
    [ Background.color (rgb 1 1 1)
    , Border.width 1
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


pageTitle : Page -> String
pageTitle page =
    case page of
        Dashboard ->
            "Dashboard"

        Definitions ->
            "Definitions"


pageFromTitle : Page -> String -> Page
pageFromTitle def title =
    case title of
        "Dashboard" ->
            Dashboard

        "Definitions" ->
            Definitions

        _ ->
            def


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
                            (if s == pageTitle selected then
                                Selected

                             else
                                NotSelected
                            )
                            (pageFromTitle Dashboard s)
                            m
                    )
    in
    row [ spacing 20, paddingXY 200 20 ]
        tabs_


shadowEl : List (Attribute Msg) -> Element Msg -> Element Msg
shadowEl attrs =
    el
        ([ Border.solid
         , Border.width 1
         , Border.color (rgb 0.85 0.85 0.85)
         , Border.shadow { offset = ( 0, 4 ), size = 0, blur = 8, color = rgba 0 0 0 0.2 }
         ]
            ++ attrs
        )


mainContent : Model -> Element Msg
mainContent model =
    let
        pageView =
            case model.currentPage of
                Dashboard ->
                    dashboardView

                Definitions ->
                    definitionsView
    in
    column [ spacing 20, width fill, height fill, paddingEach { top = 10, right = 200, left = 200, bottom = 40 } ]
        [ shadowEl [ Background.color (rgb 1 1 1), width fill, padding 10 ] (el [ Font.bold, Font.size 25 ] (text (pageTitle model.currentPage)))
        , shadowEl
            [ Background.color (rgb 1 1 1)
            , width fill
            , height fill
            , paddingEach
                { top = 10
                , right = 40
                , bottom = 10
                , left = 40
                }
            ]
            (pageView model)
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


dashboardView : Model -> Element Msg
dashboardView model =
    el [] (text "Dashboard")


definitionsView : Model -> Element Msg
definitionsView model =
    el [ centerX ] <|
        column [ spacing 20 ]
            [ card (text "title") (text "description") (text "alternateInfo") Element.none
            , card (text "title") (text "description") (text "alternateInfo") Element.none
            ]


card :
    Element Msg
    -> Element Msg
    -> Element Msg
    -> Element Msg
    -> Element Msg
card title description alternateInfo tools =
    column
        [ Border.color (rgb 1 0 0)
        , Border.width 1
        , padding 10
        , width (fill |> minimum 500)
        , spacing 10
        ]
        [ row [ width fill, spacing 20 ]
            [ el [] title
            , el [ alignRight ] alternateInfo
            ]
        , row [ width fill, spacing 20 ]
            [ el [] description
            , el [ alignRight ] tools
            ]
        ]
