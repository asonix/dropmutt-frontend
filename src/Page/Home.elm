module Page.Home exposing (Home, init, view)

{-| Defines the home page

    @docs Home
    @docs init

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Message exposing (HomeMessage)


{-| The state for the Home Page
-}
type alias Home =
    { tmp : String
    }


{-| Initial state for the home page
-}
init : Maybe Home -> Home
init model =
    case model of
        Just model ->
            model

        Nothing ->
            { tmp = "Page Home"
            }


{-| Rendering the Home Page
-}
view : Home -> Html HomeMessage
view model =
    section []
        [ article []
            [ p [] [ text model.tmp ]
            , p [] [ text model.tmp ]
            , p [] [ text model.tmp ]
            , p [] [ text model.tmp ]
            , p [] [ text model.tmp ]
            ]
        ]
