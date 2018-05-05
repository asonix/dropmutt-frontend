module Page.Home exposing (HomeModel, init, view)

{-| Defines the home page

    @docs HomeModel
    @docs init

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Message exposing (HomeMessage)


{-| The state for the Home Page
-}
type alias HomeModel =
    { tmp : String
    }


{-| Initial state for the home page
-}
init : Maybe HomeModel -> HomeModel
init model =
    case model of
        Just model ->
            model

        Nothing ->
            { tmp = "Page Home"
            }


{-| Rendering the Home Page
-}
view : HomeModel -> Html HomeMessage
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
