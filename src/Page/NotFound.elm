module Page.NotFound exposing (NotFound, init, view)

{-| Defines the notFound page

    @docs NotFound
    @docs init

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Message exposing (NotFoundMessage)


{-| The state for the NotFound Page
-}
type alias NotFound =
    { tmp : String
    }


{-| Initial state for the notFound page
-}
init : Maybe NotFound -> NotFound
init model =
    case model of
        Just model ->
            model

        Nothing ->
            { tmp = "Not found"
            }


{-| Rendering the NotFound Page
-}
view : NotFound -> Html NotFoundMessage
view model =
    section []
        [ article []
            [ p [] [ text model.tmp ]
            ]
        ]
