module Page.One exposing (OneModel, init, view)

{-| Defines the one page

    @docs OneModel
    @docs init

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Message exposing (Msg)


{-| The state for the One Page
-}
type alias OneModel =
    { tmp : String
    }


{-| Initial state for the one page
-}
init : OneModel
init =
    { tmp = "Page One"
    }


{-| Rendering the One Page
-}
view : OneModel -> Html Msg
view model =
    section []
        [ article []
            [ p [] [ text model.tmp ]
            ]
        ]
