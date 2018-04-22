module Page.NotFound exposing (NotFoundModel, init, view)

{-| Defines the notFound page

    @docs NotFoundModel
    @docs init

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Message exposing (Msg)


{-| The state for the NotFound Page
-}
type alias NotFoundModel =
    { tmp : String
    }


{-| Initial state for the notFound page
-}
init : NotFoundModel
init =
    { tmp = "Not found"
    }


{-| Rendering the NotFound Page
-}
view : NotFoundModel -> Html Msg
view model =
    section []
        [ article []
            [ p [] [ text model.tmp ]
            ]
        ]
