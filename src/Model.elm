module Model exposing (Model, init, loadPage)

{-| Defines the root model

@docs Model
@docs init
@docs loadPage

-}

import Message exposing (Msg)
import Route exposing (Route)
import Session exposing (Session(..))
import View exposing (PageModel)


{-| The model shared between all pages
-}
type alias Model =
    { page : PageModel
    , session : Session
    }


{-| Initialize the application model
-}
init : Model
init =
    { page = View.init
    , session = LoggedOut
    }


{-| Load a new page
-}
loadPage : Route -> Model -> ( Model, Cmd Msg )
loadPage route model =
    model.page
        |> View.loadPage route
        |> Tuple.mapFirst (\page -> { model | page = page })
