module Model exposing (Model, init, loadPage)

{-| Defines the root model

@docs Model
@docs init
@docs loadPage

-}

import Message exposing (Msg)
import Route exposing (Route)
import Session exposing (Session, SessionAuth(..))
import Page exposing (Page, PageSession)


{-| The model shared between all pages
-}
type alias Model =
    { page : Page
    , session : Session PageSession
    }


{-| Initialize the application model
-}
init : ( Model, Cmd Msg )
init =
    Page.initPageSession
        |> Session.init
        |> Tuple.mapFirst
            (\session ->
                { page = Page.init
                , session = session
                }
            )
        |> Tuple.mapSecond (Cmd.map Message.Session)


{-| Load a new page
-}
loadPage : Route -> Model -> ( Model, Cmd Msg )
loadPage route model =
    model.page
        |> Page.loadPage route (Session.getPageSession model.session)
        |> Tuple.mapFirst (\page -> { model | page = page })
