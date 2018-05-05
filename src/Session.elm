module Session exposing (Session, SessionAuth(..), init, update, setPageSession, getPageSession)

import Task
import Window
import Session.Auth exposing (logoutRequest, checkAuth)
import Message exposing (Msg(..), SessionMessage(..))
import Route


type alias Session pageSession =
    { auth : SessionAuth
    , dimensions : Window.Size
    , pageSession : pageSession
    }


type SessionAuth
    = LoggedOut
    | LoggedIn


init : pageSession -> ( Session pageSession, Cmd SessionMessage )
init pageSession =
    ( { auth = LoggedOut
      , dimensions =
            { width = 0
            , height = 0
            }
      , pageSession = pageSession
      }
    , Cmd.batch [ checkAuth, getSize ]
    )


getSize : Cmd SessionMessage
getSize =
    let
        handleResponse response =
            case response of
                Ok size ->
                    Resize size

                Err a ->
                    NoSize
    in
        Task.attempt handleResponse Window.size


setPageSession : pageSession -> Session pageSession -> Session pageSession
setPageSession pageSession session =
    { session | pageSession = pageSession }


getPageSession : Session pageSession -> pageSession
getPageSession session =
    session.pageSession


update : SessionMessage -> Session pageSession -> ( Session pageSession, Cmd Msg )
update msg model =
    let
        ( newModel, newMsg ) =
            case msg of
                LogoutMsg route ->
                    ( model, Cmd.map Message.Session (logoutRequest route) )

                LoginMsg ->
                    ( model, Cmd.none )

                Resize size ->
                    ( { model | dimensions = size }, Cmd.none )

                NoSize ->
                    ( model, Cmd.map Message.Session getSize )

                Message.LoggedIn route ->
                    ( { model | auth = LoggedIn }, Route.modifyUrl route )

                LoggedInNoRedirect ->
                    ( { model | auth = LoggedIn }, Cmd.none )

                Message.LoggedOut route ->
                    ( { model | auth = LoggedOut }, Route.modifyUrl route )

                LoggedOutNoRedirect ->
                    ( { model | auth = LoggedOut }, Cmd.none )
    in
        ( newModel, newMsg )
