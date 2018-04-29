module Session exposing (Session, SessionAuth(..), init, update)

import Task
import Window
import Auth exposing (logoutRequest, checkAuth)
import Message exposing (Msg(..), SessionMessage(..))


type alias Session =
    { auth : SessionAuth
    , dimentions : Window.Size
    }


type SessionAuth
    = LoggedOut
    | LoggedIn


init : ( Session, Cmd Msg )
init =
    ( { auth = LoggedOut
      , dimentions =
            { width = 0
            , height = 0
            }
      }
    , Cmd.batch [ checkAuth, getSize ]
    )


getSize : Cmd Msg
getSize =
    let
        handleResponse response =
            case response of
                Ok size ->
                    Message.Session <| Resize size

                Err a ->
                    Message.Session NoSize
    in
        Task.attempt handleResponse Window.size


update : SessionMessage -> Session -> ( Session, Cmd Msg )
update msg model =
    case msg of
        LogoutMsg ->
            ( { model | auth = LoggedOut }, logoutRequest )

        LoginMsg ->
            ( { model | auth = LoggedIn }, Cmd.none )

        Resize size ->
            ( { model | dimentions = size }, Cmd.none )

        NoSize ->
            ( model, getSize )
