module Page.Auth exposing (Auth, init, view, update)

{-| Defines the auth page

    @docs Auth
    @docs init

-}

import Css exposing (..)
import Debug
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, for, name, type_, value)
import Html.Styled.Events exposing (onSubmit, onInput, onClick)
import Http
import Json.Decode
import Json.Encode
import Session.Auth exposing (..)
import Message exposing (AuthPageMessage(..), SessionMessage(..))
import Route exposing (Route(..))


{-| The state for the Auth Page
-}
type alias Auth =
    { authType : AuthType
    , username : String
    , password : String
    }


type AuthType
    = SignUp
    | LogIn


{-| Initial state for the auth page
-}
init : Maybe Auth -> Auth
init model =
    case model of
        Just model ->
            model

        Nothing ->
            { authType = LogIn
            , username = ""
            , password = ""
            }


{-| Rendering the Auth Page
-}
view : Auth -> Html AuthPageMessage
view model =
    section []
        [ article []
            [ h2 []
                [ case model.authType of
                    LogIn ->
                        text "Login"

                    SignUp ->
                        text "Sign Up"
                ]
            , form
                [ formOnSubmit model.authType ]
                [ div []
                    [ label
                        [ for "username"
                        , css [ display block ]
                        ]
                        [ text "Username" ]
                    , input
                        [ type_ "text"
                        , name "username"
                        , onInput (Username)
                        ]
                        []
                    ]
                , div []
                    [ label
                        [ for "password"
                        , css [ display block ]
                        ]
                        [ text "Password" ]
                    , input
                        [ type_ "password"
                        , name "password"
                        , onInput (Password)
                        ]
                        []
                    ]
                , div []
                    [ input
                        [ type_ "submit"
                        , formOnSubmit model.authType
                        , value "Submit"
                        ]
                        []
                    ]
                ]
            , div []
                [ label
                    [ for "switch"
                    , css
                        [ display block
                        , marginTop (Css.em 2)
                        ]
                    ]
                    [ case model.authType of
                        LogIn ->
                            text "Switch to Sign Up"

                        SignUp ->
                            text "Switch to Login"
                    ]
                , input
                    [ type_ "button"
                    , name "switch"
                    , value "Switch!"
                    , onClick (SwitchAuth)
                    ]
                    []
                ]
            ]
        ]


formOnSubmit : AuthType -> Attribute AuthPageMessage
formOnSubmit authType =
    let
        msg =
            case authType of
                SignUp ->
                    Signup

                LogIn ->
                    Login
    in
        onSubmit msg


update : AuthPageMessage -> Auth -> ( Auth, Cmd SessionMessage )
update msg model =
    case msg of
        Login ->
            ( model, authRequest Route.Home loginUrl (newAuthParams model.username model.password) )

        Signup ->
            ( model, authRequest Route.Home signupUrl (newAuthParams model.username model.password) )

        Username username ->
            ( { model | username = username }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        SwitchAuth ->
            ( switchAuth model, Cmd.none )


switchAuth : Auth -> Auth
switchAuth model =
    case model.authType of
        LogIn ->
            { model | authType = SignUp }

        SignUp ->
            { model | authType = LogIn }
