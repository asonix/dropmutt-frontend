module Page.Auth exposing (AuthModel, init, view, update)

{-| Defines the auth page

    @docs AuthModel
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
import Auth exposing (..)
import Message exposing (Msg(..), PageMessage(..), AuthMessage(..))
import Route exposing (Route(..))
import Session exposing (Session, SessionAuth(..))


{-| The state for the Auth Page
-}
type alias AuthModel =
    { authType : AuthType
    , username : String
    , password : String
    }


type AuthType
    = SignUp
    | LogIn


{-| Initial state for the auth page
-}
init : AuthModel
init =
    { authType = LogIn
    , username = ""
    , password = ""
    }


{-| Rendering the Auth Page
-}
view : AuthModel -> Html Msg
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
                        , onInput (Page << AuthMsg << Username)
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
                        , onInput (Page << AuthMsg << Password)
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
                    , onClick (Page <| AuthMsg <| SwitchAuth)
                    ]
                    []
                ]
            ]
        ]


formOnSubmit : AuthType -> Attribute Msg
formOnSubmit authType =
    let
        msg =
            case authType of
                SignUp ->
                    Page <| AuthMsg <| Signup

                LogIn ->
                    Page <| AuthMsg <| Login
    in
        onSubmit msg


update : AuthMessage -> Session -> AuthModel -> ( ( AuthModel, Cmd Msg ), Session )
update msg session model =
    case msg of
        Login ->
            ( ( model, authRequest loginUrl (newAuthParams model.username model.password) ), session )

        Signup ->
            ( ( model, authRequest signupUrl (newAuthParams model.username model.password) ), session )

        Username username ->
            ( ( { model | username = username }, Cmd.none ), session )

        Password password ->
            ( ( { model | password = password }, Cmd.none ), session )

        Authenticated ->
            ( ( model, Route.modifyUrl Home ), { session | auth = LoggedIn } )

        NotAuthenticated ->
            ( ( model, Cmd.none ), { session | auth = LoggedOut } )

        SwitchAuth ->
            ( ( switchAuth model, Cmd.none ), session )


switchAuth : AuthModel -> AuthModel
switchAuth model =
    case model.authType of
        LogIn ->
            { model | authType = SignUp }

        SignUp ->
            { model | authType = LogIn }
