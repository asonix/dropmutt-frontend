module Main exposing (main)

{-| The main module of the application


# main function

@docs main

-}

import Html exposing (Html)
import Html.Styled
import Navigation exposing (Location, program)
import Auth exposing (logoutRequest, checkAuth)
import Message exposing (Msg(..), PageMessage(..), AdminMessage(..))
import Model exposing (Model)
import Ports exposing (..)
import Route exposing (Route)
import Session exposing (Session)
import View


-- MAIN


{-| The main function of the application
-}
main : Program Never Model Msg
main =
    program (Render << Route.fromLocation)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : Location -> ( Model, Cmd Msg )
init location =
    Model.loadPage (Route.fromLocation location) Model.init
        |> Tuple.mapSecond (\msg -> Cmd.batch [ msg, checkAuth ])



-- VIEW


view : Model -> Html Msg
view model =
    model.page
        |> View.layout model.session
        |> Html.Styled.toUnstyled



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Render route ->
            Model.loadPage route model

        Load route ->
            ( model, Route.modifyUrl route )

        Page msg ->
            View.update msg model.session model.page
                |> (\( ( page, cmd ), session ) -> ( { model | page = page, session = session }, cmd ))

        LogoutMsg ->
            ( { model | session = Session.LoggedOut }, logoutRequest )

        LoginMsg ->
            ( { model | session = Session.LoggedIn }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ uploadPercentage (Page << AdminMsg << UploadPercentage)
        , uploadFailed (Page << AdminMsg << UploadFailed)
        , uploadSucceeded (Page << AdminMsg << UploadSucceeded)
        ]
