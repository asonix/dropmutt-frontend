module Main exposing (main)

{-| The main module of the application


# main function

@docs main

-}

import Html exposing (Html)
import Html.Styled
import Navigation exposing (Location, program)
import Window
import Message exposing (Msg(..), PageMessage(..), AdminMessage(..), SessionMessage(..))
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
    let
        ( model, msgs ) =
            Model.init

        ( newModel, moreMsgs ) =
            Model.loadPage (Route.fromLocation location) model
    in
        ( newModel, Cmd.batch [ msgs, moreMsgs ] )



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

        Session msg ->
            Session.update msg model.session
                |> Tuple.mapFirst (\session -> { model | session = session })



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ uploadPercentage (Page << AdminMsg << UploadPercentage)
        , uploadFailed (Page << AdminMsg << UploadFailed)
        , uploadSucceeded (Page << AdminMsg << UploadSucceeded)
        , uploadProcessing (Page << AdminMsg << UploadProcessing)
        , Window.resizes (Message.Session << Resize)
        ]
