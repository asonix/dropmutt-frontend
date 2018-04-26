module Main exposing (main)

{-| The main module of the application


# main function

@docs main

-}

import Html exposing (Html)
import Html.Styled
import Navigation exposing (Location, program)
import Message exposing (Msg(..), PageMessage(..))
import Model exposing (Model)
import Route exposing (Route)
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
