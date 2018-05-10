module Page.Galleries exposing (..)

import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http
import Json.Decode
import Window exposing (Size)
import Message exposing (Msg, GalleriesMessage(..))
import Page.Galleries.Gallery exposing (Gallery)
import Route
import Session.Auth exposing (apiEndpoint)


type alias Galleries =
    { options : List String
    , currentGallery : Maybe String
    , gallerySession : GallerySession
    }


type alias GallerySession =
    Dict String Gallery


loadPage : Maybe Route.Gallery -> Maybe Galleries -> ( Galleries, Cmd GalleriesMessage )
loadPage route model =
    case route of
        Just gallery ->
            let
                updateGalleries galleries =
                    let
                        newModel =
                            { galleries | currentGallery = Just gallery.name }
                    in
                        newModel.gallerySession
                            |> Dict.get gallery.name
                            |> Page.Galleries.Gallery.init gallery.name
                            |> Tuple.mapSecond (Cmd.map Message.GalleryMsg)
                            |> Tuple.mapFirst
                                ((Galleries newModel.options newModel.currentGallery)
                                    << flip (Dict.insert gallery.name) newModel.gallerySession
                                )
            in
                case model of
                    Just galleries ->
                        updateGalleries galleries

                    Nothing ->
                        let
                            ( galleries, cmd ) =
                                init Nothing

                            ( finalModel, otherCmd ) =
                                updateGalleries galleries
                        in
                            ( finalModel, Cmd.batch [ cmd, otherCmd ] )

        Nothing ->
            case model of
                Just model ->
                    init <| Just { model | currentGallery = Nothing }

                Nothing ->
                    init Nothing


init : Maybe Galleries -> ( Galleries, Cmd GalleriesMessage )
init model =
    case model of
        Just galleries ->
            ( galleries, Cmd.none )

        Nothing ->
            ( { options = []
              , currentGallery = Nothing
              , gallerySession = Dict.empty
              }
            , galleriesRequest
            )


galleriesRequest : Cmd GalleriesMessage
galleriesRequest =
    let
        handleResponse res =
            case res of
                Ok galleries ->
                    Message.Galleries galleries

                Err e ->
                    NoGalleries
    in
        Http.get galleriesEndpoint (Json.Decode.list Json.Decode.string)
            |> Http.send handleResponse


galleriesEndpoint : String
galleriesEndpoint =
    apiEndpoint ++ "/galleries"


view : Size -> Galleries -> Html GalleriesMessage
view size model =
    case model.currentGallery of
        Just name ->
            case Dict.get name model.gallerySession of
                Just gallery ->
                    gallery
                        |> Page.Galleries.Gallery.view size
                        |> Html.Styled.map Message.GalleryMsg

                Nothing ->
                    model.options
                        |> listGalleries

        Nothing ->
            model.options
                |> listGalleries


listGalleries : List String -> Html GalleriesMessage
listGalleries galleryNames =
    ul
        [ css [ listStyle none ] ]
        (List.map renderGalleryName galleryNames)


renderGalleryName : String -> Html GalleriesMessage
renderGalleryName name =
    a
        [ Route.href <|
            Route.Galleries <|
                Just { name = name, image = Nothing }
        , css [ display block ]
        ]
        [ div [] [ text name ] ]


update : GalleriesMessage -> Galleries -> ( Galleries, Cmd GalleriesMessage )
update msg model =
    case msg of
        GalleryMsg galleryMsg ->
            case model.currentGallery of
                Just name ->
                    case Dict.get name model.gallerySession of
                        Just gallery ->
                            gallery
                                |> Page.Galleries.Gallery.update galleryMsg
                                |> Tuple.mapSecond (Cmd.map GalleryMsg)
                                |> Tuple.mapFirst
                                    ((Galleries model.options model.currentGallery)
                                        << flip (Dict.insert name) model.gallerySession
                                    )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Select string ->
            ( model, Cmd.none )

        Message.Galleries options ->
            ( { model
                | options = options
              }
            , Cmd.none
            )

        Message.NoGalleries ->
            ( model, Cmd.none )
