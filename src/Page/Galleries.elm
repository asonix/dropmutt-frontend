module Page.Galleries exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Message exposing (Msg, GalleriesMessage(..))
import Page.Galleries.Gallery exposing (Gallery)
import Route


type alias Galleries =
    { options : List String
    , currentGallery : Maybe Gallery
    }


loadPage : Maybe Route.Gallery -> GalleriesSession -> Maybe Galleries -> ( Galleries, Cmd Msg )
loadPage route session galleries =
    case route of
        Just gallery ->
            case galleries of
                Just galleries ->
                    Page.Galleries.Gallery.init galleries.currentGallery

                Nothing ->
                    init Nothing

        Nothing ->
            init galleries


init : Maybe Galleries -> ( Galleries, Cmd GalleriesMessage )
init list =
    case list of
        Just list ->
            ( list, Cmd.none )

        Nothing ->
            ( { options = []
              , currentGallery = Nothing
              }
            , Cmd.none
            )


view : Galleries -> Html GalleriesMessage
view model =
    case model.currentGallery of
        Just gallery ->
            gallery
                |> Page.Galleries.Gallery.view
                |> Tuple.mapSecond (Html.map GallerysMsg)

        Nothing ->
            div [] (listGalleries model.options)


listGalleries : List String -> Html GalleriesMessage
listGalleries galleryNames =
    ul
        [ css [ listStyle none ] ]
        (List.map renderGalleryName galleryNames)


renderGalleryName : String -> Html GalleriesMessage
renderGalleryName name =
    a
        [ Route.href <|
            Route.Gallery <|
                Just { name = name, image = Nothing }
        , css [ display block ]
        ]
        [ div [] [ text name ] ]


update : GalleriesMessage -> Galleries -> ( Galleries, Cmd GalleriesMessage )
update msg model =
    case msg of
        GalleryMsg galleryMsg ->
            case model.currentGallery of
                Just gallery ->
                    gallery
                        |> Page.Galleries.Gallery.update galleryMsg
                        |> Tuple.mapSecond (Cmd.map GalleryMsg)

                Nothing ->
                    ( model, Cmd.none )

        Select string ->
            ( model, Cmd.none )
