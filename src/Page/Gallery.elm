module Page.Gallery exposing (GalleryModel, init, view, update)

{-| Defines the gallery page

@docs GalleryModel
@docs init

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, title)
import Html.Styled.Events exposing (onWithOptions)
import Http
import Json.Decode exposing (Decoder)
import Window exposing (Size)
import Session.Auth exposing (apiEndpoint)
import Colors exposing (..)
import Message exposing (GalleryMessage(..))
import Page.Gallery.RemoteImage exposing (..)


{-| The state for the Gallery Page
-}
type alias GalleryModel =
    { currentImage : Maybe RemoteImage
    , remotes : List RemoteImage
    }


{-| Initial state for the gallery page
-}
init : Maybe GalleryModel -> ( GalleryModel, Cmd GalleryMessage )
init model =
    case model of
        Just model ->
            ( model, Cmd.none )

        Nothing ->
            ( { currentImage = Nothing
              , remotes = []
              }
            , imageRequest 15 Nothing
            )


galleryEndpoint : Int -> Maybe Int -> String
galleryEndpoint count before =
    case before of
        Just b ->
            apiEndpoint ++ "/images?count=" ++ (toString count) ++ "&id=" ++ (toString before)

        Nothing ->
            apiEndpoint ++ "/images?count=" ++ (toString count)


imageRequest : Int -> Maybe Int -> Cmd GalleryMessage
imageRequest count before =
    let
        handleResponse res =
            case res of
                Ok images ->
                    Images images

                Err e ->
                    NoImages
    in
        Http.get (galleryEndpoint count before) (Json.Decode.list decodeRemoteImage)
            |> Http.send handleResponse


{-| Modify the Gallery
-}
update : GalleryMessage -> GalleryModel -> ( GalleryModel, Cmd GalleryMessage )
update msg model =
    case msg of
        ViewImage remoteImage ->
            ( { model | currentImage = Just remoteImage }, Cmd.none )

        HideImage ->
            ( { model | currentImage = Nothing }, Cmd.none )

        Images images ->
            ( { model | remotes = images }, Cmd.none )

        NoImages ->
            ( model, Cmd.none )


{-| Rendering the Gallery Page
-}
view : Size -> GalleryModel -> Html GalleryMessage
view screenSize model =
    let
        contentWidth =
            if screenSize.width - 88 > 264 * 3 then
                264 * 3
            else if screenSize.width - 88 > 264 * 2 then
                264 * 2
            else
                264
    in
        section []
            [ article []
                [ previewList contentWidth model.remotes
                ]
            , case model.currentImage of
                Just image ->
                    let
                        largeImage =
                            largeOrFull image

                        widthRatio =
                            (toFloat largeImage.width) / (toFloat screenSize.width)

                        heightRatio =
                            (toFloat largeImage.height) / (toFloat screenSize.height)

                        imgRatio =
                            (toFloat largeImage.width) / (toFloat largeImage.height)

                        screenRatio =
                            (toFloat screenSize.width) / (toFloat screenSize.height)

                        ( imgWidth, imgHeight ) =
                            if imgRatio > screenRatio && largeImage.width > screenSize.width then
                                ( px <| (toFloat <| screenSize.width - 20)
                                , px <| (toFloat <| screenSize.width - 20) / imgRatio
                                )
                            else if largeImage.height > screenSize.height then
                                ( px <| (toFloat <| screenSize.height - 20) * imgRatio
                                , px <| (toFloat <| screenSize.height - 20)
                                )
                            else
                                ( px <| toFloat largeImage.width
                                , px <| toFloat largeImage.height
                                )
                    in
                        article
                            [ css
                                [ position fixed
                                , top (px 0)
                                , left (px 0)
                                , right (px 0)
                                , bottom (px 0)
                                , width (pct 100)
                                , height (pct 100)
                                , backgroundColor gray
                                , displayFlex
                                , flexDirection column
                                , justifyContent spaceAround
                                , overflow hidden
                                ]
                            , galleryClickAway
                            ]
                            [ div
                                [ css
                                    [ height imgHeight
                                    , width imgWidth
                                    , margin auto
                                    ]
                                ]
                                [ div []
                                    [ img
                                        [ src <| asUrl largeImage
                                        , css
                                            [ maxHeight <| pct 100
                                            , width <| pct 100
                                            ]
                                        ]
                                        []
                                    ]
                                ]
                            ]

                Nothing ->
                    text ""
            ]


previewList : Int -> List RemoteImage -> Html GalleryMessage
previewList contentWidth remoteImages =
    ul
        [ css
            [ textDecoration none
            , listStyleType none
            , displayFlex
            , flexDirection row
            , flexWrap wrap
            , justifyContent left
            , padding <| px 0
            , margin auto
            , width <| px <| toFloat contentWidth
            ]
        ]
        (List.map previewImage remoteImages)


imageHover : List Style
imageHover =
    [ backgroundColor black
    , color white
    ]


previewImage : RemoteImage -> Html GalleryMessage
previewImage image =
    let
        remoteImage =
            image.files
                |> List.filter (\file -> file.width == 200 || file.height == 200)
                |> List.head
    in
        case remoteImage of
            Just remoteImage ->
                li
                    [ css
                        [ display inlineBlock
                        , margin <| px 24
                        ]
                    ]
                    [ a
                        [ href "#"
                        , galleryOnClick image
                        , css
                            [ textDecoration none
                            , backgroundColor darkGray
                            , hover imageHover
                            , active imageHover
                            , display block
                            , cursor pointer
                            ]
                        ]
                        [ div
                            [ css
                                [ padding <| px 8
                                , cursor pointer
                                , textAlign center
                                , width <| px 200
                                ]
                            ]
                            [ div
                                [ css
                                    [ displayFlex
                                    , flexDirection column
                                    , justifyContent spaceAround
                                    , height <| px 200
                                    , width <| px 200
                                    ]
                                ]
                                [ div
                                    [ css [ overflow hidden ] ]
                                    [ img
                                        [ src <| Page.Gallery.RemoteImage.asUrl remoteImage
                                        , title <| Page.Gallery.RemoteImage.alternateText image
                                        , if remoteImage.width > remoteImage.height then
                                            css
                                                [ width <| px 200
                                                , height auto
                                                ]
                                          else
                                            css
                                                [ height (px 200)
                                                , width auto
                                                ]
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    ]

            Nothing ->
                text ""


galleryClickAway : Attribute GalleryMessage
galleryClickAway =
    onWithOptions
        "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Json.Decode.succeed HideImage)


galleryOnClick : RemoteImage -> Attribute GalleryMessage
galleryOnClick remoteImage =
    onWithOptions
        "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Json.Decode.succeed <| ViewImage <| remoteImage)
