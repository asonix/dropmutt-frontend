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
import Auth exposing (apiEndpoint)
import Colors exposing (..)
import ImageFile exposing (..)
import Message exposing (Msg(..), PageMessage(..), GalleryMessage(..))
import RemoteImage exposing (..)


{-| The state for the Gallery Page
-}
type alias GalleryModel =
    { files : List ImageFile
    , currentImage : Maybe RemoteImage
    , remotes : List RemoteImage
    }


{-| Initial state for the gallery page
-}
init : ( GalleryModel, Cmd Msg )
init =
    ( { files = []
      , currentImage = Nothing
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


imageRequest : Int -> Maybe Int -> Cmd Msg
imageRequest count before =
    let
        handleResponse res =
            case res of
                Ok images ->
                    Page <| GalleryMsg <| Images images

                Err e ->
                    Page <| GalleryMsg <| NoImages
    in
        Http.get (galleryEndpoint count before) (Json.Decode.list decodeRemoteImage)
            |> Http.send handleResponse


{-| Modify the Gallery
-}
update : GalleryMessage -> GalleryModel -> ( GalleryModel, Cmd Msg )
update msg model =
    case msg of
        ViewImage imageFile ->
            ( { model | currentImage = Just imageFile }, Cmd.none )

        HideImage ->
            ( { model | currentImage = Nothing }, Cmd.none )

        Images images ->
            ( { model | remotes = images }, Cmd.none )

        NoImages ->
            ( model, Cmd.none )


{-| Rendering the Gallery Page
-}
view : GalleryModel -> Html Msg
view model =
    section []
        [ article []
            [ previewList model.remotes
            ]
        , case model.currentImage of
            Just image ->
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
                            [ width auto
                            , height (vh 75)
                            , margin2 (px 0) auto
                            ]
                        ]
                        [ img
                            [ src <| fullImage image
                            , css
                                [ maxHeight (pct 100)
                                , width auto
                                ]
                            ]
                            []
                        ]
                    ]

            Nothing ->
                text ""
        ]


type alias Trio =
    { first : List (Html Msg)
    , second : List (Html Msg)
    , third : List (Html Msg)
    }


renderTrio : Trio -> Html Msg
renderTrio trio =
    ul
        [ css
            [ textDecoration none
            , listStyleType none
            , displayFlex
            , flexDirection row
            , justifyContent spaceAround
            , padding (px 0)
            , margin (px 0)
            ]
        ]
        [ renderTrioPart <| List.reverse trio.first
        , renderTrioPart <| List.reverse trio.second
        , renderTrioPart <| List.reverse trio.third
        ]


renderTrioPart : List (Html Msg) -> Html Msg
renderTrioPart part =
    li []
        [ ul
            [ css
                [ textDecoration none
                , listStyleType none
                , displayFlex
                , flexDirection column
                , padding (px 0)
                , margin (px 0)
                ]
            ]
            part
        ]


previewList : List RemoteImage -> Html Msg
previewList imageFiles =
    imageFiles
        |> List.indexedMap (\index -> \file -> ( index, file ))
        |> List.foldl makeImageLists { first = [], second = [], third = [] }
        |> renderTrio


makeImageLists : ( Int, RemoteImage ) -> Trio -> Trio
makeImageLists ( index, imageFile ) lists =
    let
        rendered =
            previewImage imageFile
    in
        case index % 3 of
            0 ->
                { lists | first = rendered :: lists.first }

            1 ->
                { lists | second = rendered :: lists.second }

            2 ->
                { lists | third = rendered :: lists.third }

            _ ->
                lists


imageHover : List Style
imageHover =
    [ backgroundColor black
    , color white
    ]


previewImage : RemoteImage -> Html Msg
previewImage image =
    let
        imageFile =
            image.files
                |> List.filter (\file -> file.width == 200)
                |> List.head
    in
        case imageFile of
            Just imageFile ->
                li
                    [ css
                        [ display inlineBlock
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
                            , margin (Css.em 0.5)
                            , cursor pointer
                            ]
                        ]
                        [ div
                            [ css
                                [ padding (Css.em 0.5)
                                , cursor pointer
                                ]
                            ]
                            [ div
                                [ css
                                    [ overflow hidden
                                    , displayFlex
                                    , flexDirection column
                                    , justifyContent spaceAround
                                    , textAlign center
                                    ]
                                ]
                                [ img
                                    [ src <| smallImage image
                                    , title <| smallImage image
                                    , css
                                        [ width (pct 100)
                                        , height auto
                                        ]
                                    ]
                                    []
                                , div [ css [ color grey ] ]
                                    [ p [] [ text "placeholder text" ] ]
                                ]
                            ]
                        ]
                    ]

            Nothing ->
                text ""


galleryClickAway : Attribute Msg
galleryClickAway =
    onWithOptions
        "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (HideImage
            |> GalleryMsg
            |> Page
            |> Json.Decode.succeed
        )


galleryOnClick : RemoteImage -> Attribute Msg
galleryOnClick imageFile =
    onWithOptions
        "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (imageFile
            |> ViewImage
            |> GalleryMsg
            |> Page
            |> Json.Decode.succeed
        )
