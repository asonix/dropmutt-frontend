module Page.Gallery exposing (GalleryModel, init, view, update)

{-| Defines the gallery page

    @docs GalleryModel
    @docs init

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, title)
import Html.Styled.Events exposing (onWithOptions)
import Json.Decode
import Colors exposing (..)
import ImageFile exposing (..)
import Message exposing (Msg(..), PageMessage(..), GalleryMessage(..))


{-| The state for the Gallery Page
-}
type alias GalleryModel =
    { files : List ImageFile
    , currentImage : Maybe ImageFile
    }


type Url
    = Url String


imageFiles : List ImageFile
imageFiles =
    [ ImageFile
        { path = "1_5091690665780183050.png"
        , text = "A profile picture for asonix"
        }
    , ImageFile
        { path = "ArloRefSheet.png"
        , text = "A ref sheet for Arlo"
        }
    , ImageFile
        { path = "august_sign.png"
        , text = "A ref sheet for August"
        }
    , ImageFile
        { path = "DERG.png"
        , text = "A drawing of Atlas"
        }
    , ImageFile
        { path = "face.png"
        , text = "Another profile for asonix"
        }
    , ImageFile
        { path = "finasonix_1.png"
        , text = "A phone wallpaper for asonix"
        }
    , ImageFile
        { path = "IMG_20180125_190231_754.jpg"
        , text = "A concept for August"
        }
    , ImageFile
        { path = "photo_2018-03-26_00-03-09.jpg"
        , text = "A dog that ended up being asonix"
        }
    ]


{-| Initial state for the gallery page
-}
init : GalleryModel
init =
    { files = imageFiles
    , currentImage = Nothing
    }


{-| Modify the Gallery
-}
update : GalleryMessage -> GalleryModel -> ( GalleryModel, Cmd Msg )
update msg model =
    case msg of
        ViewImage imageFile ->
            ( { model | currentImage = Just imageFile }, Cmd.none )

        HideImage ->
            ( { model | currentImage = Nothing }, Cmd.none )


{-| Rendering the Gallery Page
-}
view : GalleryModel -> Html Msg
view model =
    section []
        [ article []
            [ previewList model.files
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
                            [ src <| fullLink image
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


previewList : List ImageFile -> Html Msg
previewList imageFiles =
    imageFiles
        |> List.indexedMap (\index -> \file -> ( index, file ))
        |> List.foldl makeImageLists { first = [], second = [], third = [] }
        |> renderTrio


makeImageLists : ( Int, ImageFile ) -> Trio -> Trio
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


previewImage : ImageFile -> Html Msg
previewImage imageFile =
    li
        [ css
            [ display inlineBlock
            ]
        ]
        [ a
            [ href "#"
            , galleryOnClick imageFile
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
                        [ src <| previewLink imageFile
                        , title <| imageText imageFile
                        , css
                            [ width (pct 100)
                            , height auto
                            ]
                        ]
                        []
                    , div [ css [ color grey ] ]
                        [ p [] [ text <| imageText imageFile ] ]
                    ]
                ]
            ]
        ]


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


galleryOnClick : ImageFile -> Attribute Msg
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
