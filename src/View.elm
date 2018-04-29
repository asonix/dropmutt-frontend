module View exposing (PageModel, PageModels, Link, init, layout, loadPage, update)

{-| Defines the main layout for the application

@docs PageModel
@docs PageModels
@docs Link
@docs init
@docs layout
@docs loadPage

-}

import Css exposing (..)
import Css.Media exposing (withMedia)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, class, rel, src, title, tabindex, type_)
import Html.Styled.Events exposing (onCheck, onWithOptions)
import Json.Decode
import Task exposing (perform)
import Colors exposing (..)
import Message exposing (Msg(..), PageMessage(..), LayoutMessage(..), GalleryMessage(..), SessionMessage(..))
import Page.Home exposing (HomeModel)
import Page.Admin exposing (AdminModel)
import Page.Gallery exposing (GalleryModel)
import Page.Auth exposing (AuthModel)
import Page.NotFound exposing (NotFoundModel)
import Route exposing (Route)
import Session exposing (Session, SessionAuth(..))


{-| The main model for the application
-}
type alias PageModel =
    { header : Header
    , nav : Nav
    , footer : Footer
    , currentPage : PageModels
    }


{-| A union type between all the types of page models
-}
type PageModels
    = Home HomeModel
    | Admin AdminModel
    | Gallery GalleryModel
    | Auth AuthModel
    | NotFound NotFoundModel


type alias Header =
    Link


type alias Nav =
    { nav : List Link
    , loggedOut : List Link
    , loggedIn : List Link
    }


type alias Footer =
    { tmp : String
    }


{-| A representation of a Link in the application

This type cannot link to pages outside of the application

-}
type alias Link =
    { text : String
    , title : String
    , kind : LinkKind
    }


type LinkKind
    = ToRoute Route
    | ToAction Msg
    | ToHref String


{-| Initialize the base state of the layout
-}
init : PageModel
init =
    { header =
        { text = "Bubble letters maybe?"
        , title = "The home page of the application"
        , kind = ToRoute Route.Home
        }
    , nav =
        { nav =
            [ { text = "Some other text"
              , title = "Another useful link"
              , kind = ToRoute Route.Two
              }
            , { text = "Gallery"
              , title = "View the image gallery"
              , kind = ToRoute Route.Gallery
              }
            ]
        , loggedOut =
            [ { text = "Auth"
              , title = "Note: Admins only. Administer the website"
              , kind = ToRoute Route.Auth
              }
            ]
        , loggedIn =
            [ { text = "Administration"
              , title = "Upload files and such"
              , kind = ToRoute Route.Admin
              }
            , { text = "Logout"
              , title = "Log out"
              , kind = ToAction <| Message.Session LogoutMsg
              }
            ]
        }
    , footer =
        { tmp = "copyright asonix 2018"
        }
    , currentPage = Home Page.Home.init
    }


{-| Render a new page
-}
loadPage : Route -> PageModel -> ( PageModel, Cmd Msg )
loadPage route model =
    case route of
        Route.Home ->
            case model.currentPage of
                Home _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | currentPage = Home Page.Home.init }, Cmd.none )

        Route.Admin ->
            case model.currentPage of
                Admin _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | currentPage = Admin Page.Admin.init }, Cmd.none )

        Route.Gallery ->
            case model.currentPage of
                Gallery _ ->
                    ( model, Cmd.none )

                _ ->
                    Page.Gallery.init
                        |> Tuple.mapFirst (\galleryModel -> { model | currentPage = Gallery galleryModel })

        Route.Auth ->
            case model.currentPage of
                Auth _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | currentPage = Auth Page.Auth.init }, Cmd.none )

        Route.NotFound ->
            case model.currentPage of
                NotFound _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | currentPage = NotFound Page.NotFound.init }, Cmd.none )

        _ ->
            ( model, Route.modifyUrl Route.NotFound )


{-| The main application layout
-}
layout : Session -> PageModel -> Html Msg
layout session model =
    div
        [ css
            [ backgroundColor lightGrey
            , fontFamily sansSerif
            , color black
            , margin (px 0)
            , padding2 (px 0) (Css.em 1)
            , boxSizing borderBox
            , position absolute
            , top (px 0)
            , left (px 0)
            , minWidth (pct 100)
            , minHeight (vh 100)
            , withMedia
                [ Css.Media.all [ Css.Media.maxWidth (px 650) ] ]
                [ padding (px 0) ]
            ]
        ]
        [ div
            [ css
                [ maxWidth (px 900)
                , minWidth (px 620)
                , margin2 (Css.em 4) auto
                ]
            ]
            [ headerView model.header
            , navView session model.nav
            , pageView session model.currentPage
            , footerView model.footer
            ]
        ]


darkShadow : Style
darkShadow =
    boxShadow4 (px 0) (px 3) (px 4) darkGrey


lightShadow : Style
lightShadow =
    boxShadow4 (px 0) (px 3) (px 4) grey


pageView : Session -> PageModels -> Html Msg
pageView session model =
    div
        [ css
            [ backgroundColor white
            , color black
            , padding (Css.em 2)
            , margin2 (Css.em 2) (px 0)
            , borderRadius (px 8)
            , lightShadow
            ]
        ]
        [ case model of
            Home homeModel ->
                Page.Home.view homeModel

            Admin oneModel ->
                Page.Admin.view oneModel

            Gallery galleryModel ->
                Page.Gallery.view session.dimentions galleryModel

            Auth authModel ->
                Page.Auth.view authModel

            NotFound notFoundModel ->
                Page.NotFound.view notFoundModel
        ]


mobileWidth : Px
mobileWidth =
    px 900


headerView : Header -> Html Msg
headerView link =
    header
        [ css [ textAlign center ] ]
        [ h1 []
            [ a
                ([ title link.title
                 , css
                    [ color black
                    , textDecoration none
                    , hover [ color gray ]
                    , active [ color gray ]
                    ]
                 ]
                    ++ linkKindAttribute link.kind
                )
                [ text link.text ]
            ]
        ]


navView : Session -> Nav -> Html Msg
navView session model =
    let
        startIndex =
            1

        ( end, navIntermediate ) =
            model.nav
                |> addListIndex (startIndex + 1)

        ( newEnd, navAuth ) =
            (case session.auth of
                LoggedOut ->
                    model.loggedOut

                LoggedIn ->
                    model.loggedIn
            )
                |> addListIndex end

        navContent =
            navAuth
                |> (++) navIntermediate
                |> mapLinks
                |> navList

        navHeight =
            Css.em 4
    in
        nav
            [ css
                [ height navHeight
                , backgroundColor black
                , color white
                , flexDirection row
                , justifyContent left
                , backgroundColor black
                , displayFlex
                , width (pct 100)
                , borderRadius (px 8)
                , darkShadow
                ]
            ]
            [ navContent ]


navList : List (Html Msg) -> Html Msg
navList links =
    if List.isEmpty links then
        text ""
    else
        ul
            [ css
                [ padding (px 0)
                , textDecoration none
                , displayFlex
                , justifyContent spaceAround
                , flexDirection row
                , margin (px 0)
                , width (pct 100)
                , height (pct 100)
                ]
            ]
            links


addListIndex : Int -> List Link -> ( Int, List ( Int, Link ) )
addListIndex start link_list =
    link_list
        |> List.foldl addLinkIndex ( start, [] )
        |> Tuple.mapSecond List.reverse


addLinkIndex : Link -> ( Int, List ( Int, Link ) ) -> ( Int, List ( Int, Link ) )
addLinkIndex link ( index, links ) =
    ( index + 1, ( index, link ) :: links )


mapLinks : List ( Int, Link ) -> List (Html Msg)
mapLinks =
    List.map (navItem (defaultNavColors ++ navLinkCss))


linkKindAttribute : LinkKind -> List (Attribute Msg)
linkKindAttribute kind =
    case kind of
        ToRoute route ->
            [ Route.href route
            , navLinkOnClick route
            ]

        ToAction action ->
            [ Html.Styled.Attributes.href "#"
            , actionLinkOnClick action
            ]

        ToHref href ->
            [ Html.Styled.Attributes.href href ]


navItem : List Style -> ( Int, Link ) -> Html Msg
navItem styles ( index, link ) =
    li
        [ css
            [ display inlineBlock
            , width (pct 100)
            , textAlign center
            , lastChild [ borderRadius4 (px 0) (px 8) (px 8) (px 0) ]
            , firstChild [ borderRadius4 (px 8) (px 0) (px 0) (px 8) ]
            , active navHover
            , hover navHover
            , focus
                (navHover
                    ++ [ borderColor lightGrey
                       , borderWidth (px 1)
                       , borderStyle solid
                       , margin (px -1)
                       ]
                )
            ]
        ]
        [ a
            ([ css styles
             , tabindex index
             , title link.title
             ]
                ++ linkKindAttribute link.kind
            )
            [ div
                [ css [ cursor pointer ] ]
                [ text link.text ]
            ]
        ]


actionLinkOnClick : Msg -> Attribute Msg
actionLinkOnClick action =
    onWithOptions
        "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Json.Decode.succeed <| action)


navLinkOnClick : Route -> Attribute Msg
navLinkOnClick route =
    onWithOptions
        "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Json.Decode.succeed <| Load route)


navHover : List Style
navHover =
    [ backgroundColor darkGray
    , color white
    ]


navLinkCss : List Style
navLinkCss =
    [ displayFlex
    , flexDirection column
    , justifyContent spaceAround
    , height (pct 100)
    , width (pct 100)
    , textDecoration none
    , cursor pointer
    ]


defaultNavColors : List Style
defaultNavColors =
    [ color grey
    ]


homeNavColors : List Style
homeNavColors =
    [ color white
    ]


homeLinkCss : List Style
homeLinkCss =
    (fontWeight (int 600)) :: navLinkCss ++ homeNavColors


footerView : Footer -> Html Msg
footerView model =
    footer
        [ css
            [ width (pct 100)
            , color white
            , backgroundColor black
            , borderRadius (px 8)
            , darkShadow
            ]
        ]
        [ p
            [ css
                [ margin (px 0)
                , padding (Css.em 2)
                , textAlign center
                ]
            ]
            [ text model.tmp ]
        ]


{-| Update the state of the layout
-}
update : PageMessage -> Session -> PageModel -> ( ( PageModel, Cmd Msg ), Session )
update msg session model =
    case msg of
        LayoutMsg layoutMsg ->
            ( ( model, Cmd.none ), session )

        GalleryMsg galleryMsg ->
            case model.currentPage of
                Gallery galleryModel ->
                    galleryModel
                        |> Page.Gallery.update galleryMsg
                        |> Tuple.mapFirst (\x -> { model | currentPage = Gallery x })
                        |> (\x -> ( x, session ))

                _ ->
                    ( ( model, Cmd.none ), session )

        AuthMsg authMsg ->
            case model.currentPage of
                Auth authModel ->
                    authModel
                        |> Page.Auth.update authMsg session
                        |> Tuple.mapFirst (Tuple.mapFirst (\x -> { model | currentPage = Auth x }))

                _ ->
                    ( ( model, Cmd.none ), session )

        AdminMsg adminMsg ->
            case model.currentPage of
                Admin adminModel ->
                    adminModel
                        |> Page.Admin.update adminMsg
                        |> Tuple.mapFirst (\x -> { model | currentPage = Admin x })
                        |> (\x -> ( x, session ))

                _ ->
                    ( ( model, Cmd.none ), session )
