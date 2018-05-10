module Page exposing (Page, Pages, PageSession, Link, init, layout, loadPage, update, initPageSession)

{-| Defines the main layout for the application

@docs Page
@docs Pages
@docs Link
@docs init
@docs layout
@docs loadPage

-}

import Css exposing (..)
import Css.Media exposing (withMedia)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, class, rel, src, title, tabindex, type_)
import Html.Styled.Events exposing (onCheck, onWithOptions)
import Json.Decode
import Task exposing (perform)
import Colors exposing (..)
import Message exposing (Msg(..), PageMessage(..), LayoutMessage(..), GalleriesMessage(..), SessionMessage(..))
import Page.Home exposing (Home)
import Page.Admin exposing (Admin)
import Page.Galleries exposing (Galleries)
import Page.Auth exposing (Auth)
import Page.NotFound exposing (NotFound)
import Route exposing (Route)
import Session exposing (Session, SessionAuth(..))


{-| The main model for the application
-}
type alias Page =
    { header : Header
    , nav : Nav
    , footer : Footer
    , currentPage : Pages
    , route : Route
    }


{-| A union type between all the types of page models
-}
type Pages
    = Home Home
    | Admin Admin
    | Galleries Galleries
    | Auth Auth
    | NotFound NotFound


type alias PageSession =
    { home : Maybe Home
    , admin : Maybe Admin
    , galleries : Maybe Galleries
    , auth : Maybe Auth
    , notFound : Maybe NotFound
    }


initPageSession : PageSession
initPageSession =
    { home = Nothing
    , admin = Nothing
    , galleries = Nothing
    , auth = Nothing
    , notFound = Nothing
    }


type alias Header =
    Link


type alias Nav =
    { nav : List Link
    , loggedOut : List Link
    , loggedIn : Route -> List Link
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
init : Page
init =
    { header =
        { text = "Bubble letters maybe?"
        , title = "The home page of the application"
        , kind = ToRoute Route.Home
        }
    , nav =
        { nav =
            [ { text = "Galleries"
              , title = "View the image galleries"
              , kind = ToRoute <| Route.Galleries Nothing
              }
            ]
        , loggedOut =
            [ { text = "Auth"
              , title = "Note: Admins only. Administer the website"
              , kind = ToRoute Route.Auth
              }
            ]
        , loggedIn =
            (\route ->
                [ { text = "Administration"
                  , title = "Upload files and such"
                  , kind = ToRoute Route.Admin
                  }
                , { text = "Logout"
                  , title = "Log out"
                  , kind = ToAction <| Message.Session <| LogoutMsg route
                  }
                ]
            )
        }
    , footer =
        { tmp = "copyright asonix 2018"
        }
    , currentPage = Home <| Page.Home.init Nothing
    , route = Route.Home
    }


{-| Render a new page
-}
loadPage : Route -> PageSession -> Page -> ( Page, Cmd Msg )
loadPage route pageSession model =
    case route of
        Route.Home ->
            case model.currentPage of
                Home _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | currentPage = Home <| Page.Home.init pageSession.home
                        , route = route
                      }
                    , Cmd.none
                    )

        Route.Admin ->
            case model.currentPage of
                Admin _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | currentPage = Admin <| Page.Admin.init pageSession.admin
                        , route = route
                      }
                    , Cmd.none
                    )

        Route.Galleries galleriesRoute ->
            pageSession.galleries
                |> Page.Galleries.loadPage galleriesRoute
                |> Tuple.mapFirst
                    (\galleryModel ->
                        { model
                            | currentPage = Galleries galleryModel
                            , route = route
                        }
                    )
                |> (Tuple.mapSecond (Cmd.map (Message.Page << Message.GalleriesMsg)))

        Route.Auth ->
            case model.currentPage of
                Auth _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | currentPage = Auth <| Page.Auth.init pageSession.auth
                        , route = route
                      }
                    , Cmd.none
                    )

        Route.NotFound ->
            case model.currentPage of
                NotFound _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | currentPage = NotFound <| Page.NotFound.init pageSession.notFound
                        , route = route
                      }
                    , Cmd.none
                    )

        _ ->
            ( model, Route.modifyUrl Route.NotFound )


{-| The main application layout
-}
layout : Session PageSession -> Page -> Html Msg
layout session model =
    div
        [ css
            [ backgroundColor lightGrey
            , fontFamily sansSerif
            , color black
            , margin (px 0)
            , padding2 (px 0) (px 16)
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
                , minWidth (px 320)
                , margin2 (Css.em 4) auto
                ]
            ]
            [ headerView model.header
            , navView session model.route model.nav
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


pageView : Session PageSession -> Pages -> Html Msg
pageView session model =
    div
        [ css
            [ backgroundColor white
            , color black
            , padding (px 32)
            , margin2 (px 32) (px 0)
            , borderRadius (px 8)
            , lightShadow
            ]
        ]
        [ case model of
            Home homeModel ->
                homeModel
                    |> Page.Home.view
                    |> Html.Styled.map (Message.Page << HomeMsg)

            Admin adminModel ->
                adminModel
                    |> Page.Admin.view
                    |> Html.Styled.map (Message.Page << AdminMsg)

            Galleries galleryModel ->
                galleryModel
                    |> Page.Galleries.view session.dimensions
                    |> Html.Styled.map (Message.Page << GalleriesMsg)

            Auth authModel ->
                authModel
                    |> Page.Auth.view
                    |> Html.Styled.map (Message.Page << AuthMsg)

            NotFound notFoundModel ->
                notFoundModel
                    |> Page.NotFound.view
                    |> Html.Styled.map (Message.Page << NotFoundMsg)
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


navView : Session PageSession -> Route -> Nav -> Html Msg
navView session route model =
    let
        logoutRoute =
            case route of
                Route.Admin ->
                    Route.Home

                other ->
                    other

        startIndex =
            1

        ( end, navIntermediate ) =
            model.nav
                |> addListIndex (startIndex + 1)

        ( newEnd, navAuth ) =
            (case session.auth of
                Session.LoggedOut ->
                    model.loggedOut

                Session.LoggedIn ->
                    model.loggedIn logoutRoute
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
update : PageMessage -> Session PageSession -> Page -> ( ( Page, Cmd Msg ), Session PageSession )
update msg session model =
    case msg of
        LayoutMsg layoutMsg ->
            ( ( model, Cmd.none ), session )

        GalleriesMsg galleryMsg ->
            case model.currentPage of
                Galleries galleryModel ->
                    let
                        ( newGalleries, newGalleriesMsg ) =
                            Page.Galleries.update galleryMsg galleryModel

                        newModel =
                            { model | currentPage = Galleries newGalleries }

                        cmd =
                            Cmd.map (Message.Page << GalleriesMsg) newGalleriesMsg

                        pageSession =
                            Session.getPageSession session

                        newPageSession =
                            { pageSession | galleries = Just newGalleries }

                        newSession =
                            Session.setPageSession newPageSession session
                    in
                        ( ( newModel, cmd ), newSession )

                _ ->
                    ( ( model, Cmd.none ), session )

        AuthMsg authMsg ->
            case model.currentPage of
                Auth authModel ->
                    let
                        ( newAuth, newSessionMsg ) =
                            Page.Auth.update authMsg authModel

                        newModel =
                            { model | currentPage = Auth newAuth }

                        cmd =
                            Cmd.map (Message.Session) newSessionMsg

                        pageSession =
                            Session.getPageSession session

                        newPageSession =
                            { pageSession | auth = Just newAuth }

                        newSession =
                            Session.setPageSession newPageSession session
                    in
                        ( ( newModel, cmd ), newSession )

                _ ->
                    ( ( model, Cmd.none ), session )

        AdminMsg adminMsg ->
            case model.currentPage of
                Admin adminModel ->
                    let
                        ( newAdmin, newAdminMsg ) =
                            Page.Admin.update adminMsg adminModel

                        newModel =
                            { model | currentPage = Admin newAdmin }

                        cmd =
                            Cmd.map (Message.Page << AdminMsg) newAdminMsg

                        pageSession =
                            Session.getPageSession session

                        newPageSession =
                            { pageSession | admin = Just newAdmin }

                        newSession =
                            Session.setPageSession newPageSession session
                    in
                        ( ( newModel, cmd ), newSession )

                _ ->
                    ( ( model, Cmd.none ), session )

        _ ->
            ( ( model, Cmd.none ), session )
