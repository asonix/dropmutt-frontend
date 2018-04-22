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
import Message exposing (Msg(..), PageMessage(..), LayoutMessage(..))
import Page.Home exposing (HomeModel)
import Page.One exposing (OneModel)
import Page.NotFound exposing (NotFoundModel)
import Route exposing (Route)


{-| The main model for the application
-}
type alias PageModel =
    { leftNav : LeftNav
    , rightNav : List Link
    , showNav : Bool
    , currentPage : PageModels
    }


{-| Render a new page
-}
loadPage : Route -> PageModel -> ( PageModel, Cmd Msg )
loadPage route model =
    let
        mod_model =
            { model | showNav = False }
    in
        case route of
            Route.Home ->
                case mod_model.currentPage of
                    Home _ ->
                        ( mod_model, Cmd.none )

                    _ ->
                        ( { mod_model | currentPage = Home Page.Home.init }, Cmd.none )

            Route.One ->
                case model.currentPage of
                    One _ ->
                        ( mod_model, Cmd.none )

                    _ ->
                        ( { mod_model | currentPage = One Page.One.init }, Cmd.none )

            Route.NotFound ->
                case model.currentPage of
                    NotFound _ ->
                        ( mod_model, Cmd.none )

                    _ ->
                        ( { mod_model | currentPage = NotFound Page.NotFound.init }, Cmd.none )

            _ ->
                ( mod_model, Route.modifyUrl Route.NotFound )


{-| A union type between all the types of page models
-}
type PageModels
    = Home HomeModel
    | One OneModel
    | NotFound NotFoundModel


type alias LeftNav =
    { homeLink : Link
    , navLinks : List Link
    }


{-| A representation of a Link in the application

This type cannot link to pages outside of the application

-}
type alias Link =
    { text : String
    , title : String
    , route : Route
    }


{-| Initialize the base state of the layout
-}
init : PageModel
init =
    { leftNav =
        { homeLink =
            { text = "Home Link"
            , title = "The home page of the application"
            , route = Route.Home
            }
        , navLinks =
            [ { text = "Some Link Text"
              , title = "Some really useful link"
              , route = Route.One
              }
            , { text = "Some other text"
              , title = "Another useful link"
              , route = Route.Two
              }
            ]
        }
    , rightNav =
        [ { text = "Some Link Text"
          , title = "Wow, so many links"
          , route = Route.Three
          }
        , { text = "Some other text"
          , title = "Incredible!! another link"
          , route = Route.Four
          }
        ]
    , showNav = False
    , currentPage = Home Page.Home.init
    }


{-| The main application layout
-}
layout : PageModel -> Html Msg
layout model =
    let
        page =
            case model.currentPage of
                Home homeModel ->
                    Page.Home.view homeModel

                One oneModel ->
                    Page.One.view oneModel

                NotFound notFoundModel ->
                    Page.NotFound.view notFoundModel
    in
        div
            [ css
                [ backgroundColor white
                , fontFamily sansSerif
                , color black
                , margin (px 0)
                , padding (px 0)
                , position absolute
                , top (px 0)
                , left (px 0)
                , width (pct 100)
                , minHeight (vh 100)
                ]
            ]
            [ ourNav model
            , page
            , ourFooter model
            ]


mobileWidth : Px
mobileWidth =
    px 900


ourNav : PageModel -> Html Msg
ourNav model =
    let
        startIndex =
            if model.showNav then
                2
            else
                1

        ( end, leftNav ) =
            model.leftNav.navLinks
                |> addListIndex (startIndex + 1)
                |> Tuple.mapSecond
                    (navList
                        << (::) (navItem homeLinkCss ( startIndex, model.leftNav.homeLink ))
                        << mapLinks
                    )

        ( _, rightNav ) =
            model.rightNav
                |> addListIndex end
                |> Tuple.mapSecond (navList << mapLinks)

        navHeight =
            Css.em 4
    in
        header []
            [ div
                [ css
                    [ height navHeight
                    , withMedia
                        [ Css.Media.all [ Css.Media.maxWidth mobileWidth ] ]
                        [ padding (Css.em 2)
                        , height auto
                        ]
                    ]
                ]
                [ text "tmp" ]
            , div
                [ css
                    (if model.showNav then
                        [ withMedia
                            [ Css.Media.all [ Css.Media.maxWidth mobileWidth ] ]
                            [ minHeight (pct 100)
                            , maxHeight (pct 100)
                            , overflow auto
                            ]
                        ]
                     else
                        []
                    )
                , css
                    [ position fixed
                    , top (px 0)
                    , left (px 0)
                    , width (pct 100)
                    , backgroundColor black
                    , color white
                    , zIndex (int 1)
                    ]
                ]
                [ div
                    [ css
                        [ display none
                        , withMedia
                            [ Css.Media.all [ Css.Media.maxWidth mobileWidth ] ]
                            [ display block
                            ]
                        ]
                    ]
                    [ a
                        [ toggleNav model
                        , tabindex 1
                        , Html.Styled.Attributes.href "#"
                        , css navLinkCss
                        , css [ color white ]
                        ]
                        [ div
                            [ css
                                (if model.showNav then
                                    navHover
                                 else
                                    []
                                )
                            , css
                                [ padding (Css.em 2)
                                ]
                            ]
                            [ if model.showNav then
                                text "Hide Navigation"
                              else
                                text "Navigation"
                            ]
                        ]
                    ]
                , nav
                    [ css
                        [ withMedia
                            [ Css.Media.all [ Css.Media.maxWidth mobileWidth ] ]
                            [ position relative
                            , flexDirection column
                            , justifyContent top
                            , height auto
                            , if model.showNav then
                                displayFlex
                              else
                                display none
                            ]
                        , position fixed
                        , height navHeight
                        , top (px 0)
                        , left (px 0)
                        , backgroundColor black
                        , color white
                        , flexDirection row
                        , justifyContent spaceBetween
                        , backgroundColor black
                        , displayFlex
                        , width (pct 100)
                        ]
                    ]
                    [ leftNav, rightNav ]
                ]
            ]


navList : List (Html Msg) -> Html Msg
navList links =
    if List.isEmpty links then
        text ""
    else
        ul
            [ css
                [ withMedia
                    [ Css.Media.all [ Css.Media.maxWidth mobileWidth ] ]
                    [ width (pct 100)
                    , flexDirection column
                    ]
                , padding (px 0)
                , textDecoration none
                , displayFlex
                , justifyContent flexStart
                , flexDirection row
                , margin (px 0)
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


navItem : List Style -> ( Int, Link ) -> Html Msg
navItem styles ( index, link ) =
    li
        [ css [ display inlineBlock ] ]
        [ a
            [ css styles
            , Route.href link.route
            , tabindex index
            , title link.title
            , navLinkOnClick link
            ]
            [ div
                [ css
                    [ withMedia
                        [ Css.Media.all [ Css.Media.maxWidth mobileWidth ] ]
                        [ padding (Css.em 2) ]
                    , cursor pointer
                    , padding2 (px 0) (Css.em 2)
                    ]
                ]
                [ text link.text ]
            ]
        ]


navLinkOnClick : Link -> Attribute Msg
navLinkOnClick link =
    onWithOptions
        "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Json.Decode.succeed <| Load link.route)


toggleNav : PageModel -> Attribute Msg
toggleNav model =
    onWithOptions
        "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Json.Decode.succeed <| (Page <| Layout <| ShowNav (not model.showNav)))


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
    , focus
        (navHover
            ++ [ withMedia
                    [ Css.Media.all [ Css.Media.minWidth mobileWidth ] ]
                    [ borderColor lightGrey
                    , borderWidth (px 1)
                    , borderStyle solid
                    , margin (px -1)
                    ]
               ]
        )
    , active navHover
    , hover navHover
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


footerHeight : Em
footerHeight =
    Css.em 6


ourFooter : PageModel -> Html Msg
ourFooter _ =
    div []
        [ div [ css [ height footerHeight ] ] []
        , footer
            [ css
                [ height footerHeight
                , position absolute
                , bottom (px 0)
                , left (px 0)
                , width (pct 100)
                , color white
                , backgroundColor gray
                ]
            ]
            [ p [] [ text "tmp" ]
            ]
        ]


{-| Update the state of the layout
-}
update : LayoutMessage -> PageModel -> ( PageModel, Cmd msg )
update msg model =
    case msg of
        ShowNav shown ->
            ( { model | showNav = shown }, Cmd.none )
