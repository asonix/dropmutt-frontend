module Route exposing (..)

{-| Define the Route type

This type provides a type-safe mechanism for changing pages


# Route type

@docs route
@docs fromLocation
@docs href
@docs modifyUrl

-}

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes
import Navigation
import UrlParser exposing (Parser, (</>), oneOf, parseHash, string, int)


{-| Define which pages are available
-}
type Route
    = Home
    | Two
    | Galleries (Maybe Gallery)
    | Auth
    | NotFound


type alias Gallery =
    { name : String
    , subpage : Maybe GallerySubPage
    }


type GallerySubPage
    = Image String
    | Edit
    | Add


galleries : Parser (Route -> a) a
galleries =
    oneOf
        [ UrlParser.map (Galleries Nothing) (UrlParser.s "galleries")
        , UrlParser.map (Galleries << Just) gallery
        ]


gallery : Parser (Gallery -> a) a
gallery =
    oneOf
        [ UrlParser.map (flip Gallery Nothing) (UrlParser.s "galleries" </> string)
        , UrlParser.map Gallery (UrlParser.s "galleries" </> string </> UrlParser.map Just gallerySubPage)
        ]


rawGalleryImage : Parser (String -> a) a
rawGalleryImage =
    UrlParser.s "image" </> string


gallerySubPage : Parser (GallerySubPage -> a) a
gallerySubPage =
    oneOf
        [ UrlParser.map Image rawGalleryImage
        , UrlParser.map Edit (UrlParser.s "edit")
        , UrlParser.map Add (UrlParser.s "upload")
        ]


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map Home (UrlParser.s "")
        , UrlParser.map Two (UrlParser.s "two")
        , galleries
        , UrlParser.map Auth (UrlParser.s "auth")
        , UrlParser.map NotFound (UrlParser.s "404")
        ]


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Home ->
                    []

                Two ->
                    [ "two" ]

                Galleries gallery ->
                    case gallery of
                        Just gallery ->
                            case gallery.subpage of
                                Just subpage ->
                                    case subpage of
                                        Image image ->
                                            [ "galleries", gallery.name, "image", image ]

                                        Edit ->
                                            [ "galleries", gallery.name, "edit" ]

                                        Add ->
                                            [ "galleries", gallery.name, "upload" ]

                                Nothing ->
                                    [ "galleries", gallery.name ]

                        Nothing ->
                            [ "galleries" ]

                Auth ->
                    [ "auth" ]

                NotFound ->
                    [ "404" ]
    in
        "#/" ++ String.join "/" pieces


{-| Convert a route to an Html Attribute
-}
href : Route -> Attribute msg
href route =
    Html.Styled.Attributes.href (routeToString route)


{-| Tell the runtime to modify the current URL
-}
modifyUrl : Route -> Cmd msg
modifyUrl =
    Navigation.modifyUrl << routeToString


{-| Convert a given location to a Route
-}
fromLocation : Navigation.Location -> Route
fromLocation location =
    if String.isEmpty location.hash then
        Home
    else
        case parseHash route location of
            Just r ->
                r

            Nothing ->
                NotFound
