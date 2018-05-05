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
    | Admin
    | Two
    | Galleries (Maybe Gallery)
    | Auth
    | NotFound


type alias Gallery =
    { name : String
    , image : Maybe String
    }


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map Home (UrlParser.s "")
        , UrlParser.map Admin (UrlParser.s "admin")
        , UrlParser.map Two (UrlParser.s "two")
        , UrlParser.map (Galleries (Maybe Gallery)) (UrlParser.s "galleries")
        , UrlParser.map (Galleries (Maybe Gallery)) (UrlParser.s "galleries" </> string)
        , UrlParser.map (Galleries (Maybe Gallery)) (UrlParser.s "galleries" </> string </> string)
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

                Admin ->
                    [ "admin" ]

                Two ->
                    [ "two" ]

                Galleries gallery ->
                    case gallery of
                        Just gallery ->
                            case gallery.image of
                                Just image ->
                                    [ "galleries", gallery.name, image ]

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
