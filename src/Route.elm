module Route exposing (Route(..), fromLocation, href, modifyUrl)

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
import UrlParser exposing (Parser, (</>), oneOf, parseHash, string)


{-| Define which pages are available
-}
type Route
    = Home
    | One
    | Two
    | Gallery
    | Four
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map Home (UrlParser.s "")
        , UrlParser.map One (UrlParser.s "one")
        , UrlParser.map Two (UrlParser.s "two")
        , UrlParser.map Gallery (UrlParser.s "gallery")
        , UrlParser.map Four (UrlParser.s "four")
        , UrlParser.map NotFound (UrlParser.s "404")
        ]


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Home ->
                    []

                One ->
                    [ "one" ]

                Two ->
                    [ "two" ]

                Gallery ->
                    [ "gallery" ]

                Four ->
                    [ "four" ]

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
