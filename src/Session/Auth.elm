module Session.Auth exposing (..)

import Http
import Json.Decode
import Json.Encode
import Message exposing (SessionMessage(..))
import Route exposing (Route)


type alias AuthParams =
    { username : String
    , password : String
    }


newAuthParams : String -> String -> AuthParams
newAuthParams username password =
    { username = username
    , password = password
    }


apiEndpoint : String
apiEndpoint =
    "http://localhost:8080/api/v1"


loginUrl : String
loginUrl =
    apiEndpoint ++ "/login"


signupUrl : String
signupUrl =
    apiEndpoint ++ "/signup"


logoutUrl : String
logoutUrl =
    apiEndpoint ++ "/logout"


checkAuthUrl : String
checkAuthUrl =
    apiEndpoint ++ "/check-auth"


type Method
    = HEAD
    | GET
    | POST
    | PUT
    | PATCH
    | DELETE


methodToString : Method -> String
methodToString method =
    case method of
        HEAD ->
            "HEAD"

        GET ->
            "GET"

        POST ->
            "POST"

        PUT ->
            "PUT"

        PATCH ->
            "PATCH"

        DELETE ->
            "DELETE"


cookiePost : String -> Http.Body -> Json.Decode.Decoder a -> Http.Request a
cookiePost url body decoder =
    cookieRequest POST url body decoder


cookieDelete : String -> Json.Decode.Decoder a -> Http.Request a
cookieDelete url decoder =
    cookieRequest DELETE url Http.emptyBody decoder


cookieRequest : Method -> String -> Http.Body -> Json.Decode.Decoder a -> Http.Request a
cookieRequest method url body decoder =
    Http.request
        { method = methodToString method
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = True
        }


logoutRequest : Route -> Cmd SessionMessage
logoutRequest route =
    let
        handleResponse res =
            case res of
                Ok a ->
                    LoggedOut route

                Err e ->
                    LoggedInNoRedirect
    in
        cookieDelete logoutUrl (Json.Decode.succeed 0)
            |> Http.send handleResponse


checkAuth : Cmd SessionMessage
checkAuth =
    let
        handleResponse res =
            case res of
                Ok a ->
                    LoggedInNoRedirect

                Err error ->
                    LoggedOutNoRedirect
    in
        cookieRequest GET checkAuthUrl Http.emptyBody (Json.Decode.succeed 0)
            |> Http.send handleResponse


authPayload : AuthParams -> Http.Body
authPayload model =
    Json.Encode.object
        [ ( "username", Json.Encode.string model.username )
        , ( "password", Json.Encode.string model.password )
        ]
        |> Http.jsonBody


authRequest : Route -> String -> AuthParams -> Cmd SessionMessage
authRequest route url model =
    let
        body =
            authPayload model

        request =
            cookiePost url body (Json.Decode.succeed 0)

        handleResponse res =
            case res of
                Ok a ->
                    LoggedIn route

                Err error ->
                    LoggedOutNoRedirect
    in
        Http.send handleResponse request
