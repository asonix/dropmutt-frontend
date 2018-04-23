module ImageFile exposing (..)


type ImageFile
    = ImageFile
        { path : String
        , text : String
        }


baseUrl : String
baseUrl =
    "http://localhost:8080"


fullLink : ImageFile -> String
fullLink imageFile =
    case imageFile of
        ImageFile { path } ->
            baseUrl ++ "/static/full/" ++ path


previewLink : ImageFile -> String
previewLink imageFile =
    case imageFile of
        ImageFile { path } ->
            baseUrl ++ "/static/preview/" ++ path


imageText : ImageFile -> String
imageText imageFile =
    case imageFile of
        ImageFile { text } ->
            text
