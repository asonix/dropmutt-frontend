module Page.Galleries.Gallery.RemoteImage exposing (..)

import Json.Decode exposing (Decoder)


type alias RemoteImage =
    { id : Int
    , files : List RemoteFile
    , description : String
    , alternateText : String
    }


smallImage : RemoteImage -> String
smallImage remoteImage =
    remoteImage.files
        |> List.filter (\file -> file.width == 200 || file.height == 200)
        |> List.head
        |> Maybe.map asUrl
        |> Maybe.withDefault ""


largeOrFull : RemoteImage -> RemoteFile
largeOrFull remoteImage =
    let
        large =
            List.filter (\file -> file.width == 1200 || file.height == 1200) remoteImage.files
                |> List.head
    in
        case large of
            Just l ->
                l

            Nothing ->
                remoteImage.files
                    |> List.foldr
                        (\file1 ->
                            \file2 ->
                                if file1.width > file2.width then
                                    file1
                                else
                                    file2
                        )
                        { path = ""
                        , width = 0
                        , height = 0
                        }


alternateText : RemoteImage -> String
alternateText remoteImage =
    remoteImage.alternateText


description : RemoteImage -> String
description remoteImage =
    remoteImage.description


decodeRemoteImage : Decoder RemoteImage
decodeRemoteImage =
    Json.Decode.map4 RemoteImage
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "files" <| Json.Decode.list decodeRemoteFile)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "alternate_text" Json.Decode.string)


type alias RemoteFile =
    { path : String
    , width : Int
    , height : Int
    }


asUrl : RemoteFile -> String
asUrl extension =
    "http://localhost:8080/" ++ extension.path


decodeRemoteFile : Decoder RemoteFile
decodeRemoteFile =
    Json.Decode.map3 RemoteFile (Json.Decode.field "path" Json.Decode.string) (Json.Decode.field "width" Json.Decode.int) (Json.Decode.field "height" Json.Decode.int)
