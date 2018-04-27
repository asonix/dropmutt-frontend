port module Ports exposing (..)

import Message exposing (Msg(..))


port imagesSelected : String -> Cmd msg


port uploadPercentage : (Int -> msg) -> Sub msg


port uploadFailed : (Int -> msg) -> Sub msg


port uploadSucceeded : (Int -> msg) -> Sub msg


port performUpload : String -> Cmd msg
