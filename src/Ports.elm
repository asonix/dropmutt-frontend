port module Ports exposing (..)

import Message exposing (Msg(..))


port uploadPercentage : (Int -> msg) -> Sub msg


port uploadFailed : (Int -> msg) -> Sub msg


port uploadSucceeded : (Int -> msg) -> Sub msg


port performUpload : ( String, String ) -> Cmd msg


port uploadProcessing : (Int -> msg) -> Sub msg
