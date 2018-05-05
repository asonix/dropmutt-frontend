module Page.Galleries.Gallery.Entry exposing (EntryModel)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src, title)
import Html.Styled.Events exposing (onWithOptions)


type alias EntryModel =
    { tmp : String
    }


init : ( EntryModel, Cmd Msg )
init =
    ( { tmp = "tmp"
      }
    , Cmd.none
    )
