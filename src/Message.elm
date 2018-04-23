module Message exposing (Msg(..), PageMessage(..), LayoutMessage(..), GalleryMessage(..))

{-| Define the Msg type and related methods

@docs Msg

-}

import ImageFile exposing (ImageFile)
import Route exposing (Route)


{-| The main Msg type
-}
type Msg
    = Render Route
    | Load Route
    | Page PageMessage


{-| The Msg type for the pages
-}
type PageMessage
    = LayoutMsg LayoutMessage
    | GalleryMsg GalleryMessage


{-| The Msg type for the main layout
-}
type LayoutMessage
    = ShowNav Bool


{-| The Msg type for the Image gallery
-}
type GalleryMessage
    = ViewImage ImageFile
    | HideImage
