module Message exposing (Msg(..), AuthMessage(..), PageMessage(..), LayoutMessage(..), GalleryMessage(..))

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
    | AuthMsg AuthMessage


{-| The Msg type for authentication
-}
type AuthMessage
    = Login
    | Signup
    | Logout
    | Username String
    | Password String
    | Authenticated
    | NotAuthenticated
    | SwitchAuth


{-| The Msg type for the main layout
-}
type LayoutMessage
    = ShowNav Bool


{-| The Msg type for the Image gallery
-}
type GalleryMessage
    = ViewImage ImageFile
    | HideImage
