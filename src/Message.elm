module Message exposing (Msg(..), AdminMessage(..), AuthMessage(..), PageMessage(..), LayoutMessage(..), GalleryMessage(..), SessionMessage(..))

{-| Define the Msg type and related methods

@docs Msg

-}

import Window exposing (Size)
import ImageFile exposing (ImageFile)
import RemoteImage exposing (RemoteImage)
import Route exposing (Route)


{-| The main Msg type
-}
type Msg
    = Render Route
    | Load Route
    | Page PageMessage
    | Session SessionMessage


type SessionMessage
    = LogoutMsg
    | LoginMsg
    | Resize Size
    | NoSize


{-| The Msg type for the pages
-}
type PageMessage
    = LayoutMsg LayoutMessage
    | GalleryMsg GalleryMessage
    | AuthMsg AuthMessage
    | AdminMsg AdminMessage


{-| The Msg type for authentication
-}
type AuthMessage
    = Login
    | Signup
    | Username String
    | Password String
    | Authenticated
    | NotAuthenticated
    | SwitchAuth


type AdminMessage
    = ImagesSelected
    | PerformUpload
    | UploadPercentage Int
    | UploadFailed Int
    | UploadSucceeded Int
    | UploadProcessing Int


{-| The Msg type for the main layout
-}
type LayoutMessage
    = ShowNav Bool


{-| The Msg type for the Image gallery
-}
type GalleryMessage
    = ViewImage RemoteImage
    | HideImage
    | Images (List RemoteImage)
    | NoImages
