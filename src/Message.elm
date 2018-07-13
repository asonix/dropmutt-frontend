module Message exposing (Msg(..), AuthPageMessage(..), PageMessage(..), LayoutMessage(..), GalleriesMessage(..), GalleryMessage(..), SessionMessage(..), HomeMessage(..), NotFoundMessage(..))

{-| Define the Msg type and related methods

@docs Msg

-}

import Window exposing (Size)
import Page.Galleries.Gallery.RemoteImage exposing (RemoteImage)
import Route exposing (Route)


{-| The main Msg type
-}
type Msg
    = Render Route
    | Load Route
    | Page PageMessage
    | Session SessionMessage


type SessionMessage
    = LogoutMsg Route
    | LoginMsg
    | Resize Size
    | NoSize
    | LoggedOut Route
    | LoggedOutNoRedirect
    | LoggedIn Route
    | LoggedInNoRedirect


{-| The Msg type for the pages
-}
type PageMessage
    = LayoutMsg LayoutMessage
    | GalleriesMsg GalleriesMessage
    | AuthMsg AuthPageMessage
    | HomeMsg HomeMessage
    | NotFoundMsg NotFoundMessage


{-| The Msg type for authentication
-}
type AuthPageMessage
    = Login
    | Signup
    | Username String
    | Password String
    | SwitchAuth


type HomeMessage
    = HomeMessage


type NotFoundMessage
    = NotFoundMessage


type AdminMessage
    = PerformUpload
    | UploadPercentage Int
    | UploadFailed Int
    | UploadSucceeded Int
    | UploadProcessing Int


{-| The Msg type for the main layout
-}
type LayoutMessage
    = ShowNav Bool


type GalleriesMessage
    = GalleryMsg GalleryMessage
    | Select String
    | Galleries (List String)
    | NoGalleries


{-| The Msg type for the Image gallery
-}
type GalleryMessage
    = ViewImage RemoteImage
    | HideImage
    | Images (List RemoteImage)
    | NoImages
