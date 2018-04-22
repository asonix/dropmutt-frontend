module Message exposing (Msg(..), PageMessage(..), LayoutMessage(..))

{-| Define the Msg type and related methods

@docs Msg

-}

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
    = Layout LayoutMessage


{-| The Msg type for the main layout
-}
type LayoutMessage
    = ShowNav Bool
