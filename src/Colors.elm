module Colors exposing (white, lightGrey, grey, darkGrey, lightGray, gray, darkGray, black)

{-| Defines the colors used in this application

@docs white
@docs lightGrey
@docs grey
@docs darkGrey
@docs lightGray
@docs gray
@docs darkGray
@docs black

-}

import Css exposing (Color, rgb)


{-| white
-}
white : Color
white =
    rgb 255 255 255


{-| lightGrey
-}
lightGrey : Color
lightGrey =
    rgb 243 243 243


{-| grey
-}
grey : Color
grey =
    rgb 229 229 229


{-| darkGray
-}
darkGrey : Color
darkGrey =
    rgb 184 184 184


{-| lightGray
-}
lightGray : Color
lightGray =
    rgb 112 112 112


{-| gray
-}
gray : Color
gray =
    rgb 84 84 84


{-| darkGray
-}
darkGray : Color
darkGray =
    rgb 42 42 42


{-| black
-}
black : Color
black =
    rgb 0 0 0
