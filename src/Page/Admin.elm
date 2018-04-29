module Page.Admin exposing (AdminModel, init, view, update)

{-| Defines the admin page

    @docs AdminModel
    @docs init

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (accept, action, css, for, id, method, multiple, name, type_, value)
import Html.Styled.Events exposing (on, onSubmit)
import Json.Decode
import Auth exposing (Method(..), apiEndpoint, methodToString)
import Message exposing (Msg(..), PageMessage(..), AdminMessage(..))
import Ports exposing (performUpload, imagesSelected)


{-| The state for the Admin Page
-}
type alias AdminModel =
    { tmp : String
    , uploadState : UploadState
    }


type UploadState
    = NotStarted
    | Started
    | Percentage Int
    | Failed
    | Succeeded


{-| Initial state for the admin page
-}
init : AdminModel
init =
    { tmp = "Page Admin"
    , uploadState = NotStarted
    }


uploadUrl : String
uploadUrl =
    apiEndpoint ++ "/upload"


fileInputId : String
fileInputId =
    "file-upload"


{-| Rendering the Admin Page
-}
view : AdminModel -> Html Msg
view model =
    section []
        [ article []
            (case model.uploadState of
                NotStarted ->
                    [ div []
                        [ h2 [] [ text "Upload file" ] ]
                    , form
                        [ formOnSubmit ]
                        [ div []
                            [ label
                                [ for "image"
                                , css [ display block ]
                                ]
                                [ text "Select file" ]
                            , input
                                [ type_ "file"
                                , name "image"
                                , accept "image/*"
                                , multiple True
                                , id fileInputId
                                , on "change" (Json.Decode.succeed <| Page <| AdminMsg <| ImagesSelected)
                                ]
                                []
                            ]
                        , div []
                            [ label
                                [ for "submit"
                                , css [ display block ]
                                ]
                                [ text "Submit" ]
                            , input
                                [ type_ "submit"
                                , name "submit"
                                , value "Submit"
                                , formOnSubmit
                                ]
                                []
                            ]
                        ]
                    ]

                Started ->
                    [ h2 [] [ text "Uploading..." ] ]

                Percentage percentage ->
                    [ h2 [] [ text ("Uploading... " ++ (toString percentage)) ] ]

                Failed ->
                    [ h2 [] [ text "Upload failed." ] ]

                Succeeded ->
                    [ h2 [] [ text "Upload succeeded!" ] ]
            )
        ]


formOnSubmit : Attribute Msg
formOnSubmit =
    onSubmit <| Page <| AdminMsg <| PerformUpload


update : AdminMessage -> AdminModel -> ( AdminModel, Cmd Msg )
update msg model =
    case msg of
        ImagesSelected ->
            ( model, imagesSelected fileInputId )

        PerformUpload ->
            ( { model | uploadState = Started }, performUpload uploadUrl )

        UploadPercentage percentage ->
            ( { model | uploadState = Percentage percentage }, Cmd.none )

        UploadFailed a ->
            ( { model | uploadState = Failed }, Cmd.none )

        UploadSucceeded a ->
            ( { model | uploadState = Succeeded }, Cmd.none )
