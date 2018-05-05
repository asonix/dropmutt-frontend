module Page.Admin exposing (AdminModel, init, view, update)

{-| Defines the admin page

    @docs AdminModel
    @docs init

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (accept, action, css, for, id, method, name, type_, value)
import Html.Styled.Events exposing (on, onSubmit)
import Json.Decode
import Session.Auth exposing (Method(..), apiEndpoint)
import Message exposing (AdminMessage(..))
import Ports exposing (performUpload)


{-| The state for the Admin Page
-}
type alias AdminModel =
    { galleryName : String
    , uploadState : UploadState
    }


type UploadState
    = NotStarted
    | Started
    | Percentage Int
    | Failed
    | Succeeded
    | Processing


{-| Initial state for the admin page
-}
init : Maybe AdminModel -> AdminModel
init model =
    case model of
        Just model ->
            model

        Nothing ->
            { galleryName = "ref-sheets"
            , uploadState = NotStarted
            }


uploadUrl : String
uploadUrl =
    apiEndpoint ++ "/upload"


formId : String
formId =
    "upload-form"


{-| Rendering the Admin Page
-}
view : AdminModel -> Html AdminMessage
view model =
    section []
        [ article []
            (case model.uploadState of
                NotStarted ->
                    [ div []
                        [ h2 [] [ text "Upload file" ] ]
                    , form
                        [ formOnSubmit
                        , id formId
                        ]
                        [ div []
                            [ label
                                [ for "description"
                                , css [ display block ]
                                ]
                                [ text "Description" ]
                            , input
                                [ type_ "text"
                                , name "description"
                                ]
                                []
                            ]
                        , div []
                            [ label
                                [ for "alternate-text"
                                , css [ display block ]
                                ]
                                [ text "Alternate Text" ]
                            , input
                                [ type_ "text"
                                , name "alternate-text"
                                ]
                                []
                            ]
                        , div []
                            [ label
                                [ for "image"
                                , css [ display block ]
                                ]
                                [ text "Select file" ]
                            , input
                                [ type_ "file"
                                , name "file-upload"
                                , accept "image/*"
                                ]
                                []
                            ]
                        , input
                            [ type_ "hidden"
                            , name "gallery-name"
                            , value model.galleryName
                            ]
                            []
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

                Processing ->
                    [ h2 [] [ text "Wait for it..." ] ]

                Failed ->
                    [ h2 [] [ text "aWe fUcK :c saD FacE :c ;A;" ] ]

                Succeeded ->
                    [ h2 [] [ text "Fuck yeah!" ] ]
            )
        ]


formOnSubmit : Attribute AdminMessage
formOnSubmit =
    onSubmit PerformUpload


update : AdminMessage -> AdminModel -> ( AdminModel, Cmd AdminMessage )
update msg model =
    case msg of
        PerformUpload ->
            ( { model | uploadState = Started }, performUpload ( formId, uploadUrl ) )

        UploadPercentage percentage ->
            ( { model | uploadState = Percentage percentage }, Cmd.none )

        UploadFailed a ->
            ( { model | uploadState = Failed }, Cmd.none )

        UploadSucceeded a ->
            ( { model | uploadState = Succeeded }, Cmd.none )

        UploadProcessing a ->
            ( { model | uploadState = Processing }, Cmd.none )
