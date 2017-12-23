module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import List.Extra exposing (..)
import String.Extra exposing (..)
import Task


-- Demo Application Modules

import API


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd msg )
init flags =
    ( { flags = flags
      , branchId = ""
      , commitId = ""
      , errorMessage = ""
      , login = ""
      , password = ""
      , repositoryId = ""
      , repositoryURL = ""
      , requestsResults = []
      , responseMessage = ""
      , selectedVCSType = ""
      }
    , Cmd.none
    )



-- Model


type alias Model =
    { flags : Flags
    , branchId : String
    , commitId : String
    , errorMessage : String
    , login : String
    , password : String
    , repositoryId : String
    , repositoryURL : String
    , requestsResults : List RequestResult
    , responseMessage : String
    , selectedVCSType : String
    }


type alias Flags =
    { brokers : List Broker }


type alias Broker =
    { vcsType : String
    , vcsName : String
    , protocol : String
    , host : String
    , brokerPort : Int
    }


type RequestResult
    = RequestResult API.Request (Result Http.Error String)



-- Update


type Msg
    = BranchIdInput String
    | CommitIdInput String
    | LoginInput String
    | PasswordInput String
    | RepositoryIdInput String
    | URLInput String
    | VCSSelect String
    | Request API.Request
    | RequestFail { request : API.Request, error : Http.Error }
    | RequestSucceed { request : API.Request, message : String }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Inputs
        BranchIdInput branchId ->
            ( { model | branchId = branchId }, Cmd.none )

        CommitIdInput commitId ->
            ( { model | commitId = commitId }, Cmd.none )

        LoginInput login ->
            ( { model | login = login }, Cmd.none )

        PasswordInput password ->
            ( { model | password = password }, Cmd.none )

        RepositoryIdInput repositoryId ->
            ( { model | repositoryId = repositoryId }, Cmd.none )

        URLInput url ->
            ( { model | repositoryURL = url }, Cmd.none )

        VCSSelect vcsType ->
            ( { model | selectedVCSType = vcsType }, Cmd.none )

        -- Requests
        RequestFail result ->
            ( { model
                | requestsResults =
                    (RequestResult result.request (Err result.error))
                        :: model.requestsResults
              }
            , Cmd.none
            )

        RequestSucceed result ->
            ( { model
                | requestsResults =
                    (RequestResult result.request (Ok result.message))
                        :: model.requestsResults
              }
            , Cmd.none
            )

        Request request ->
            case request of
                API.RegisterRepositoryRequest params ->
                    ( model
                    , Task.attempt
                        (\result ->
                            case result of
                                Err err ->
                                    RequestFail { request = request, error = err }

                                Ok msg ->
                                    RequestSucceed { request = request, message = msg }
                        )
                        ((Http.toTask <|
                            Http.post
                                (buildRequestURL
                                    (getBrokerConfig model.flags (API.getVCSType request))
                                    request
                                )
                                Http.emptyBody
                                string
                         )
                        )
                    )

                _ ->
                    ( model
                    , Task.attempt
                        (\result ->
                            case result of
                                Err err ->
                                    RequestFail { request = request, error = err }

                                Ok msg ->
                                    RequestSucceed { request = request, message = msg }
                        )
                        (Http.toTask <|
                            Http.getString <|
                                buildRequestURL
                                    (getBrokerConfig model.flags (API.getVCSType request))
                                    request
                        )
                    )



-- Utils


toHumanReadable : Http.Error -> String
toHumanReadable error =
    "HTTP request error "
        ++ case error of
            Http.BadUrl info ->
                "<BadUrl> " ++ info

            Http.Timeout ->
                "<Timeout>"

            Http.NetworkError ->
                "<NetworkError>"

            Http.BadStatus response ->
                "<BadStatus> "
                    ++ "["
                    ++ (response.status.code |> toString)
                    ++ "] "
                    ++ response.status.message

            Http.BadPayload info _ ->
                "<BadPayload> " ++ info


buildRequestURL : Maybe Broker -> API.Request -> String
buildRequestURL brokerConfig request =
    getBrokerProtocol brokerConfig
        ++ "://"
        ++ getBrokerHost brokerConfig
        ++ ":"
        ++ getBrokerPort brokerConfig
        ++ API.buildRequestURL request


getBrokerConfig : Flags -> String -> Maybe Broker
getBrokerConfig flags vcsType =
    flags.brokers
        |> List.Extra.find (\broker -> broker.vcsType == vcsType)


getBrokerHost : Maybe Broker -> String
getBrokerHost brokerConfig =
    brokerConfig
        |> Maybe.map .host
        |> Maybe.withDefault "_"


getBrokerPort : Maybe Broker -> String
getBrokerPort brokerConfig =
    brokerConfig
        |> Maybe.map (toString << .brokerPort)
        |> Maybe.withDefault "_"


getBrokerProtocol : Maybe Broker -> String
getBrokerProtocol brokerConfig =
    brokerConfig
        |> Maybe.map .protocol
        |> Maybe.withDefault "_"



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- View


secret : String -> String
secret str =
    String.repeat (String.length str) "*"


buildRequestForm : Model -> API.Request -> String -> Html Msg
buildRequestForm model request title =
    let
        vcsType =
            model.selectedVCSType

        url =
            buildRequestURL (getBrokerConfig model.flags vcsType) request
    in
        div [ class "request-form" ]
            [ div []
                [ Html.span [ class "attribute-title" ] [ text "Request: " ]
                , text
                    ("GET " ++ url)
                ]
            , div []
                [ button [ onClick <| Request request ] [ text title ]
                ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "title" ]
            [ text "The Demo Application for Repositories Brokers" ]
        , div [ class "repositories-requests" ]
            [ div [ class "repository-fields" ]
                [ div []
                    [ Html.span [ class "attribute-title" ] [ text "VCS Type: " ]
                    , Html.select [ onInput VCSSelect ] <|
                        (option [ Html.Attributes.value "" ] [ text "" ])
                            :: (List.map
                                    (\broker ->
                                        option [ Html.Attributes.value broker.vcsType ]
                                            [ text broker.vcsName ]
                                    )
                                    model.flags.brokers
                               )
                    ]
                , div []
                    [ Html.span [ class "attribute-title" ] [ text "Repository URL: " ]
                    , input [ onInput URLInput ] []
                    ]
                , div []
                    [ Html.span [ class "attribute-title" ] [ text "Repository Login: " ]
                    , input [ onInput LoginInput ] []
                    ]
                , div []
                    [ Html.span [ class "attribute-title" ] [ text "Repository Password: " ]
                    , input [ onInput PasswordInput ] []
                    ]
                , div []
                    [ Html.span [ class "attribute-title" ] [ text "Repository Id: " ]
                    , input [ onInput RepositoryIdInput ] []
                    ]
                , div []
                    [ Html.span [ class "attribute-title" ] [ text "Branch Id: " ]
                    , input [ onInput BranchIdInput ] []
                    ]
                , div []
                    [ Html.span [ class "attribute-title" ] [ text "Commit Id: " ]
                    , input [ onInput CommitIdInput ] []
                    ]
                ]
            , let
                vcsType =
                    model.selectedVCSType

                request =
                    API.RegisterRepositoryRequest
                        { vcsType = vcsType
                        , url = model.repositoryURL
                        , login = model.login
                        , password = model.password
                        , repositoryId = model.repositoryId
                        }
              in
                div [ class "request-form" ]
                    [ div []
                        [ Html.span [ class "attribute-title" ] [ text "Request: " ]
                        , text
                            ("POST "
                                ++ buildRequestURL (getBrokerConfig model.flags model.selectedVCSType) request
                                ++ "?url="
                                ++ (model.repositoryURL |> nonEmpty |> Maybe.withDefault "_")
                                ++ "&login="
                                ++ (model.login |> nonEmpty |> Maybe.withDefault "_")
                                ++ "&password="
                                ++ (model.password |> secret |> nonEmpty |> Maybe.withDefault "_")
                                ++ "&id="
                                ++ (model.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                            )
                        ]
                    , div []
                        [ button
                            [ onClick <| Request request ]
                            [ text "Register Repository" ]
                        ]
                    ]
            , buildRequestForm model
                (API.RepositoriesListRequest
                    { vcsType = model.selectedVCSType }
                )
                "Request Repositories List"
            , buildRequestForm model
                (API.RepositoryOverviewRequest
                    { vcsType = model.selectedVCSType
                    , repositoryId = model.repositoryId
                    }
                )
                "Request Repository Overview"
            , buildRequestForm model
                (API.BranchesListRequest
                    { vcsType = model.selectedVCSType
                    , repositoryId = model.repositoryId
                    }
                )
                "Request Branches List"
            , buildRequestForm model
                (API.BranchOverviewRequest
                    { vcsType = model.selectedVCSType
                    , repositoryId = model.repositoryId
                    , branchId = model.branchId
                    }
                )
                "Request Branch Overview"
            , buildRequestForm model
                (API.CommitsListRequest
                    { vcsType = model.selectedVCSType
                    , repositoryId = model.repositoryId
                    }
                )
                "Request Commits List"
            , buildRequestForm model
                (API.CommitOverviewRequest
                    { vcsType = model.selectedVCSType
                    , repositoryId = model.repositoryId
                    , commitId = model.commitId
                    }
                )
                "Request Commit Overview"
            ]
        , div []
            (List.map
                (\result ->
                    let
                        styleClass =
                            "request-result "
                                ++ case info of
                                    Err _ ->
                                        "request-result-fail"

                                    Ok _ ->
                                        "request-result-succeed"

                        request =
                            case result of
                                RequestResult request _ ->
                                    request

                        info =
                            case result of
                                RequestResult _ info ->
                                    info

                        vcsType =
                            API.getVCSType request
                    in
                        div [ class styleClass ]
                            [ div []
                                [ Html.span [ class "attribute-title" ] [ text "VCS Type: " ]
                                , if String.isEmpty vcsType then
                                    Html.span [ class "empty-attribute" ] [ text "Not selected" ]
                                  else
                                    model.flags.brokers
                                        |> List.Extra.find (\broker -> broker.vcsType == vcsType)
                                        |> Maybe.map (text << .vcsName)
                                        |> Maybe.withDefault (text "Unknown")
                                ]
                            , div []
                                [ Html.span [ class "attribute-title" ] [ text "Request Method: " ]
                                , text <| API.toString <| API.getRequestMethod request
                                ]
                            , div []
                                [ Html.span [ class "attribute-title" ] [ text "Request URL: " ]
                                , text <| buildRequestURL (getBrokerConfig model.flags (API.getVCSType request)) request
                                ]
                            , div []
                                [ text <|
                                    case info of
                                        Err err ->
                                            toHumanReadable err

                                        Ok message ->
                                            message
                                ]
                            ]
                )
                model.requestsResults
            )
        ]
