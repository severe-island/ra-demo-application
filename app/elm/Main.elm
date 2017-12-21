module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Http
import List.Extra exposing (..)
import String.Extra exposing (..)
import Task


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


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



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
    { vcsType : String, vcsName : String, brokerPort : Int }


type alias RequestResult =
    { vcsType : String, requestType : String, url : String, info : RequestInfo }


type RequestInfo
    = RegisterRepositoryResult { status : String, reason : String }
    | RequestFailResult { message : String }
    | RequestRepositoriesListSucceedResult { status : String, reason : String, data : List String }
    | UnknownResponse { message : String, response : String }



-- Update


type Msg
    = BranchIdInput String
    | CommitIdInput String
    | LoginInput String
    | PasswordInput String
    | RegisterRepository
    | RepositoryIdInput String
    | RequestBranchesList
    | RequestBranchOverview
    | RequestCommitsList
    | RequestCommitOverview
    | RequestRepositoriesList { vcsType : String, url : String }
    | RequestRepositoriesListFail { vcsType : String, requestType : String, url : String, error : Http.Error }
    | RequestRepositoriesListSucceed { vcsType : String, requestType : String, url : String, message : String }
    | RequestRepositoryOverview
    | URLInput String
    | VCSSelect String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        RegisterRepository ->
            ( model, Cmd.none )

        RequestBranchesList ->
            ( model, Cmd.none )

        RequestBranchOverview ->
            ( model, Cmd.none )

        RequestCommitsList ->
            ( model, Cmd.none )

        RequestCommitOverview ->
            ( model, Cmd.none )

        RequestRepositoriesList params ->
            ( model
            , Task.attempt
                (\result ->
                    case result of
                        Err err ->
                            RequestRepositoriesListFail
                                { vcsType = params.vcsType
                                , requestType = "GET"
                                , url = params.url
                                , error = err
                                }

                        Ok msg ->
                            RequestRepositoriesListSucceed
                                { vcsType = params.vcsType
                                , requestType = "GET"
                                , url = params.url
                                , message = msg
                                }
                )
                (Http.toTask <| Http.getString <| buildRepositoriesListRequestURL model)
            )

        RequestRepositoriesListFail params ->
            ( { model
                | requestsResults =
                    { vcsType = params.vcsType
                    , requestType = params.requestType
                    , url = params.url
                    , info = RequestFailResult { message = toHumanReadable params.error }
                    }
                        :: model.requestsResults
              }
            , Cmd.none
            )

        RequestRepositoriesListSucceed msg ->
            ( model, Cmd.none )

        RequestRepositoryOverview ->
            ( model, Cmd.none )

        URLInput url ->
            ( { model | repositoryURL = url }, Cmd.none )

        VCSSelect vcsType ->
            ( { model | selectedVCSType = vcsType }, Cmd.none )


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


buildRepositoriesListRequestURL : Model -> String
buildRepositoriesListRequestURL model =
    "http://localhost:"
        ++ (getVCSPort model)
        ++ "/repositories"


getVCSPort : Model -> String
getVCSPort model =
    model.flags.brokers
        |> List.Extra.find (\broker -> broker.vcsType == model.selectedVCSType)
        |> Maybe.map (toString << .brokerPort)
        |> Maybe.withDefault "_"



-- View


secret : String -> String
secret str =
    String.repeat (String.length str) "*"


view : Model -> Html Msg
view model =
    div []
        [ div [ class "title" ]
            [ text "The Demo Application for Repositories Brokers" ]
        , div [ class "repositories-requests" ]
            [ div [ class "repository-fields" ]
                [ div []
                    [ Html.span [ class "attribute-title" ] [ text "VCS Type: " ]
                    , Html.select [] <|
                        (option [ onClick <| VCSSelect "" ] [ text "" ])
                            :: (List.map
                                    (\broker ->
                                        option [ onClick <| VCSSelect broker.vcsType ]
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
            , div [ class "request-form" ]
                [ div []
                    [ Html.span [ class "attribute-title" ] [ text "Request: " ]
                    , text
                        ("POST http://localhost:"
                            ++ (getVCSPort model)
                            ++ "/repositories"
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
                    [ button [ onClick <| RegisterRepository ] [ text "Register Repository" ]
                    ]
                ]
            , let
                url =
                    buildRepositoriesListRequestURL model
              in
                div [ class "request-form" ]
                    [ div []
                        [ Html.span [ class "attribute-title" ] [ text "Request: " ]
                        , text
                            ("GET " ++ url)
                        ]
                    , div []
                        [ button [ onClick <| RequestRepositoriesList { vcsType = model.selectedVCSType, url = url } ]
                            [ text "Request Repositories List" ]
                        ]
                    ]
            , div [ class "request-form" ]
                [ div []
                    [ Html.span [ class "attribute-title" ] [ text "Request: " ]
                    , text
                        ("GET http://localhost:"
                            ++ (getVCSPort model)
                            ++ "/repositories"
                            ++ "/"
                            ++ (model.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                        )
                    ]
                , div []
                    [ button [ onClick <| RequestRepositoryOverview ] [ text "Request Repository Overview" ]
                    ]
                ]
            , div [ class "request-form" ]
                [ div []
                    [ Html.span [ class "attribute-title" ] [ text "Request: " ]
                    , text
                        ("GET http://localhost:"
                            ++ (getVCSPort model)
                            ++ "/repositories"
                            ++ "/"
                            ++ (model.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                            ++ "/branches"
                        )
                    ]
                , div []
                    [ button [ onClick <| RequestBranchesList ] [ text "Request Branches List" ]
                    ]
                ]
            , div [ class "request-form" ]
                [ div []
                    [ Html.span [ class "attribute-title" ] [ text "Request: " ]
                    , text
                        ("GET http://localhost:"
                            ++ (getVCSPort model)
                            ++ "/repositories"
                            ++ "/"
                            ++ (model.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                            ++ "/branches"
                            ++ "/"
                            ++ (model.branchId |> nonEmpty |> Maybe.withDefault "_")
                        )
                    ]
                , div []
                    [ button [ onClick <| RequestBranchOverview ] [ text "Request Branch Overview" ]
                    ]
                ]
            , div [ class "request-form" ]
                [ div []
                    [ Html.span [ class "attribute-title" ] [ text "Request: " ]
                    , text
                        ("GET http://localhost:"
                            ++ (getVCSPort model)
                            ++ "/repositories"
                            ++ "/"
                            ++ (model.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                            ++ "/commits"
                        )
                    ]
                , div []
                    [ button [ onClick <| RequestCommitsList ] [ text "Request Commits List" ]
                    ]
                ]
            , div [ class "request-form" ]
                [ div []
                    [ Html.span [ class "attribute-title" ] [ text "Request: " ]
                    , text
                        ("GET http://localhost:"
                            ++ (getVCSPort model)
                            ++ "/repositories"
                            ++ "/"
                            ++ (model.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                            ++ "/commits"
                            ++ "/"
                            ++ (model.commitId |> nonEmpty |> Maybe.withDefault "_")
                        )
                    ]
                , div []
                    [ button [ onClick <| RequestCommitOverview ] [ text "Request Commit Overview" ]
                    ]
                ]
            ]
        , div []
            (List.map
                (\result ->
                    div [ class "request-result" ]
                        [ div []
                            [ Html.span [ class "attribute-title" ] [ text "VCS Type: " ]
                            , if String.isEmpty result.vcsType then
                                Html.span [ class "empty-attribute" ] [ text "Not selected" ]
                              else
                                model.flags.brokers
                                    |> List.Extra.find (\broker -> broker.vcsType == result.vcsType)
                                    |> Maybe.map (text << .vcsName)
                                    |> Maybe.withDefault (text "Unknown")
                            ]
                        , div []
                            [ Html.span [ class "attribute-title" ] [ text "Request Type: " ]
                            , text result.requestType
                            ]
                        , div []
                            [ Html.span [ class "attribute-title" ] [ text "Request URL: " ]
                            , text result.url
                            ]
                        , div []
                            [ case result.info of
                                RegisterRepositoryResult _ ->
                                    text "Not yet implemented."

                                RequestRepositoriesListSucceedResult _ ->
                                    text "Not yet implemented."

                                RequestFailResult info ->
                                    text info.message

                                UnknownResponse _ ->
                                    text "Not yet implemented."
                            ]
                        ]
                )
                model.requestsResults
            )
        ]
