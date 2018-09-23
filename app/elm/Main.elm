module Main exposing (main)

-- Demo Application Modules

import API
import Browser
import Html exposing (..)
import Http
import Json.Decode exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Task
import Utils exposing (..)
import View exposing (..)


main : Program Flags Model Msg
main =
    Browser.document
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



-- Update


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
                    API.RequestResult result.request (Err <| toHumanReadable result.error)
                        :: model.requestsResults
              }
            , Cmd.none
            )

        RequestSucceed result ->
            let
                status =
                    decodeString (field "status" string) result.message

                reason =
                    decodeString (field "reason" string) result.message

                data =
                    case result.request of
                        API.RepositoriesListRequest _ ->
                            Result.map API.RepositoriesListData <|
                                decodeString
                                    (field "data" (list string))
                                    result.message

                        API.RepositoryOverviewRequest _ ->
                            Result.map API.RepositoryOverviewData <|
                                decodeString
                                    (field "data"
                                        (Json.Decode.map4 API.RepositoryOverviewFields
                                            (field "last_sync_date" int)
                                            (field "repo_type" string)
                                            (field "url" string)
                                            (field "size" int)
                                        )
                                    )
                                    result.message

                        API.BranchesListRequest _ ->
                            Result.map API.BranchesListData <|
                                decodeString
                                    (field "data" (list string))
                                    result.message

                        API.BranchOverviewRequest _ ->
                            Result.map API.BranchOverviewData <|
                                decodeString
                                    (field "data"
                                        (Json.Decode.map4 API.BranchOverviewFields
                                            (field "name" string)
                                            (field "created_at" int)
                                            (field "initial_commit" string)
                                            (field "author" (nullable string))
                                        )
                                    )
                                    result.message

                        API.CommitsListRequest _ ->
                            Result.map API.CommitsListData <|
                                decodeString
                                    (field "data" (list string))
                                    result.message

                        API.CommitOverviewRequest _ ->
                            Result.map API.CommitOverviewData <|
                                decodeString
                                    (field "data"
                                        (Json.Decode.map7 API.CommitOverviewFields
                                            (field "committed_at" int)
                                            (field "message" string)
                                            (field "committer" string)
                                            (field "negativeDelta" int)
                                            (field "positiveDelta" int)
                                            (field "branch" string)
                                            (field "files"
                                                (list
                                                    (Json.Decode.map5 API.FileOverviewInCommitFields
                                                        (field "size" int)
                                                        (field "flag" string)
                                                        (field "negativeDelta" int)
                                                        (field "positiveDelta" int)
                                                        (field "path" string)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                    result.message

                        _ ->
                            Ok <| API.SimpleData ""

                requestsAnswer =
                    Result.map3
                        (\statusValue reasonValue dataValue ->
                            { status = statusValue, reason = reasonValue, data = Just dataValue }
                        )
                        status
                        reason
                        data
                        |> Result.mapError Json.Decode.errorToString
            in
            ( { model
                | requestsResults =
                    API.RequestResult result.request requestsAnswer
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

                                Ok m ->
                                    RequestSucceed { request = request, message = m }
                        )
                        (Http.toTask <|
                            Http.post
                                (buildRequestURL
                                    (getBrokerConfig model.flags (API.getVCSType request))
                                    request
                                )
                                Http.emptyBody
                                string
                        )
                    )

                _ ->
                    ( model
                    , Task.attempt
                        (\result ->
                            case result of
                                Err err ->
                                    RequestFail { request = request, error = err }

                                Ok m ->
                                    RequestSucceed { request = request, message = m }
                        )
                        (Http.toTask <|
                            Http.getString <|
                                buildRequestURL
                                    (getBrokerConfig model.flags (API.getVCSType request))
                                    request
                        )
                    )



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
