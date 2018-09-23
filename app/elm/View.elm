module View exposing (buildBranchOverviewDataView, buildBranchesListDataView, buildCommitOverviewDataView, buildCommitsListDataView, buildFilesOverviewsDataView, buildRepositoriesListDataView, buildRepositoryOverviewDataView, buildRequestForm, secret, view)

-- Demo Application Modules

import API
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import List.Extra exposing (..)
import Model exposing (..)
import Msg exposing (..)
import String.Extra exposing (..)
import Time
import Utils exposing (..)


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


view : Model.Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ div []
            [ div [ class "title" ]
                [ text "The Demo Application for Repositories Brokers" ]
            , div [ class "repositories-requests" ]
                [ div [ class "repository-fields" ]
                    [ div []
                        [ Html.span [ class "attribute-title" ] [ text "VCS Type: " ]
                        , Html.select [ onInput VCSSelect ] <|
                            option [ Html.Attributes.value "" ] [ text "" ]
                                :: List.map
                                    (\broker ->
                                        option [ Html.Attributes.value broker.vcsType ]
                                            [ text broker.vcsName ]
                                    )
                                    model.flags.brokers
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
                , div [] <|
                    (let
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
                    )
                        :: (let
                                requests =
                                    [ API.RepositoriesListRequest
                                        { vcsType = model.selectedVCSType }
                                    , API.RepositoryOverviewRequest
                                        { vcsType = model.selectedVCSType
                                        , repositoryId = model.repositoryId
                                        }
                                    , API.BranchesListRequest
                                        { vcsType = model.selectedVCSType
                                        , repositoryId = model.repositoryId
                                        }
                                    , API.BranchOverviewRequest
                                        { vcsType = model.selectedVCSType
                                        , repositoryId = model.repositoryId
                                        , branchId = model.branchId
                                        }
                                    , API.CommitsListRequest
                                        { vcsType = model.selectedVCSType
                                        , repositoryId = model.repositoryId
                                        }
                                    , API.CommitOverviewRequest
                                        { vcsType = model.selectedVCSType
                                        , repositoryId = model.repositoryId
                                        , commitId = model.commitId
                                        }
                                    ]
                            in
                            List.map
                                (\request ->
                                    buildRequestForm model
                                        request
                                        ("Request " ++ API.getRequestTitle request)
                                )
                                requests
                           )
                ]
            , div []
                (List.map
                    (\result ->
                        let
                            styleClass =
                                "request-result "
                                    ++ (case info of
                                            Err _ ->
                                                "request-result-fail"

                                            Ok answer ->
                                                case answer.status of
                                                    "success" ->
                                                        "request-result-succeed"

                                                    "warning" ->
                                                        "request-result-warn"

                                                    _ ->
                                                        "request-result-fail"
                                       )

                            request =
                                case result of
                                    API.RequestResult r _ ->
                                        r

                            info =
                                case result of
                                    API.RequestResult _ i ->
                                        i

                            vcsType =
                                API.getVCSType request
                        in
                        div [ class styleClass ]
                            [ div [ class "request-title" ]
                                [ Html.span [] [ text <| API.getRequestTitle request ]
                                ]
                            , div []
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
                                (case info of
                                    Err err ->
                                        [ text err ]

                                    Ok answer ->
                                        [ div []
                                            [ Html.span [ class "attribute-title" ] [ text "Status: " ]
                                            , text answer.status
                                            ]
                                        , div []
                                            [ Html.span [ class "attribute-title" ] [ text "Reason: " ]
                                            , text answer.reason
                                            ]
                                        , case answer.data of
                                            Nothing ->
                                                text ""

                                            Just data ->
                                                div []
                                                    [ Html.span [ class "attribute-title" ] [ text "Data: " ]
                                                    , case data of
                                                        API.RepositoriesListData repositoriesList ->
                                                            buildRepositoriesListDataView repositoriesList

                                                        API.RepositoryOverviewData overview ->
                                                            buildRepositoryOverviewDataView overview

                                                        API.BranchesListData branchesList ->
                                                            buildBranchesListDataView branchesList

                                                        API.BranchOverviewData overview ->
                                                            buildBranchOverviewDataView overview

                                                        API.CommitsListData commitsList ->
                                                            buildCommitsListDataView commitsList

                                                        API.CommitOverviewData overview ->
                                                            buildCommitOverviewDataView overview

                                                        API.SimpleData s ->
                                                            text s
                                                    ]
                                        ]
                                )
                            ]
                    )
                    model.requestsResults
                )
            ]
        ]
    }


buildBranchesListDataView : List String -> Html Msg
buildBranchesListDataView branchesList =
    ul [ class "request-result-data" ] <|
        List.map
            (\branchId ->
                li []
                    [ text branchId
                    ]
            )
            branchesList


toEnglishMonth month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


makeDate dt =
    (String.fromInt <| Time.toYear Time.utc dt)
        ++ " "
        ++ (toEnglishMonth <| Time.toMonth Time.utc dt)
        ++ " "
        ++ (String.fromInt <| Time.toDay Time.utc dt)


buildBranchOverviewDataView : API.BranchOverviewFields -> Html Msg
buildBranchOverviewDataView overview =
    ul [ class "request-result-data" ]
        [ li []
            [ Html.span [ class "attribute-title" ] [ text "Name: " ]
            , text overview.name
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Created At: " ]
            , let
                dt =
                    Time.millisToPosix overview.created_at
              in
              text <| makeDate dt
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Initial Commit: " ]
            , text overview.initial_commit
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Author: " ]
            , text <| Maybe.withDefault "<null>" overview.author
            ]
        ]


buildCommitsListDataView : List String -> Html Msg
buildCommitsListDataView commitsList =
    ul [ class "request-result-data" ] <|
        List.map
            (\commitId ->
                li []
                    [ text commitId
                    ]
            )
            commitsList


buildFilesOverviewsDataView : List API.FileOverviewInCommitFields -> Html Msg
buildFilesOverviewsDataView filesList =
    div [] <|
        List.map
            (\overview ->
                ul [ class "request-result-data" ]
                    [ li []
                        [ Html.span [ class "attribute-title" ] [ text "Committed At: " ]
                        , text <| String.fromInt overview.size
                        ]
                    , li []
                        [ Html.span [ class "attribute-title" ] [ text "Flag: " ]
                        , text overview.flag
                        ]
                    , li []
                        [ Html.span [ class "attribute-title" ] [ text "Negative Delta: " ]
                        , text <| String.fromInt overview.negativeDelta
                        ]
                    , li []
                        [ Html.span [ class "attribute-title" ] [ text "Positive Delta: " ]
                        , text <| String.fromInt overview.positiveDelta
                        ]
                    , li []
                        [ Html.span [ class "attribute-title" ] [ text "Path: " ]
                        , text overview.path
                        ]
                    ]
            )
            filesList


buildCommitOverviewDataView : API.CommitOverviewFields -> Html Msg
buildCommitOverviewDataView overview =
    ul [ class "request-result-data" ]
        [ li []
            [ Html.span [ class "attribute-title" ] [ text "Committed At: " ]
            , let
                dt =
                    Time.millisToPosix overview.committed_at
              in
              text <| makeDate dt
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Message: " ]
            , text overview.message
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Committer: " ]
            , text overview.committer
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Negative Delta: " ]
            , text <| String.fromInt overview.negativeDelta
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Positive Delta: " ]
            , text <| String.fromInt overview.positiveDelta
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Branch: " ]
            , text overview.branch
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Files Overview: " ]
            , buildFilesOverviewsDataView overview.files
            ]
        ]


buildRepositoriesListDataView : List String -> Html Msg
buildRepositoriesListDataView repositoriesList =
    ul [ class "request-result-data" ] <|
        List.map
            (\repositoryId ->
                li []
                    [ text repositoryId
                    ]
            )
            repositoriesList


buildRepositoryOverviewDataView : API.RepositoryOverviewFields -> Html Msg
buildRepositoryOverviewDataView overview =
    ul [ class "request-result-data" ]
        [ li []
            [ Html.span [ class "attribute-title" ] [ text "Last Synchronization Date: " ]
            , let
                dt =
                    Time.millisToPosix overview.last_sync_date
              in
              text <| makeDate dt
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Repository Type: " ]
            , text overview.repo_type
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Repository URL: " ]
            , text overview.url
            ]
        , li []
            [ Html.span [ class "attribute-title" ] [ text "Size: " ]
            , text <| String.fromInt overview.size
            ]
        ]
