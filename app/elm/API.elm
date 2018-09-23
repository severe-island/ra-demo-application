module API exposing (Answer, BranchOverviewFields, CommitOverviewFields, FileOverviewInCommitFields, Method(..), RepositoryOverviewFields, Request(..), RequestResult(..), SuccessData(..), buildRequestURL, getRequestMethod, getRequestTitle, getVCSType, toString)

import String.Extra exposing (..)


type Method
    = GET
    | POST


type Request
    = RegisterRepositoryRequest
        { vcsType : String
        , url : String
        , login : String
        , password : String
        , repositoryId : String
        }
    | RepositoriesListRequest
        { vcsType : String
        }
    | RepositoryOverviewRequest
        { vcsType : String
        , repositoryId : String
        }
    | BranchesListRequest
        { vcsType : String
        , repositoryId : String
        }
    | BranchOverviewRequest
        { vcsType : String
        , repositoryId : String
        , branchId : String
        }
    | CommitsListRequest
        { vcsType : String
        , repositoryId : String
        }
    | CommitOverviewRequest
        { vcsType : String
        , repositoryId : String
        , commitId : String
        }


getRequestMethod : Request -> Method
getRequestMethod request =
    case request of
        RegisterRepositoryRequest _ ->
            POST

        RepositoriesListRequest _ ->
            GET

        RepositoryOverviewRequest _ ->
            GET

        BranchesListRequest _ ->
            GET

        BranchOverviewRequest _ ->
            GET

        CommitsListRequest _ ->
            GET

        CommitOverviewRequest _ ->
            GET


getVCSType : Request -> String
getVCSType request =
    case request of
        RegisterRepositoryRequest params ->
            params.vcsType

        RepositoriesListRequest params ->
            params.vcsType

        RepositoryOverviewRequest params ->
            params.vcsType

        BranchesListRequest params ->
            params.vcsType

        BranchOverviewRequest params ->
            params.vcsType

        CommitsListRequest params ->
            params.vcsType

        CommitOverviewRequest params ->
            params.vcsType


getRequestTitle : Request -> String
getRequestTitle request =
    case request of
        RegisterRepositoryRequest _ ->
            "Register Repository"

        RepositoriesListRequest _ ->
            "Repositories List"

        RepositoryOverviewRequest _ ->
            "Repository Overview"

        BranchesListRequest _ ->
            "Branches List"

        BranchOverviewRequest _ ->
            "Branch Overview"

        CommitsListRequest _ ->
            "Commits List"

        CommitOverviewRequest _ ->
            "Commit Overview"


buildRequestURL : Request -> String
buildRequestURL request =
    case request of
        RegisterRepositoryRequest params ->
            "/repositories"

        RepositoriesListRequest params ->
            "/repositories"

        RepositoryOverviewRequest params ->
            "/repositories"
                ++ "/"
                ++ (params.repositoryId |> nonEmpty |> Maybe.withDefault "_")

        BranchesListRequest params ->
            "/repositories"
                ++ "/"
                ++ (params.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                ++ "/branches"

        BranchOverviewRequest params ->
            "/repositories"
                ++ "/"
                ++ (params.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                ++ "/branches"
                ++ "/"
                ++ (params.branchId |> nonEmpty |> Maybe.withDefault "_")

        CommitsListRequest params ->
            "/repositories"
                ++ "/"
                ++ (params.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                ++ "/commits"

        CommitOverviewRequest params ->
            "/repositories"
                ++ "/"
                ++ (params.repositoryId |> nonEmpty |> Maybe.withDefault "_")
                ++ "/commits"
                ++ "/"
                ++ (params.commitId |> nonEmpty |> Maybe.withDefault "_")


toString : Method -> String
toString method =
    case method of
        GET ->
            "GET"

        POST ->
            "POST"


type alias Answer =
    { status : String, reason : String, data : Maybe SuccessData }


type alias BranchOverviewFields =
    { name : String
    , created_at : Int
    , initial_commit : String
    , author : Maybe String
    }


type alias FileOverviewInCommitFields =
    { size : Int
    , flag : String
    , negativeDelta : Int
    , positiveDelta : Int
    , path : String
    }


type alias CommitOverviewFields =
    { committed_at : Int
    , message : String
    , committer : String
    , negativeDelta : Int
    , positiveDelta : Int
    , branch : String
    , files : List FileOverviewInCommitFields
    }


type alias RepositoryOverviewFields =
    { last_sync_date : Int
    , repo_type : String
    , url : String
    , size : Int
    }


type SuccessData
    = SimpleData String
    | RepositoriesListData (List String)
    | RepositoryOverviewData RepositoryOverviewFields
    | BranchesListData (List String)
    | BranchOverviewData BranchOverviewFields
    | CommitsListData (List String)
    | CommitOverviewData CommitOverviewFields


type RequestResult
    = RequestResult Request (Result String Answer)
