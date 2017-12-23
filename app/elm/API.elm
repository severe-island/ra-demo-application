module API exposing (..)

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
