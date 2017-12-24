module Msg exposing (..)

import Http


-- Demo Application Modules

import API


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
