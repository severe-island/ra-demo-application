module Model exposing (Broker, Flags, Model)

import API


type alias Model =
    { flags : Flags
    , branchId : String
    , commitId : String
    , errorMessage : String
    , login : String
    , password : String
    , repositoryId : String
    , repositoryURL : String
    , requestsResults : List API.RequestResult
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
