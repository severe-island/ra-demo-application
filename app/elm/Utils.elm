module Utils exposing (..)

import Http
import List.Extra


-- Demo Application Modules

import API
import Model exposing (..)


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
