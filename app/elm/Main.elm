module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import List.Extra exposing (..)


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
      , selectedVCSType = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- Model


type alias Model =
    { flags : Flags
    , selectedVCSType : Maybe String
    }


type alias Flags =
    { brokers : List Broker }


type alias Broker =
    { vcsType : String, vcsName : String, brokerPort : Int }



-- Update


type Msg
    = VCSSelect (Maybe String)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        VCSSelect vcsType ->
            ( { model | selectedVCSType = vcsType }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [ class "title" ]
            [ text "The Demo Application for Repositories Brokers" ]
        , div [ class "repository-register-form" ]
            [ Html.span [ class "attribute-title" ] [ text "VCS Type: " ]
            , Html.select [] <|
                (option [ onClick <| VCSSelect Nothing ] [ text "" ])
                    :: (List.map
                            (\broker ->
                                option [ onClick <| VCSSelect (Just broker.vcsType) ]
                                    [ text broker.vcsName ]
                            )
                            model.flags.brokers
                       )
            ]
        , div [ class "request-result" ]
            [ div []
                [ Html.span [ class "attribute-title" ] [ text "VCS Type: " ]
                , case model.selectedVCSType of
                    Nothing ->
                        Html.span [ class "empty-attribute" ] [ text "Not selected" ]

                    Just vcsType ->
                        model.flags.brokers
                            |> List.Extra.find (\broker -> broker.vcsType == vcsType)
                            |> Maybe.map (text << .vcsName)
                            |> Maybe.withDefault (text "Unknown")
                ]
            ]
        ]
