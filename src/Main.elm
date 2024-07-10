module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Browser.Navigation
import Html exposing (Html, div, h3, input, node, pre, span, text, textarea)
import Html.Attributes exposing (class, href, property, readonly, rel, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Url
import Url.Builder


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Alert =
    { color : String
    , message : String
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , alert : Maybe Alert
    , prefixUrl : Url.Url
    , columns : Array Column
    }


type alias Column =
    { name : String
    , rows : Array String
    }


type alias Flags =
    {}


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | SetPrefixUrl String
    | AddColumn
    | SetName Int String
    | SetRows Int String


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ baseUrl navKey =
    ( { navKey = navKey
      , alert = Nothing
      , prefixUrl = baseUrl
      , columns =
            Array.fromList
                [ { name = "Your name", rows = Array.fromList [ "John Doe", "Mary Jane" ] }
                ]
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    Browser.Document "App"
        [ node "link"
            [ rel "stylesheet"
            , href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css"
            ]
            []
        , div
            [ class "flex bg-gray-200 min-h-screen" ]
            [ div
                [ class "md:w-3/5 my-8 ml-auto mr-auto bg-white min-h-full shadow" ]
                [ div [ class "md:p-4" ]
                    [ viewMaybe viewAlert model.alert
                    , div [ class "mb-5" ]
                        [ input
                            [ class "border border-gray-300 p-2 w-full"
                            , type_ "text"
                            , onInput SetPrefixUrl
                            , property "defaultValue" (Json.Encode.string (Url.toString model.prefixUrl))
                            ]
                            []
                        ]
                    , div [ class "mb-5" ]
                        [ div [ class "flex" ]
                            [ div [ class "w-1/2" ]
                                [ text "Name"
                                ]
                            , div [ class "w-1/2" ]
                                [ text "Lines of values"
                                ]
                            ]
                        ]
                    , div
                        []
                        (Array.indexedMap
                            (\index column ->
                                div [ class "flex" ]
                                    [ div [ class "w-1/2" ]
                                        [ input
                                            [ class "border border-gray-300 p-2 w-full"
                                            , type_ "text"
                                            , onInput (SetName index)
                                            , property "defaultValue" (Json.Encode.string column.name)
                                            ]
                                            []
                                        ]
                                    , div [ class "w-1/2" ]
                                        [ textarea
                                            [ class "border border-gray-300 p-2 w-full"
                                            , onInput (SetRows index)
                                            , property "defaultValue" (Json.Encode.string (String.join "\n" (Array.toList column.rows)))
                                            ]
                                            []
                                        ]
                                    ]
                            )
                            model.columns
                            |> Array.toList
                        )
                    , div [ class "mb-5" ]
                        [ input
                            [ class "border border-gray-300 p-2 w-full hover:bg-blue-500 hover:text-white pointer"
                            , type_ "button"
                            , value "Add"
                            , onClick AddColumn
                            ]
                            []
                        ]
                    , h3 [] [ text "Output URLs" ]
                    , case Array.toList model.columns of
                        [] ->
                            text ""

                        (first :: _) as columns ->
                            div [ class "mb-5" ]
                                [ textarea
                                    [ class "border border-gray-300 p-2 w-full"
                                    , readonly True
                                    ]
                                    [ columnsAddQueryStrings model.prefixUrl first (List.filter (\{ rows } -> Array.length rows > 0) columns)
                                        |> Array.toList
                                        |> List.map Url.toString
                                        |> String.join "\n"
                                        |> text
                                    ]
                                ]

                    -- , pre [ class "whitespace-pre-wrap" ] [ text (Debug.toString model) ]
                    ]
                ]
            ]
        ]


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe f maybeValue =
    case maybeValue of
        Just a ->
            f a

        Nothing ->
            text ""


viewAlert : Alert -> Html Msg
viewAlert alert =
    div [ class "mb-5" ]
        [ div [ class ("bg-" ++ alert.color ++ "-100 border border-" ++ alert.color ++ "-400 text-" ++ alert.color ++ "-700 px-4 py-3 md:rounded relative") ]
            [ span [ class "block sm:inline" ] [ text alert.message ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of
        -- [url] decide what to do
        OnUrlRequest (Browser.Internal urlUrl) ->
            ( model, Browser.Navigation.pushUrl model.navKey (Url.toString urlUrl) )

        OnUrlRequest (Browser.External urlString) ->
            ( model, Browser.Navigation.load urlString )

        -- [url] given that we _are at this url_ how should our model change?
        OnUrlChange _ ->
            ( model, Cmd.none )

        SetPrefixUrl url ->
            case Url.fromString url of
                Just prefixUrl ->
                    ( { model
                        | alert = Nothing
                        , prefixUrl = prefixUrl
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | alert = Just { color = "red", message = "Invalid url" }
                      }
                    , Cmd.none
                    )

        AddColumn ->
            ( { model
                | alert = Nothing
                , columns = Array.push { name = "Column " ++ String.fromInt (Array.length model.columns), rows = Array.empty } model.columns
              }
            , Cmd.none
            )

        SetName index name ->
            case Array.get index model.columns of
                Just column ->
                    ( { model
                        | alert = Nothing
                        , columns = Array.set index { column | name = name } model.columns
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | alert = Just { color = "red", message = "Invalid column" }
                      }
                    , Cmd.none
                    )

        SetRows index rows ->
            case Array.get index model.columns of
                Just column ->
                    ( { model
                        | alert = Nothing
                        , columns = Array.set index { column | rows = Array.fromList (String.split "\n" rows) } model.columns
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | alert = Just { color = "red", message = "Invalid column" }
                      }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--


columnsAddQueryStrings : Url.Url -> Column -> List Column -> Array Url.Url
columnsAddQueryStrings prefixUrl firstColumn columns =
    firstColumn.rows
        |> Array.indexedMap (rowToQueryParameters Array.empty columns)
        |> Array.map
            (\params ->
                { prefixUrl
                    | query =
                        Just (String.dropLeft 1 (Url.Builder.toQuery (Array.toList params)))
                }
            )


rowToQueryParameters : Array Url.Builder.QueryParameter -> List Column -> Int -> String -> Array Url.Builder.QueryParameter
rowToQueryParameters params columns index row =
    case columns of
        [] ->
            params

        column :: rest ->
            let
                value =
                    Array.get index column.rows
                        |> Maybe.withDefault ""

                newParams =
                    Array.push (Url.Builder.string column.name value) params
            in
            rowToQueryParameters newParams rest index row
