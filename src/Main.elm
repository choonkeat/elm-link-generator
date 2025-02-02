module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view)

import AppUrl
import Array exposing (Array)
import Browser
import Browser.Navigation
import Csv.Decode
import Dict
import Html exposing (Html, a, div, h3, hr, input, label, node, p, span, text, textarea)
import Html.Attributes exposing (checked, class, href, property, rel, target, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Keyed
import Json.Encode
import Platform.Cmd as Cmd
import Task
import Url
import Url.Builder


main : Program Flags Model Msg
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
    , sanitizeRows : Bool
    , currentUrl : Url.Url
    , prefixUrl : Url.Url
    , columns : Array Column
    , maxRows : Int
    }


type alias Column =
    { name : String
    , rawText : String
    , fixedText : Maybe String
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
    | SetSanitizeRows Bool
    | ManyMessages (List Msg)


colNameSeparator : String
colNameSeparator =
    ","


colNameParam : String
colNameParam =
    "columns"


urlParam : String
urlParam =
    "url"


sanitizeRowsParams : String
sanitizeRowsParams =
    "sanitizeRows"


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ currentUrl navKey =
    let
        appUrl =
            AppUrl.fromUrl currentUrl

        columnNames =
            appUrl.queryParameters
                |> Dict.get colNameParam
                |> Maybe.map (String.join colNameSeparator)
                |> Maybe.withDefault "Your name"
                |> String.split colNameSeparator

        prefixUrl =
            appUrl.queryParameters
                |> Dict.get urlParam
                |> Maybe.andThen List.head
                |> Maybe.andThen Url.fromString
                |> Maybe.withDefault { currentUrl | query = Nothing, fragment = Nothing }

        sanitizeRows =
            appUrl.queryParameters
                |> Dict.get sanitizeRowsParams
                |> Maybe.map (\words -> not (List.any (\a -> List.member a [ "0", "false", "f" ]) words))
                |> Maybe.withDefault True

        rows =
            [ "John Doe", "Mary Jane" ]

        rawText =
            String.join "\n" rows
    in
    ( { navKey = navKey
      , alert = Nothing
      , sanitizeRows = sanitizeRows
      , currentUrl = currentUrl
      , prefixUrl = prefixUrl
      , columns =
            columnNames
                |> List.map
                    (\name ->
                        { name = name
                        , rawText = rawText
                        , fixedText = Nothing
                        , rows = Array.fromList rows
                        }
                    )
                |> Array.fromList
      , maxRows = List.length rows
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    let
        effectiveColumnsList =
            Array.toList model.columns
                |> List.filter (\row -> row.name /= "" || row.rows /= emptyRow)
    in
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
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                , onCheck SetSanitizeRows
                                , checked model.sanitizeRows
                                ]
                                []
                            , text " Sanitize copy-paste CSV data"
                            ]
                        ]
                    , div [ class "mb-5" ]
                        [ input
                            [ class "border border-gray-300 p-2 w-full"
                            , type_ "text"
                            , onInput (String.trim >> SetPrefixUrl)
                            , property "defaultValue" (Json.Encode.string (Url.toString model.prefixUrl))
                            ]
                            []
                        ]
                    , Html.Keyed.node "div"
                        []
                        (Array.indexedMap
                            (\index column ->
                                ( String.fromInt index
                                , div [ class "mb-8" ]
                                    [ div []
                                        [ input
                                            [ class "border border-gray-300 p-2 w-full"
                                            , type_ "text"
                                            , onInput (SetName index)
                                            , property "defaultValue" (Json.Encode.string column.name)
                                            ]
                                            []
                                        ]
                                    , div []
                                        [ textarea
                                            [ class "border border-gray-300 p-2 w-full h-32"
                                            , onInput (SetRows index)
                                            , case column.fixedText of
                                                Just fixedText ->
                                                    value fixedText

                                                Nothing ->
                                                    value column.rawText
                                            ]
                                            []
                                        ]
                                    , span [ class "text-xs" ]
                                        [ text (String.fromInt (Array.length column.rows) ++ " rows") ]
                                    ]
                                )
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
                    , case effectiveColumnsList of
                        [] ->
                            text ""

                        (first :: _) as columns ->
                            let
                                outputRows =
                                    columnsAddQueryStrings model.prefixUrl first (List.filter (\{ rows } -> Array.length rows > 0) columns)
                                        |> Array.toList
                                        |> List.map Url.toString
                            in
                            div [ class "mb-5" ]
                                [ textarea
                                    [ class "border border-gray-300 p-2 w-full h-32"
                                    ]
                                    [ outputRows
                                        |> String.join "\n"
                                        |> text
                                    ]
                                , span [ class "text-xs" ]
                                    [ text (String.fromInt (List.length outputRows) ++ " rows") ]
                                ]

                    -- , h3 [] [ text "Output links" ]
                    -- , case effectiveColumnsList of
                    --     [] ->
                    --         text ""
                    --     (first :: _) as columns ->
                    --         let
                    --             outputRows =
                    --                 columnsAddQueryStrings model.prefixUrl first (List.filter (\{ rows } -> Array.length rows > 0) columns)
                    --                     |> Array.toList
                    --                     |> List.map Url.toString
                    --         in
                    --         div [ class "mb-5" ]
                    --             [ div []
                    --                 (outputRows
                    --                     |> List.map
                    --                         (\url ->
                    --                             p []
                    --                                 [ a
                    --                                     [ class "underline"
                    --                                     , target "_blank"
                    --                                     , href url
                    --                                     ]
                    --                                     [ text url ]
                    --                                 ]
                    --                         )
                    --                 )
                    --             , span [ class "text-xs" ]
                    --                 [ text (String.fromInt (List.length outputRows) ++ " rows") ]
                    --             ]
                    -- , pre [ class "whitespace-pre-wrap" ] [ text (Debug.toString model) ]
                    , a
                        [ class "text-xs gray-500 float-right"
                        , target "_blank"
                        , href "https://github.com/choonkeat/elm-link-generator"
                        ]
                        [ text "github.com/choonkeat/elm-link-generator" ]
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


newURL : { a | currentUrl : Url.Url, prefixUrl : Url.Url } -> Array { b | name : String } -> Url.Url
newURL { currentUrl, prefixUrl } newColumns =
    let
        colNames =
            newColumns
                |> Array.map .name
                |> Array.toList
                |> String.join colNameSeparator

        newQuery =
            Url.Builder.toQuery
                [ Url.Builder.string urlParam (Url.toString prefixUrl)
                , Url.Builder.string colNameParam colNames
                ]
    in
    { currentUrl
        | query = Just (String.dropLeft 1 newQuery)
    }


emptyRow : Array String
emptyRow =
    Array.fromList [ "" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    , Browser.Navigation.pushUrl model.navKey (Url.toString (newURL model model.columns))
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
                , columns =
                    Array.push
                        { name = "Column " ++ String.fromInt (Array.length model.columns)
                        , rawText = ""
                        , fixedText = Nothing
                        , rows = Array.fromList []
                        }
                        model.columns
              }
            , Browser.Navigation.pushUrl model.navKey (Url.toString (newURL model model.columns))
            )

        SetName index name ->
            case Array.get index model.columns of
                Just column ->
                    let
                        newColumns =
                            Array.set index { column | name = String.trim name } model.columns
                    in
                    ( { model
                        | alert = Nothing
                        , columns =
                            newColumns
                      }
                    , Browser.Navigation.pushUrl model.navKey (Url.toString (newURL model newColumns))
                    )

                Nothing ->
                    ( { model
                        | alert = Just { color = "red", message = "Invalid column" }
                      }
                    , Cmd.none
                    )

        SetRows index rawText ->
            case Array.get index model.columns of
                Just column ->
                    let
                        rawTextWithoutTrailingWhitespace =
                            String.trimRight rawText

                        splitRawText =
                            String.split "\n" rawTextWithoutTrailingWhitespace
                                |> List.map String.trim

                        csvRowsResult =
                            Csv.Decode.decodeCsv Csv.Decode.NoFieldNames (Csv.Decode.column 0 Csv.Decode.string) rawTextWithoutTrailingWhitespace
                                |> Result.map (List.map (String.trim >> splitByWhitespace >> String.join " "))
                    in
                    case csvRowsResult of
                        Ok csvRows ->
                            if csvRows == splitRawText || not model.sanitizeRows then
                                ( { model
                                    | alert = Nothing
                                    , maxRows = Basics.max model.maxRows (List.length splitRawText)
                                    , columns =
                                        Array.set index
                                            { column
                                                | rawText = rawText
                                                , fixedText = Nothing
                                                , rows = Array.fromList splitRawText
                                            }
                                            model.columns
                                  }
                                , Cmd.none
                                )

                            else
                                -- not naive CSV encoding!
                                -- fix rawText
                                ( { model
                                    | alert = Nothing
                                    , maxRows = Basics.max model.maxRows (List.length csvRows)
                                    , columns =
                                        Array.set index
                                            { column
                                                | rawText = rawText
                                                , fixedText = Just (String.join "\n" csvRows ++ "\n")
                                                , rows = Array.fromList csvRows
                                            }
                                            model.columns
                                  }
                                , Cmd.none
                                )

                        Err _ ->
                            ( { model
                                | alert = Nothing
                                , maxRows = Basics.max model.maxRows (List.length splitRawText)
                                , columns =
                                    Array.set index
                                        { column
                                            | rawText = rawText
                                            , fixedText = Nothing
                                            , rows = Array.fromList splitRawText
                                        }
                                        model.columns
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( { model
                        | alert = Just { color = "red", message = "Invalid column" }
                      }
                    , Cmd.none
                    )

        SetSanitizeRows bool ->
            ( { model
                | sanitizeRows = bool
              }
            , model.columns
                |> Array.toList
                |> List.indexedMap (\index column -> SetRows index column.rawText)
                |> Task.succeed
                |> Task.perform ManyMessages
            )

        ManyMessages [] ->
            ( model, Cmd.none )

        ManyMessages (m :: ms) ->
            update m model
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ cmd
                            , Task.succeed ms
                                |> Task.perform ManyMessages
                            ]
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--


columnsAddQueryStrings : Url.Url -> Column -> List Column -> Array Url.Url
columnsAddQueryStrings prefixUrl firstColumn columns =
    let
        mergeQueryFn =
            -- create a function once, based on prefixUrl.query
            -- then use it repeatedly below at Array.map
            case prefixUrl.query of
                Just oldQuery ->
                    \queryParams ->
                        Url.Builder.toQuery (Array.toList queryParams) ++ "&" ++ oldQuery

                Nothing ->
                    \queryParams ->
                        Url.Builder.toQuery (Array.toList queryParams)
    in
    firstColumn.rows
        |> Array.indexedMap (rowToQueryParameters Array.empty columns)
        |> Array.map
            (\params ->
                { prefixUrl
                    | query = Just (String.dropLeft 1 (mergeQueryFn params))
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



--


whitespaceCodePoints : List Int
whitespaceCodePoints =
    [ 0x20
    , 0x09
    , 0x0A
    , 0x0D
    , 0xA0
    , 0x1680
    , 0x2000
    , 0x2001
    , 0x2002
    , 0x2003
    , 0x2004
    , 0x2005
    , 0x2006
    , 0x2007
    , 0x2008
    , 0x2009
    , 0x200A
    , 0x202F
    , 0x205F
    , 0x3000
    ]


isWhitespace : Char -> Bool
isWhitespace char =
    List.member (Char.toCode char) whitespaceCodePoints


splitByWhitespace : String -> List String
splitByWhitespace str =
    let
        chars =
            String.toList str

        ( words, currentWord ) =
            List.foldl
                (\char ( words1, currentWord1 ) ->
                    if isWhitespace char then
                        if String.isEmpty currentWord1 then
                            ( words1, currentWord1 )

                        else
                            ( words1 ++ [ currentWord1 ], "" )

                    else
                        ( words1, currentWord1 ++ String.fromChar char )
                )
                ( [], "" )
                chars
    in
    if String.isEmpty currentWord then
        words

    else
        words ++ [ currentWord ]
