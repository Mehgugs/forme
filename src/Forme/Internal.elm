module Forme.Internal exposing (..)

import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Json
import Time


identifier : String
identifier =
    "__forme-0.0.1"


type RawFormValue
    = Text String
    | Number Float String
    | Instant Time.Posix String


type alias FormData =
    Dict String ( RawFormValue, List RawFormValue )


type alias FormEntry =
    ( RawFormValue, List RawFormValue )


type alias ValidityReport =
    { name : String, message : String }


type Ctx
    = InternalCtx_ Ctx_


type alias Ctx_ =
    { key : String, dict : FormData, duplicates : Bool, value : Maybe RawFormValue, optional : Bool }


type Error_
    = NotFound_ Ctx
    | DuplicateValues_ Ctx
    | FailedToConvert_ String Ctx
    | UserDefined_ String Ctx
    | NoValidDecoder_ Ctx


type Decoder a
    = Decoder_ (Ctx_ -> Result Error_ a)


formValueToString : RawFormValue -> String
formValueToString rfv =
    case rfv of
        Text s ->
            s

        Number _ s ->
            s

        Instant _ s ->
            s


formValueToInt : RawFormValue -> Maybe Int
formValueToInt rfv =
    case rfv of
        Text s ->
            String.toInt s

        Number n s ->
            String.toInt s

        Instant _ _ ->
            Nothing


formValueToFloat : RawFormValue -> Maybe Float
formValueToFloat rfv =
    case rfv of
        Text s ->
            String.toFloat s

        Number n s ->
            Just n

        Instant _ _ ->
            Nothing


formValueToTime : RawFormValue -> Maybe Time.Posix
formValueToTime rfv =
    case rfv of
        Instant t _ ->
            Just t

        _ ->
            Nothing


decodeFormValue : Json.Decoder RawFormValue
decodeFormValue =
    Json.map3
        (\text fl instant ->
            case instant of
                Just posix ->
                    Instant posix text

                Nothing ->
                    case fl of
                        Just number ->
                            Number number text

                        Nothing ->
                            Text text
        )
        (Json.index 1 Json.string)
        (Json.index 2 (Json.maybe Json.float))
        (Json.index 3 (Json.maybe (Json.int |> Json.map Time.millisToPosix)))


decodeValidationMessage : Json.Decoder ( String, String )
decodeValidationMessage =
    Json.map2 Tuple.pair (Json.index 0 Json.string) (Json.index 4 Json.string)


decodeFormEntry : Json.Decoder ( String, RawFormValue )
decodeFormEntry =
    Json.map2 Tuple.pair (Json.index 0 Json.string) decodeFormValue


decodeFormEntries : Json.Decoder FormData
decodeFormEntries =
    Json.at [ "currentTarget", identifier ++ "__entries" ]
        (Json.list decodeFormEntry
            |> Json.map zipUp
        )


decodeValidationMessages : Json.Decoder (List ValidityReport)
decodeValidationMessages =
    Json.at [ "currentTarget", identifier ++ "__messages" ]
        (Json.list (Json.map2 ValidityReport (Json.index 0 Json.string) (Json.index 1 Json.string)))


zipUp items =
    List.foldr
        (\( key, item ) acc ->
            Dict.update
                key
                (Maybe.map (Tuple.mapSecond ((::) item) >> Just)
                    >> Maybe.withDefault (Just ( item, [] ))
                )
                acc
        )
        Dict.empty
        items


decodeValidityState : Json.Decoder Bool
decodeValidityState =
    Json.at [ "currentTarget", identifier ++ "__validity" ] Json.bool


decodeSubmitterIsValid =
    let
        check type_ tag =
            case ( tag, type_ ) of
                ( "BUTTON", "" ) ->
                    True

                ( "BUTTON", "submit" ) ->
                    True

                ( "INPUT", "submit" ) ->
                    True

                ( "INPUT", "image" ) ->
                    True

                _ ->
                    False
    in
    Json.field "target" (Json.map2 check (Json.field "type" Json.string) (Json.field "tagName" Json.string))
