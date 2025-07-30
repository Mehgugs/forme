module Forme.Internal exposing (..)

import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Json


identifier : String
identifier =
    "__forme-0.0.1"


type RawFormValue
    = Simple String
    | Complex String (List String)


type alias FormEntry =
    { value : RawFormValue
    , label : String
    , message : Maybe String
    , id : String
    }


type alias FormData =
    Dict String FormEntry


type Ctx
    = InternalCtx_ Ctx_


type alias Ctx_ =
    { key : String, dict : Dict String FormEntry, duplicates : Bool, value : Maybe String }


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
        Simple s ->
            s

        Complex f _ ->
            f


additionalValues : RawFormValue -> List String
additionalValues rfv =
    case rfv of
        Simple s ->
            []

        Complex f fs ->
            fs


unconsOnto : (a -> List a -> b) -> List a -> Maybe b
unconsOnto f items =
    case items of
        first :: rest ->
            Just (f first rest)

        _ ->
            Nothing


decodeFormValue : Json.Decoder RawFormValue
decodeFormValue =
    Json.oneOf
        [ Json.list Json.string
            |> Json.andThen (unconsOnto Complex >> Maybe.map Json.succeed >> Maybe.withDefault (Json.fail "cannot have an empty complex form value"))
        , Json.string |> Json.map Simple
        ]


decodeFormEntry : Json.Decoder FormEntry
decodeFormEntry =
    Json.succeed FormEntry
        |> Json.map2 (|>) (Json.field "value" decodeFormValue)
        |> Json.map2 (|>) (Json.field "label" Json.string)
        |> Json.map2 (|>)
            (Json.field "message" Json.string
                |> Json.map
                    (\s ->
                        if s == "" then
                            Nothing

                        else
                            Just s
                    )
            )
        |> Json.map2 (|>)
            (Json.field "id" Json.string)


decodeFormEntries : Json.Decoder FormData
decodeFormEntries =
    Json.at [ "currentTarget", identifier ++ "__entries" ] (Json.dict decodeFormEntry)


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
