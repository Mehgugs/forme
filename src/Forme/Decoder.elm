module Forme.Decoder exposing
    ( Decoder, Error(..), Ctx, details
    , field
    , succeed, fail, string, int, float, bool
    , oneOf, list, nonEmpty, exactly, color, date, time
    , map, optional, default, try
    , andThen
    , andMap
    , map2
    , allowDuplicates
    )

{-| Tell Forme how to turn captured formdata into nice elm values!


# Decoders and Errors

@docs Decoder, Error, Ctx, details


# Creating a Decoder

@docs field


# Decoding Values

@docs succeed, fail, string, int, float, bool


# Fancier Decoders

@docs oneOf, list, nonEmpty, exactly, color, date, time


# Manipulating Decoders

The following functions let us change the behaviour of a decoder.

@docs map, optional, default, try


# Combining Multiple Decoders

@docs andThen

@docs andMap

This library works best if you use a pipeline style to construct decoders, instead of
a `mapN` function for the specific number of fields. The `andMap` function lets us chain together
multiple decoders in a more erganomic way than [`andThen`](#andThen).

    type alias LoginForm =
        { username : String
        , rememberMe : Bool
        , password : String
        }

    formDecoder : Decoder LoginForm
    formDecoder =
        succeed LoginForm
        |> andMap (field "username" string)
        |> andMap (field "remember-me" bool)
        |> andMap (field "password" nonEmpty)

@docs map2

The `map2` is still provided by this library as a primitive, it's simply a bit more terse when you're decoding things like tuples or
two argument variants.

    pointDecoder : (Float, Float)
    pointDecoder =
        map2 Tuple.pair (field "x" float) (field "y" float)


# Controlling the Decoding

@docs allowDuplicates

-}

import Bitwise
import Color exposing (Color)
import Date exposing (Date)
import Dict exposing (Dict)
import Forme.Internal as Internal exposing (..)
import Time


{-| This type describes the different types of errors that can occur when parsing formdata.

Most of the variants contain a context which can provide details about what was happening when the error was constructed.

  - **NotFound** is used to signal that the key supplied by [`field`](#field) did not exist in the form data.
  - **DuplicateValues** is used to signal that the formdata contained multiple values for the same name,
    which can be ignored situationally by using [`allowDuplicates`](#allowDuplicates).
  - **FailedToConvert** is used to signal that internally a string conversion failed; for example if you use [`int`](#int).
    but the formdata value is not recognized by [`String.toInt`](/packages/elm/core/latest/String#toInt).
  - **NoValidDecoder** is used to signal that a [`oneOf`](#oneOf) had no decoders to run.
  - **UserDefined** is used to hold error messages provided to [`fail`](#fail).

-}
type Error
    = NotFound Ctx
    | DuplicateValues Ctx
    | FailedToConvert String Ctx
    | UserDefined String Ctx
    | NoValidDecoder Ctx


{-| This context value is created as the formdata travels through the decoder,
and can be queried for information about what was being decoded at the time an error occurred by using the [`details`](#details) helper function.
-}
type alias Ctx =
    Internal.Ctx


type alias Ctx_ =
    { key : String, dict : Internal.FormData, duplicates : Bool, value : Maybe Internal.RawFormValue, optional : Bool }


{-| Extract the details from the context value. This can tell you more about what was being decoded when the error was constructed.
-}
details : Ctx -> { name : String, value : Maybe ( String, List String ), duplicates : Bool }
details (InternalCtx_ ctx) =
    let
        nemap f ( a, aa ) =
            ( f a, List.map f aa )
    in
    { name = ctx.key
    , value =
        Dict.get ctx.key ctx.dict
            |> Maybe.map (nemap formValueToString)
    , duplicates = ctx.duplicates
    }


{-| This type represents a decoder that when ran on formdata will produce a value of type `a` or an [`Error`](#Error).
-}
type alias Decoder a =
    Internal.Decoder a


error : (Ctx -> Error_) -> Ctx_ -> Result Error_ x
error f internal =
    internal |> InternalCtx_ |> f |> Err



-- HELPER


getCurrentValue : Ctx_ -> Result Error_ RawFormValue
getCurrentValue ({ key, dict, duplicates, value } as ctx) =
    case Dict.get key dict of
        Just entry ->
            case value of
                Just v_ ->
                    Ok v_

                Nothing ->
                    entryToValue ctx entry

        Nothing ->
            error NotFound_ ctx


entryToValue : Ctx_ -> FormEntry -> Result Error_ RawFormValue
entryToValue ({ duplicates } as ctx) value =
    case value of
        ( s, [] ) ->
            Ok s

        ( s, more ) ->
            if duplicates then
                Ok s

            else
                error DuplicateValues_ ctx


getCurrentEntry : Ctx_ -> Result Error_ FormEntry
getCurrentEntry ({ key, dict, duplicates, value } as ctx) =
    case Dict.get key dict of
        Just entry ->
            Ok entry

        Nothing ->
            error NotFound_ ctx



-- CONTROL


{-| Run the decoder on the formdata entry whose `"name"` attribute matches the `name` argument.

This decoder is critical for retrieving values from formdata, any decoder which needs to actually
look at a formdata entry will fail with `NotFound` if you don't tell it which field you want it to look at!

Some data formats like JSON support different kinds of values at the top level, but formdata is always a list of
key-value pairs so we need to use this function to access them, and because formdata cannot the most recent field call will win:

    field "a" (field "b" string)
    --     ^          ^
    --     |          |_ this field is the one that will be used.
    --     |_ this is a no-op.

See [these functions](#fancier-decoders) for an example of using multiple `fields`.

-}
field : String -> Decoder a -> Decoder a
field name (Decoder_ f) =
    Decoder_ <| \ctx -> f { ctx | key = name }


{-| Allow the decoder to use first value of multiple instead of signaling an error.
This function will be ignored when decoders like `list` that consume the multiple values are being processed.
-}
allowDuplicates : Decoder a -> Decoder a
allowDuplicates (Decoder_ f) =
    Decoder_ <| \ctx -> f { ctx | duplicates = True }



-- DECODE


{-| Decode a constant value, or, lift a value of type `a` into the Decoder world.
-}
succeed : a -> Decoder a
succeed a =
    Decoder_ (always (Ok a))


{-| Stop decoding and signal an error with a message. This will become a `UserDefined` error.
-}
fail : String -> Decoder a
fail msg =
    Decoder_ <| \ctx -> error (UserDefined_ msg) ctx


fail_ : (Ctx -> Error_) -> Decoder a
fail_ e =
    Decoder_ <| \ctx -> error e ctx


{-| Decode a string value.
-}
string : Decoder String
string =
    Decoder_ <| (Result.map formValueToString << getCurrentValue)


{-| Decode an integer value.
-}
int : Decoder Int
int =
    Decoder_ <|
        \ctx ->
            case getCurrentValue ctx of
                Ok v ->
                    case formValueToInt v of
                        Just i ->
                            Ok i

                        Nothing ->
                            error (FailedToConvert_ "INT") ctx

                Err x ->
                    Err x


{-| Decode a float value.
-}
float : Decoder Float
float =
    Decoder_ <|
        \ctx ->
            case getCurrentValue ctx of
                Ok v ->
                    case formValueToFloat v of
                        Just i ->
                            Ok i

                        Nothing ->
                            error (FailedToConvert_ "FLOAT") ctx

                Err x ->
                    Err x


{-| Decode a boolean from formdata assuming a check box format.

This means that the decoder will look for a value of `"on"` and not the
literal text `"true"`. This decoder can also never fail due to a field not being found, because
unchecked checkboxes do not appear in formdata.

-}
bool : Decoder Bool
bool =
    Decoder_ <|
        \ctx ->
            case getCurrentValue ctx |> Result.map formValueToString of
                Ok "on" ->
                    Ok True

                Ok "off" ->
                    Ok False

                Ok text ->
                    error (FailedToConvert_ "BOOL") ctx

                Err (NotFound_ _) ->
                    Ok False

                Err x ->
                    Err x


{-| Run a decoder for all values of a formdata entry, as a `List` of the results.

This decoder captures the results of things like `<select multiple>` elements which contribute multiple values for a given name.
This decoder requires all of the values parse successfully.
This decoder will lift a single formdata value into a singleton list.

-}
list : Decoder a -> Decoder (List a)
list (Decoder_ f) =
    Decoder_ <|
        \ctx ->
            case getCurrentEntry ctx of
                Ok ( first, more ) ->
                    let
                        allValues =
                            first :: more

                        folder item acc =
                            Result.map2 (::) (f { ctx | value = Just item }) acc
                    in
                    List.foldr folder (Ok []) allValues

                Err x ->
                    Err x


{-| Given a list of decoders, try each of them in sequence until one succeeds.

If the input decoder list is empty, or they all fail then `NoValidDecoder` is signaled.

This can be used to decode variants with helpers like [`exactly`](#exactly):

    type Direction = North | South | East | West

    decodeDirection : Decoder Direction
    decodeDirection =
        oneOf
            [ exactly "north" |> map (always North)
            , exactly "south" |> map (always South)
            , exactly "east" |> map (always East)
            , exactly "west" |> map (always West)
            ]

-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder_ <|
        \ctx ->
            let
                folder ds =
                    case ds of
                        (Decoder_ decoder) :: rest ->
                            case decoder ctx of
                                Ok v ->
                                    Ok v

                                _ ->
                                    folder rest

                        [] ->
                            error NoValidDecoder_ ctx
            in
            folder decoders


{-| Given a string, returns a decoder which only succeeds if the formdata value is the exact text provided.

See [`oneOf`](#oneOf) for an example of using `exactly`.

-}
exactly : String -> Decoder ()
exactly text =
    string
        |> andThen
            (\value ->
                if value == text then
                    succeed ()

                else
                    fail_ (FailedToConvert_ <| "EXACTLY '" ++ text ++ "'")
            )


{-| Decodes a string but discards the empty string.
-}
nonEmpty : Decoder String
nonEmpty =
    Decoder_ <|
        \ctx ->
            case getCurrentValue ctx of
                Ok (Text "") ->
                    error NotFound_ ctx

                Ok fv ->
                    Ok (formValueToString fv)

                Err x ->
                    Err x


hexalphabet =
    "0123456789abcdef" |> String.toList |> List.indexedMap (\i x -> ( x, i )) |> Dict.fromList


hexn =
    String.foldl
        (\item acc ->
            Dict.get item hexalphabet
                |> Maybe.map2 (\ret i -> ret |> Bitwise.shiftLeftBy 4 |> Bitwise.or i) acc
        )
        (Just 0)


{-| Decode a colour, formatted the same way the value of `<input type="color" />` is formatted.
-}
color : Decoder Color
color =
    let
        process text =
            let
                hexbits =
                    String.dropLeft 1 text

                ( rr, gg, bb ) =
                    ( String.slice 0 2 hexbits, String.slice 2 4 hexbits, String.slice 4 6 hexbits )
            in
            Maybe.map3 Color.rgb255 (hexn rr) (hexn gg) (hexn bb)
                |> Maybe.map succeed
                |> Maybe.withDefault (fail_ (FailedToConvert_ "COLOUR"))
    in
    string |> andThen process


{-| Decode a date, formatted the same way the value of `<input type="date" />` is formatted, or in other words, a
string in the form `yyyy-mm-dd`. This decoder also enforces the length of the year part of the input text.
-}
date : Decoder Date
date =
    let
        asLongAs n s =
            if String.length s < n then
                Nothing

            else
                Just s

        process text =
            if text == "" then
                fail_ NotFound_

            else
                case text |> String.split "-" of
                    [ yyyy, mm, dd ] ->
                        Maybe.map3 Date.fromCalendarDate
                            (yyyy |> asLongAs 4 |> Maybe.andThen String.toInt)
                            (String.toInt mm |> Maybe.map Date.numberToMonth)
                            (String.toInt dd)
                            |> Maybe.map succeed
                            |> Maybe.withDefault (fail_ (FailedToConvert_ "DATE"))

                    _ ->
                        fail_ (FailedToConvert_ "DATE")
    in
    string |> andThen process


{-| Decode a time, by using the input's `valueAsDate` value if it was a valid Date.

In practice this will only decode values from `"datetime-local"` type inputs, since they are the only inputs that define this field by default.

-}
time : Decoder Time.Posix
time =
    Decoder_ <|
        \ctx ->
            case getCurrentValue ctx of
                Ok v ->
                    case formValueToTime v of
                        Just i ->
                            Ok i

                        Nothing ->
                            error (FailedToConvert_ "TIME") ctx

                Err x ->
                    Err x



-- CONVERT


{-| Transform a decoder.

    Decoder.string |> Decoder.map (String.dropLeft 1)

-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder_ d) =
    Decoder_ (d >> Result.map f)


{-| Create decoders that depend on the results of previous decoders.

    Decoder.field "include-foo" Decoder.bool
        |> Decoder.andThen
            (\value ->
                if value then
                    Decoder.field "foo" Decoder.string |> Decoder.map Just
                else
                    Decoder.succeed Nothing)

-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f (Decoder_ d) =
    Decoder_ <|
        \ctx ->
            d ctx |> Result.map f |> Result.andThen (\(Decoder_ d2) -> d2 ctx)


{-| -}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f (Decoder_ da) (Decoder_ db) =
    Decoder_ <|
        \ctx ->
            Result.map2 f (da ctx) (db ctx)


{-| -}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap (Decoder_ da) (Decoder_ df) =
    Decoder_ <|
        \ctx ->
            Result.map2 (<|) (df ctx) (da ctx)


{-| Stop a decoder from failing if a value could not be found.

**Note:** This function will not discard errors like `FailedToConvert` or `UserDefined`, only `NotFound`.

-}
optional : Decoder a -> Decoder (Maybe a)
optional (Decoder_ da) =
    Decoder_ <|
        \ctx ->
            case da { ctx | optional = True } of
                Err (NotFound_ _) ->
                    Ok Nothing

                Err other ->
                    Err other

                Ok a ->
                    Ok (Just a)


{-| Stop a decoder from failing if a value could not be found, using a default value instead.
-}
default : a -> Decoder a -> Decoder a
default value d =
    d |> optional |> map (Maybe.withDefault value)


{-| Given a decoder, returns a decoder which never fails.
-}
try : Decoder x -> Decoder (Result Error x)
try (Decoder_ d) =
    Decoder_ <|
        \ctx ->
            d ctx |> Result.mapError toPublicError |> Ok


toPrivateError : Error -> Error_
toPrivateError ie =
    case ie of
        NotFound c ->
            NotFound_ c

        DuplicateValues c ->
            DuplicateValues_ c

        FailedToConvert what c ->
            FailedToConvert_ what c

        UserDefined msg c ->
            UserDefined_ msg c

        NoValidDecoder c ->
            NoValidDecoder_ c


toPublicError : Internal.Error_ -> Error
toPublicError ie =
    case ie of
        NotFound_ c ->
            NotFound c

        DuplicateValues_ c ->
            DuplicateValues c

        FailedToConvert_ what c ->
            FailedToConvert what c

        UserDefined_ msg c ->
            UserDefined msg c

        NoValidDecoder_ c ->
            NoValidDecoder c
