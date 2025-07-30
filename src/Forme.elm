module Forme exposing
    ( Form(..)
    , submit
    , ValidityReport, invalid, noValidationPopups
    , new
    )

{-| Work with forms by handling the form data directly from the browser, instead of looking after the state yourself.


# Representing forms in elm

@docs Form


# Submitting a form

@docs submit


# Form Errors

@docs ValidityReport, invalid, noValidationPopups


# Utilities

@docs new

-}

import Dict
import Forme.Decoder as Decoder exposing (Decoder)
import Forme.Internal as Internal exposing (Error_(..))
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Json
import Json.Encode as Js


{-| A representation of a form submission, if the constraint validation API is happy and your decoder succeeded then this value is `Valid`.
If either the constraint validation fails, or your decoder faild then the value is `Invalid` and contains error information for both scenarios.
-}
type Form a
    = Valid a
    | Invalid (Result Decoder.Error a) (List ValidityReport)


{-| A validity report containing the message from the constraint validation API, as well as some properties describing which form control has the issue.
-}
type alias ValidityReport =
    { id : String, message : String, name : String, label : String }


type alias FormEvents msg =
    { submit : Html.Attribute msg, invalid : Html.Attribute msg }


decode : Decoder a -> Internal.FormData -> Result Decoder.Error a
decode (Internal.Decoder_ f) dict =
    f { dict = dict, key = "", duplicates = False, value = Nothing } |> Result.mapError toPublicError


{-| You may want to use the same form decoder and message handler, this little convenience function
makes that look a bit more terse:

    let
        {submit, invalid} = Forme.new myFormDecoder MyFormSubmitWasClicked
    in
    ...etc...

-}
new : Decoder a -> (Form a -> msg) -> FormEvents msg
new decoder toMsg =
    { submit = submit decoder toMsg, invalid = invalid decoder toMsg }


{-| Listen to the forms submit event, providing a decoder and a message handler.

This event handler prevents the default submit event behaviour from being triggered, and is not fired if an invalid form is submitted.

-}
submit : Decoder a -> (Form a -> msg) -> Html.Attribute msg
submit decoder handler =
    Events.preventDefaultOn "submit"
        (Internal.decodeValidityState
            |> Json.andThen
                (\valid ->
                    if valid then
                        Internal.decodeFormEntries
                            |> Json.map (decode decoder)
                            |> Json.map (submitHelp handler)

                    else
                        Json.fail "wasn't valid"
                )
        )


submitHelp : (Form a -> msg) -> Result Decoder.Error a -> ( msg, Bool )
submitHelp toMsg result =
    case result of
        Ok a ->
            ( toMsg <| Valid a, True )

        e ->
            ( toMsg <| Invalid e [], True )


{-| Listen for invalid form submissions, providing a decoder and a message handler.

This event is fired when the browser fires an `invalid` event which has reached the form.

This event does not prevent the default behaviour (constraint validation popups) from happening,
but that can be enabled if desired using another attribute: @see noValidationPopups.

-}
invalid : Decoder a -> (Form a -> msg) -> Html.Attribute msg
invalid decoder handler =
    Events.on "click"
        (Internal.decodeValidityState
            |> Json.andThen
                (\valid ->
                    if not valid then
                        Internal.decodeFormEntries
                            |> Json.map (invalidHelp decoder handler)

                    else
                        Json.fail "is valid"
                )
        )


invalidHelp : Decoder a -> (Form a -> msg) -> Internal.FormData -> msg
invalidHelp decoder toMsg dict =
    let
        fakeCtx k =
            Internal.InternalCtx_
                { key = k, dict = dict, duplicates = False, value = Nothing }

        constraintErrors =
            Dict.filter (\k v -> v.message /= Nothing) dict
                |> Dict.foldl (\k v acc -> { id = v.id, message = v.message |> Maybe.withDefault "", name = k, label = v.label } :: acc)
                    []
    in
    case constraintErrors of
        [] ->
            toMsg <| Invalid (decode decoder dict) []

        errors ->
            toMsg <| Invalid (decode decoder dict) errors


{-| Turn off the default browser behaviour when an invalid event is fired within the form.

This attribute needs to be attached to a `<form>` or it will do nothing!

This does not prevent event handlers from firing: @see invalid.

-}
noValidationPopups : Bool -> Html.Attribute msg
noValidationPopups b =
    Attr.property (Internal.identifier ++ "__suppress") (Js.bool b)


toPublicError : Internal.Error_ -> Decoder.Error
toPublicError ie =
    case ie of
        NotFound_ c ->
            Decoder.NotFound c

        DuplicateValues_ c ->
            Decoder.DuplicateValues c

        FailedToConvert_ what c ->
            Decoder.FailedToConvert what c

        UserDefined_ msg c ->
            Decoder.UserDefined msg c

        NoValidDecoder_ c ->
            Decoder.NoValidDecoder c
