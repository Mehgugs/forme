# Forme 

A different approach to handling HTML forms in elm. 

## Motivation 

Browsers provide a lot of functionality out of the box, but often elm applications disable or sidestep 
the features of the web so that they can write more ideomatic code. For example, a standardised pattern in elm develop is to attach an `"input"` event 
handler to text `<input>` elements coupled with setting the `value` attribute, even though the browser already stores and tracks this state for you! When developing a form with a lot of inputs, we would then need to write a lot of elm code just to hold on the raw textual data... which isn't very fun.

I don't want to mimic the browser and book-keep all the fields individually, I want to parse the HTML form when it is submitted into a nicely typed value. I also want to use the default constraint validation api when possible, but if I had a way to just switch it off and roll my own I'd be able to deal with the places where it falls short. 

I want this: 

```elm 
import Forme exposing (Form)
import Forme.Decoder as Decoder

-- somewhere types are kept -- 

type Msg  
    = LoginSubmitClicked (Form LoginForm)


type LoginForm = 
    { username : String 
    , password : String
    }

loginFormDecoder = 
    Decoder.succeed LoginForm 
    |> Decoder.andMap 
        (Decoder.field "username" Decoder.string)
    |> Decoder.andMap 
        (Decoder.field "password" Decoder.string)

-- somewhere within a view -- 

update msg model = 
    case msg of 
        LoginSubmitClicked (Forme.Valid {username, password}) -> 

        LoginSubmitClicked (Forme.Invalid (Err error) _) -> 
            case error of 
                Decoder.FailedToConvert what ctx -> 
                    let 
                        labelInnerText = Decoder.labelFromContext ctx
                    in 
                    (model, Toast.show {message = fmt "There was a problem with the value in '$1'" [labelInnerText]})
        ...etc...



renderForm : Html Msg 
renderForm = 
    let 
        {submit, invalid} = Forme.new loginFormDecoder LoginSubmitClicked
    in 
    Html.form [submit, invalid]
        [ Html.input [Attr.name "username"] [] 
        , Html.input [Attr.name "password", Attr.type_ "password", Attr.required] [] 
        , Html.button [] [Html.text "Submit"] 
        ]
```

We're in luck, because this is the kind of interface provided by this library!

## Decoding Forms

Usually in elm, we keep the entire intermediate state of the form in memory so that we can readily construct 
a data structure to represent the form in order to use it further. I have personally found this approach hard to scale, 
and have had to deal with the instinct to try and be "clever" and store values derived from user input instead of the raw textual data. 

By using the `FormData` class in javascript, it is possible to convert a `<form>` into an association list of all the data that will be submitted. Using this in the form's events allows us to take the serialised data and use it for our own purposes without needing to store any intermediate state. 

If we could get access to this collection in elm, we could write a decoder style API to convert it into a domain type. 
This has the additional benefit of gracefully handling things like `<select>` (event multiple option selects) and `<input type=radio>`. 

By adding a getter to instances of `HtmlFormElement` we can extract it from the `currentTarget` of a `SubmitEvent` and parse it into an elm value!


