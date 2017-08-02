module Elm.Docs exposing
  ( Module
  , Alias, Union, Value
  , Name(..), Associativity(..)
  , decoder
  )


{-| When packages are published to [package.elm-lang.org][pkg], documentation
is generated for all of the exposed modules (and all of the exposed values).
These docs are formatted as JSON for easy consumption by anyone.

[pkg]: http://package.elm-lang.org/

This module helps you decode the JSON docs into nice Elm values! It is
currently used by [package.elm-lang.org][pkg] to help turn JSON into nice
web pages!

# Decode Docs
@docs decoder

# Work with Docs
@docs Module, Alias, Union, Value, Name, Associativity

-}


import Json.Decode exposing (..)

import Elm.Docs.Type as Type exposing (Type)



-- DOCUMENTATION


{-| All the documentation for a particular module.

  * `name` is the module name
  * `comment` is the module comment

The actual exposed stuff is broken into categories. So all of the type aliases
are available in `aliases`, all of the union types are in `unions`, and
everything else is in `values`.
-}
type alias Module =
  { name : String
  , comment : String
  , aliases : List Alias
  , unions : List Union
  , values : List Value
  }


{-| Documentation for a type alias. For example, if you had the source code:

    {-| pair of values -}
    type alias Pair a = ( a, a )

When it became an `Alias` it would be like this:

    { name = "Pair"
    , comment = " pair of values "
    , args = ["a"]
    , tipe = Tuple [ Var "a", Var "a" ]
    }
-}
type alias Alias =
  { name : String
  , comment : String
  , args : List String
  , tipe : Type
  }


{-| Documentation for a union type. For example, if you had the source code:

    {-| maybe -}
    type Maybe a = Nothing | Just a

When it became a `Union` it would be like this:

    { name = "Maybe"
    , comment = " maybe "
    , args = ["a"]
    , tipe =
        [ ("Nothing", [])
        , ("Just", [Var "a"])
        ]
    }
-}
type alias Union =
  { name : String
  , comment : String
  , args : List String
  , tags : List (String, List Type)
  }


{-| Documentation for values, functions, and operators. For example, if you
had the source code:

    {-| do not do anything -}
    identity : a -> a
    identity value =
      value

The `Value` would look like this:

    { name = Name "identity"
    , comment = " do not do anything "
    , tipe = Lambda (Var "a") (Var "a")
    }
-}
type alias Value =
  { name : Name
  , comment : String
  , tipe : Type
  }


{-| Helps differentiate normal functions like `add` from operators like `(+)`.

So when you get a [`Value`](#Value) of an operator, it would look like this:

    { name = Op "+" Left 6
    , comment = ""
    , tipe = Lambda (Var "number") (Lambda (Var "number") (Var "number"))
    }
-}
type Name
  = Name String
  | Op String Associativity Int


{-| The [associativity][] of an infix operator. This determines how we add
parentheses around everything. Here are some examples:

    1 + 2 + 3 + 4

We have to do the operations in *some* order, so which of these interpretations
should we choose?

    ((1 + 2) + 3) + 4   -- left-associative
    1 + (2 + (3 + 4))   -- right-associative

This is really important for operators like `(|>)`!

Some operators are non-associative though, meaning we do not try to add
missing parentheses. `(==)` is a nice example. `1 == 2 == 3` just is not
allowed!

[associativity]: https://en.wikipedia.org/wiki/Operator_associativity

-}
type Associativity = Left | None | Right



-- DECODE


{-| Decode the JSON documentation produced by `elm-make` for an individual
module. The documentation for a whole package is an array of module docs,
so you may need to say `(Decode.list Docs.decoder)` depending on what you
want to do.
-}
decoder : Decoder Module
decoder =
  succeed Module
    & field "name" string
    & field "comment" string
    & field "aliases" (list aliasDecoder)
    & field "types" (list unionDecoder)
    & field "values" (list valueDecoder)


(&) : Decoder (a -> b) -> Decoder a -> Decoder b
(&) =
  map2 (<|)


aliasDecoder : Decoder Alias
aliasDecoder =
  succeed Alias
    & field "name" string
    & field "comment" string
    & field "args" (list string)
    & field "type" Type.decoder


unionDecoder : Decoder Union
unionDecoder =
  succeed Union
    & field "name" string
    & field "comment" string
    & field "args" (list string)
    & field "cases" (list tagDecoder)


tagDecoder : Decoder (String, List Type)
tagDecoder =
  succeed (,)
    & index 0 string
    & index 1 (list Type.decoder)



valueDecoder : Decoder Value
valueDecoder =
  succeed Value
    & nameDecoder
    & field "comment" string
    & field "type" Type.decoder


nameDecoder : Decoder Name
nameDecoder =
  oneOf
    [
      succeed Op
        & field "name" string
        & field "associativity" assocDecoder
        & field "precedence" int
    ,
      map Name (field "name" string)
    ]


assocDecoder : Decoder Associativity
assocDecoder =
  andThen toAssoc string


toAssoc : String -> Decoder Associativity
toAssoc str =
  case str of
    "left" ->
      succeed Left

    "non" ->
      succeed None

    "right" ->
      succeed Right

    _ ->
      fail "expecting one of the following values: left, non, right"
