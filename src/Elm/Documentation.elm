module Elm.Documentation exposing
  ( Documentation
  , decoder
  )


import Json.Decode exposing (..)

import Elm.Documentation.Type as Type exposing (Type)



-- DOCUMENTATION


type alias Documentation =
  { name : String
  , comment : String
  , aliases : List Alias
  , unions : List Union
  , values : List Value
  }


type alias Alias =
  { name : String
  , comment : String
  , args : List String
  , tipe : Type
  }


type alias Union =
  { name : String
  , comment : String
  , args : List String
  , tags : List (String, List Type)
  }


type Value =
  { name : Name
  , comment : String
  , tipe : Type
  }


type Name
  = Name String
  | Op String Associativity Int


type Associativity = Left | None | Right



-- DECODE


decoder : Decoder Documentation
decoder =
  succeed Documentation
    & field "name" string
    & field "comment" string
    & field "aliases" (list aliasDecoder)
    & field "unions" (list unionDecoder)
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
      Left

    "non" ->
      None

    "right" ->
      Right
