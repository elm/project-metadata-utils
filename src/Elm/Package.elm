module Elm.Package exposing
  ( Name
  , toString
  , fromString
  , encode
  , decoder
  )


{-| Helpers for working with package name strings in `elm.json` files.

# Packages
@docs Name

# String Conversions
@docs toString, fromString

# JSON Conversions
@docs encode, decoder

-}


import Json.Decode as D
import Json.Encode as E



-- MODULE


{-| A guaranteed valid Elm package name.
-}
type Name =
  Name String String



-- STRINGS


{-| Convert a `Name` to a `String` that works in `elm.json`
-}
toString : Name -> String
toString (Name user project) =
  user ++ "/" ++ project


{-| Try to convert a `String` into a `Name`:

    fromString "elm-lang/core" == Just ...
    fromString "elm-lang/html" == Just ...
    fromString "elm_lang/html" == Nothing
    fromString "elm-lang"      == Nothing
    fromString "html"          == Nothing
-}
fromString : String -> Maybe Name
fromString string =
  case String.split "/" string of
    [user, project] ->
      if String.all isGood user && String.all isGood project then
        Just (Name user project)
      else
        Nothing

    _ ->
      Nothing


isGood : Char -> Bool
isGood char =
  Char.isAlpha char || char == '-'



-- JSON


{-| Turn a `Name` into a string for use in `elm.json`
-}
encode : Name -> E.Value
encode name =
  E.string (toString name)


{-| Decode the module name strings that appear in `elm.json`
-}
decoder : D.Decoder Name
decoder =
  D.andThen decoderHelp D.string


decoderHelp : String -> D.Decoder Name
decoderHelp string =
  case fromString string of
    Just name ->
      D.succeed name

    Nothing ->
      D.fail "I need a valid package name like \"elm-lang/core\""
