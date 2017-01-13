module Elm.Documentation.Type exposing
  ( Type(..)
  , decoder
  )

import Char
import Parser exposing (Parser, (|=), (|.))
import Json.Decode as Decode exposing (Decoder)
import String



-- TYPES


type Type
  = Var String
  | Lambda Type Type
  | Tuple (List Type)
  | Type String (List Type)
  | Record (List (String, Type)) (Maybe Type)



-- DECODE


decoder : Decoder Type
decoder =
  Decode.andThen decoderHelp Decode.string


decoderHelp : String -> Decoder Type
decoderHelp string =
  case parse string of
    Err error ->
      Decode.fail (Debug.crash "TODO")

    Ok tipe ->
      Decode.succeed tipe



-- PARSE TYPES


parse : String -> Result Parser.Error Type
parse source =
  Parser.run tipe source



-- FUNCTIONS


tipe : Parser Type
tipe =
  Parser.lazy <| \_ ->
    Parser.oneOrMore arrow tipeTerm


tipeTerm : Parser Type
tipeTerm =
  Parser.oneOf
    [ Parser.map Var lowVar
    , Parser.succeed Type
        |= qualifiedCapVar
        |= Parser.zeroOrMore _ term
    , record
    , tuple
    ]


term : Parser Type
term =
  Parser.oneOf
    [ Parser.map Var lowVar
    , Parser.map (flip Type []) qualifiedCapVar
    , record
    , tuple
    ]



-- RECORDS


record : Parser Type
record =
  Parser.succeed (flip Record)
    |. Parser.string "{"
    |. spaces
    |= extension
    |= Parser.zeroOrMore comma field
    |. spaces
    |. Parser.string "}"


extension : Parser (Maybe String)
extension =
  oneOf
    [ Parser.try <|
        Parser.succeed Just
          |= lowVar
          |. spaces
          |. Parser.string "|"
          |. spaces
    , Parser.succeed Nothing
    ]


field : Parser (String, Type)
field =
  Parser.succeed (,)
    |= lowVar
    |. spaces
    |. Parser.string ":"
    |. spaces
    |= tipe



-- TUPLE


tuple : Parser Type
tuple =
  Parser.succeed tuplize
    |. string "("
    |. spaces
    |= Parser.zeroOrMore comma tipe
    |. spaces
    |. string ")"


tuplize : List Type -> Type
tuplize args =
  case args of
    [tipe] ->
      tipe

    _ ->
      Tuple args



-- VAR HELPERS


lowVar : Parser Type
lowVar =
  Parser.variable Char.isLower isInnerVarChar


capVar : Parser String
capVar =
  Parser.variable Char.isUpper isInnerVarChar


isInnerVarChar : Char -> Bool
isInnerVarChar char =
  Char.isLower char
  || Char.isUpper char
  || Char.isDigit char
  || char == '_'


qualifiedCapVar : Parser String
qualifiedCapVar =
  Parser.map (String.join ".") <|
    Parser.oneOrMore dot capVar


dot : Parser.Separator
dot =
  { before = Parser.succeed ()
  , separator = Parser.string "."
  , after = Parser.succeed ()
  }



-- SPACE HELPERS


spaces : Parser ()
spaces =
  Parser.ignoreWhile (\char -> char == ' ')


comma : Parser.Separator
comma =
  { before = space
  , separator = Parser.string ","
  , after = space
  }


arrow : Parser.Separator
arrow =
  { before = space
  , separator = Parser.string "->"
  , after = space
  }
