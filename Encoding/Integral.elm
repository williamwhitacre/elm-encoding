module Encoding.Integral
  ( hexDigits
  , octDigits
  , binDigits
  , decDigits

  , encodeBasePadded
  , encodeBase

  , encodeHexPadded
  , encodeOctPadded
  , encodeBinPadded
  , encodeDecPadded

  , encodeHex
  , encodeOct
  , encodeBin
  , encodeDec

  , decodeBase

  , decodeHex
  , decodeOct
  , decodeBin
  , decodeDec

  , digitChars
  , hexChars
  , octChars
  , binChars
  , decChars

  , regexSourceBase
  , regexSourceHex
  , regexSourceOct
  , regexSourceBin
  , regexSourceDec

  , regexBase
  , regexHex
  , regexOct
  , regexBin
  , regexDec
  ) where

{-| Encoding and decoding facilities for integral integrals. This is mostly intended for machine
encodings, so there are no frills in place for pretty printing and the like. This is used by the
Encoding.URL module.

# Digits configurations.
@docs hexDigits, octDigits, binDigits, decDigits

# Arbitrary Encoding
@docs encodeBasePadded, encodeBase

# Encode With Padding
@docs encodeHexPadded, encodeOctPadded, encodeBinPadded, encodeDecPadded

# Encode
@docs encodeHex, encodeOct, encodeBin, encodeDec

# Arbitrary Decoding
@docs decodeBase

# Decode
@docs decodeHex, decodeOct, decodeBin, decodeDec

# Character Groups
@docs digitChars, hexChars, octChars, binChars, decChars

# Regex
@docs regexSourceBase, regexSourceHex, regexSourceOct, regexSourceBin, regexSourceDec, regexBase, regexHex, regexOct, regexBin, regexDec
-}

import Char
import String
import Array exposing (Array)
import Regex exposing (Regex)
import Set exposing (Set)


zeroDigit_ =
  List.map (String.uncons >> Maybe.map fst)
  >> Maybe.oneOf
  >> Maybe.withDefault '?'


ithDigit_ i =
  List.map (String.dropLeft i >> String.uncons >> Maybe.map fst)
  >> Maybe.oneOf
  >> Maybe.withDefault '?'


digitValue_ digit =
  List.map (String.indices (String.fromChar digit) >> List.head)
  >> Maybe.oneOf


digitsBase_ =
  List.foldl
    (\digits mn ->
      let
        len_this = String.length digits

      in
        case mn of
          Nothing -> Just len_this
          Just prior ->
            if prior /= len_this then
              Debug.crash "Digits cannot be defined by a list of strings with different lengths."
            else
              Just prior)
    Nothing
  >> \mn -> case mn of
    Nothing -> Debug.crash "No digits specified!"
    Just mn -> mn


encodeBase_inner negation digits base padto num =
  if num == 0 then
    zeroDigit_ digits
    |> String.fromChar
  else if num < 0 then
    String.append negation (encodeBase_init padto digits base -num)
  else
    encodeBase_init padto digits base num



encodeBase_init minlen = encodeBase_ "" (max minlen 1)


encodeBase_ accum remlen digits base num =
  let zero = zeroDigit_ digits in

  if num /= 0 then
    let
      place = num % base
      remaining = num // base

      cons' =
        String.cons (ithDigit_ place digits)

    in
      encodeBase_ (cons' accum) (remlen - 1) digits base remaining
  else if remlen > 0 then
    encodeBase_ (String.cons zero accum) (remlen - 1) digits base 0
  else
    accum


{-| Digits configuration for hexadecimal integrals. -}
hexDigits : List String
hexDigits = ["0123456789ABCDEF", "0123456789abcdef"]

{-| Digits configuration for octal integrals. -}
octDigits : List String
octDigits = ["01234567"]

{-| Digits configuration for binary integrals. -}
binDigits : List String
binDigits = ["01"]

{-| Digits configuration for decimal integrals. -}
decDigits : List String
decDigits = ["0123456789"]


{-| Encode using a given digits configuration and minimum length padding. The remaining digits will
be taken up by zeroes. -}
encodeBasePadded : List String -> Int -> number -> String
encodeBasePadded digits padto num =
  encodeBase_inner "-" digits (digitsBase_ digits) padto num


{-| Encode using a given digits configuration without padding. -}
encodeBase : List String -> number -> String
encodeBase =
  flip encodeBasePadded 0


{-| Encode a hexadecimal integral with padding using a the default `hexDigits` configuration. -}
encodeHexPadded : Int -> number -> String
encodeHexPadded = encodeBasePadded hexDigits

{-| Encode an octal integral with padding using a the default `octDigits` configuration. -}
encodeOctPadded : Int -> number -> String
encodeOctPadded = encodeBasePadded octDigits

{-| Encode a binary integral with padding using a the default `binDigits` configuration. -}
encodeBinPadded : Int -> number -> String
encodeBinPadded = encodeBasePadded binDigits

{-| Encode a decimal integral with padding using a the default `decDigits` configuration. -}
encodeDecPadded : Int -> number -> String
encodeDecPadded = encodeBasePadded decDigits


{-| Encode a hexadecimal integral without padding using a the default `hexDigits` configuration. -}
encodeHex : number -> String
encodeHex = encodeBase hexDigits

{-| Encode an octal integral without padding using a the default `octDigits` configuration. -}
encodeOct : number -> String
encodeOct = encodeBase octDigits

{-| Encode a binary integral without padding using a the default `binDigits` configuration. -}
encodeBin : number -> String
encodeBin = encodeBase binDigits

{-| Encode a decimal integral without padding using a the default `decDigits` configuration. -}
encodeDec : number -> String
encodeDec = encodeBase decDigits


decodeBase_inner : String -> List String -> String -> Maybe Int
decodeBase_inner negation digits encoded =
  if String.startsWith negation encoded then
    String.dropLeft (String.length negation) encoded
    |> decodeBase_inner negation digits
    |> Maybe.map ((-) 0)
  else
    let
      base = digitsBase_ digits

    in
      String.foldl
        (\digit n -> digitValue_ digit digits
        |> Maybe.map2 (\n d -> base * n + d) n)
        (Just 0) encoded


{-| Decode a integral using a given digits configuration. -}
decodeBase : List String -> String -> Maybe Int
decodeBase = decodeBase_inner "-"


{-| Decode a hexadecimal encoded integral using the default `hexDigits` configuration. -}
decodeHex : String -> Maybe Int
decodeHex = decodeBase hexDigits

{-| Decode an octal encoded integral using the default `octDigits` configuration. -}
decodeOct : String -> Maybe Int
decodeOct = decodeBase octDigits

{-| Decode a binary encoded integral using the default `binDigits` configuration. -}
decodeBin : String -> Maybe Int
decodeBin = decodeBase binDigits

{-| Decode a decimal encoded integral using the default `decDigits` configuration. -}
decodeDec : String -> Maybe Int
decodeDec = decodeBase decDigits


{-| A list of the unique digit characters possible in an integral with the given digits configuration. -}
digitChars : List String -> List Char
digitChars digits =
  let
    base = digitsBase_ digits
    uniqueDigits = List.foldl (flip <| String.foldl Set.insert) Set.empty digits

  in
    Set.foldr (::) [] uniqueDigits


{-| List of characters appearing in hexadecimal numbers. -}
hexChars : List Char
hexChars = digitChars hexDigits

{-| List of characters appearing in octal numbers. -}
octChars : List Char
octChars = digitChars octDigits

{-| List of characters appearing in binary numbers. -}
binChars : List Char
binChars = digitChars binDigits

{-| List of characters appearing in decimal numbers. -}
decChars : List Char
decChars = digitChars decDigits


{-| Regex that matches an encoded integral using the given digits configuration. -}
regexSourceBase : List String -> String
regexSourceBase digits =
  String.join ""
    [ "-?["
    , List.foldr String.cons "" (digitChars digits)
      |> Regex.escape
    , "]+"
    ]


{-| MRegex that matches an encoded hexadecimal integral. -}
regexSourceHex : String
regexSourceHex = regexSourceBase hexDigits

{-| MRegex that matches an encoded octal integral. -}
regexSourceOct : String
regexSourceOct = regexSourceBase octDigits

{-| MRegex that matches an encoded binary integral. -}
regexSourceBin : String
regexSourceBin = regexSourceBase binDigits

{-| MRegex that matches an encoded decimal integral. -}
regexSourceDec : String
regexSourceDec = regexSourceBase decDigits



{-| Match an encoded integral using the given digits configuration. -}
regexBase : List String -> Regex
regexBase = regexSourceBase >> Regex.regex


{-| Match an encoded hexadecimal integral. -}
regexHex : Regex
regexHex = regexBase hexDigits

{-| Match an encoded octal integral. -}
regexOct : Regex
regexOct = regexBase octDigits

{-| Match an encoded binary integral. -}
regexBin : Regex
regexBin = regexBase binDigits

{-| Match an encoded decimal integral. -}
regexDec : Regex
regexDec = regexBase decDigits
