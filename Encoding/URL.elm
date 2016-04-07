module Encoding.URL
  ( encode
  , decode
  ) where

{-| Support for URL encoding and decoding.

# Standard URL Encoding
@docs encode, decode
-}


import Encoding.Integral

import Combine exposing (andThen)
import Combine.Char

import String
import Char


{-| Proper URL encoding. -}
encode : String -> String
encode =
  crashOnFailParser_ "URL encoding should never fail" combinatorEncodeURL_


{-| Proper URL decoding. -}
decode : String -> String
decode =
  crashOnFailParser_ "URL decoding should never fail" combinatorDecodeURL_


crashOnFailParser_ msg f =
  Combine.parse f
  >> (\(result, _) ->
      case result of
        Result.Ok output -> output
        Result.Err err ->
          String.join "" [msg, ": ", toString err]
          |> Debug.crash)


encodeHexes_ =
  Combine.regex "[^\\w ]+"
    `andThen` \s ->
      String.foldr
        (\c ls -> Char.toCode c
        |> Encoding.Integral.encodeHexPadded 2
        |> (++) "%"
        |> flip (::) ls)
        [] s
      |> String.join ""
      |> Combine.succeed


decodeHex_ =
  Combine.Char.char '%'
    `andThen` \_ -> Combine.count 2 (Combine.Char.oneOf Encoding.Integral.hexChars)
    `andThen` (List.map String.fromChar >> String.join "" >> Combine.succeed)
    `andThen`
      (Encoding.Integral.decodeHex
      >> Maybe.map (Char.fromCode >> String.fromChar >> Combine.succeed)
      >> Maybe.withDefault (Combine.fail ["Failed to decode code in URL!"]))


combinatorEncodeURL_ =
  Combine.many
    ( Combine.choice
        [ Combine.Char.newline `andThen` (always "\r\n" >> Combine.succeed)
        , encodeHexes_
        , Combine.Char.space `andThen` (always "+" >> Combine.succeed)
        , Combine.Char.anyChar `andThen` (String.fromChar >> Combine.succeed)
        ]
    )
  `andThen`
    (String.join "" >> Combine.succeed)


combinatorDecodeURL_ =
  Combine.many
    ( Combine.choice
        [ Combine.Char.char '+' `andThen` (always " " >> Combine.succeed)
        , decodeHex_
        , Combine.string "\r\n" `andThen` (always "\n" >> Combine.succeed)
        , Combine.Char.anyChar `andThen` (String.fromChar >> Combine.succeed)
        ]
    )
  `andThen`
    (String.join "" >> Combine.succeed)
