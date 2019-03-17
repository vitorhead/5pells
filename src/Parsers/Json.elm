module Parsers.Json exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

type alias Components = 
    { material : Bool
    , raw : String
    , somatic : Bool
    , verbal : Bool
    }

type alias Spell = 
    { castingTime : String
    , classes : List String
    , components : Components
    , description : String
    , duration : String
    , level : String
    , name : String
    , range : String
    , ritual : Bool
    , school : String
    , tags : List String
    , spelltype : String
    }

-- TODO Decode other fields
decodeComponents : Decode.Decoder Components
decodeComponents = 
    Decode.succeed Components
        |> required "material" Decode.bool 
        |> optional "raw" Decode.string ""
        |> required "somatic" Decode.bool 
        |> required "verbal" Decode.bool

decodeSpell : Decode.Decoder Spell
decodeSpell =
    Decode.succeed Spell 
        |> required "casting_time" Decode.string
        |> required "classes" (Decode.list Decode.string)
        |> custom (Decode.at ["components"] decodeComponents) 
        |> required "description" Decode.string
        |> required "duration" Decode.string
        |> required "level" Decode.string
        |> required "name" Decode.string
        |> required "range" Decode.string
        |> required "ritual" Decode.bool
        |> required "school" Decode.string
        |> required "tags" (Decode.list Decode.string)
        |> required "type" Decode.string
    