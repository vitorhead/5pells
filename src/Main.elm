module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Parsers.Json exposing (..)
import Json.Decode as Decode exposing (..)
import Http exposing (..)
import Set exposing (..)

type Class = 
      Cleric
    | Druid
    | Ranger
    | Sorcerer
    | Warlock
    | Wizard
    | Bard 
    | Paladin

type Msg
    = ShowSpells (Result Http.Error (List Spell))
    | ChangeClass Class
    | SelectSpell Spell

    
type alias Model =
    { 
        texto : String
    ,   spells : List Spell
    ,   currentClass : Class
    ,   selectedSpell : Spell
    }

init : () -> (Model, Cmd Msg)
init _ = 
    ( Model "oi" [] Druid initSpell
    , getSpells )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        ChangeClass class -> ({ model | currentClass = class, selectedSpell = initSpell }, Cmd.none)

        SelectSpell spell -> ({ model | selectedSpell = spell}, Cmd.none)

        ShowSpells result ->
            case result of
                Err e -> ({ model | texto = "ERRO!! " }, Cmd.none)

                Ok data -> ({ model | spells = data }, Cmd.none) 


view : Model -> Html Msg
view model = 
    let
        classSpells = getSpellsOf (classToStr model.currentClass) model.spells
        classLevels = List.map (\s -> s.level) classSpells
    in      
        div [] [
          navClasses model
        , br [] []
        , focusedSpell model.selectedSpell
        --, div [] (List.map (\level -> spellsList level model.currentClass model.spells) ["cantrip", "1", "2", "3", "4", "5", "6", "7", "8", "9"]) 
        , div [] (List.map (\level -> spellsList level model.currentClass classSpells) (Set.toList <| Set.fromList classLevels)) 
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

initSpell : Spell
initSpell =
    let
        initComponent = Components False "" False False 
    in
        Spell "" [] initComponent "" "" "" "" "" False "" [] ""

spellsUrl : String
spellsUrl = "./spells.json"

getSpells : Cmd Msg
getSpells = 
    --Http.send ShowSpells (Http.get spellsUrl (Decode.list decodeSpell))
    Http.get 
        { url = spellsUrl
        , expect = Http.expectJson ShowSpells (Decode.list decodeSpell)}


classToStr : Class -> String
classToStr class =
    case class of
      Cleric -> "cleric"
      Druid -> "druid"
      Ranger -> "ranger"
      Sorcerer -> "sorcerer"
      Warlock -> "warlock"
      Wizard -> "wizard"
      Bard  -> "bard"
      Paladin -> "paladin"

getSpellsOf : String -> List Spell -> List Spell
getSpellsOf className spells =
    List.filter (\spell -> List.member className spell.classes) spells

focusedSpell : Spell -> Html Msg
focusedSpell spell = 
    div [ class "container" ]
    [ div [ class "panel panel-default" ]
        [ div [ class "panel-heading" ]
            [ h3 [] [text <| spell.name] ]
        , div [ class "panel-body" ]
            [ p []
                [ text <| spell.description ]
            , p [class "text-secondary"]
                [ text <| String.toLower spell.duration ++ " - " ++ String.toLower spell.range]
            ]
        ]
    ]

spellListElement : Spell -> Html Msg
spellListElement spell =
    li [ class "lispell", onClick <| SelectSpell spell]
        [ text <| spell.name ]

spellsContainerLvl : List Spell -> Html Msg
spellsContainerLvl spells = 
    let
        first = List.head spells
        level = 
            case first of
                Just s -> s.level
                Nothing -> ""
        formatLevel = 
            case level of
                "cantrip" -> String.toUpper level
                _ -> "LEVEL " ++ level

    in       
        div [ class "" ]
            [ h5 [ class "" ]
                [ text formatLevel]
            , ul [] (List.map spellListElement spells)
            ]

spellsList : String -> Class -> List Spell -> Html Msg
spellsList level currClass spells = 
    let
        currSpellsLvl = List.filter (\s -> s.level == level) spells
    in
        div [ class "d-inline-flex" ]
        [ div [ class "p-2 m-5" ]
            [spellsContainerLvl currSpellsLvl]
        ]

classNavItemElement : Class -> Html Msg
classNavItemElement c =
    li [ class "nav-item" ] [ a [ class "nav-link", onClick <| ChangeClass c ] [ text <| classToStr <| c ] ]
    

navClasses : Model -> Html Msg
navClasses model = 
    nav [ class "navbar navbar-expand-lg navbar-light bg-light" ]
        [ a [ class "navbar-brand" ]
            [ h1 [ class "display-4" ]
                [ text <| String.toUpper <| classToStr <| model.currentClass ]
            ]
        , div [ class "collapse navbar-collapse", id "navbarNav" ]
            [ ul [ class "navbar-nav" ]
                (List.map classNavItemElement [Cleric, Druid, Ranger, Bard, Sorcerer, Wizard, Warlock, Paladin])
                
            ]
        ]




