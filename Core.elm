module Core exposing (..)

import Json.Decode exposing (..)

-- TYPES

type alias Champion =
    { allytips: List String
    , blurb: String
    , enemytips: List String
    , id: Int
    , image: Image
    , info: Info
    , key: String
    , lore: String
    , name: String
    , partype: String
    , passive: Passive
    , recommended: List Recommended
    , skins: List Skin
    , spells: List ChampionSpell
    , stats: Stats
    , tags: List String
    , title: String
    }


type alias ChampionSpell =
    { altimages: List Image
    , cooldown: List Float
    , cooldownBurn: String
    , cost: List Int
    , costBurn: String
    , costType: String
    , description: String
    , effect: List (List Float)
    , effectBurn: List String
    , image: Image
    , key: String
    , leveltip: LevelTip
    , maxrank: Int
    , name: String
    , range: Result String (List Int) -- either List of Ints or "self"
    , rangeBurn: String
    , resource: String
    , sanitizedDescription: String
    , sanitizedTooltip: String
    , tooltip: String
    , vars: List SpellVars
    }


type alias Image =
    { full: String
    , group: String
    , h: Int
    , sprite: String
    , w: Int
    , x: Int
    , y: Int
    }


type alias Info = 
    { attack: Int
    , defense: Int
    , difficulty: Int
    , magic: Int
    }


type alias Passive =
    { description: String
    , image: Image
    , name: String
    , sanitizedDescription: String
    }


type alias Recommended =
    { blocks: List Block
    , champion: String
    , map: String
    , mode: String
    , priority: Bool
    , title: String
    , typ: String -- originally type
    }


type alias Skin =
    { id: Int
    , name: String
    , num: Int
    }


type alias Stats =
    { armor: Float
    , armorperlevel: Float
    , attackdamage: Float
    , attackdamageperlevel: Float
    , attackrange: Float
    , attackspeedoffset: Float
    , attackspeedperlevel: Float
    , crit: Float
    , critperlevel: Float
    , hp: Float
    , hpperlevel: Float
    , hpregen: Float
    , hpregenperlevel: Float
    , movespeed: Float
    , mp: Float
    , mpperlevel: Float
    , mpregen: Float
    , mpregenperlevel: Float
    , spellblock: Float
    , spellblockperlevel: Float
    }


type alias LevelTip =
    { effect: List String
    , label: List String
    }

type alias SpellVars =
    { coeff: List Float
    , dyn: String
    , key: String
    , link: String
    , ranksWith: String
    }


type alias Block = 
    { items: List BlockItem
    , recMath: Bool
    , typ: String -- originally type
    }



type alias BlockItem =
    { count: Int
    , id: Int
    }

-- for convenience:

infixl 4 <$>
(<$>) : (a -> b) -> Decoder a -> Decoder b
(<$>) = map

infixl 4 <+>
(<+>) : Decoder (a -> b) -> Decoder a -> Decoder b
(<+>) func val = object2 (<|) func val

-- PARSERS

blockItem : Decoder BlockItem
blockItem = 
    BlockItem 
    <$> "count" := int
    <+> "id" := int

block : Decoder Block
block =
    Block
    <$> "items" := list blockItem
    <+> "recMath" := bool
    <+> "type" := string

spellVars : Decoder SpellVars
spellVars = 
    SpellVars
    <$> "coeff" := list float
    <+> "dyn" := string
    <+> "key" := string
    <+> "link" := string
    <+> "ranksWith" := string


levelTip : Decoder LevelTip
levelTip =
    LevelTip
    <$> "effect":= list string
    <+> "label":= list string


stats : Decoder Stats
stats =
    Stats
    <$> "armor" := float
    <+> "armorperlevel" := float
    <+> "attackdamage" := float
    <+> "attackdamageperlevel" := float
    <+> "attackrange" := float
    <+> "attackspeedoffset" := float
    <+> "attackspeedperlevel" := float
    <+> "crit" := float
    <+> "critperlevel" := float
    <+> "hp" := float
    <+> "hpperlevel" := float
    <+> "hpregen" := float
    <+> "hpregenperlevel" := float
    <+> "movespeed" := float
    <+> "mp" := float
    <+> "mpperlevel" := float
    <+> "mpregen" := float
    <+> "mpregenperlevel" := float
    <+> "spellblock" := float
    <+> "spellblockperlevel" := float


skin : Decoder Skin
skin =
    Skin
    <$> "id" := int
    <+> "name" := string
    <+> "num" := int


recommended : Decoder Recommended
recommended = 
    Recommended
    <$> "blocks" := list block
    <+> "champion" := string
    <+> "map" := string
    <+> "mode" := string
    <+> "priority" := bool
    <+> "title" := string
    <+> "type" := string


passive : Decoder Passive
passive =
    Passive
    <$> "description" := string
    <+> "image" := image
    <+> "name" := string
    <+> "sanitizedDescription" := string


info : Decoder Info
info =
    Info
    <$> "attack" := int
    <+> "defense" := int
    <+> "difficulty" := int
    <+> "magic" := int


image : Decoder Image
image = 
    Image
    <$> "full" := string
    <+> "group" := string
    <+> "h" := int
    <+> "sprite" := string
    <+> "w" := int
    <+> "x" := int
    <+> "y" := int


championSpell : Decoder ChampionSpell
championSpell =
    ChampionSpell
    <$> "altimages" := list image
    <+> "cooldown" := list float
    <+> "cooldownBurn" := string
    <+> "cost" := list int
    <+> "costBurn" := string
    <+> "costType" := string
    <+> "description" := string
    <+> "effect" := list (list float)
    <+> "effectBurn" := list string
    <+> "image" := image
    <+> "key" := string
    <+> "leveltip" := levelTip
    <+> "maxrank" := int
    <+> "name" := string
    <+> "range" := (oneOf [Err <$> string, Ok <$> list int])
    <+> "rangeBurn" := string
    <+> "resource" := string
    <+> "sanitizedDescription" := string
    <+> "sanitizedTooltip" := string
    <+> "tooltip" := string
    <+> "vars" := list spellVars


champion : Decoder Champion
champion =
    Champion
    <$> "allytips" := list string
    <+> "blurb" := string 
    <+> "enemytips" := list string 
    <+> "id" := int 
    <+> "image" := image
    <+> "info" := info
    <+> "key" := string 
    <+> "lore" := string 
    <+> "name" := string 
    <+> "partype" := string 
    <+> "passive" := passive
    <+> "recommended" := list recommended
    <+> "skins" := list skin
    <+> "spells" := list championSpell
    <+> "stats" := stats
    <+> "tags" := list string 
    <+> "title" := string