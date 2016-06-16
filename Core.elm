module Core exposing (..)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Task exposing (Task)
import Http

-- TYPES
type alias Request a = Task Http.Error a
type alias ChampionList =
    { data: Dict String Champion
    , format: String
    , keys: Dict String String
    , typ: String -- originally type
    , version: String
    }


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


type alias Realm =
    { cdn: String
    , css: String
    , dd: String
    , l: String
    , lg: String
    , n: Dict String String
    , profileiconmax: Int
    , store: String
    , v: String
    }

 
-- CONSTANTS

ddragon : String
ddragon = "http://ddragon.leagueoflegends.com/cdn"

-- for testing:

emptyChampionList : ChampionList
emptyChampionList = ChampionList Dict.empty "" Dict.empty "" ""

emptyRecommended: Recommended
emptyRecommended = Recommended [] "" "" "" False "" ""

emptyBlock: Block
emptyBlock = Block [] False ""

emptyItem: BlockItem
emptyItem = BlockItem 0 0

emptySpell: ChampionSpell
emptySpell = ChampionSpell [] [] "" [] "" "" "" [] [] emptyImage "" emptyLevelTip 0 "" (Err "") "" "" "" "" "" []

emptyLevelTip: LevelTip
emptyLevelTip = LevelTip [] []

emptyImage: Image
emptyImage = Image "" "" 0 "" 0 0 0

emptyInfo : Info
emptyInfo = Info 0 0 0 0

emptyPassive : Passive
emptyPassive = Passive "" emptyImage "" ""

emptyStats: Stats
emptyStats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

emptyChampion : Champion
emptyChampion = 
    Champion [] "" [] 34 emptyImage emptyInfo "" "" "" "" emptyPassive [] [] [] emptyStats [] ""

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
    <+> oneOf ["recMath" := bool, succeed False] -- optional
    <+> oneOf ["type" := string, succeed ""] -- optional

spellVars : Decoder SpellVars
spellVars = 
    SpellVars
    <$> "coeff" := list float
    <+> oneOf ["dyn" := string, succeed ""] -- optional
    <+> "key" := string
    <+> "link" := string
    <+> oneOf ["ranksWith" := string, succeed ""] -- optional


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
    <+>  oneOf ["priority" := bool, succeed False] -- not always included, default to False
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
    <$> oneOf ["altimages" := list image, succeed []] -- optional
    <+> "cooldown" := list float
    <+> "cooldownBurn" := string
    <+> "cost" := list int
    <+> "costBurn" := string
    <+> "costType" := string
    <+> "description" := string
    <+> "effect" := list (oneOf [null [-1], (list float)])  -- 1-indexed arrays, ignore the first null and insert -1
    <+> "effectBurn" := list string
    <+> "image" := image
    <+> "key" := string
    <+> "leveltip" := levelTip
    <+> "maxrank" := int
    <+> "name" := string
    <+> "range" := oneOf [Err <$> string, Ok <$> list int]
    <+> "rangeBurn" := string
    <+> oneOf ["resource" := string, succeed ""] -- optional
    <+> "sanitizedDescription" := string
    <+> "sanitizedTooltip" := string
    <+> "tooltip" := string
    <+> oneOf ["vars" := list spellVars, succeed []] -- optional


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


championList : Decoder ChampionList
championList = 
    ChampionList
    <$> "data" := dict champion
    <+> "format" := string 
    <+> "keys" := dict string 
    <+> "type" := string 
    <+> "version" := string 


realm: Decoder Realm
realm =
    Realm
    <$> "cdn" := string 
    <+> "css" := string 
    <+> "dd" := string 
    <+> "l" := string 
    <+> "lg" := string 
    <+> "n" := dict string 
    <+> "profileiconmax" := int 
    <+> oneOf ["store" := string, succeed ""] -- optional 
    <+> "v" := string
    