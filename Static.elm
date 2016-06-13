module Static exposing(..)

version: String
version = "v1.2"

-- TYPES

type Champion = Champion
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


type ChampionSpell = ChampionSpell
    { altimages: List Image
    , cooldown: List Float
    , cooldownBurn: String
    , cost: List Int
    , costBurn: String
    , costType: String
    , description: String
    , effect: List List Float
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
    , tooltip: String
    , vars: List SpellVars
    }


type Image = Image
    { full: String
    , group: String
    , h: Int
    , sprite: String
    , w: Int
    , x: Int
    , y: Int
    }


type Info = Info
    { attack: Int
    , defense: Int
    , difficulty: Int
    , magic: Int
    }


type Passive = Passive
    { description: String
    , image: Image
    , name: String
    , sanitizedDescription: String
    }


type Recommended = Recommended
    { blocks: List Block
    , champion: String
    , map: String
    , mode: String
    , priority: Bool
    , title: String
    , typ: String -- originally type
    }


type Skin = Skin
    { id: Int
    , name: String
    , num: Int
    }


type Stats = Stats
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


type LevelTip = LevelTip
    { effect: List String
    , label: List String
    }

type SpellVars = SpellVars
    { coeff: List Float
    , dyn: String
    , key: String
    , link: String
    , ranksWith: String
    }


type Block = Block
    { count: Int
    , id: Int
    }
