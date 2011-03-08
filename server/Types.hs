module Types where
import System.IO (Handle)
import qualified Data.Map as M

data Event = Join Handle
             | SetNick Handle String
             | Draw Handle String
             | Message Handle String
             | Guess Handle String
             | Leave Handle
             | RoundStart
             deriving (Eq, Show)

data Role = Artist | Guesser deriving (Show, Eq)

data PlayState = AwaitingPlayers | ReadyToStart | InPlay deriving (Eq, Show)

data GameState = GameState { playState::PlayState, teams::Teams, 
                             nameMap::HToNameMap, gWords::[String] } deriving (Show)


data Team = Team { teamMembers::[Handle], 
                   artist :: Maybe Handle, 
                   score::Int} deriving (Show, Eq)

data Teams = Teams {team1::Team, team2::Team} deriving (Show)

type HandleStr = String
type HToNameMap = M.Map HandleStr String
