module Types where
import System.IO (Handle)
import qualified Data.Map as M

data Team = Team { teamMembers::[Handle], 
                   artist :: Maybe Handle, 
                   score::Int} deriving (Show, Eq)

data Teams = Teams {team1::Team, team2::Team} deriving (Show)

type HandleStr = String
type HToNameMap = M.Map HandleStr String
