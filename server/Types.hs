module Types where
import System.IO (Handle)

type Team = [Handle]
data Teams = Teams {team1::Team, team2::Team} deriving (Show)
