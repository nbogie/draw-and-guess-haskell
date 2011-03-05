{-# LANGUAGE DeriveDataTypeable #-}
module Messages where

import Data.Typeable
import Data.Data
import Text.JSON
import Text.JSON.Generic (toJSON)
import Text.HTML.TagSoup (escapeHTML)
import Types
-- These data types are only a quick way of getting conversion to json.
-- The field names are part of the external interface (via json)
-- which is just wrong (fragile), but it is a quick start.
data TeamsMsg = TeamsMsg
     { mteams :: [TeamMsg] } deriving (Eq, Show, Typeable, Data)

data TeamMsg = TeamMsg { teamId :: Integer, 
                         members :: [String], 
                         mscore :: Int
     } deriving (Eq, Show, Typeable, Data)

-- TODO ensure this makes safe json, and safe simple html
teamsMsgToJSON :: Teams -> String
teamsMsgToJSON tms = let tm = TeamsMsg [ makeTeamMsg (team1 tms) 0, makeTeamMsg (team2 tms) 1 ]
                     in encode $ toJSON tm

makeTeamMsg :: Team -> Integer -> TeamMsg
makeTeamMsg t i = TeamMsg i (map (escapeHTML . show) (teamMembers t)) (score t)
