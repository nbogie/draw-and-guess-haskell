{-# LANGUAGE DeriveDataTypeable #-}
module Messages where

import Data.Typeable
import Data.Data
import Text.JSON
import Text.JSON.Generic (toJSON)
import Text.HTML.TagSoup (escapeHTML)
import Types
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

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
teamsMsgToJSON :: Teams -> HToNameMap -> String
teamsMsgToJSON tms nMap = let tm = TeamsMsg [ makeTeamMsg (team1 tms) 0 nMap, makeTeamMsg (team2 tms) 1 nMap ]
                          in encode $ toJSON tm

makeTeamMsg :: Team -> Integer -> HToNameMap -> TeamMsg
makeTeamMsg t i nMap = TeamMsg i (map hToHTML (teamMembers t)) (score t)
  where hToHTML h = escapeHTML $ fromMaybe "anonymous" $ M.lookup (show h) nMap
