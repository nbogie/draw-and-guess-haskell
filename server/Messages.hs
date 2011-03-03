{-# LANGUAGE DeriveDataTypeable #-}
module Messages where

import Data.Typeable
import Data.Data
import Text.JSON
import Text.JSON.Generic (toJSON)

import Types

data TeamsMsg = TeamsMsg
     { mteams :: [TeamMsg] } deriving (Eq, Show, Typeable, Data)

data TeamMsg = TeamMsg { teamId :: Integer, 
                         members :: [String]
     } deriving (Eq, Show, Typeable, Data)

-- TODO ensure this makes safe json, and safe simple html
teamsMsgToJSON :: Teams -> String
teamsMsgToJSON tms = let tm = TeamsMsg [ TeamMsg 0 (map show (team1 tms))
                                        ,TeamMsg 1 (map show (team2 tms)) ]
                 in encode $ toJSON tm
 
