module Teams where
import System.IO (Handle)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (delete, (\\))

import Types

inTeam :: Handle -> Team -> Bool
inTeam h t = h `elem` teamMembers t

teamFor :: Handle -> Teams -> Maybe Team
teamFor h ts | inTeam h (team1 ts) = Just $ team1 ts
teamFor h ts | inTeam h (team2 ts) = Just $ team2 ts
             | otherwise = Nothing

incScore :: Team -> Int -> Team
incScore t amt = t { score = score t + amt }

artists :: GameState -> [Handle]
artists gs = 
  let t1 = team1 $ teams gs
      t2 = team2 $ teams gs
  in mapMaybe artist [t1,t2] 

guessers :: GameState -> [Handle]
guessers gs = 
  let t1 = team1 $ teams gs
      t2 = team2 $ teams gs
      allMembers = concatMap teamMembers [t1,t2]
  in  allMembers \\ artists gs

addToTeams :: Handle -> Teams -> Teams
addToTeams h nowTeams@(Teams {team1=t1, team2=t2}) = 
  let l1 = length (teamMembers t1)
      l2 = length (teamMembers t2)
  -- We want p2 to join on same team as p1 to get play going fast.  
  -- Assignment will alternate thereafter.
  in if l1+l2 < 2 || l1 < l2  
       then nowTeams { team1 = addToTeam h t1 }
       else nowTeams { team2 = addToTeam h t2 }

addToTeam :: Handle -> Team -> Team
addToTeam h t = let ms = teamMembers t
                in t {teamMembers = ms++[h], -- new joiners are last to draw
                      artist=Just (fromMaybe h (artist t))}

removeFromTeams :: Teams -> Handle -> Teams
removeFromTeams (Teams {team1=t1, team2=t2}) h = 
  let t1' = removeFromTeam t1 h
      t2' = removeFromTeam t2 h
  in Teams { team1 = t1', team2 = t2' }

removeFromTeam :: Team -> Handle -> Team
removeFromTeam t h = 
   let ms = delete h (teamMembers t)
   in if Just h == artist t
        then t {teamMembers = ms, artist = if null ms then Nothing else Just (head ms) }
        else t { teamMembers = ms }

                           
cycleArtists :: Teams -> Teams
cycleArtists ts@(Teams {team1 = t1, team2 = t2}) = 
     ts{ team1 = cycleArtist t1, team2 = cycleArtist t2 }

cycleArtist :: Team -> Team
cycleArtist t@(Team {teamMembers=ms}) 
                      | null ms   = t
                      | otherwise = 
                          let ms' = tail ms ++ [head ms]
                          in t {teamMembers = ms', artist = Just (head ms')}

