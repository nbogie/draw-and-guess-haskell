module Main where

import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
import System.IO (Handle, hClose)
import qualified Data.ByteString as B (null)
import qualified Data.ByteString.UTF8 as BU (fromString, toString) -- this is from utf8-string
import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Control.Concurrent.Chan
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (delete, isPrefixOf,(\\))
import qualified Data.Map as M
import Types
import Messages
import Text.HTML.TagSoup (escapeHTML)
import Data.Char (isAlphaNum, isAlpha, toLower)

-- Accepts clients, spawns a single handler for each one.
main :: IO ()
main = withSocketsDo $ do
  channel <- newChan
  ws <- fmap lines $ readFile "guesswords.txt"
  forkIO $ dispatcher channel [] $ initGameState ws
  let pNum = 12345
  socket <- listenOn (PortNumber pNum)
  putStrLn $ "Listening on port " ++ show pNum
  forever $ do
    (h, _, _) <- accept socket
    forkIO (welcome h channel)

data PlayState = AwaitingPlayers | ReadyToStart | InPlay deriving (Eq, Show)

data Event = Join Handle
             | SetNick Handle String
             | Draw Handle String
             | Message Handle String
             | Guess Handle String
             | Leave Handle
             | RoundStart
             deriving (Eq, Show)

data Role = Artist | Guesser deriving (Show, Eq)


--
-- Shakes hands with client. If no error, starts talking.
welcome :: Handle -> Chan Event -> IO ()
welcome h channel = do
  request <- shakeHands h
  case request of
    Left err -> print $ "ERR: " ++ show err
    Right  _ -> do
      writeChan channel $ Join h
      putFrame h (BU.fromString ("hello you are handle "++show h))
      putStrLn $ "Shook hands with "++show h ++" sent welcome message."
      talkLoop h channel

data GameState = GameState { playState::PlayState, teams::Teams, nameMap::HToNameMap, gWords::[String] } deriving (Show)

currentWord :: GameState -> String
currentWord g | null (gWords g) = error "No words in game state"
               |otherwise        = head $ gWords g

-- TODO: can we just use cycle words to make it an infinite stream?
initGameState :: [String] -> GameState
initGameState ws = GameState { teams = Teams {team1 = Team {teamMembers=[], artist = Nothing, score = 0}, 
                                              team2 = Team {teamMembers=[], artist = Nothing, score = 0} }
                             , gWords = ws 
                             , playState = AwaitingPlayers
                             , nameMap = M.empty }

addToTeams :: Teams -> Handle -> Teams
addToTeams nowTeams@(Teams {team1=t1, team2=t2}) h = do
  let l1 = length (teamMembers t1)
  let l2 = length (teamMembers t2)
  if l1 < l2
    then nowTeams { team1 = addToTeam t1 h }
    else nowTeams { team2 = addToTeam t2 h }

addToTeam :: Team -> Handle -> Team
addToTeam t h = let ms = teamMembers t
                    existingArtist = artist t
                    newArtist = if existingArtist==Nothing then Just h else existingArtist
                in t {teamMembers = h:ms, artist=newArtist} 

removeFromTeams :: Teams -> Handle -> Teams
removeFromTeams (Teams {team1=t1, team2=t2}) h = 
  let t1' = removeFromTeam t1 h
      t2' = removeFromTeam t2 h
  in Teams { team1 = t1', team2 = t2' }

removeFromTeam :: Team -> Handle -> Team
removeFromTeam t h = let ms = delete h (teamMembers t)
                     in if Just h == artist t
                          then t {teamMembers = ms, artist = if null ms then Nothing else Just (head ms) }
                          else t { teamMembers = ms }
                                       
dispatcher :: Chan Event -> [Handle] -> GameState -> IO ()
dispatcher channel handles gameState = do
  ev <- readChan channel 
  putStrLn $ "DISPATCHER readChan: " ++ show ev ++ ". play state is: "++ show (playState gameState) ++ " word is: "++currentWord gameState
  case ev of
    Join h -> 
      if h `elem` handles -- ignore this re-join from same handle, or better still, drop the handle.
        then
          do putStrLn "ignoring join because handle contains already"
             dispatcher channel handles gameState
        else do
          let newHandles = h:handles
          putStrLn $ "DISP: Join Event.  Handle: "++ show h
          let newTeams = addToTeams (teams gameState) h
          let gs' = gameState {teams = newTeams} 
          putStrLn $ "New Teams: " ++ show (teams gs')
          broadcast newHandles $ "Joined: " ++ show h
          broadcastTeamsAndState newHandles gs'
          gs'' <- if (playState gs' == AwaitingPlayers) && length newHandles > 1
                      then do 
                        writeChan channel RoundStart
                        return $ gs' {playState = ReadyToStart}
                      else return gs'
          dispatcher channel newHandles gs''
    Leave h -> do
      putStrLn "DISP: Leave Event"
      let newHandles = delete h handles
      let gs' = removeFromGame gameState h
      broadcastRaw newHandles $ teamsMsgToJSON (teams gs') (nameMap gs')
      dispatcher channel newHandles gs'
    SetNick h uname -> do
      putStrLn $ "DISP: handle "++show h ++" set nick to " ++ uname
      let nameMap' = M.insert (show h) uname $ nameMap gameState
      let gs' = gameState {nameMap = nameMap'}
      broadcastRaw handles $ teamsMsgToJSON (teams gs') nameMap'
      sendOne h $ "YOURNICK " ++ uname
      dispatcher channel handles gs' 
    Draw h msg -> do
      case teamFor h (teams gameState) of
        -- a handle should always be part of a team, 
        -- but maybe it has been removed (kicked?) since this msg was enqueued.
        Just t -> broadcast (teamMembers t) $ "DRAW " ++ msg
        Nothing -> return () 
      dispatcher channel handles gameState
    Message h msg -> do
      let uname = nameForHandle h (nameMap gameState)
      putStrLn $ "DISP: " ++ uname ++ " got message: " ++ escapeHTML msg ++ " from " ++ show h 
      broadcast handles $ uname ++ ": " ++ msg
      dispatcher channel handles gameState
    RoundStart -> do
      gs' <- case playState gameState of
        ReadyToStart -> do
          let g = cycleWord $ gameState {playState = InPlay
                             , teams = cycleArtists (teams gameState)}
          print $ "NEW WORD for round " ++ currentWord g
          broadcast handles "ROUNDSTART"
          broadcastTeamsAndState handles g
          return g
        _ -> do putStrLn "Round already started"
                return gameState
      dispatcher channel handles gs'
    Guess h guess -> do
      putStrLn $ "Got guess of " ++ guess ++ " from "++show h
      case playState gameState of
        InPlay -> 
             if map toLower guess == map toLower (currentWord gameState)
               then do 
                 putStrLn $ "Correct guess by "++ show h
                 let gs' = processCorrectGuess gameState h
                 broadcastRaw handles $ teamsMsgToJSON (teams gs') (nameMap gs') -- updates score
                 broadcast handles $ "GUESS CORRECT " ++ nameForHandle h (nameMap gs') ++ " " ++ guess
                 sendOne h "CORRECT_GUESS_BY_YOU"
                 writeChan channel RoundStart
                 dispatcher channel handles gs'
               else do
                 putStrLn "wrong guess"
                 broadcast handles $ "GUESS WRONG " ++ nameForHandle h (nameMap gameState) ++ " " ++ guess
                 dispatcher channel handles gameState
        _        -> do 
                      putStrLn "Not currently accepting guesses"
                      dispatcher channel handles gameState

broadcastTeamsAndState :: [Handle] -> GameState -> IO ()
broadcastTeamsAndState hs g = do
          broadcastRaw hs $ teamsMsgToJSON (teams g) (nameMap g)
          broadcast (artists g) $ "ROLE artist WORD " ++ currentWord g
          broadcast (guessers g) "ROLE guesser"
          broadcast hs $ "STATE "++ show (playState g)

cycleWord :: GameState -> GameState
cycleWord g = g { gWords = ws ++ [w] }
                where (w:ws) = gWords g
              

cycleArtists :: Teams -> Teams
cycleArtists ts@(Teams {team1 = t1, team2 = t2}) = ts{ team1 = cycleArtist t1, team2 = cycleArtist t2 }

cycleArtist :: Team -> Team
cycleArtist t@(Team {artist=aMaybe, teamMembers=ms}) | null ms   = t
                                                     | otherwise = let ms' = tail ms ++ [head ms]
                                                                   in t {teamMembers = ms', artist = Just (head ms')}

nameForHandle :: Handle -> HToNameMap -> String
nameForHandle h nmap = escapeHTML $ fromMaybe "Anonymous" (M.lookup (show h) nmap)

removeFromGame :: GameState -> Handle -> GameState
removeFromGame gs h =  let newTeams = removeFromTeams (teams gs) h
                           newNameMap = M.delete (show h) (nameMap gs)
                       in gs { teams = newTeams, nameMap = newNameMap } 
                           
processCorrectGuess :: GameState -> Handle -> GameState
processCorrectGuess gs guesserH = 
  let t1 = team1 $ teams gs
      t2 = team2 $ teams gs
      t1' = if inTeam guesserH t1 then incScore t1 1 else t1
      t2' = if inTeam guesserH t2 then incScore t2 1 else t2
      newTeams = (teams gs) {team1 = t1', team2 = t2'}
  in gs {playState = ReadyToStart, teams = newTeams}

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

-- guessers :: GameState -> [Handle]
    
-- broadcast escapes html chars first.  if you don't want this, write broadcastRaw.
-- broadcast :: [Handle] -> String -> IO ()
-- TODO: handle case that the handle is closed (putFrame to a closed handle will kill the thread).
broadcast handles msg = mapM_ (\h -> sendOne h msg) handles 
broadcastRaw handles msg = mapM_ (\h -> sendOneRaw h msg) handles 

-- sendOne escapes html chars first.  if you don't want this, write broadcastRaw.
sendOne :: Handle -> String -> IO ()
sendOne handle msg = putFrame handle (BU.fromString (escapeHTML msg))
sendOneRaw handle msg = putFrame handle (BU.fromString msg)

-- Talks to the client (by echoing messages back) until EOF.
talkLoop :: Handle -> Chan Event -> IO ()
talkLoop h channel = do
  msgB <- getFrame h
  if B.null msgB
    then do
      putStrLn "EOF encountered. Closing handle."
      hClose h
      writeChan channel $ Leave h
    else do
      let msg = escapeHTML $ BU.toString msgB -- TODO: by going via String we'll unfortunately kill any invalid chars, where we'd prefer to leave the msg untouched.
      let ev | "NICK: "  `isPrefixOf` msg = SetNick h $ nickFromMsg msg
             | "GUESS: " `isPrefixOf` msg = guessFromMsg h msg
             | "DRAW: " `isPrefixOf` msg = Draw h (drop (length "DRAW: ") msg)
             | otherwise                  = Message h msg
      putStrLn $ "talkLoop: got message " ++ msg ++ " parsed as: "++show ev
      writeChan channel ev
      putFrame h $ BU.fromString "ok"
      talkLoop h channel

guessFromMsg h msg = let raw = drop (length "GUESS: ") msg
                         stripped = takeWhile isAlpha raw
                         final = if null stripped then "x" else stripped
                     in Guess h final

nickFromMsg msg = let raw = drop (length "NICK: ") msg
                      stripped = takeWhile isAlphaNum raw
                  in  if length stripped < 1 then "Anonymous" else stripped
