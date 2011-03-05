module Main where

import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
import System.IO (Handle, hClose)
import qualified Data.ByteString as B (null)
import qualified Data.ByteString.UTF8 as BU (fromString, toString) -- this is from utf8-string
import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Control.Concurrent.Chan
import Data.Maybe (mapMaybe)
import Data.List (delete, isPrefixOf,(\\))

import Types
import Messages

-- Accepts clients, spawns a single handler for each one.
main :: IO ()
main = withSocketsDo $ do
  channel <- newChan
  ws <- fmap lines $ readFile "guesswords.txt"
  forkIO $ dispatcher channel [] $ initGameState ws
  socket <- listenOn (PortNumber 12345)
  putStrLn "Listening on port 12345."
  forever $ do
    (h, _, _) <- accept socket
    forkIO (welcome h channel)

data PlayState = AwaitingPlayers | ReadyToStart | InPlay deriving (Eq, Show)

data User = User String deriving (Eq, Ord, Show)
data Event = Join Handle
             | SetNick Handle User
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

data GameState = GameState { playState::PlayState, teams::Teams, currentWord::String, gWords::[String] } deriving (Show)

-- TODO: can we just use cycle words to make it an infinite stream?
initGameState :: [String] -> GameState
initGameState ws = GameState { teams = Teams {team1 = Team {teamMembers=[], artist = Nothing, score = 0}, 
                                              team2 = Team {teamMembers=[], artist = Nothing, score = 0} }
                             , gWords = ws 
                             , currentWord = head ws
                             , playState = AwaitingPlayers }

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
                          then t {teamMembers = ms, artist = Nothing }
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
          putStrLn $ "New Teams: " ++ show newTeams
          broadcast newHandles $ "Joined: " ++  show h
          putStrLn $ "Will broadcast " ++ teamsMsgToJSON newTeams
          broadcast newHandles $ teamsMsgToJSON newTeams
          sendOne h $ "STATE: " ++ show (playState gs')
          gs'' <- if (playState gs' == AwaitingPlayers) && length newHandles > 1
                      then do 
                        writeChan channel RoundStart
                        return $ gs' {playState = ReadyToStart}
                      else return gs'
          dispatcher channel newHandles gs''
    Leave h -> do
      putStrLn "DISP: Leave Event"
      let newHandles = delete h handles
      let newTeams = removeFromTeams (teams gameState) h
      let gs' = gameState { teams = newTeams } 
      broadcast newHandles "left"
      dispatcher channel newHandles gs'
    SetNick h (User uname) -> do
      putStrLn $ "DISP: User " ++ uname ++ " set nick"
      broadcast handles $ "NICK SET: " ++ uname
      dispatcher channel handles gameState
    Message h msg -> do
      let uname = "unknown" --lookup n htoNameMap
      putStrLn $ "DISP: " ++ uname ++ " got message: " ++ msg ++ " from " ++ show h 
      broadcast handles $ uname ++ ": " ++ msg
      dispatcher channel handles gameState
    RoundStart -> do
      gs' <- case playState gameState of
        ReadyToStart -> do
          let g = gameState {playState = InPlay, 
                             currentWord = head (gWords gameState), 
                             gWords = tail (gWords gameState) ++ [currentWord gameState]}
          broadcast handles $ "STATE: "++ show (playState g)
          broadcast handles "ROUNDSTART"
          broadcast (artists g) $ "ROLE artist WORD " ++ currentWord g
          broadcast (guessers g) "ROLE guesser"
          return g
        _ -> do putStrLn "Round already started"
                return gameState
      dispatcher channel handles gs'
    Guess h guess -> do
      putStrLn $ "Got guess of " ++ guess ++ " from "++show h
      case playState gameState of
        InPlay -> 
             if guess == currentWord gameState
               then do 
                 putStrLn $ "Correct guess by "++ show h
                 let gs' = processCorrectGuess gameState h
                 broadcast handles $ teamsMsgToJSON (teams gs') -- updates score
                 sendOne h "Correct!"
                 writeChan channel RoundStart
                 dispatcher channel handles gs'
               else do
                 putStrLn "wrong guess"
                 dispatcher channel handles gameState
        _        -> do 
                      putStrLn "Not currently accepting guesses"
                      dispatcher channel handles gameState

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
    
broadcast :: [Handle] -> String -> IO ()
-- TODO: handle case that the handle is closed (putFrame to a closed handle will kill the thread).
broadcast handles msg = mapM_ (\h -> sendOne h msg) handles 

sendOne :: Handle -> String -> IO ()
sendOne handle msg = putFrame handle (BU.fromString msg)

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
      let msg = BU.toString msgB -- TODO: by going via String we'll unfortunately kill any invalid chars, where we'd prefer to leave the msg untouched.
      putStrLn $ "client handler: got message " ++ msg
      let ev = if "NICK: " `isPrefixOf` msg
                 then SetNick h (User msg) 
                 else if "GUESS: " `isPrefixOf` msg
                        then Guess h (drop (length "GUESS: ") msg)
                        else Message h msg
      putStrLn $ "type of event: " ++ show ev
      writeChan channel ev
      putFrame h $ BU.fromString "ok"
      talkLoop h channel

