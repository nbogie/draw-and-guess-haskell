module Main where

import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
import System.IO (Handle, hClose)
import qualified Data.ByteString as B (append, null, unpack, isPrefixOf)
import qualified Data.ByteString.UTF8 as BU (fromString, toString) -- this is from utf8-string
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)

import Control.Concurrent.Chan

import Data.List (delete, intercalate, isPrefixOf, isInfixOf)

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

data PlayState = AwaitingPlayers | ReadyToStart | Guessing deriving (Eq, Show)

data User = User String deriving (Eq, Ord, Show)
data Event = Join Handle User 
             | SetNick Handle User
             | Message Handle User String
             | Guess Handle String
             | Leave Handle User
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
      writeChan channel $ Join h (User "unknown")
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
                in t {teamMembers = h:ms} 

removeFromTeams :: Teams -> Handle -> Teams
removeFromTeams teams@(Teams {team1=t1, team2=t2}) h = 
  let t1' = removeFromTeam t1 h
      t2' = removeFromTeam t2 h
  in Teams { team1 = t1', team2 = t2' }

removeFromTeam :: Team -> Handle -> Team
removeFromTeam t h = let ms = delete h (teamMembers t)
                     in if (Just h) == artist t
                          then t {teamMembers = ms, artist = Nothing}
                          else t { teamMembers = ms}
                                       
dispatcher :: Chan Event -> [Handle] -> GameState -> IO ()
dispatcher channel handles gameState = do
  ev <- readChan channel 
  putStrLn $ "DISPATCHER readChan: " ++ show ev ++ ". play state is: "++ show (playState gameState) ++ " word is: "++currentWord gameState
  case ev of
    Join h (User uname) -> do
      if h `elem` handles -- ignore this re-join from same handle, or better still, drop the handle.
        then
          do putStrLn "ignoring join because handle contains already"
             dispatcher channel handles gameState
        else do
          let newHandles = h:handles
          putStrLn $ "DISP: Join Event.  Handle: "++ show h
          let nowTeams = teams gameState
          let newTeams = addToTeams (teams gameState) h
          let gs' = gameState {teams = newTeams} 
          putStrLn $ "New Teams: " ++ show newTeams
          broadcast newHandles $ show ["Joined: ", show h, uname]
          putStrLn $ "Will broadcast " ++ (teamsMsgToJSON newTeams)
          broadcast newHandles $ teamsMsgToJSON newTeams
          gs'' <- if (playState gs' == AwaitingPlayers) && (length newHandles) > 1
                      then do 
                        writeChan channel RoundStart
                        return $ gs' {playState = ReadyToStart}
                      else return gs'
          dispatcher channel newHandles gs''
    Leave h (User uname) -> do
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
    Message h (User uname) msg -> do
      putStrLn $ "DISP: User " ++ uname ++ " got message: " ++ msg ++ " from " ++ show h 
      broadcast handles $ uname ++ ": " ++ msg
      dispatcher channel handles gameState
    RoundStart -> do
      gs' <- case playState gameState of
        ReadyToStart -> do
          let g = gameState {playState = Guessing, currentWord = head (gWords gameState), 
                             gWords = tail (gWords gameState) ++ [currentWord gameState]}
          broadcast handles $ "ROUNDSTART"
          broadcastArtists g handles $ "ROLE:artist WORD:" ++ currentWord g
          -- broadcastGuessers g handles $ "ROUNDSTART.  ROLE=guesser"
          return g
        _ -> do putStrLn "Round already started"
                return gameState
      dispatcher channel handles gs'
    Guess h guess -> do
      putStrLn $ "Got guess of " ++ guess ++ " from "++(show h)
      case playState gameState of
        Guessing -> 
          do if guess == currentWord gameState
               then do 
                 putStrLn "WIN"
                 let gs' = processCorrectGuess gameState h
                 broadcast handles $ teamsMsgToJSON (teams gs') -- updates score
                 writeChan channel RoundStart
                 dispatcher channel handles gs'
               else do
                 putStrLn "wrong"
                 dispatcher channel handles gameState
        _        -> do 
                      putStrLn "Not currently accepting guesses"
                      dispatcher channel handles gameState

processCorrectGuess :: GameState -> Handle -> GameState
processCorrectGuess gs guesserH = let t1 = team1 $ teams gs
                                      t2 = team2 $ teams gs
                                      t1' = if inTeam guesserH t1 then incScore t1 1 else t1
                                      t2' = if inTeam guesserH t2 then incScore t2 1 else t2
                                      newTeams = (teams gs) {team1 = t1', team2 = t2'}
                                  in gs {playState = ReadyToStart, teams = newTeams}

inTeam :: Handle -> Team -> Bool
inTeam h t = h `elem` (teamMembers t)

incScore :: Team -> Int -> Team
incScore t amt = t { score = score t + amt }

broadcastArtists :: GameState -> [Handle] -> String -> IO ()
broadcastArtists gs hs msg = let artistHs = artistHandles gs hs
                             in broadcast artistHs msg

artistHandles :: GameState -> [Handle] -> [Handle]
artistHandles gs hs = []
    
broadcast :: [Handle] -> String -> IO ()
-- TODO: handle case that the handle is closed (putFrame to a closed handle will kill the thread).
broadcast handles msg = mapM_ (\h -> putFrame h (BU.fromString msg)) handles 
sendOne handle msg = putFrame handle (BU.fromString msg)

teamsMessage :: Teams -> String
teamsMessage teams = teamsMsgToJSON teams

-- Talks to the client (by echoing messages back) until EOF.
talkLoop :: Handle -> Chan Event -> IO ()
talkLoop h channel = do
  msgB <- getFrame h
  if B.null msgB
    then do
      putStrLn "EOF encountered. Closing handle."
      hClose h
      writeChan channel $ Leave h (User "foo") 
    else do
      let msg = BU.toString msgB -- TODO: by going via String we'll unfortunately kill any invalid chars, where we'd prefer to leave the msg untouched.
      putStrLn $ "client handler: got message " ++ msg
      let ev = if "NICK: " `isPrefixOf` msg
                 then SetNick h (User msg) 
                 else if "GUESS: " `isPrefixOf` msg
                        then Guess h (drop (length "GUESS: ") msg)
                        else Message h (User "unknownx") msg
      putStrLn $ "type of event: " ++ show ev
      writeChan channel ev
      putFrame h $ BU.fromString "ok"
      talkLoop h channel


-- useful stuff
--      threadDelay $ 3 * 1000000 -- probably not the right way to do it. 
