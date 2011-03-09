module Main where

import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
import System.IO (Handle, hClose)
import qualified Data.ByteString as B (null)
import qualified Data.ByteString.UTF8 as BU (fromString, toString) -- this is from utf8-string
import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Control.Concurrent.Chan
import Data.Maybe (fromMaybe)
import Data.List (delete, isPrefixOf)
import qualified Data.Map as M
import Types
import Teams
import Messages
import Text.HTML.TagSoup (escapeHTML)
import Data.Char (isAlphaNum, isAlpha, toLower)

import Control.Monad.Reader
import Control.Monad.State

type GSMonad = StateT GameState IO

-- Accepts clients, spawns a single handler for each one.
main :: IO ()
main = withSocketsDo $ do
  channel <- newChan
  ws <- fmap lines $ readFile "guesswords.txt"
  forkIO $ evalStateT (dispatcher channel) $ initGameState ws
  let pNum = 12345
  socket <- listenOn (PortNumber pNum)
  putStrLn $ "Listening on port " ++ show pNum
  forever $ do
    (h, _, _) <- accept socket
    forkIO (welcome h channel)

currentWord :: GameState -> String
currentWord g | null (gWords g) = error "No words in game state"
               |otherwise        = head $ gWords g

--
-- Shakes hands with client. If no error, starts talking.
welcome :: Handle -> Chan Event -> IO ()
welcome h channel = do
  request <- shakeHands h
  case request of
    Left err -> print $ "ERROR: " ++ show err
    Right  _ -> do
      writeChan channel $ Join h
      sendOne h "WELCOME"
      putStrLn $ "Shook hands with "++show h ++" and sent welcome message."
      talkLoop h channel

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
      -- TODO: by going via String we'll unfortunately kill any invalid chars, 
      -- where we'd prefer to leave the msg untouched.
      let msg = escapeHTML $ BU.toString msgB
      let ev | "NICK: "  `isPrefixOf` msg = Just $ SetNick h $ nickFromMsg msg
             | "GUESS: " `isPrefixOf` msg = Just $ guessFromMsg h msg
             | "DRAW: " `isPrefixOf` msg = Just $ Draw h (drop (length "DRAW: ") msg)
             | otherwise                  = Nothing
      putStrLn $ "talkLoop: got message " ++ msg ++ " parsed as: "++show ev
      case ev of
        Just e -> do writeChan channel e
                     sendOne h "ok"
        Nothing -> return ()
      talkLoop h channel

guessFromMsg h msg = let raw = drop (length "GUESS: ") msg
                         stripped = takeWhile isAlpha raw
                         final = if null stripped then "x" else stripped
                     in Guess h final

nickFromMsg msg = let raw = drop (length "NICK: ") msg
                      stripped = takeWhile isAlphaNum raw
                  in  if length stripped < 1 then "Anonymous" else stripped

dispatcher :: Chan Event -> GSMonad ()
dispatcher channel = do
  ev <- liftIO (readChan channel)
  gameState <- get
  liftIO (putStrLn $ "DISPATCHER readChan: " ++ show ev ++ ". play state is: " ++ 
             show (playState gameState) ++ " word is: "++currentWord gameState)
  nextGameState <- liftIO (case ev of
    Join h -> handleJoin h channel gameState
    Leave h -> handleLeave h channel gameState
    SetNick h uname -> handleSetNick h uname channel gameState
    Message h msg -> handleMessage h msg channel gameState 
    RoundStart -> handleRoundStart channel gameState
    Draw h msg -> handleDraw h msg channel gameState
    Guess h guess -> handleGuess h guess channel gameState)
  put nextGameState
  dispatcher channel

-------------------------------------------------------------------------------
-- handlers
-------------------------------------------------------------------------------
handleJoin :: Handle -> Chan Event -> GameState -> IO GameState
handleJoin h ch gameState = 
      if h `elem` handles gameState
        then
          -- ignore this re-join from same handle (or drop the handle?)
          do putStrLn "ignoring join because handle contains already"
             return gameState
        else do
          let newTeams = addToTeams h (teams gameState)
          let gs' = gameState {teams = newTeams, handles = h:handles gameState} 
          putStrLn $ "New Teams: " ++ show (teams gs')
          broadcastTeamsAndState (handles gs') gs'
          if (playState gs' == AwaitingPlayers) && length (handles gs') > 1
                      then do 
                        writeChan ch RoundStart
                        return $ gs' {playState = ReadyToStart}
                      else return gs'

handleLeave :: Handle -> Chan Event -> GameState -> IO GameState 
handleLeave h ch gameState = do
    let gs' = removeFromGame gameState h
    broadcastRaw (handles gs') $ teamsMsgToJSON (teams gs') (nameMap gs')
    return gs'
                                       
handleSetNick h uname ch gameState = do
      let nameMap' = M.insert (show h) uname $ nameMap gameState
      let gs' = gameState {nameMap = nameMap'}
      broadcastRaw (handles gs') $ teamsMsgToJSON (teams gs') nameMap'
      sendOne h $ "YOURNICK " ++ uname
      return gs'

handleMessage h msg ch gameState = do
      let uname = nameForHandle h (nameMap gameState)
      broadcast (handles gameState) $ uname ++ ": " ++ msg
      return gameState

handleDraw h msg ch gameState = do
      case teamFor h (teams gameState) of
        -- a handle should always be part of a team, 
        -- but maybe it has been removed (kicked?) since this msg was enqueued.
        Just t -> broadcast (teamMembers t) $ "DRAW " ++ msg
        Nothing -> return () 
      return gameState

-- TODO: this needn't be an event at all
-- just invoke the work directly when a round start is needed.
handleRoundStart ch gameState = 
      case playState gameState of
        ReadyToStart -> do
          let g = cycleWord $ gameState {playState = InPlay
                             , teams = cycleArtists (teams gameState)}
          print $ "NEW WORD for round " ++ currentWord g
          broadcast (handles g) "ROUNDSTART"
          broadcastTeamsAndState (handles g) g
          return g
        _ -> do 
          putStrLn "Round already started"
          return gameState

handleGuess :: Handle -> String -> Chan Event -> GameState -> IO GameState
handleGuess h guess ch gameState = do
      putStrLn $ "Got guess of " ++ guess ++ " from "++show h
      case playState gameState of
        InPlay -> 
          if map toLower guess == map toLower (currentWord gameState)
            then do 
              putStrLn $ "Correct guess by "++ show h
              let gs' = processCorrectGuess gameState h
              broadcastRaw (handles gs') $ teamsMsgToJSON (teams gs') (nameMap gs') -- updates score
              broadcast (handles  gs') $ "GUESS CORRECT " ++ nameForHandle h (nameMap gs') ++ " " ++ guess
              sendOne h "CORRECT_GUESS_BY_YOU"
              writeChan ch RoundStart
              return gs'
            else do
              putStrLn "wrong guess"
              broadcast (handles gameState) $ "GUESS WRONG " ++ nameForHandle h (nameMap gameState) ++ " " ++ guess
              return gameState
        _      -> do 
          putStrLn "Not currently accepting guesses"
          return gameState

--------------------------------------------------------------------------------
---- broadcast functions
--------------------------------------------------------------------------------
processCorrectGuess :: GameState -> Handle -> GameState
processCorrectGuess gs guesserH = 
  let t1 = team1 $ teams gs
      t2 = team2 $ teams gs
      t1' = if inTeam guesserH t1 then incScore t1 1 else t1
      t2' = if inTeam guesserH t2 then incScore t2 1 else t2
      newTeams = (teams gs) {team1 = t1', team2 = t2'}
  in gs {playState = ReadyToStart, teams = newTeams}

-- broadcast escapes html chars first.  if you don't want this, write broadcastRaw.
-- broadcast :: [Handle] -> String -> IO ()
-- TODO: handle case that the handle is closed (putFrame to a closed handle will kill the thread).
broadcast hs msg = mapM_ (\h -> sendOne h msg) hs 
broadcastRaw hs msg = mapM_ (\h -> sendOneRaw h msg) hs 

-- sendOne escapes html chars first.  if you don't want this, write broadcastRaw.
sendOne :: Handle -> String -> IO ()
sendOne handle msg = putFrame handle (BU.fromString (escapeHTML msg))
sendOneRaw handle msg = putFrame handle (BU.fromString msg)

broadcastTeamsAndState :: [Handle] -> GameState -> IO ()
broadcastTeamsAndState hs g = do
          broadcastRaw hs $ teamsMsgToJSON (teams g) (nameMap g)
          broadcast (artists g) $ "ROLE artist WORD " ++ currentWord g
          broadcast (guessers g) "ROLE guesser"
          broadcast hs $ "STATE "++ show (playState g)

--------------------------------------------------------------------------------
---- misc functions
--------------------------------------------------------------------------------

-- QUESTION: can we just use (cycle words) to make it an infinite stream?
initGameState :: [String] -> GameState
initGameState ws = GameState { teams = Teams {team1 = Team {teamMembers=[], artist = Nothing, score = 0}, 
                                              team2 = Team {teamMembers=[], artist = Nothing, score = 0} }
                             , handles = []
                             , gWords = ws 
                             , playState = AwaitingPlayers
                             , nameMap = M.empty }

cycleWord :: GameState -> GameState
cycleWord g = g { gWords = ws ++ [w] }
                where (w:ws) = gWords g

nameForHandle :: Handle -> HToNameMap -> String
nameForHandle h nmap = escapeHTML $ fromMaybe "Anonymous" (M.lookup (show h) nmap)

removeFromGame :: GameState -> Handle -> GameState
removeFromGame gs h =  let newHandles = delete h (handles gs)
                           newTeams = removeFromTeams (teams gs) h
                           newNameMap = M.delete (show h) (nameMap gs)
                       in gs { teams = newTeams, handles = newHandles, nameMap = newNameMap } 

------------------------------
-- monad manipulation examples
------------------------------
launchTest :: IO ()
launchTest = do
  let gs = initGameState ["word1", "word2"]
  evalStateT manipTest gs
  evalStateT (manip2Test "secondtest") gs
 
-- runStateT xtest 
-- testing out how to use our monad stack
manipTest :: GSMonad ()
manipTest = do 
  ps <- gets playState
  liftIO $ putStrLn ("Play state is: "++show ps)
  return ()

manip2Test :: String -> GSMonad ()
manip2Test label = do 
  liftIO $ putStrLn $ "Label: " ++ label
  ps <- gets playState
  modify (\g -> g{playState = InPlay}) -- ugly - can we modify a specific part?
  ps2 <- gets playState
  liftIO $ putStrLn ("Play state is: "++show ps2)
  return ()

-- A first attempt at converting handleJoin to use the GSMonad
-- it works but it's extremely ugly.  Left for learning purposes.
handleJoinUgly :: Handle -> Chan Event -> GSMonad ()
handleJoinUgly h ch = do
      hs <- gets handles
      if elem h hs
        then
          -- ignore this re-join from same handle (or drop the handle?)
          liftIO $ putStrLn "ignoring join because handle contains already"
        else do
          newTeams <- fmap (addToTeams h) (gets teams) 
          modify (\g -> g{teams = newTeams, handles = h:hs})
          gets teams >>= (\ts -> liftIO $ putStrLn $ "New Teams: " ++ show ts)

          get >>= (\g -> liftIO $ broadcastTeamsAndState hs g)

          ps <- gets playState
          if (ps == AwaitingPlayers) && length hs > 1
                      then do 
                        liftIO $ writeChan ch RoundStart
                        modify (\g -> g{playState = ReadyToStart})
                      else return ()
