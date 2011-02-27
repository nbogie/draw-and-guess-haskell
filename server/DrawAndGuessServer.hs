import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
import System.IO (Handle, hClose)
import qualified Data.ByteString as B (append, null, unpack, isPrefixOf)
import qualified Data.ByteString.UTF8 as BU (fromString, toString) -- this is from utf8-string
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)

import Control.Concurrent.Chan

import Data.List (delete, intercalate, isPrefixOf)

-- Accepts clients, spawns a single handler for each one.
main :: IO ()
main = withSocketsDo $ do
  channel <- newChan
  forkIO $ dispatcher channel [] initGameState
  socket <- listenOn (PortNumber 12345)
  putStrLn "Listening on port 12345."
  forever $ do
    (h, _, _) <- accept socket
    forkIO (welcome h channel)

data User = User String deriving (Eq, Ord, Show)
data Event = Join Handle User 
             | SetNick Handle User
             | Message Handle User String
             | Leave Handle User
             deriving (Eq, Show)

data Team = Team [Handle] deriving (Show, Eq)
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

data GameState = GameState { team1::Team, team2::Team } deriving (Show)

initGameState :: GameState
initGameState = GameState { team1 = Team [], team2 = Team []}

dispatcher :: Chan Event -> [Handle] -> GameState -> IO ()
dispatcher channel handles gameState = do
  ev <- readChan channel 
  case ev of
    Join h (User uname) -> do
      putStrLn $ "DISP: Join Event.  Handle: "++ show h
      let newHandles = h:handles
      broadcast newHandles $ show ["Joined: ", show h, uname]
      dispatcher channel newHandles gameState
    Leave h (User uname) -> do
      putStrLn "DISP: Leave Event"
      let newHandles = delete h handles
      broadcast newHandles "left"
      dispatcher channel newHandles gameState
    SetNick h (User uname) -> do
      putStrLn $ "DISP: User " ++ uname ++ " set nick"
      broadcast handles $ "NICK SET: " ++ uname
      dispatcher channel handles gameState
    Message h (User uname) msg -> do
      putStrLn $ "DISP: User " ++ uname ++ " got message: " ++ msg ++ " from " ++ show h 
      broadcast handles $ uname ++ ": " ++ msg
      dispatcher channel handles gameState

broadcast :: [Handle] -> String -> IO ()
broadcast handles msg = mapM_ (\h -> putFrame h (BU.fromString msg)) handles -- TODO: handle case that the handle is closed (putFrame to a closed handle will kill the thread).

-- Talks to the client (by echoing messages back) until EOF.
talkLoop :: Handle -> Chan Event -> IO ()
talkLoop h channel = do
  msgB <- getFrame h
  if B.null msgB
    then do
      putStrLn "EOF encountered. Closing handle."
      hClose h
    else do
      let msg = BU.toString msgB -- TODO: by going via String we'll unfortunately kill any invalid chars, where we'd prefer to leave the msg untouched.
      putStrLn $ "client handler: got message " ++ msg
      
      let ev = case isPrefixOf "nick=" msg of
                 True -> SetNick h (User msg) 
                 _    -> Message h (User "unknownx") msg
      putStrLn $ "type of event: " ++ show ev
      writeChan channel $ ev
      putFrame h $ BU.fromString "ok"
      talkLoop h channel


-- useful stuff
--      threadDelay $ 3 * 1000000 -- probably not the right way to do it. 
