{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{- |
Module      :  $Header$
Description :  The blubber server entry point.
Copyright   :  (c) plaimi 2015
License     :  AGPL-3

Maintainer  :  blubber@plaimi.net
-} module Main where

import Control.Arrow
  (
  (>>>),
  second,
  )
import Control.Concurrent
  (
  forkIO,
  )
import Control.Concurrent.Chan
  (
  Chan,
  newChan,
  readChan,
  writeChan,
  )
import Control.Monad
  (
  forever,
  void,
  )
import Control.Monad.IO.Class
  (
  liftIO,
  )
import Data.Default.Class
  (
  def,
  )
import Data.IORef
  (
  IORef,
  newIORef,
  readIORef,
  writeIORef,
  )
import Data.Functor
  (
  (<$>),
  )
import qualified Data.Foldable as F
import Data.Map
  (
  Map,
  delete,
  empty,
  fromList,
  insert,
  toList,
  )
import qualified Data.Map as M
import Data.Text.Lazy
  (
  pack,
  )
import Data.Serialize
  (
  decode,
  encode,
  )
import Network.Socket
  (
  Family (AF_INET),
  SockAddr (SockAddrInet),
  Socket,
  SocketType (Datagram),
  bind,
  iNADDR_ANY,
  socket,
  withSocketsDo
  )
import Network.Socket.ByteString
  (
  recvFrom,
  sendTo,
  )
import System.Random
  (
  newStdGen,
  )
import System.Posix.Unistd
  (
  usleep,
  )
import Text.Pandoc.Readers.Markdown
  (
  readMarkdown,
  )
import Text.Pandoc.Writers.HTML
  (
  writeHtmlString,
  )
import Web.Scotty
  (
  file,
  get,
  html,
  setHeader,
  scotty,
  )
import System.Environment
  (
  getArgs,
  )

import Blubber.Server.Message
  (
  Message (AddPlayer, Connect, Disconnect, SrcPls, UpdateVelocity),
  Reply (SrcLink, View)
  )
import Blubber.Server.ViewPort
  (
  ViewPort,
  mkViewPort,
  )
import Blubber.Server.World
  (
  World,
  addNeutral,
  addPlayer,
  createWorld,
  delPlayer,
  handleInput,
  updateWorld,
  )

import Plailude

import Paths_blubber_server

-- | An 'Event' can either be a message from the client, or a 'Tick' to
--   the 'World' a bit further ahead.
data Event = ClientMessage SockAddr Message
           -- ^ A 'Message' from the client.
           | Tick Double Double
           -- A 'Tick' means an update is due.
           deriving (Show)

-- | An internal wrapper type for server news to the client.
data ServerNews = WorldUpdate (Map SockAddr ViewPort)
                -- ^ The 'World' has been updated; new 'ViewPorts' required.
                | SourceLink SockAddr String
                -- ^ Someone's asked for a link to the source code.
                deriving (Show)

main :: IO ()
-- | Our 'main' entry point. Sets up chans & IOrefs, & spawns the right
--   threads to do the actual work.
--
--   It also sets up a tiny Web server at port 8765, serving some info and
--   the source code. The game itself is on port 12354.
main = withSocketsDo $ do
  s <- socket AF_INET Datagram 0
  r <- newStdGen
  as <- getArgs
  let (gp, wp) = case as of
             [g', w'] -> (fromInteger . read $ g', fromInteger . read $ w')
             _        -> error "start with ./blubber-server [PORT] [PORT]"
  bind s $ SockAddrInet gp iNADDR_ANY
  wi <- newChan
  wo <- newChan
  is <- newIORef empty
  _ <- forkIO $ forever $ listen s wi is
  _ <- forkIO $ forever $ talk s wo
  _ <- forkIO $ watch wi 0.01 0.0
  _ <- forkIO $ serve wp
  putStrLn $ "Setting blubbers to blub... (port " ++ show gp
                                                  ++ ") (ctrl-c to quit)"
  step wp wi wo is (createWorld r) empty

watch :: Chan Event -> Double -> Double -> IO ()
-- | Sleep a bit, and add a new 'Tick' via a 'Chan Event'
watch wi dt t = do
  usleep . round $ 1000000 * dt
  writeChan wi $ Tick dt t
  watch wi dt (t + dt)

step :: Int
    -> Chan Event
    -> Chan ServerNews
    -> IORef (Map SockAddr (Double, Double))
    -> World
    -> Map SockAddr String
    -> IO ()
-- | 'step' the 'World' one 'step' ahead. It takes five arguments, and is
-- jolly complicated at present.
--
-- First 'wi', which is a 'Chan' written to by 'listen'
-- when it wants to 'Connect', 'DisConnect', or 'AddPlayer', and by 'watch'
-- when it wants to 'Tick'.
--
-- Then 'wo', which is the 'IORef' which 'step' writes to when it has updated
-- the clients and/or the 'World'. This is used by 'talk' to send out
-- a 'ViewPort' based on the 'World' to each client.
--
-- Next up, we have 'is' -- an 'IORef' with a 'Map' of 'SockAddr's and their
-- latest desired inputs, as two 'Double's, one for x, one for y. 'listen'
-- writes to this channel; 'step' reads it and is responsible for actually
-- doing 'handleInput'
--
-- Then the two last arguments are the 'World' and a list of clients. These
-- are updated in 'step', and written to the 'wo' channel for 'talk' to use.
-- 'step' recurses with these two arguments as its state.
step wp wi wo is w cs = do
  wj <- readChan wi
  (cs', w') <- case wj of
                 Tick dt t         -> do
                   js <- readIORef is
                   let w' = handleInput (toList js) cs
                        >>> updateWorld dt
                        >>> if' (round (t / 5) /= round ((t - dt) / 5))
                                addNeutral id
                          $ w
                   writeChan wo $ WorldUpdate (mkViewPorts cs w')
                   return (cs, w')
                 ClientMessage c m ->
                   case m of
                     Connect       -> do
                       return (insert c "" cs, w)
                     Disconnect    -> do
                       case M.lookup c cs of
                                        Just p  -> return (delete c cs
                                                          ,w `delPlayer` p)
                                        Nothing -> return (cs, w)
                     (AddPlayer p) -> return (insert c p cs, w `addPlayer` p)
                     SrcPls i      -> do
                       writeChan wo (SourceLink c (i ++ ":" ++ show wp
                                                     ++ "/src.tar.gz"))
                       return (cs, w)
                     _             -> return (cs, w)
  step wp wi wo is w' cs'


listen :: Socket -> Chan Event -> IORef (Map SockAddr (Double, Double))
       -> IO ()
-- | Check if the clients have anything to say. Put the last thing each client
-- has to say in the 'Map' of 'IORef's.
listen sock wi is = do
  (m, c) <- recvFrom sock 4096
  case decode m of
    Right cm -> case cm of
                  UpdateVelocity (x, y) -> do
                    js <- readIORef is
                    writeIORef is $ insert c (x, y) js
                  _                     -> writeChan wi $ ClientMessage c cm
    Left  _  -> return ()

talk :: Socket -> Chan ServerNews -> IO ()
-- | Send out updated 'ViewPort's to the connect clients.
talk s wo = do
  m <- readChan wo
  case m of
    WorldUpdate cv    -> (\(c, vp) ->
      sendTo s (encode (View vp)) c) `F.mapM_` toList cv
    SourceLink c a -> void $ sendTo s (encode (SrcLink a)) c

serve :: Int -> IO ()
-- | Very simple Web server with an info page and the source code of the game.
--
-- The info page is compiled from web/index.markdown.
--
-- The source is served from web/src.tar.gz.
serve = flip scotty $ do
  get "/src.tar.gz"  $ do
    setHeader "Content-Disposition" "attachment"
    setHeader "Content-Type" "application/x-gzip"
    setHeader "filename" "src.tar.gz"
    file =<< liftIO (getDataFileName "web/src.tar.gz")
  get "/" $ do
    setHeader "Content-Type" "text/html"
    f <- liftIO $ readFile =<< getDataFileName "web/index.markdown"
    case readMarkdown def f of
      Left  _ -> html "blubber-server"
      Right m -> html . pack . writeHtmlString def $ m

mkViewPorts :: Map SockAddr String -> World -> Map SockAddr ViewPort
-- | Make a 'ViewPort' for ever client.
mkViewPorts cs w = fromList $ (mkViewPort w `second`) <$> toList cs
