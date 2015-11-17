module Play
  ( hitToMidiEvent
  , play
  )
where

import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Concurrent
import System.MIDI
import System.Info

import Types

hitToMidiEvent :: Hit -> MidiEvent
hitToMidiEvent h = MidiEvent d (MidiMessage 1 (NoteOn t v))
  where
    t = 35 + fromEnum (h ^. tone)
    d = fromIntegral $ h ^. dur
    v = h ^. vol

getConnection :: IO Connection
getConnection = do
    dstlist <- enumerateDestinations
    case dstlist of
      [] -> error "No MIDI Devices found."
      (dst:_) -> openDestination dst

play :: Composition -> IO ()
play comp = do
    conn <- getConnection
    start conn
    evalStateT runComposition (conn, comp)
    close conn

runComposition :: StateT (Connection, Composition) IO ()
runComposition = do
  (conn, comp) <- get
  t <- lift $ currentTime conn
  case comp of
    (Composition [] _)     -> return ()
    (Composition (h:hs) ld) -> do
      let x@(MidiEvent s ev) = hitToMidiEvent h
      when (s < t) $ do
        put (conn, Composition hs ld)
        lift $ send conn ev
      lift $ threadDelay 250
      runComposition
