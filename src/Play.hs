module Play (play, play', loop') where

import Data.Ratio
import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Concurrent
import System.MIDI

import Types
import Drum
import Interpret

-- | convert a `Hit` to a `MidiEvent`
hitToMidiEvent :: Hit -> MidiEvent
hitToMidiEvent h = MidiEvent d (MidiMessage 1 (NoteOn t v))
  where
    t  = 35 + fromEnum (h ^. tone)
    d  = fromR $ h ^. dur
    v  = fromR $ h ^. vol
    fromR r = fromIntegral $ numerator r `div` denominator r

getConnection :: IO Connection
getConnection = do
    dstlist <- enumerateDestinations
    case dstlist of
      [] -> error "No MIDI Devices found."
      (dst:_) -> openDestination dst

-- | play a composition at a given tempo
play :: Composition a -> Rational -> IO ()
play comp n = do
    conn <- getConnection
    start conn
    evalStateT runComposition (conn, interpret n comp)
    close conn

play' :: Composition a -> IO ()
play' = flip play 120

-- | loop a composition at a given tempo
loop' :: Song -> Rational -> IO ()
loop' s = play (loop s)

runComposition :: StateT (Connection, [Hit]) IO ()
runComposition = do
  (conn, comp) <- get
  t <- lift $ currentTime conn
  case comp of
    []     -> return ()
    (h:hs) -> do
      let (MidiEvent s ev) = hitToMidiEvent h
      when (s < t) $ do
        put (conn, hs)
        lift $ send conn ev
      lift $ threadDelay 250
      runComposition
