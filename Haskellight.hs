{-# language TupleSections #-}

module Haskellight (
  module Animation,
  module Fixtures,
  module Generic, 
  module Control.Arrow,
  Word8,
  Context(..),
  GroupAnimation,
  runner,
  testRunner,
  chase,
  chaseEach,
  sequence,
  fadeColors,
  fade,
  grow,
  speed,
  beat,
  vu,
  count,
  waitForPlay,
  mkRandom,
  rev,
  alternate,
  zigzag,
  cue,
  sin,
  halfsin,
  pallette
) where

import Prelude hiding (id, sequence, sin)
import qualified Prelude as P
import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.IORef
import Data.Word
import qualified Data.Vector as V
import Control.Category hiding ((.))
import Control.Arrow
import Control.Concurrent
import qualified Control.Monad as M
import System.Random

import Animation
import Fixtures
import Generic hiding (take, drop)
import qualified Generic as G
import Midi
import UDMX

data Context = Context {cFrame :: !Int, cBeat :: !Int, cVU :: !Word8, cPlay :: !Bool}
type GroupAnimation a = Animation Context (Stream a)
type Stepper a = Animation Int (Stream a)

mkMidiHandler :: (Context -> IO ()) -> IO ([Word8] -> IO ())
mkMidiHandler h = do
  frameCounter <- newIORef 0
  beatCounter <- newIORef 0
  play <- newIORef False
  return $ \msg ->
    case msg of
      [0x90, 0x01, _] ->
        modifyIORef' beatCounter succ
      [0x90, 0x02, _] ->
        writeIORef play True
      [0xb0, 0x00, v] -> do
        frame <- readIORef frameCounter
        beat <- readIORef beatCounter
        writeIORef frameCounter $! succ frame
        isPlay <- readIORef play
        M.when isPlay $ writeIORef play False
        h (Context frame beat v isPlay)
      _ -> return ()

runner :: Fixture frame -> Runner Context frame
runner fixture sequencer waitForStop = withUDMX $ \dev -> do
  mh <- mkMidiHandler $ \ctx -> do
    frame <- sequencer ctx
    UDMX.set (toStrict $ toLazyByteString $ fixture frame) dev
  listenMidi mh waitForStop
  putStrLn "Stopped"

testRunner :: Fixture frame -> Runner Context frame
testRunner fixture sequencer waitForStop = withUDMX $ \dev -> do
  stop <- newIORef False
  let
    go i = do
      frame <- sequencer $ Context i (shiftR i 4) (fromIntegral i) (i .&. 0xff == 0)
      UDMX.set (toStrict $ toLazyByteString $ fixture frame) dev
      threadDelay 40000
      s <- readIORef stop
      M.unless s $ go (i + 1)
  forkIO $ go 0 
  waitForStop
  writeIORef stop True
  putStrLn "Stopped"



------------------------------------------------------------------------
-- Steppers

chase :: [a] -> Stepper a
chase as = arr $ (`G.drop` as') . (`mod` l)
  where
    l = length as
    as' = cycleList as

chaseEach :: (Monoid a) => [a] -> Stepper a
chaseEach as = arr $ G.unfold (\(i, a :$: as) -> (if i `mod` l == 0 then a else mempty, (succ i, as))) .  (, as')
  where
    l = length as
    as' = cycleList as

sequence :: [a] -> Stepper a
sequence as = arr $ pure . (as !!) . (`mod` l)
  where
    l = length as

fadeColors :: (Color c) => Int -> [c] -> Animation Int c
fadeColors speed as = arr f
  where
    f i = mix v c2 c1
      where
        c1 = as !! (index `mod` l)
        c2 = as !! (succ index `mod` l)
        v = sin (shiftL speed 1) (i `mod` speed)
        index = i `div` speed  
    l = length as

fade :: (Color c) => Int -> [c] -> Stepper c
fade speed as = fadeColors speed as >>^ pure

grow :: a -> a -> Stepper a
grow fg bg = arr $ \i -> G.unfold (\ n -> (if n < i then fg else bg, n + 1)) 0



------------------------------------------------------------------------
-- Context access

speed :: Int -> Animation Context Int
speed n = arr $ (`div` n) . cFrame

beat :: Animation Context Int
beat = arr cBeat

frame :: Animation Context Int
frame = arr cFrame

vu :: Int -> Int -> Int -> Animation Context Int
vu low high range = arr $ min range . (`div` (high - low)) . (* range). max 0 . subtract low . fromIntegral . cVU

count :: Animation Int Int
count = mkAnimation $ arr . subtract

waitForPlay :: Animation Context Bool
waitForPlay = arr $ not . cPlay



------------------------------------------------------------------------
-- Randomisation

mkRandom :: (Random r) => ([r] -> Animation Context a) -> Animation Context a
mkRandom f = mkAnimation $ f . randoms . mkStdGen . cFrame 

randomMap :: (Random r) => Int -> Animation Context (Int -> r)
randomMap size = mkRandom $ (\vect -> pure $ (vect V.!) . (`mod` size)) . V.fromListN size



------------------------------------------------------------------------
-- Animation Combinators

rev :: Int -> Animation (Stream a) (Stream a)
rev n = arr $ cycleList . reverse . G.take n

alternate :: Int -> Int -> Animation (Stream a, Stream a) (Stream a)
alternate n1 n2 = arr $ \(o1, o2) -> cycleList $ G.take n1 o1 ++ G.take n2 o2

zigzag :: Int -> Stepper a -> Stepper a
zigzag n a = a >>> (id &&& rev n) >>> alternate n n



------------------------------------------------------------------------
-- Show

cue :: Animation Context a -> Animation Context a -> Animation Context a
cue new old = while waitForPlay old new



------------------------------------------------------------------------
-- Functions

sin :: Int -> Int -> Word8
sin period v = round $ (P.sin (fromIntegral v * 2 * pi / fromIntegral period :: Double) + 1) * 127.5

halfsin :: Int -> Int -> Word8
halfsin period v = round $ (P.sin (fromIntegral v * 2 * pi / fromIntegral period :: Double) `max` 0) * 255

pallette :: (Color c) => Int -> Int -> Int -> Int -> Int -> c
pallette p r g b v = rgb (halfsin p $ v + r) (halfsin p $ v + g) (halfsin p $ v + b)

