{-# language Arrows, DataKinds, FlexibleContexts, GADTs, MultiParamTypeClasses #-}

module MyShow where

import Prelude hiding (sequence, break, sin, take, drop)
import qualified Prelude as P
import Data.Bits
import Data.List (unfoldr)
import Haskellight
import Generic

type Frame = IVect Stream [RGB, RGBW, RGBA, IMove7s]
type Scene = Animation Context Frame

myRig :: Fixture Frame
myRig = rig $ Group 40 rbgPixel :&: Group 4 ledj252 :&: Group 4 ledj7q5 :&: Group 2 imove7s :&: INil

-- TODO: there must be a neater way to do this
scene
  :: GroupAnimation RGB
  -> GroupAnimation RGBW
  -> GroupAnimation RGBA
  -> GroupAnimation IMove7s
  -> Scene
scene a b c d = proc i -> do
  av <- a -< i
  bv <- b -< i
  cv <- c -< i
  dv <- d -< i
  returnA -< av :&: bv :&: cv :&: dv :&: INil

bo :: (Color c) => GroupAnimation c
bo = pure black

blackout :: Scene
blackout = scene bo bo bo home

startShow :: IO (Animator Context Frame)
startShow = start blackout $ runner myRig

dummyShow :: IO (Animator Context Frame)
dummyShow = start blackout $ dummyRunner myRig



------------------------------------------------------------------------
-- Colours

black :: (Color c) => c
black = rgb 0 0 0

white :: (Color c) => c
white = rgb 255 255 255

red :: (Color c) => c
red = rgb 255 0 0

green :: (Color c) => c
green = rgb 0 255 0

blue :: (Color c) => c
blue = rgb 0 0 255

lightBlue :: (Color c) => c
lightBlue = rgb 40 0 255

purple :: (Color c) => c
purple = rgb 255 0 170

orange :: (Color c) => c
orange = rgb 255 40 0

yellow :: (Color c) => c
yellow = rgb 255 255 0

amber :: RGBA
amber = RGBA 0 0 0 255



------------------------------------------------------------------------
-- iMove 7S

imHome :: IMPos
imHome = IMPos 85 207

imNarrow :: Word8 -> Word8 -> IMPos
imNarrow p t = IMPos (shiftR p 3 + 57) (shiftR t 3 + 188)

imWide :: Word8 -> Word8 -> IMPos
imWide p t = IMPos (shiftR p 2 + 40) (shiftR t 2 + 166)

home :: GroupAnimation IMove7s
home = pure $ pure $ IMove7s imHome imClosed imNone imWhite imStatic imOff



------------------------------------------------------------------------
-- Patterns

chaseRGB :: (Color rgb) => Int -> GroupAnimation rgb
chaseRGB s = speed s >>> chase [red, green, blue]

sequenceRGB :: (Color rgb) => Int -> GroupAnimation rgb
sequenceRGB s = speed s >>> sequence [red, green, blue]

vuMeter :: RGB -> RGB -> GroupAnimation RGB
vuMeter fg bg = vu 0 255 40 >>> grow fg bg

vuMeter2 :: RGB -> RGB -> GroupAnimation RGB
vuMeter2 fg bg = vu 0 255 20 >>> zigzag 20 (grow fg bg)

sunsetColors :: (Color c) => Int -> [c]
sunsetColors l = unfoldr gen 0
  where
    gen i
      | i < l     = Just (rgb 255 (sin l i) 0, succ i)
      | otherwise = Nothing

peak :: Animation Context Word8
peak = vu 98 106 255 >>^ fromIntegral

peakFlicker :: (Color a) => GroupAnimation a -> GroupAnimation a
peakFlicker a = (\l -> mix l (grey 255)) <$> peak <*> a

strobe :: (Color c) => Int -> GroupAnimation c
strobe v = speed v >>> sequence [white, black]

blueOrange :: (Color c) => Int -> GroupAnimation c
blueOrange offset = speed 1 >>^ pure . (\v -> mix v blue orange) . sin 5000 . (offset +)

orangeChain :: (Color c) => GroupAnimation c
orangeChain = mkRandom $ pure . unfold f . cycleList
  where
    f (r :$: g :$: xs) = (rgb (shiftR r 1 .|. 0x80) (shiftR g 2) 0, xs)

randomCycle :: (Color c) => Int -> Int -> [c] -> GroupAnimation c
randomCycle s variations cs = mkRandom $ (speed 1 >>>) . fadeColors s . P.take variations . map pick
  where
    pick i = drop (i `mod` l) stream
    stream = cycleList cs
    l = length cs

purpleWhite :: (Color c) => GroupAnimation c
purpleWhite = randomCycle 50 30 [white, purple, purple, purple]



------------------------------------------------------------------------
-- Moving Heads

slowScanner :: Animation Context IMShutter -> IMGobo -> IMColor -> IMRotation -> GroupAnimation IMove7s
slowScanner sa g c r = (speed 1 &&& sa) >>^ \(v, s) -> cycleList [f 0 0 v s, f 200 380 v s]
  where
    f p t v s = IMove7s (imWide (sin 750 $ p + v) (shiftR (sin 1200 $ t + v) 2 + 64)) s g c r imFull

narrowScanner :: Animation Context IMShutter -> IMGobo -> IMColor -> IMRotation -> GroupAnimation IMove7s
narrowScanner sa g c r = (speed 1 &&& sa) >>^ \(v, s) -> cycleList [f 0 0 v s, f 20 32 v s]
  where
    f p t v s = IMove7s (imNarrow (sin 100 $ p + v) (sin 180 $ t + v)) s g c r imFull

slowRotatingScanner :: GroupAnimation IMove7s
slowRotatingScanner = slowScanner (pure imOpen) imFour imWhite (imClockwise 20)

sphereScanner :: GroupAnimation IMove7s
sphereScanner = slowScanner (peak >>^ imShake) imSphere imWhite (imClockwise 100)

orangeScanner :: GroupAnimation IMove7s
orangeScanner = slowScanner (pure imOpen) imNoise imOrange imStatic

rainbowScanner :: GroupAnimation IMove7s
rainbowScanner = narrowScanner (pure imOpen) imNone (imColorChange 200) imStatic

whiteBeamScanner :: GroupAnimation IMove7s
whiteBeamScanner = narrowScanner (pure imOpen) imDots imWhite (imClockwise 255)

swirlScanner :: GroupAnimation IMove7s
swirlScanner = narrowScanner (pure imOpen) imSwirl imYellow (imAntiClockwise 128)



------------------------------------------------------------------------
-- Breaks

break :: Animation Context Bool -> Scene -> Scene -> Scene
break p b c = while p ((b &&& c) >>^ override) c
  where
    override :: (Frame, Frame) -> Frame
    override (a :&: b :&: c :&: _ :&: INil, _ :&: _ :&: _ :&: d :&: INil) =
      a :&: b :&: c :&: blank d :&: INil

breakBeats :: Int -> Scene -> Scene -> Scene
breakBeats beats = break $ beat >>> count >>^ (< beats)

strobeBreak :: Int -> Scene -> Scene
strobeBreak beats = breakBeats beats (scene bo (strobe 1) (strobe 1) home)

rainbowBreak :: Int -> Scene -> Scene
rainbowBreak beats = breakBeats beats (scene bo (sequenceRGB 1) (sequenceRGB 1) home)



------------------------------------------------------------------------
-- Scenes

initial :: Scene
initial = scene orangeChain (pure purple) (pure $ pure amber) home

trap :: Scene
trap = scene (blueOrange 125) (peakFlicker $ blueOrange 0) (blueOrange 250) slowRotatingScanner

trap2 :: Scene
trap2 = scene (vuMeter white (dim 20 purple)) purpleWhite purpleWhite sphereScanner

sunset :: Scene
sunset = scene (speed 10 >>> chase (sunsetColors 80)) (speed 15 >>> sequence (sunsetColors 80)) (pure red) orangeScanner

highEnergy :: Scene
highEnergy = scene (peak >>^ flip dim white) (beat >>> sequence [red, green, blue]) (beat >>> sequence [red, green, blue]) rainbowScanner

highEnergy2:: Scene
highEnergy2 = scene (speed 1 >>> chase [red, green, blue]) (beat >>> sequence [red, green, blue]) (beat >>> sequence [green, blue, red]) whiteBeamScanner

dance :: Scene
dance = scene (vuMeter lightBlue black) (beat >>> chase [red, green, blue]) (beat >>> chase [green, blue, red]) rainbowScanner

dance2 :: Scene
dance2 = scene (beat >>> chase [red, green, blue]) (beat >>> sequence [red, green, blue, yellow]) (beat >>> chase [green, blue, yellow, red]) swirlScanner

dance3 :: Scene
dance3 = scene (beat >>> chase [purple, purple, purple, orange, orange, orange]) (beat >>> sequence [purple, orange, lightBlue]) (beat >>> sequence [amber, lightBlue, purple]) orangeScanner

redWhite :: Scene
redWhite = scene (beat >>> zigzag 20 (chase [red, white, white])) (beat >>> chase [red, white, red]) (beat >>> chase [red, red, white, red]) whiteBeamScanner
