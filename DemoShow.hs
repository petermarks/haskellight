{-# LANGUAGE DataKinds #-}

module DemoShow where

import Prelude hiding (sequence, break, sin, take, drop)
import qualified Prelude as P
import Data.Bits
import Data.List (unfoldr)

import Haskellight

type Frame = IVect Stream [RGB, RGBW]
type Scene = Animation Context Frame

frame :: f a -> f b -> IVect f [a, b]
frame a b = a :&: b :&: INil

demoRig :: Fixture Frame
demoRig = rig $ Group 20 rgbPixel :&: Group 2 ledj252 :&: INil

scene :: StreamAnimation c a -> StreamAnimation c b -> RigAnimation c '[a, b]
scene a b = (:&:) <$> a <*> ((:&:) <$> b <*> pure INil) 

bo :: (Color c) => StreamAnimation a c
bo = pure mempty

blackout :: Scene
blackout = scene bo bo

startShow :: IO (Animator Context Frame)
startShow = start blackout $ runner demoRig

dummyShow :: IO (Animator Context Frame)
dummyShow = start blackout $ dummyRunner demoRig



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



------------------------------------------------------------------------
-- Patterns

chaseRGB :: (Color rgb) => Int -> GroupAnimation rgb
chaseRGB s = speed s >>> chase [red, green, blue]

sequenceRGB :: (Color rgb) => Int -> GroupAnimation rgb
sequenceRGB s = speed s >>> sequence [red, green, blue]

vuMeter :: RGB -> RGB -> GroupAnimation RGB
vuMeter fg bg = vu 0 255 40 >>> grow fg bg

vuMeter2 :: RGB -> RGB -> GroupAnimation RGB
vuMeter2 fg bg = vu 0 255 20 >>> zigzag 10 (grow fg bg)

sunsetColors :: (Color c) => Int -> [c]
sunsetColors l = unfoldr gen 0
  where
    gen i
      | i < l     = Just (rgb 255 (sin l i `shiftR` 1) 0, succ i)
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
    f (r :$: g :$: xs) = (rgb (shiftR r 1 .|. 0x80) (shiftR g 3) 0, xs)

randomCycle :: (Color c) => Int -> Int -> [c] -> GroupAnimation c
randomCycle s variations cs = mkRandom $ (speed 1 >>>) . fadeColors s . P.take variations . map pick
  where
    pick i = drop (i `mod` l) stream
    stream = cycleList cs
    l = length cs

purpleWhite :: (Color c) => GroupAnimation c
purpleWhite = randomCycle 50 30 [white, purple, purple, purple]

purpleOrange :: (Color c) => GroupAnimation c
purpleOrange = speed 1 >>> fade 50 [purple, orange]

sunsetChase :: (Color c) => GroupAnimation c
sunsetChase = speed 3 >>> chase (sunsetColors 120)


------------------------------------------------------------------------
-- Breaks

break :: Int -> Scene -> Scene -> Scene
break beats = while $ beat >>> count >>^ (< beats)

strobeBreak :: Int -> Scene -> Scene
strobeBreak beats = break beats (scene bo (strobe 1))

rainbowBreak :: Int -> Scene -> Scene
rainbowBreak beats = break beats (scene (sequenceRGB 1) (sequenceRGB 1))



------------------------------------------------------------------------
-- Scenes

initial :: Scene
initial = scene orangeChain (pure purple)

trap :: Scene
trap = scene (blueOrange 125) (peakFlicker $ blueOrange 0)

trap2 :: Scene
trap2 = scene (vuMeter white (dim 20 purple)) purpleWhite

sunset :: Scene
sunset = scene (speed 10 >>> chase (sunsetColors 80)) (speed 15 >>> sequence (sunsetColors 80))

highEnergy :: Scene
highEnergy = scene (peak >>^ flip dim white) (beat >>> sequence [red, green, blue])

highEnergy2:: Scene
highEnergy2 = scene (speed 1 >>> chase [red, green, blue]) (beat >>> sequence [red, green, blue])

dance :: Scene
dance = scene (vuMeter lightBlue black) (beat >>> chase [red, green, blue])

dance2 :: Scene
dance2 = scene (beat >>> chase [red, green, blue]) (beat >>> sequence [red, green, blue, yellow])

dance3 :: Scene
dance3 = scene (beat >>> chase [purple, purple, purple, orange, orange, orange]) (beat >>> sequence [purple, orange, lightBlue])

redWhite :: Scene
redWhite = scene (beat >>> zigzag 20 (chase [red, white, white])) (beat >>> chase [red, white, red])

slow :: Scene
slow = scene sunsetChase purpleOrange
