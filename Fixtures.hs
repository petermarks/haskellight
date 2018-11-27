{-# language GADTs, MultiParamTypeClasses #-} 

module Fixtures where

import Data.Bits
import Data.ByteString.Builder
import Data.Monoid
import Data.Word

import Generic

data RGB = RGB Word8 Word8 Word8
data RGBW = RGBW Word8 Word8 Word8 Word8
data RGBA = RGBA Word8 Word8 Word8 Word8

dimWord :: Word8 -> Word8 -> Word8
dimWord a b = fromIntegral r
  where
    r :: Int
    r = fromIntegral a * fromIntegral b `div` 255

class (Monoid c) => Color c where
  rgb :: Word8 -> Word8 -> Word8 -> c
  grey :: Word8 -> c
  grey v = rgb v v v
  dim :: Word8 -> c -> c

instance Color RGB where
  rgb = RGB
  dim l (RGB r g b) = RGB (dimWord l r) (dimWord l g) (dimWord l b)

instance Semigroup RGB where
  (RGB r1 g1 b1) <> (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)

instance Monoid RGB where
  mempty = grey 0

instance Color RGBW where
  rgb r g b = RGBW r g b 0
  grey v = RGBW v v v v
  dim l (RGBW r g b w) = RGBW (dimWord l r) (dimWord l g) (dimWord l b) (dimWord l w)
  
instance Semigroup RGBW where
  (RGBW r1 g1 b1 w1) <> (RGBW r2 g2 b2 w2) = RGBW (r1 + r2) (g1 + g2) (b1 + b2) (w1 + w2)

instance Monoid RGBW where
  mempty = grey 0

instance Color RGBA where
  rgb r g b = RGBA r g b 0
  grey v = RGBA v v v v
  dim l (RGBA r g b a) = RGBA (dimWord l r) (dimWord l g) (dimWord l b) (dimWord l a)
  
instance Semigroup RGBA where
  (RGBA r1 g1 b1 a1) <> (RGBA r2 g2 b2 a2) = RGBA (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)

instance Monoid RGBA where
  mempty = grey 0

instance (Color c) => Color (Stream c) where
  rgb r g b = pure $ rgb r g b
  grey = pure . grey
  dim l = fmap (dim l)
  
mix :: (Color c) => Word8 -> c -> c -> c
mix l c1 c2 = dim l c1 <> dim (255 - l) c2

data IMPos = IMPos !Word8 !Word8
newtype IMShutter = IMShutter Word8
newtype IMGobo = IMGobo Word8
newtype IMColor = IMColor Word8
newtype IMRotation = IMRotation Word8
newtype IMDimmer = IMDimmer Word8

data IMove7s = IMove7s IMPos IMShutter IMGobo IMColor IMRotation IMDimmer

class MovingHead m where
  blank :: m -> m
  reflect :: m -> m

instance MovingHead IMove7s where
  blank   (IMove7s p _ g c r d) = IMove7s p imClosed g c r d
  reflect (IMove7s (IMPos p t) s g c (IMRotation r) d) = IMove7s (IMPos (complement p) t) s g c (IMRotation (complement r)) d

instance (MovingHead m) => MovingHead (Stream m) where
  blank = fmap blank
  reflect = fmap reflect

-- Pan: 128 is centre, 32 units = 67.5 degrees
-- Tilt: 223 is horizontal, 32 units ~= 45 degrees

imOpen :: IMShutter
imOpen = IMShutter 8

imClosed :: IMShutter
imClosed = IMShutter 0

imShake :: Word8 -> IMShutter
imShake v = IMShutter $ if v == 0 then 8 else shiftR v 4 + 132

imStrobe :: Word8 -> IMShutter
imStrobe v = IMShutter $ if v == 0 then 0 else min 131 $ shiftR v 1 + 16

imNone :: IMGobo
imNone = IMGobo 0

imDots :: IMGobo
imDots = IMGobo 16

imFlower :: IMGobo
imFlower = IMGobo 32

imSwirl :: IMGobo
imSwirl = IMGobo 48

imSphere :: IMGobo
imSphere = IMGobo 64

imNoise :: IMGobo
imNoise = IMGobo 80

imTriangle :: IMGobo
imTriangle = IMGobo 96 

imFour :: IMGobo
imFour = IMGobo 112

imGoboChange :: Word8 -> IMGobo
imGoboChange v = IMGobo $ shiftR v 1 .|. 0x80

imWhite :: IMColor
imWhite = IMColor 0 

imGreen :: IMColor
imGreen = IMColor 13 

imMagenta :: IMColor
imMagenta = IMColor 26

imLightBlue :: IMColor
imLightBlue = IMColor 39

imYellow :: IMColor
imYellow = IMColor 52

imOrange :: IMColor
imOrange = IMColor 64

imBlue :: IMColor
imBlue = IMColor 77

imPurple :: IMColor
imPurple = IMColor 90

imLightGreen :: IMColor
imLightGreen = IMColor 103

imPink :: IMColor
imPink = IMColor 116

imColorChange :: Word8 -> IMColor
imColorChange v = IMColor $ shiftR v 1 .|. 0x80

imStatic :: IMRotation
imStatic = IMRotation 0

imClockwise :: Word8 -> IMRotation
imClockwise v = IMRotation  $ max 135 $ min 245 $ shiftR v 1 .|. 0x80

imAntiClockwise :: Word8 -> IMRotation
imAntiClockwise = imClockwise . complement

imOff :: IMDimmer
imOff = IMDimmer 0

imFull :: IMDimmer
imFull = IMDimmer 255

type Fixture a = a -> Builder

rgbPixel :: Fixture RGB
rgbPixel (RGB r g b) = word8 r <> word8 g <> word8 b

rbgPixel :: Fixture RGB
rbgPixel (RGB r g b) = word8 r <> word8 b <> word8 g

ledj252 :: Fixture RGBW
ledj252 (RGBW r g b w) = word32BE 0xff000000 <> word8 r <> word8 g <> word8 b <> word8 w

ledj7q5 :: Fixture RGBA
ledj7q5 (RGBA r g b a) = word8 r <> word8 g <> word8 b <> word8 a

imove7s :: Fixture IMove7s
imove7s (IMove7s (IMPos p t) (IMShutter s) (IMGobo g) (IMColor c) (IMRotation r) (IMDimmer d)) =
  word8 p <> word8 t <> word8 s <> word8 g <> word8 c <> word8 r <> word8 d

data Group a = Group Int (Fixture a)

type Rig as = IVect Group as

rig :: Rig as -> Fixture (IVect Stream as)
rig INil _ = mempty
rig (Group i f :&: gs) (s :&: ss) = go i s <> rig gs ss
  where
    go 0 _ = mempty
    go i (a :$: as) = f a <> go (i - 1) as  