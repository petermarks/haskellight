module Demo where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Control.Concurrent

import DemoShow
import Haskellight
import Animation
import qualified UDMX as U
import Generic

render :: Frame -> ByteString
render = toStrict . toLazyByteString . demoRig

setFrame :: Frame -> IO ()
setFrame = U.withUDMX . U.set . render

runAnimation :: Animation Int Frame -> IO ()
runAnimation = U.withUDMX . go 0
  where
    go i a h = do
      let (a', f) = step i a
      U.set (render f) h
      threadDelay 40000
      go (i + 1) a' h

