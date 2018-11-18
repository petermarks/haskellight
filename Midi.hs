module Midi where

import Control.Exception
-- import Control.Monad
import Data.Coerce
import Data.Word
import Foreign.C.Types
import Sound.RtMidi

listenMidi :: ([Word8] -> IO ()) -> IO ()-> IO ()
listenMidi f wait = bracket
  (createOutput AlsaApi "Haskellight")
  (\out -> do
    closePort out
    closeOutput out
  )
  (\out -> do
    openVirtualPort out "Haskellight port"
    -- port <- findPort out "Virtual Raw MIDI 1-0"
    -- openPort out port "Haskellight port"
    bracket
      (createInput AlsaApi "Haskellight" 128)
      (\dev -> do
        closePort dev
        cancelCallback dev
        closeInput dev
      )
      (\dev -> do
        setCallback dev (\_ msg -> f (coerce msg))
        openVirtualPort dev "Haskellight port"
        -- port <- findPort dev "Virtual Raw MIDI 1-0"
        -- openPort dev port "Haskellight port"
        wait
      )
  )

-- findPort :: Device -> String -> IO Int
-- findPort dev target = do
--   count <- portCount dev
--   foldr check (fail $ "port " ++ target ++ " not found") [0..count - 1]
--   where
--     check port cont = do
--       name <- portName dev port
--       if target `isPrefixOf` name then return port else cont