module UDMX where

import Control.Monad
import qualified Data.ByteString as B
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V 
import System.USB

vendorId :: VendorId
vendorId = 0x03EB

productId :: ProductId
productId = 0x8888

withUDMX :: (DeviceHandle -> IO ()) -> IO ()
withUDMX f = do
  ctx <- newCtx
  mdev <- findDevice ctx
  case mdev of
    Just dev -> withDeviceHandle dev f
    Nothing -> putStrLn "Device not found"

send :: Request -> Value -> Index -> B.ByteString -> DeviceHandle -> IO ()
send cmd v idx p h = void $ writeControl h setup p 500
  where
    setup = ControlSetup Vendor ToDevice cmd v idx

set :: B.ByteString -> DeviceHandle -> IO ()
set p = send 0x02 (fromIntegral $ B.length p) 0 p

-- get :: Handle -> IO B.ByteString
-- get h = do
--   send h 20 []
--   B.hGet h 7

checkDevice :: IO ()
checkDevice = do
  ctx <- newCtx
  mdev <- findDevice ctx
  case mdev of
    Just dev -> withDeviceHandle dev $ \h -> do
      desc <- getDeviceDesc dev
      s <- maybe (return $ T.pack "No description") (\ix -> getStrDescFirstLang h ix 100) $ deviceProductStrIx desc
      T.putStrLn s
    Nothing -> putStrLn "Device not found"

findDevice :: Ctx -> IO (Maybe Device)
findDevice ctx = do
    devs <- V.toList <$> getDevices ctx
    deviceDescs <- mapM getDeviceDesc devs
    return $ fmap fst $ find (match . snd) $ zip devs deviceDescs
  where
    match :: DeviceDesc -> Bool
    match devDesc =  
      deviceVendorId  devDesc == vendorId &&
      deviceProductId devDesc == productId
