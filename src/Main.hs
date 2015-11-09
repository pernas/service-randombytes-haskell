{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (object, (.=), Value (Object, String))
import System.IO (withBinaryFile, IOMode( ReadMode ))
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors                                    --wai-cors
import Network.Wai.Middleware.RequestLogger                          --wai-extra
import Network.HTTP.Types                                           --http-types
import Web.Scotty.Trans
import Data.String (fromString)
import Text.Read (readMaybe)
import qualified Data.ByteString        as BS (ByteString, length, hGet, empty)
import qualified Data.ByteString.Lazy   as BL (fromStrict)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString.Char8  as C (pack, unpack)
import qualified Data.Text.Lazy         as T

-- Define a custom exception type.
data Except = StringEx String deriving (Show, Eq)

instance ScottyError Except where
    stringError = StringEx
    showError = fromString . show

-- Handler for uncaught exceptions.
handleEx :: Monad m => Except -> ActionT Except m ()
handleEx (StringEx msg) = do
    status status400
    json $ object [ ("error" .= msg)]

-- Server code
main :: IO ()
main = do
  putStrLn "Starting RandomBytes Server..."
  scottyT 3000 id $ do
    middleware logStdoutDev    -- logs activated
    middleware simpleCors      -- add cors headers
    defaultHandler handleEx    -- define what to do with uncaught exceptions

    get "/v2/randombytes" $ do
      format  <- param "format"
      bytes   <- param "bytes"
      case ((readMaybe bytes) :: Maybe Int) of
        Just nbytes ->
          if nbytes >= 1 && nbytes <= maxLength
            then do
              entropy <- liftIO $ devURandom nbytes
              printEntropy format entropy
            else raise $ StringEx $ "1 <= bytes <= " ++ show maxLength
        Nothing -> raise $ StringEx "bytes must be integer"

    get "/healthz" $ json statusOK

    get "/statz" $ json statusOK

-- definitions
bsToString :: BS.ByteString -> String
bsToString = C.unpack

bsToHex :: BS.ByteString -> String
bsToHex = bsToString . B16.encode

bsToBase64 :: BS.ByteString -> String
bsToBase64 = bsToString . B64.encode

devURandom :: Int -> IO BS.ByteString
devURandom i = withBinaryFile "/dev/urandom" ReadMode $ flip BS.hGet i

printEntropy :: Monad m => T.Text -> BS.ByteString -> ActionT Except m ()
printEntropy "raw"    h = do
  setHeader "Content-Type" "application/octet-stream"
  raw $ BL.fromStrict h
printEntropy "base64" h = text $ T.pack $ bsToBase64 h
printEntropy "hex"    h = text $ T.pack $ bsToHex h
printEntropy _        h = raise $ StringEx "format must be 'hex', 'base64' or 'raw'"

statusOK :: Value
statusOK = object [ ("status" .= ("ok" :: String))]

maxLength :: Int
maxLength = 128
