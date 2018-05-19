{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AWSLambda.Events.APIGateway.Wai.Internal where

import AWSLambda.Events.APIGateway as AWS hiding (requestBody)
import qualified AWSLambda.Events.APIGateway as AWS
import Control.Lens
import qualified Data.Aeson.TextValue as Aeson
import Data.ByteString (ByteString, intercalate)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (splitOn)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Network.HTTP.Types as HTTP
import qualified Network.Socket as Net
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai

toWaiRequest ::
     (t -> ByteString) -> IORef Bool -> APIGatewayProxyRequest t -> Wai.Request
toWaiRequest f first req = Wai.Request {..}
  where
    requestMethod = req ^. agprqHttpMethod
    parseProtocolVersion prot =
      case splitOn "/" prot of
        ["HTTP", version] ->
          case decimal <$> splitOn "." version of
            [Right (httpMajor, _), Right (httpMinor, _)] ->
              Right HttpVersion {..}
            _ -> Left "Couldn't parse protocol version"
        _ -> Left "Couldn't parse protocol"
    httpVersion =
      req ^. agprqRequestContext . prcProtocol .
      to (either error id . parseProtocolVersion)
    rawPathInfo = req ^. agprqPath
    rawQueryString =
      req ^. agprqQueryStringParameters .
      to (intercalate "&" . fmap toParamString)
      where
        toParamString (l, Nothing) = l
        toParamString (l, Just r) = l <> "=" <> r
    requestHeaders = req ^. agprqHeaders
    isSecure = True
    remoteHost =
      Net.SockAddrInet
        443
        (req ^. agprqRequestContext . prcIdentity . riSourceIp .
         to Net.tupleToHostAddress)
    pathInfo = req ^. agprqPath . to (tail . splitOn "/" . decodeUtf8)
    queryString = req ^. agprqQueryStringParameters
    requestBody' = req ^? AWS.requestBody . _Just . to f
    requestBody = yieldOnce (fromMaybe mempty requestBody') first
    vault = mempty
    requestBodyLength =
      Wai.KnownLength . maybe 0 (fromIntegral . BS.length) $ requestBody'
    requestHeaderHost = req ^. agprqHeaders . to (lookup "Host")
    requestHeaderRange = req ^. agprqHeaders . to (lookup "Range")
    requestHeaderReferer = req ^. agprqHeaders . to (lookup "Referer")
    requestHeaderUserAgent = req ^. agprqHeaders . to (lookup "User-Agent")

yieldOnce :: Monoid b => b -> IORef Bool -> IO b
yieldOnce payload ref = do
  first <- readIORef ref
  if first
    then payload <$ writeIORef ref False
    else mempty

withIORef :: b -> (IORef b -> IO a) -> IO b
withIORef a m = do
  ref <- newIORef a
  _ <- m ref
  readIORef ref

fromWaiResponse ::
     (Eq t, Monoid t)
  => (Lazy.ByteString -> t)
  -> Wai.Response
  -> IO (APIGatewayProxyResponse t)
fromWaiResponse g res = do
  _agprsBody <-
    waiBody $ \f ->
      fmap mungeBody . withIORef mempty $ \contentRef ->
        f (\chunk -> modifyIORef' contentRef (<> chunk)) mempty
  pure APIGatewayProxyResponse {..}
  where
    mungeBody = nonEmpty . g . toLazyByteString
    nonEmpty x
      | x == mempty = Nothing
      | otherwise = Just (Aeson.TextValue x)
    (HTTP.Status _agprsStatusCode _, waiHeaders, waiBody) =
      Wai.responseToStream res
    _agprsHeaders = waiHeaders
