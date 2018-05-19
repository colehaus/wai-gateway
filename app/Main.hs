{-# LANGUAGE TypeApplications #-}

module Main where

import AWSLambda.Events.APIGateway (apiGatewayMain)
import Data.Convertible (convert)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (runEnv)
import System.Environment (lookupEnv)

import AWSLambda.Events.APIGateway.Wai

main :: IO ()
main = do
  useWarp <- lookupEnv "WARP"
  case useWarp of
    Just _ -> runEnv 8000 echo
    Nothing -> apiGatewayMain . fromWai @Text @Text convert convert $ echo

echo :: Wai.Application
echo req sendResponse = do
  print req
  body <- Wai.strictRequestBody req
  sendResponse . Wai.responseLBS HTTP.ok200 mempty $ body <> body
