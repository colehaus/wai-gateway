module AWSLambda.Events.APIGateway.Wai where

import AWSLambda.Events.APIGateway as AWS
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.IORef (newIORef, writeIORef)
import Data.Maybe (fromJust)
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai

import AWSLambda.Events.APIGateway.Wai.Internal

fromWai ::
     (Eq t, Monoid t)
  => (s -> ByteString)
  -> (Lazy.ByteString -> t)
  -> Wai.Application
  -> APIGatewayProxyRequest s
  -> IO (APIGatewayProxyResponse t)
fromWai f g app req =
  fromWaiResponse g . fromJust <=< withIORef Nothing $ \responseRef -> do
    first <- newIORef True
    app
      (toWaiRequest f first req)
      (fmap (const Wai.ResponseReceived) . writeIORef responseRef . Just)
