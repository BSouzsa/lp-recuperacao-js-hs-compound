{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Scotty
import           Network.HTTP.Types          ( status400, status204 )
import           Data.Aeson                  ( FromJSON, ToJSON )
import qualified Data.Aeson                 as A
import           GHC.Generics                ( Generic )
import           Data.Text.Lazy              ( Text )
import qualified Data.Text.Lazy             as T
import qualified Data.ByteString.Lazy       as BL
import           System.Environment          ( lookupEnv )
import           Data.Maybe                  ( fromMaybe )
import           Text.Read                   ( readMaybe )

data CompoundRequest = CompoundRequest
  { principal    :: Double
  , rate         :: Double
  , timesPerYear :: Int
  , years        :: Double
  } deriving (Show, Generic)

instance FromJSON CompoundRequest

data CompoundResponse = CompoundResponse
  { amount :: Double
  } deriving (Show, Generic)

instance ToJSON CompoundResponse

data ErrorResponse = ErrorResponse
  { error :: Text
  } deriving (Show, Generic)

instance ToJSON ErrorResponse

validate :: CompoundRequest -> Either Text CompoundRequest
validate req
  | principal req <= 0    = Left "principal must be > 0"
  | years req <= 0        = Left "years must be > 0"
  | timesPerYear req < 1  = Left "timesPerYear must be >= 1"
  | isNaN (rate req)      = Left "rate must be a valid number"
  | otherwise             = Right req

compound :: CompoundRequest -> Double
compound req =
  let p = principal req
      r = rate req
      n = fromIntegral (timesPerYear req)
      t = years req
  in  p * (1 + r / n) ** (n * t)

addCors :: Text -> ActionM ()
addCors origin = do
  setHeader "Access-Control-Allow-Origin" origin
  setHeader "Access-Control-Allow-Headers" "Content-Type"
  setHeader "Access-Control-Allow-Methods" "POST, OPTIONS"

main :: IO ()
main = do
  -- Em produção, o Render injeta a porta na variável de ambiente PORT
  portEnv <- lookupEnv "PORT"
  let defaultPort = 8080
      port = fromMaybe defaultPort (portEnv >>= readMaybe)

  putStrLn $ "Starting server on port: " ++ show port

  -- Para evitar dor de cabeça com CORS na correção, libera geral.
  -- Se quiser travar só pro front da Vercel depois, é só trocar por essa URL.
  let frontendOrigin = "*"

  scotty port $ do
  
    get "/" $ do
      text "API online"

    options "/api/compound" $ do
      addCors frontendOrigin
      status status204
      text ""

    post "/api/compound" $ do
      addCors frontendOrigin
      rawBody <- body
      case A.eitherDecode rawBody of
        Left _ -> do
          status status400
          json (ErrorResponse "invalid JSON payload")
        Right req ->
          case validate req of
            Left msg -> do
              status status400
              json (ErrorResponse msg)
            Right okReq ->
              json (CompoundResponse (compound okReq))
