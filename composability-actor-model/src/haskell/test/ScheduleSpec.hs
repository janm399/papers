{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
module ScheduleSpec where

import qualified Data.ByteString.Lazy as B
import Servant
import Test.Hspec
import Schedule
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Test.Hspec.Wai.JSON
import qualified Proto.Cam.Messages as P
import Data.ProtoLens
import Data.IORef

testData :: IO (IORef [P.ScheduleEntry])
testData = newIORef []

instance Ctx where
  findAll = do
    d <- testData
    readIORef d

  save x = do
    d <- testData
    writeIORef d [x] 

spec :: Spec
spec = with (return app) $ do
  describe "GET /schedule" $ 
    it "responds with its state" $ do
      all <- liftIO findAll
      get "/schedule" `shouldRespondWith` [json|[ {entryId: "a", datetime: "b", toFire: "c"} ]|]

  describe "POST /schedule" $ 
    it "saves the scheduled item" $ do
      let entry = P.ScheduleEntry "ff" "dd" "tf"
      let entity = B.fromStrict $ encodeMessage entry
      post "/schedule" entity `shouldRespondWith` [json|{entryId: "ff", datetime: "dd", toFire: "tf"}|]
      all <- liftIO findAll
      get "/schedule" `shouldRespondWith` 200
  where
    app = serve api server
