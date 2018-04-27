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
import CRUD

spec :: Spec
spec = with app $ do
  describe "GET /schedule" $ 
    it "responds with its state" $ do
      get "/schedule" `shouldRespondWith` [json|[]|]

  describe "POST /schedule" $ 
    it "saves the scheduled item" $ do
      let entry = P.ScheduleEntry "ff" "dd" "tf"
      let entity = B.fromStrict $ encodeMessage entry
      post "/schedule" entity `shouldRespondWith` [json|{entryId: "ff", datetime: "dd", toFire: "tf"}|]
      get "/schedule" `shouldRespondWith` [json|[{entryId: "ff", datetime: "dd", toFire: "tf"}]|]
  where
    app = serve api . server <$> newInMemoryService
    -- do
    --   service <- newInMemoryService
    --   return $ serve api $ server service
