{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ScheduleSpec where

import qualified Data.ByteString.Lazy as B
import Servant
import Test.Hspec
import Schedule
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher

instance Ctx where
    findAll = return "all"
    save x = B.putStrLn x

spec :: Spec
spec = with (return app) $ do
  describe "GET /schedule" $ 
    it "responds with its state" $ do
      all <- liftIO findAll
      get "/schedule" `shouldRespondWith` 200 { matchBody = bodyEquals all } 

  describe "POST /schedule" $ 
    it "xxx" $ do
      post "/schedule" "some body" `shouldRespondWith` 200
      all <- liftIO findAll
      get "/schedule" `shouldRespondWith` 200 { matchBody = bodyEquals all } 
  where
    app = serve api server
