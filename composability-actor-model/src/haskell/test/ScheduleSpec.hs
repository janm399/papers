{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ScheduleSpec where

import qualified Data.ByteString.Lazy as B
import Servant
import Test.Hspec
import Schedule
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Test.Hspec.Wai.JSON
import qualified Proto.Cam.Messages as P

instance Ctx where
    findAll = return [P.ScheduleEntry "" "" ""]
    save x = B.putStrLn "done"

spec :: Spec
spec = with (return app) $ do
  describe "GET /schedule" $ 
    it "responds with its state" $ do
      all <- liftIO findAll
      get "/schedule" `shouldRespondWith` 200 { matchBody = bodyEquals "" } 

  describe "POST /schedule" $ 
    it "saves the scheduled item" $ do
      post "/schedule" "some body" `shouldRespondWith` 200 { matchBody = bodyEquals B.empty }
      all <- liftIO findAll
      get "/schedule" `shouldRespondWith` 200 { matchBody = bodyEquals "" } 
  where
    app = serve api server
