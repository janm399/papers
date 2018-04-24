{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ScheduleSpec where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Servant
import Test.Hspec
import Schedule
import Test.Hspec.Wai

instance Ctx where
    findAll = return "all"
    save x = B.putStrLn x

spec :: Ctx => Spec
spec = with (return app) $ 
  describe "GET /schedule" $ do
      it "responds with 'Simple'" $ do
        all <- findAll
        get "/item" `shouldRespondWith` all
  where
    app = serve api server

instance IsString MatchBody where
   fromString = bodyEquals . encodeUtf8   