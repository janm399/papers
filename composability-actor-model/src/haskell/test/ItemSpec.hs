{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module ItemSpec where

import Servant
import Test.Hspec
import Item
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec = with (return app) $ 
  describe "GET /item" $ do
    it "responds with 200" $ 
      get "/item" `shouldRespondWith` 200
    it "responds with 'Simple'" $ 
      get "/item" `shouldRespondWith` [json|[{itemId: 0, itemText: "example item"}]|]
  where
    app = serve api server
