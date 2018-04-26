{-# LANGUAGE OverloadedStrings #-}
module ProtobufSpec where

import Test.Hspec
import Servant
import Protobuf 
import Servant.API.ContentTypes (OctetStream, PlainText, JSON)
import qualified Proto.Cam.Messages as P
    
spec :: Spec
spec = 
    describe "Rendering and unrendering" $ do
        it "Handles text/plain" $ handle plainText
        it "Handles application/octet-stream" $ handle octetStream
    where
        plainText :: Proxy (Protobuf PlainText)
        plainText = Proxy
        json :: Proxy (Protobuf JSON)
        json = Proxy
        octetStream :: Proxy (Protobuf OctetStream)
        octetStream = Proxy
        handle :: ctype -> Expectation
        handle _ = do
            let schedule = P.ScheduleEntry "a" "b" "c"
            let rendered = mimeRender plainText schedule
            let Right schedule' = mimeUnrender plainText rendered :: (Either String P.ScheduleEntry)
            schedule `shouldBe` schedule'
