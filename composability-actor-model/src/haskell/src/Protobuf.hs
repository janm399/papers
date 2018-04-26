{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Protobuf where

import Servant
import Data.ByteString.Lazy as B
import Data.ProtoLens.Encoding
import Data.ProtoLens
import Data.ProtoLens.JSON
import Network.HTTP.Media ((//))

data Protobuf = Protobuf B.ByteString

instance Accept Protobuf where
    contentType _ = "application" // "octet-stream+protobuf"

instance (Message a) => MimeRender Protobuf a where
    mimeRender _ msg = fromStrict $ encodeMessage msg

instance (Message a) => MimeRender Protobuf [a] where
    mimeRender _ msg = fromStrict $ encodeMessage msg
      
instance (Message a) => MimeUnrender Protobuf a where
    mimeUnrender _ body = decodeMessage $ toStrict body
  