{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Protobuf where

import Servant
import Servant.API.ContentTypes
import Data.ByteString.Lazy as B
import Data.ProtoLens.Encoding
import Data.ProtoLens
import qualified Data.ProtoLens.JSON as PBJ
import Data.List.NonEmpty
import Network.HTTP.Media ((//))
import qualified Data.Word8 as W8

data Protobuf wireFormat = Protobuf wireFormat

instance Accept (Protobuf OctetStream) where
    contentType _ = "application" // "octet-stream+protobuf"

instance Accept (Protobuf JSON) where
    contentType _ = "application" // "json"

instance (Message a) => MimeRender (Protobuf OctetStream) a where
    mimeRender _ msg = fromStrict $ encodeMessage msg

instance (Message a) => MimeRender (Protobuf JSON) a where
    mimeRender _ = PBJ.encodeMessageJSON

instance {-# OVERLAPS #-} (Message a) => MimeRender (Protobuf JSON) [a] where
    mimeRender _ msgs = 
        B.cons W8._bracketleft $ B.snoc encodedJsonMsgs W8._bracketright where
            encodedJsonMsgs = B.intercalate "," jsonMsgs
            jsonMsgs = Prelude.map PBJ.encodeMessageJSON msgs

instance (Message a) => MimeUnrender (Protobuf OctetStream) a where
    mimeUnrender _ body = decodeMessage $ toStrict body
  