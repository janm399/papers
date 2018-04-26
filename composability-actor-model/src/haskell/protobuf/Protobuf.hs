{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Protobuf where

import Servant
import Servant.API.ContentTypes
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import Data.ProtoLens.Encoding
import Data.ProtoLens
import qualified Data.ProtoLens.JSON as PBJ
import Data.List.NonEmpty
import Network.HTTP.Media ((//))
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Word8 as W8

data Protobuf wireFormat = Protobuf wireFormat

instance Accept (Protobuf OctetStream) where
    contentType _ = "application" // "octet-stream"

instance Accept (Protobuf JSON) where
    contentType _ = "application" // "json"

instance Accept (Protobuf PlainText) where
    contentType _ = "text" // "plain" 
    
instance (Message a) => MimeRender (Protobuf OctetStream) a where
    mimeRender _ msg = fromStrict $ encodeMessage msg

instance (Message a) => MimeRender (Protobuf JSON) a where
    mimeRender _ = PBJ.encodeMessageJSON

instance (Message a) => MimeRender (Protobuf PlainText) a where
    mimeRender _ = BC.pack . showMessage
    
instance {-# OVERLAPS #-} (Message a) => MimeRender (Protobuf JSON) [a] where
    mimeRender _ msgs = 
        B.cons W8._bracketleft $ B.snoc encodedJsonMsgs W8._bracketright where
            encodedJsonMsgs = B.intercalate "," jsonMsgs
            jsonMsgs = Prelude.map PBJ.encodeMessageJSON msgs

instance (Message a) => MimeUnrender (Protobuf OctetStream) a where
    mimeUnrender _ body = decodeMessage $ toStrict body

instance (Message a) => MimeUnrender (Protobuf PlainText) a where
    mimeUnrender _ body = readMessage $ decodeUtf8 body