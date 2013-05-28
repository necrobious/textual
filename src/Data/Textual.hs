{-# LANGUAGE FlexibleInstances,TypeSynonymInstances #-}
module Data.Textual where

import qualified Data.Text                  as StrictText
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Encoding         as StrictTextEncoding
import qualified Data.Text.Lazy.Encoding    as LazyTextEncoding
import qualified Data.ByteString            as StrictByteString
import qualified Data.ByteString.Char8      as StrictChar8 
import qualified Data.ByteString.Lazy       as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as LazyChar8


class Textual a where
  toLazyText :: a -> LazyText.Text
  toText     :: a -> StrictText.Text
  toString   :: a -> String
  toUtf8BS   :: a -> StrictByteString.ByteString  
  toUtf8LBS  :: a -> LazyByteString.ByteString

instance Textual StrictText.Text where
  toLazyText = LazyText.fromChunks . (:[]) 
  toText     = id 
  toString   = StrictText.unpack 
  toUtf8BS   = StrictTextEncoding.encodeUtf8  
  toUtf8LBS  = LazyTextEncoding.encodeUtf8 . toLazyText

instance Textual LazyText.Text where
  toLazyText = id 
  toText     = StrictText.concat . LazyText.toChunks
  toString   = LazyText.unpack 
  toUtf8BS   = StrictTextEncoding.encodeUtf8 . toText
  toUtf8LBS  = LazyTextEncoding.encodeUtf8

instance Textual String where
  toLazyText = LazyText.pack
  toText     = StrictText.pack
  toString   = id
  toUtf8BS   = StrictChar8.pack 
  toUtf8LBS  = LazyChar8.pack

instance Textual StrictByteString.ByteString where
  toLazyText = LazyTextEncoding.decodeUtf8 . toUtf8LBS
  toText     = StrictTextEncoding.decodeUtf8
  toString   = StrictChar8.unpack
  toUtf8BS   = id 
  toUtf8LBS  = LazyByteString.fromChunks . (:[]) 

instance Textual LazyByteString.ByteString where
  toLazyText = LazyTextEncoding.decodeUtf8
  toText     = StrictTextEncoding.decodeUtf8 . toUtf8BS
  toString   = LazyChar8.unpack
  toUtf8BS   = StrictByteString.concat . LazyByteString.toChunks
  toUtf8LBS  = id


