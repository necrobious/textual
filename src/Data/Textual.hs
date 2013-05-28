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
  toLazyText strictText = LazyText.fromChunks [strictText] 
  toText     strictText = strictText 
  toString   strictText = StrictText.unpack strictText 
  toUtf8BS   strictText = StrictTextEncoding.encodeUtf8 strictText 
  toUtf8LBS  strictText = LazyTextEncoding.encodeUtf8 (toLazyText strictText) 

instance Textual LazyText.Text where
  toLazyText lazyText   = lazyText
  toText     lazyText   = StrictText.concat (LazyText.toChunks lazyText)
  toString   lazyText   = LazyText.unpack lazyText
  toUtf8BS   lazyText   = StrictTextEncoding.encodeUtf8 (toText lazyText) 
  toUtf8LBS  lazyText   = LazyTextEncoding.encodeUtf8 lazyText

instance Textual String where
  toLazyText string     = LazyText.pack string
  toText     string     = StrictText.pack string
  toString   string     = string
  toUtf8BS   string     = StrictChar8.pack string 
  toUtf8LBS  string     = LazyChar8.pack string

instance Textual StrictByteString.ByteString where
  toLazyText strictBs   = LazyTextEncoding.decodeUtf8 (toUtf8LBS strictBs) 
  toText     strictBs   = StrictTextEncoding.decodeUtf8 strictBs   
  toString   strictBs   = StrictChar8.unpack strictBs
  toUtf8BS   strictBs   = strictBs
  toUtf8LBS  strictBs   = LazyByteString.fromChunks [strictBs]

instance Textual LazyByteString.ByteString where
  toLazyText lazyBs     = LazyTextEncoding.decodeUtf8 lazyBs
  toText     lazyBs     = StrictTextEncoding.decodeUtf8 (toUtf8BS lazyBs)
  toString   lazyBs     = LazyChar8.unpack lazyBs    
  toUtf8BS   lazyBs     = StrictByteString.concat (LazyByteString.toChunks lazyBs)
  toUtf8LBS  lazyBs     = lazyBs


