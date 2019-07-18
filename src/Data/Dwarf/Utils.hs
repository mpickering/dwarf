module Data.Dwarf.Utils where

import           Data.Binary.Get (getByteString, getWord8, Get, runGet)
import qualified Data.Binary.Get as Get
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Int (Int64, Int8)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import           Data.Word (Word64, Word8)

whileJust :: (Applicative m, Monad m) => m (Maybe a) -> m [a]
whileJust act = go
  where
    go = do
      res <- act
      case res of
        Nothing -> pure []
        Just x -> (x :) <$> go

-- Repeatedly perform the get operation until the boolean fails.
whileM :: Monad m => (a -> Bool) -> m a -> m [a]
whileM cond act = whileJust $ do
  res <- act
  pure $
    if cond res
    then Just res
    else Nothing

condAct :: (Applicative m, Monad m) => m Bool -> m a -> m (Maybe a)
condAct cond act = do
  e <- cond
  if e
    then pure Nothing
    else Just <$> act

getWhileNotEmpty :: Get a -> Get [a]
getWhileNotEmpty = whileJust . condAct Get.isEmpty

getByteStringLen :: Integral a => Get a -> Get B.ByteString
getByteStringLen lenGetter =
  getByteString =<< fromIntegral <$> lenGetter

getUTF8Str0 :: Get Text
getUTF8Str0 = Encoding.decodeUtf8 . B.pack <$> whileM (/= 0) getWord8

getNonEmptyUTF8Str0 :: Get (Maybe Text)
getNonEmptyUTF8Str0 = do
  str <- getUTF8Str0
  pure $ if Text.null str then Nothing else Just str

-- | Get a signed int in Little Endian Base 128
-- This is taken from the haskus package
getSLEB128 :: Get Int64
getSLEB128 = do
   let toInt8 :: Word8 -> Int8
       toInt8 = fromIntegral
   a <- getWord8
   if not (testBit a 7)
      then return . fromIntegral . toInt8 $ (a .&. 0x7f) .|. ((a .&. 0x40) `shiftL` 1)
      else do
         b <- getSLEB128
         return $ (b `shiftL` 7) .|. (fromIntegral (a .&. 0x7f))

-- Decode an unsigned little-endian base 128 encoded integer.
getULEB128 :: Get Word64
getULEB128 =
    let go acc shift' = do
        byte <- fromIntegral <$> getWord8 :: Get Word64
        let temp = acc .|. (clearBit byte 7 `shiftL` shift')
        if testBit byte 7 then
            go temp (shift' + 7)
         else
            pure temp
    in go 0 0

getAt :: Get a -> Word64 -> B.ByteString -> a
getAt action offset = strictGet $ Get.skip (fromIntegral offset) *> action

strictGet :: Get a -> B.ByteString -> a
strictGet action bs = runGet action $ L.fromChunks [bs]
