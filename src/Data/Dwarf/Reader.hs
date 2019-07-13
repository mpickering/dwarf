{-# LANGUAGE DeriveGeneric #-}
module Data.Dwarf.Reader where

import           Data.Binary.Get (getWord16be, getWord32be, getWord64be, getWord16le, getWord32le, getWord64le, Get)
import qualified Data.Binary.Get as Get
import           Data.Word (Word16, Word32, Word64)
import           GHC.Generics (Generic)

data Endianess = LittleEndian | BigEndian
  deriving (Eq, Ord, Read, Show, Generic)


data Encoding = Encoding32 | Encoding64
  deriving (Eq, Ord, Read, Show, Generic)


data TargetSize = TargetSize32 | TargetSize64
  deriving (Eq, Ord, Read, Show, Generic)


endianReader :: Endianess -> EndianReader
endianReader LittleEndian = EndianReader LittleEndian getWord16le getWord32le getWord64le
endianReader BigEndian    = EndianReader BigEndian    getWord16be getWord32be getWord64be
endianSizeReader :: Encoding -> EndianReader -> EndianSizeReader
endianSizeReader Encoding64 der = EndianSizeReader der Encoding64 0xffffffffffffffff (derGetW64 der)
endianSizeReader Encoding32 der = EndianSizeReader der Encoding32 0xffffffff (fromIntegral <$> derGetW32 der)
reader :: TargetSize -> EndianSizeReader -> Reader
reader TargetSize64 desr = Reader desr TargetSize64 0xffffffffffffffff (desrGetW64 desr)
reader TargetSize32 desr = Reader desr TargetSize32 0xffffffff         $ fromIntegral <$> desrGetW32 desr

desrGetW16 :: EndianSizeReader -> Get Word16
desrGetW16 = derGetW16 . desrEndianReader
desrGetW32 :: EndianSizeReader -> Get Word32
desrGetW32 = derGetW32 . desrEndianReader
desrGetW64 :: EndianSizeReader -> Get Word64
desrGetW64 = derGetW64 . desrEndianReader
drGetW16 :: Reader -> Get Word16
drGetW16 = desrGetW16 . drDesr
drGetW32 :: Reader -> Get Word32
drGetW32 = desrGetW32 . drDesr
drGetW64 :: Reader -> Get Word64
drGetW64 = desrGetW64 . drDesr
drGetOffset :: Reader -> Get Word64
drGetOffset = desrGetOffset . drDesr
drLargestOffset :: Reader -> Word64
drLargestOffset = desrLargestOffset . drDesr

-- Intermediate data structure for a partial Reader.
data EndianReader = EndianReader
  { derEndianess :: Endianess
  , derGetW16 :: Get Word16
  , derGetW32 :: Get Word32
  , derGetW64 :: Get Word64
  }

-- Intermediate data structure for a partial Reader.
data EndianSizeReader = EndianSizeReader
  { desrEndianReader :: EndianReader
  , desrEncoding :: Encoding
  , desrLargestOffset :: Word64
  , desrGetOffset :: Get Word64
  }

-- | Type containing functions and data needed for decoding DWARF information.
data Reader = Reader
    { drDesr                  :: EndianSizeReader
    , drTarget64              :: TargetSize
    , drLargestTargetAddress  :: Word64     -- ^ Largest permissible target address.
    , drGetTargetAddress :: Get Word64 -- ^ Action for reading a pointer for the target machine.
    }

-- Decode the DWARF size header entry, which specifies both the size of a DWARF subsection and whether this section uses DWARF32 or DWARF64.
getUnitLength :: EndianReader -> Get (EndianSizeReader, Word64)
getUnitLength der = do
    size <- derGetW32 der
    if size == 0xffffffff then do
        size64 <- derGetW64 der
        pos <- Get.bytesRead
        pure (endianSizeReader Encoding64 der, fromIntegral pos + size64)
     else
      if size < 0xffffff00 then do
        pos <- Get.bytesRead
        pure (endianSizeReader Encoding32 der, fromIntegral pos + fromIntegral size)
      else
        fail $ "Invalid DWARF size: " ++ show size
