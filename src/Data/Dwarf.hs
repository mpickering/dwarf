{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
-- | Parses the DWARF 2 and DWARF 3 specifications at http://www.dwarfstd.org given
-- the debug sections in ByteString form.
module Data.Dwarf ( parseDwarfInfo
                  , infoCompileUnit
                  , parseDwarfAranges
                  , parseDwarfPubnames
                  , parseDwarfPubtypes
                  , parseDwarfMacInfo
                  , parseDwarfRanges
                  , parseDwarfLoc
                  , parseDwarfLine
                  , parseDwarfFrame
                  , parseDW_OP
                  , dw_ate
                  , dw_ds
                  , dw_end
                  , dw_access
                  , dw_vis
                  , dw_virtuality
                  , dw_lang
                  , dw_inl
                  , dw_cc
                  , dw_ord
                  , dw_dsc
                  , (!?)
                  , DwarfReader(..)
                  , Tree(..), DIE(..), DieID
                  , DW_CFA(..)
                  , DW_MACINFO(..)
                  , DW_CIEFDE(..)
                  , DW_OP(..)
                  , DW_TAG(..)
                  , DW_AT(..)
                  , DW_ATVAL(..)
                  , DW_LNE(..)
                  , DW_ATE(..)
                  , DW_DS(..)
                  , DW_END(..)
                  , DW_ACCESS(..)
                  , DW_VIS(..)
                  , DW_VIRTUALITY(..)
                  , DW_LANG(..)
                  , DW_ID(..), dw_id
                  , DW_INL(..)
                  , DW_CC(..)
                  , DW_ORD(..)
                  , DW_DSC(..)
                  , Endianess(..)
                  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow ((&&&), (***))
import Control.Monad (replicateM)
import Data.Binary (Binary(..), getWord8)
import Data.Binary.Get (getByteString, getWord16be, getWord32be, getWord64be, getWord16le, getWord32le, getWord64le, Get, runGet)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR, clearBit, testBit)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (listToMaybe)
import Data.Traversable (traverse)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as M

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Utility functions.
---------------------------------------------------------------------------------------------------------------------------------------------------------------

whileMaybe :: (Applicative m, Monad m) => m (Maybe a) -> m [a]
whileMaybe act = go
  where
    go = do
      res <- act
      case res of
        Nothing -> pure []
        Just x -> (x :) <$> go

-- Repeatedly perform the get operation until the boolean fails.
whileM :: (Applicative m, Monad m) => (a -> Bool) -> m a -> m [a]
whileM cond act = whileMaybe $ do
  res <- act
  pure $
    if cond res
    then Just res
    else Nothing

getWhileNotEmpty :: Get a -> Get [a]
getWhileNotEmpty act = whileMaybe $ do
  e <- Get.isEmpty
  if e
    then pure Nothing
    else Just <$> act

getNullTerminatedUTF8String :: Get String
getNullTerminatedUTF8String = UTF8.toString . B.pack <$> whileM (/= 0) getWord8

-- Decode a signed little-endian base 128 encoded integer.
getSLEB128 :: Get Int64
getSLEB128 =
    let go acc shift = do
        byte <- fromIntegral <$> getWord8 :: Get Word64
        let temp = acc .|. (clearBit byte 7 `shiftL` shift)
        if testBit byte 7 then
            go temp (shift + 7)
         else
            if shift < 32  && testBit byte 6 then
                pure $ fromIntegral $ temp .|. ((-1) `shiftL` shift)
             else
                pure $ fromIntegral temp
    in go 0 0

-- Decode an unsigned little-endian base 128 encoded integer.
getULEB128 :: Get Word64
getULEB128 =
    let go acc shift = do
        byte <- fromIntegral <$> getWord8 :: Get Word64
        let temp = acc .|. (clearBit byte 7 `shiftL` shift)
        if testBit byte 7 then
            go temp (shift + 7)
         else
            pure temp
    in go 0 0

data Endianess = LittleEndian | BigEndian
  deriving (Eq, Ord, Read, Show)

-- Decode the DWARF size header entry, which specifies both the size of a DWARF subsection and whether this section uses DWARF32 or DWARF64.
getDwarfUnitLength :: DwarfEndianReader -> Get (DwarfEndianSizeReader, Word64)
getDwarfUnitLength der = do
    size <- derGetW32 der
    if size == 0xffffffff then do
        size64 <- derGetW64 der
        pure (dwarfEndianSizeReader DwarfEncoding64 der, size64)
     else if size >= 0xffffff00 then
        fail ("Invalid DWARF size " ++ show size)
      else
        pure (dwarfEndianSizeReader DwarfEncoding32 der, fromIntegral size)

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- DWARF decoder records.
---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Intermediate data structure for a partial DwarfReader.
data DwarfEndianReader = DwarfEndianReader
  { derEndianess :: Endianess
  , derGetW16 :: Get Word16
  , derGetW32 :: Get Word32
  , derGetW64 :: Get Word64
  }
dwarfEndianReader :: Endianess -> DwarfEndianReader
dwarfEndianReader LittleEndian = DwarfEndianReader LittleEndian getWord16le getWord32le getWord64le
dwarfEndianReader BigEndian    = DwarfEndianReader BigEndian    getWord16be getWord32be getWord64be

instance Show DwarfEndianReader where
  show der = "DwarfEndianReader " ++ show (derEndianess der)
instance Eq DwarfEndianReader where
    a1 == a2 = derEndianess a1 == derEndianess a2

data DwarfEncoding = DwarfEncoding32 | DwarfEncoding64
  deriving (Eq, Ord, Read, Show)

-- Intermediate data structure for a partial DwarfReader.
data DwarfEndianSizeReader = DwarfEndianSizeReader
  { desrEndianReader :: DwarfEndianReader
  , desrEncoding :: DwarfEncoding
  , desrLargestOffset :: Word64
  , desrGetDwarfOffset :: Get Word64
  }
dwarfEndianSizeReader :: DwarfEncoding -> DwarfEndianReader -> DwarfEndianSizeReader
dwarfEndianSizeReader DwarfEncoding64 der = DwarfEndianSizeReader der DwarfEncoding64 0xffffffffffffffff (derGetW64 der)
dwarfEndianSizeReader DwarfEncoding32 der = DwarfEndianSizeReader der DwarfEncoding32 0xffffffff (fromIntegral <$> derGetW32 der)

instance Show DwarfEndianSizeReader where
    show desr = "DwarfEndianSizeReader " ++ show (desrEndianReader desr) ++ " " ++ show (desrEncoding desr)
instance Eq DwarfEndianSizeReader where
    a1 == a2 = (desrEndianReader a1, desrEncoding a1) == (desrEndianReader a2, desrEncoding a2)

data TargetSize = TargetSize32 | TargetSize64
  deriving (Eq, Ord, Read, Show)

-- | Type containing functions and data needed for decoding DWARF information.
data DwarfReader = DwarfReader
    { drDesr                  :: DwarfEndianSizeReader
    , drTarget64              :: TargetSize
    , drLargestTargetAddress  :: Word64     -- ^ Largest permissible target address.
    , drGetDwarfTargetAddress :: Get Word64 -- ^ Action for reading a pointer for the target machine.
    }
instance Show DwarfReader where
    show dr = "DwarfReader " ++ show (drDesr dr) ++ " " ++ show (drTarget64 dr)
instance Eq DwarfReader where
    a1 == a2 = (drDesr a1, drTarget64 a1) == (drDesr a2, drTarget64 a2)
dwarfReader :: TargetSize -> DwarfEndianSizeReader -> DwarfReader
dwarfReader TargetSize64 desr = DwarfReader desr TargetSize64 0xffffffffffffffff (desrGetW64 desr)
dwarfReader TargetSize32 desr = DwarfReader desr TargetSize32 0xffffffff         $ fromIntegral <$> desrGetW32 desr

desrGetW16 :: DwarfEndianSizeReader -> Get Word16
desrGetW16 = derGetW16 . desrEndianReader
desrGetW32 :: DwarfEndianSizeReader -> Get Word32
desrGetW32 = derGetW32 . desrEndianReader
desrGetW64 :: DwarfEndianSizeReader -> Get Word64
desrGetW64 = derGetW64 . desrEndianReader
drGetW16 :: DwarfReader -> Get Word16
drGetW16 = desrGetW16 . drDesr
drGetW32 :: DwarfReader -> Get Word32
drGetW32 = desrGetW32 . drDesr
drGetW64 :: DwarfReader -> Get Word64
drGetW64 = desrGetW64 . drDesr
drGetDwarfOffset :: DwarfReader -> Get Word64
drGetDwarfOffset = desrGetDwarfOffset . drDesr
drLargestOffset :: DwarfReader -> Word64
drLargestOffset = desrLargestOffset . drDesr

data DW_FORM
    = DW_FORM_addr              -- ^ address
    | DW_FORM_block2 -- ^ block
    | DW_FORM_block4 -- ^ block
    | DW_FORM_data2 -- ^ constant
    | DW_FORM_data4 -- ^ constant, lineptr, loclistptr, macptr, rangelistptr
    | DW_FORM_data8 -- ^ constant, lineptr, loclistptr, macptr, rangelistptr
    | DW_FORM_string -- ^ string
    | DW_FORM_block -- ^ block
    | DW_FORM_block1 -- ^ block
    | DW_FORM_data1 -- ^ constant
    | DW_FORM_flag -- ^ flag
    | DW_FORM_sdata -- ^ constant
    | DW_FORM_strp -- ^ string
    | DW_FORM_udata -- ^ constant
    | DW_FORM_ref_addr            -- ^ reference
    | DW_FORM_ref1                -- ^ reference
    | DW_FORM_ref2                -- ^ reference
    | DW_FORM_ref4                -- ^ reference
    | DW_FORM_ref8                -- ^ reference
    | DW_FORM_ref_udata           -- ^ reference
    | DW_FORM_indirect            -- ^ (see Section 7.5.3 of DWARF3 specification)
    deriving (Show, Eq)
dw_form :: Word64 -> DW_FORM
dw_form 0x01 = DW_FORM_addr
dw_form 0x03 = DW_FORM_block2
dw_form 0x04 = DW_FORM_block4
dw_form 0x05 = DW_FORM_data2
dw_form 0x06 = DW_FORM_data4
dw_form 0x07 = DW_FORM_data8
dw_form 0x08 = DW_FORM_string
dw_form 0x09 = DW_FORM_block
dw_form 0x0a = DW_FORM_block1
dw_form 0x0b = DW_FORM_data1
dw_form 0x0c = DW_FORM_flag
dw_form 0x0d = DW_FORM_sdata
dw_form 0x0e = DW_FORM_strp
dw_form 0x0f = DW_FORM_udata
dw_form 0x10 = DW_FORM_ref_addr
dw_form 0x11 = DW_FORM_ref1
dw_form 0x12 = DW_FORM_ref2
dw_form 0x13 = DW_FORM_ref4
dw_form 0x14 = DW_FORM_ref8
dw_form 0x15 = DW_FORM_ref_udata
dw_form 0x16 = DW_FORM_indirect
dw_form n    = error $ "Unrecognized DW_FORM " ++ show n

data DW_ATVAL
    = DW_ATVAL_INT    Int64
    | DW_ATVAL_UINT   Word64
    | DW_ATVAL_STRING String
    | DW_ATVAL_BLOB   B.ByteString
    | DW_ATVAL_BOOL   Bool
    deriving (Show, Eq)

getByteStringLen :: Integral a => Get a -> Get B.ByteString
getByteStringLen lenGetter =
  getByteString =<< fromIntegral <$> lenGetter

newtype CUOffset = CUOffset Word64
  deriving (Eq, Ord, Read, Show)

inCU :: Integral a => CUOffset -> a -> Word64
inCU (CUOffset base) x = base + fromIntegral x

---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Abbreviation and form parsing
---------------------------------------------------------------------------------------------------------------------------------------------------------------
getForm :: DwarfReader -> B.ByteString -> CUOffset -> DW_FORM -> Get DW_ATVAL
getForm dr str cu form = case form of
  DW_FORM_addr      -> DW_ATVAL_UINT . fromIntegral <$> drGetDwarfTargetAddress dr
  DW_FORM_block1    -> DW_ATVAL_BLOB <$> getByteStringLen getWord8
  DW_FORM_block2    -> DW_ATVAL_BLOB <$> getByteStringLen (drGetW16 dr)
  DW_FORM_block4    -> DW_ATVAL_BLOB <$> getByteStringLen (drGetW32 dr)
  DW_FORM_block     -> DW_ATVAL_BLOB <$> getByteStringLen getULEB128
  DW_FORM_data1     -> DW_ATVAL_UINT . fromIntegral <$> getWord8
  DW_FORM_data2     -> DW_ATVAL_UINT . fromIntegral <$> drGetW16 dr
  DW_FORM_data4     -> DW_ATVAL_UINT . fromIntegral <$> drGetW32 dr
  DW_FORM_data8     -> DW_ATVAL_UINT . fromIntegral <$> drGetW64 dr
  DW_FORM_udata     -> DW_ATVAL_UINT <$> getULEB128
  DW_FORM_sdata     -> DW_ATVAL_INT <$> getSLEB128
  DW_FORM_flag      -> DW_ATVAL_BOOL . (/= 0) <$> getWord8
  DW_FORM_string    -> DW_ATVAL_STRING <$> getNullTerminatedUTF8String
  DW_FORM_ref1      -> DW_ATVAL_UINT . inCU cu <$> getWord8
  DW_FORM_ref2      -> DW_ATVAL_UINT . inCU cu <$> drGetW16 dr
  DW_FORM_ref4      -> DW_ATVAL_UINT . inCU cu <$> drGetW32 dr
  DW_FORM_ref8      -> DW_ATVAL_UINT . inCU cu <$> drGetW64 dr
  DW_FORM_ref_udata -> DW_ATVAL_UINT . inCU cu <$> getULEB128
  DW_FORM_ref_addr  -> DW_ATVAL_UINT <$> drGetDwarfOffset dr
  DW_FORM_indirect  -> getForm dr str cu . dw_form =<< getULEB128
  DW_FORM_strp      -> do
    offset <- fromIntegral <$> drGetDwarfOffset dr
    pure . DW_ATVAL_STRING .
      runGet getNullTerminatedUTF8String $ L.fromChunks [B.drop offset str]

data DW_AT
    = DW_AT_sibling              -- ^ reference
    | DW_AT_location             -- ^ block, loclistptr
    | DW_AT_name                 -- ^ string
    | DW_AT_ordering             -- ^ constant
    | DW_AT_byte_size            -- ^ block, constant, reference
    | DW_AT_bit_offset           -- ^ block, constant, reference
    | DW_AT_bit_size             -- ^ block, constant, reference
    | DW_AT_stmt_list            -- ^ lineptr
    | DW_AT_low_pc               -- ^ address
    | DW_AT_high_pc              -- ^ address
    | DW_AT_language             -- ^ constant
    | DW_AT_discr                -- ^ reference
    | DW_AT_discr_value          -- ^ constant
    | DW_AT_visibility           -- ^ constant
    | DW_AT_import               -- ^ reference
    | DW_AT_string_length        -- ^ block, loclistptr
    | DW_AT_common_reference     -- ^ reference
    | DW_AT_comp_dir             -- ^ string
    | DW_AT_const_value          -- ^ block, constant, string
    | DW_AT_containing_type      -- ^ reference
    | DW_AT_default_value        -- ^ reference
    | DW_AT_inline               -- ^ constant
    | DW_AT_is_optional          -- ^ flag
    | DW_AT_lower_bound          -- ^ block, constant, reference
    | DW_AT_producer             -- ^ string
    | DW_AT_prototyped           -- ^ flag
    | DW_AT_return_addr          -- ^ block, loclistptr
    | DW_AT_start_scope          -- ^ constant
    | DW_AT_bit_stride           -- ^ constant
    | DW_AT_upper_bound          -- ^ block, constant, reference
    | DW_AT_abstract_origin      -- ^ reference
    | DW_AT_accessibility        -- ^ constant
    | DW_AT_address_class        -- ^ constant
    | DW_AT_artificial           -- ^ flag
    | DW_AT_base_types           -- ^ reference
    | DW_AT_calling_convention   -- ^ constant
    | DW_AT_count                -- ^ block, constant, reference
    | DW_AT_data_member_location -- ^ block, constant, loclistptr
    | DW_AT_decl_column          -- ^ constant
    | DW_AT_decl_file            -- ^ constant
    | DW_AT_decl_line            -- ^ constant
    | DW_AT_declaration          -- ^ flag
    | DW_AT_discr_list           -- ^ block
    | DW_AT_encoding             -- ^ constant
    | DW_AT_external             -- ^ flag
    | DW_AT_frame_base           -- ^ block, loclistptr
    | DW_AT_friend               -- ^ reference
    | DW_AT_identifier_case      -- ^ constant
    | DW_AT_macro_info           -- ^ macptr
    | DW_AT_namelist_item        -- ^ block
    | DW_AT_priority             -- ^ reference
    | DW_AT_segment              -- ^ block, loclistptr
    | DW_AT_specification        -- ^ reference
    | DW_AT_static_link          -- ^ block, loclistptr
    | DW_AT_type                 -- ^ reference
    | DW_AT_use_location         -- ^ block, loclistptr
    | DW_AT_variable_parameter   -- ^ flag
    | DW_AT_virtuality           -- ^ constant
    | DW_AT_vtable_elem_location -- ^ block, loclistptr
    | DW_AT_allocated            -- ^ block, constant, reference
    | DW_AT_associated           -- ^ block, constant, reference
    | DW_AT_data_location        -- ^ block
    | DW_AT_byte_stride          -- ^ block, constant, reference
    | DW_AT_entry_pc             -- ^ address
    | DW_AT_use_UTF8             -- ^ flag
    | DW_AT_extension            -- ^ reference
    | DW_AT_ranges               -- ^ rangelistptr
    | DW_AT_trampoline           -- ^ address, flag, reference, string
    | DW_AT_call_column          -- ^ constant
    | DW_AT_call_file            -- ^ constant
    | DW_AT_call_line            -- ^ constant
    | DW_AT_description          -- ^ string
    | DW_AT_binary_scale         -- ^ constant
    | DW_AT_decimal_scale        -- ^ constant
    | DW_AT_small                -- ^ reference
    | DW_AT_decimal_sign         -- ^ constant
    | DW_AT_digit_count          -- ^ constant
    | DW_AT_picture_string       -- ^ string
    | DW_AT_mutable              -- ^ flag
    | DW_AT_threads_scaled       -- ^ flag
    | DW_AT_explicit             -- ^ flag
    | DW_AT_object_pointer       -- ^ reference
    | DW_AT_endianity            -- ^ constant
    | DW_AT_elemental            -- ^ flag
    | DW_AT_return                 -- ^ flag
    | DW_AT_recursive            -- ^ flag
    | DW_AT_user Word64          -- ^ user extension
    deriving (Show, Eq)
dw_at :: Word64 -> DW_AT
dw_at 0x01 = DW_AT_sibling
dw_at 0x02 = DW_AT_location
dw_at 0x03 = DW_AT_name
dw_at 0x09 = DW_AT_ordering
dw_at 0x0b = DW_AT_byte_size
dw_at 0x0c = DW_AT_bit_offset
dw_at 0x0d = DW_AT_bit_size
dw_at 0x10 = DW_AT_stmt_list
dw_at 0x11 = DW_AT_low_pc
dw_at 0x12 = DW_AT_high_pc
dw_at 0x13 = DW_AT_language
dw_at 0x15 = DW_AT_discr
dw_at 0x16 = DW_AT_discr_value
dw_at 0x17 = DW_AT_visibility
dw_at 0x18 = DW_AT_import
dw_at 0x19 = DW_AT_string_length
dw_at 0x1a = DW_AT_common_reference
dw_at 0x1b = DW_AT_comp_dir
dw_at 0x1c = DW_AT_const_value
dw_at 0x1d = DW_AT_containing_type
dw_at 0x1e = DW_AT_default_value
dw_at 0x20 = DW_AT_inline
dw_at 0x21 = DW_AT_is_optional
dw_at 0x22 = DW_AT_lower_bound
dw_at 0x25 = DW_AT_producer
dw_at 0x27 = DW_AT_prototyped
dw_at 0x2a = DW_AT_return_addr
dw_at 0x2c = DW_AT_start_scope
dw_at 0x2e = DW_AT_bit_stride
dw_at 0x2f = DW_AT_upper_bound
dw_at 0x31 = DW_AT_abstract_origin
dw_at 0x32 = DW_AT_accessibility
dw_at 0x33 = DW_AT_address_class
dw_at 0x34 = DW_AT_artificial
dw_at 0x35 = DW_AT_base_types
dw_at 0x36 = DW_AT_calling_convention
dw_at 0x37 = DW_AT_count
dw_at 0x38 = DW_AT_data_member_location
dw_at 0x39 = DW_AT_decl_column
dw_at 0x3a = DW_AT_decl_file
dw_at 0x3b = DW_AT_decl_line
dw_at 0x3c = DW_AT_declaration
dw_at 0x3d = DW_AT_discr_list
dw_at 0x3e = DW_AT_encoding
dw_at 0x3f = DW_AT_external
dw_at 0x40 = DW_AT_frame_base
dw_at 0x41 = DW_AT_friend
dw_at 0x42 = DW_AT_identifier_case
dw_at 0x43 = DW_AT_macro_info
dw_at 0x44 = DW_AT_namelist_item
dw_at 0x45 = DW_AT_priority
dw_at 0x46 = DW_AT_segment
dw_at 0x47 = DW_AT_specification
dw_at 0x48 = DW_AT_static_link
dw_at 0x49 = DW_AT_type
dw_at 0x4a = DW_AT_use_location
dw_at 0x4b = DW_AT_variable_parameter
dw_at 0x4c = DW_AT_virtuality
dw_at 0x4d = DW_AT_vtable_elem_location
dw_at 0x4e = DW_AT_allocated
dw_at 0x4f = DW_AT_associated
dw_at 0x50 = DW_AT_data_location
dw_at 0x51 = DW_AT_byte_stride
dw_at 0x52 = DW_AT_entry_pc
dw_at 0x53 = DW_AT_use_UTF8
dw_at 0x54 = DW_AT_extension
dw_at 0x55 = DW_AT_ranges
dw_at 0x56 = DW_AT_trampoline
dw_at 0x57 = DW_AT_call_column
dw_at 0x58 = DW_AT_call_file
dw_at 0x59 = DW_AT_call_line
dw_at 0x5a = DW_AT_description
dw_at 0x5b = DW_AT_binary_scale
dw_at 0x5c = DW_AT_decimal_scale
dw_at 0x5d = DW_AT_small
dw_at 0x5e = DW_AT_decimal_sign
dw_at 0x5f = DW_AT_digit_count
dw_at 0x60 = DW_AT_picture_string
dw_at 0x61 = DW_AT_mutable
dw_at 0x62 = DW_AT_threads_scaled
dw_at 0x63 = DW_AT_explicit
dw_at 0x64 = DW_AT_object_pointer
dw_at 0x65 = DW_AT_endianity
dw_at 0x66 = DW_AT_elemental
dw_at 0x67 = DW_AT_return
dw_at 0x68 = DW_AT_recursive
dw_at n | 0x2000 <= n && n <= 0x3fff = DW_AT_user n
dw_at n = error $ "Unrecognized DW_AT " ++ show n

newtype AbbrevId = AbbrevId Word64
  deriving (Eq, Ord, Read, Show)

data DW_ABBREV = DW_ABBREV
    { abbrevId        :: AbbrevId
    , abbrevTag       :: DW_TAG
    , abbrevChildren  :: Bool
    , abbrevAttrForms :: [(DW_AT, DW_FORM)]
    }

getDW_TAG :: Get DW_TAG
getDW_TAG = getULEB128 >>= dw_tag
    where dw_tag 0x01 = pure DW_TAG_array_type
          dw_tag 0x02 = pure DW_TAG_class_type
          dw_tag 0x03 = pure DW_TAG_entry_point
          dw_tag 0x04 = pure DW_TAG_enumeration_type
          dw_tag 0x05 = pure DW_TAG_formal_parameter
          dw_tag 0x08 = pure DW_TAG_imported_declaration
          dw_tag 0x0a = pure DW_TAG_label
          dw_tag 0x0b = pure DW_TAG_lexical_block
          dw_tag 0x0d = pure DW_TAG_member
          dw_tag 0x0f = pure DW_TAG_pointer_type
          dw_tag 0x10 = pure DW_TAG_reference_type
          dw_tag 0x11 = pure DW_TAG_compile_unit
          dw_tag 0x12 = pure DW_TAG_string_type
          dw_tag 0x13 = pure DW_TAG_structure_type
          dw_tag 0x15 = pure DW_TAG_subroutine_type
          dw_tag 0x16 = pure DW_TAG_typedef
          dw_tag 0x17 = pure DW_TAG_union_type
          dw_tag 0x18 = pure DW_TAG_unspecified_parameters
          dw_tag 0x19 = pure DW_TAG_variant
          dw_tag 0x1a = pure DW_TAG_common_block
          dw_tag 0x1b = pure DW_TAG_common_inclusion
          dw_tag 0x1c = pure DW_TAG_inheritance
          dw_tag 0x1d = pure DW_TAG_inlined_subroutine
          dw_tag 0x1e = pure DW_TAG_module
          dw_tag 0x1f = pure DW_TAG_ptr_to_member_type
          dw_tag 0x20 = pure DW_TAG_set_type
          dw_tag 0x21 = pure DW_TAG_subrange_type
          dw_tag 0x22 = pure DW_TAG_with_stmt
          dw_tag 0x23 = pure DW_TAG_access_declaration
          dw_tag 0x24 = pure DW_TAG_base_type
          dw_tag 0x25 = pure DW_TAG_catch_block
          dw_tag 0x26 = pure DW_TAG_const_type
          dw_tag 0x27 = pure DW_TAG_constant
          dw_tag 0x28 = pure DW_TAG_enumerator
          dw_tag 0x29 = pure DW_TAG_file_type
          dw_tag 0x2a = pure DW_TAG_friend
          dw_tag 0x2b = pure DW_TAG_namelist
          dw_tag 0x2c = pure DW_TAG_namelist_item
          dw_tag 0x2d = pure DW_TAG_packed_type
          dw_tag 0x2e = pure DW_TAG_subprogram
          dw_tag 0x2f = pure DW_TAG_template_type_parameter
          dw_tag 0x30 = pure DW_TAG_template_value_parameter
          dw_tag 0x31 = pure DW_TAG_thrown_type
          dw_tag 0x32 = pure DW_TAG_try_block
          dw_tag 0x33 = pure DW_TAG_variant_part
          dw_tag 0x34 = pure DW_TAG_variable
          dw_tag 0x35 = pure DW_TAG_volatile_type
          dw_tag 0x36 = pure DW_TAG_dwarf_procedure
          dw_tag 0x37 = pure DW_TAG_restrict_type
          dw_tag 0x38 = pure DW_TAG_interface_type
          dw_tag 0x39 = pure DW_TAG_namespace
          dw_tag 0x3a = pure DW_TAG_imported_module
          dw_tag 0x3b = pure DW_TAG_unspecified_type
          dw_tag 0x3c = pure DW_TAG_partial_unit
          dw_tag 0x3d = pure DW_TAG_imported_unit
          dw_tag 0x3f = pure DW_TAG_condition
          dw_tag 0x40 = pure DW_TAG_shared_type
          dw_tag n | 0x4080 <= n && n <= 0xffff = fail $ "User DW_TAG data requires extension of parser for code " ++ show n
          dw_tag n = fail $ "Unrecognized DW_TAG " ++ show n

getMAbbrevId :: Get (Maybe AbbrevId)
getMAbbrevId = do
  i <- getULEB128
  pure $
    if i == 0
    then Nothing
    else Just $ AbbrevId i

getAbbrevList :: Get [DW_ABBREV]
getAbbrevList =
  whileMaybe $ traverse getAbbrev =<< getMAbbrevId
  where
    getAbbrev abbrev = do
      tag       <- getDW_TAG
      children  <- (== 1) <$> getWord8
      attrForms <- getAttrFormList
      pure $ DW_ABBREV abbrev tag children attrForms
    getAttrFormList =
      (fmap . map) (dw_at *** dw_form) . whileM (/= (0,0)) $
      (,) <$> getULEB128 <*> getULEB128


---------------------------------------------------------------------------------------------------------------------------------------------------------------
-- DWARF information entry and .debug_info section parsing.
---------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Utility function for retrieving the list of values for a specified attribute from a DWARF information entry.
(!?) :: DIE -> DW_AT -> [DW_ATVAL]
(!?) die at = map snd $ filter ((== at) . fst) $ dieAttributes die

-- | Returns compilation unit id given the header offset into .debug_info
infoCompileUnit  :: B.ByteString -- ^ Contents of .debug_info
                 -> Word64 -- ^ Offset into .debug_info header
                 -> Word64 -- ^ Offset of compile unit DIE.
infoCompileUnit infoSection offset =
  case runGet getWord32be
        (L.fromChunks [B.drop (fromIntegral offset) infoSection]) of
    0xffffffff -> offset + 23
    _ -> offset + 11

getNonZeroDwarfOffset :: DwarfReader -> Get (Maybe Word64)
getNonZeroDwarfOffset dr = do
  offset <- drGetDwarfOffset dr
  pure $ if offset == 0 then Nothing else Just offset

-- Section 7.19 - Name Lookup Tables
-- TODO: Is this Word64 really a CU? It's being passed as a "debug_info_offset"
getNameLookupEntries :: DwarfReader -> Word64 -> Get [(String, [Word64])]
getNameLookupEntries dr cu_offset =
  whileMaybe $ traverse getEntry =<< getNonZeroDwarfOffset dr
  where
    getEntry die_offset = do
      name <- getNullTerminatedUTF8String
      pure (name, [cu_offset + die_offset])

getDebugInfoOffset :: TargetSize -> DwarfEndianReader -> Get (DwarfReader, Word64)
getDebugInfoOffset target64 odr = do
  (der, _)          <- getDwarfUnitLength odr
  let dr            = dwarfReader target64 der
  _version          <- drGetW16 dr
  debug_info_offset <- drGetDwarfOffset dr
  return (dr, debug_info_offset)

getNameLookupTable :: TargetSize -> DwarfEndianReader -> Get [M.Map String [Word64]]
getNameLookupTable target64 odr = getWhileNotEmpty $ do
  (dr, debug_info_offset) <- getDebugInfoOffset target64 odr
  _debug_info_length <- drGetDwarfOffset dr
  pubNames          <- M.fromListWith (++) <$> getNameLookupEntries dr debug_info_offset
  pure pubNames

-- | Parses the .debug_pubnames section (as ByteString) into a map from a value name to a debug info id in the DwarfInfo.
parseDwarfPubnames :: Endianess -> TargetSize -> B.ByteString -> M.Map String [Word64]
parseDwarfPubnames endianess target64 pubnames_section =
    let dr = dwarfEndianReader endianess
    in M.unionsWith (++) $ runGet (getNameLookupTable target64 dr) $ L.fromChunks [pubnames_section]

-- | Parses the .debug_pubtypes section (as ByteString) into a map from a type name to a debug info id in the DwarfInfo.
parseDwarfPubtypes :: Endianess -> TargetSize -> B.ByteString -> M.Map String [Word64]
parseDwarfPubtypes endianess target64 pubtypes_section =
    let dr = dwarfEndianReader endianess
    in M.unionsWith (++) $ runGet (getNameLookupTable target64 dr) $ L.fromChunks [pubtypes_section]

-- Section 7.20 - Address Range Table
getAddressRangeTable :: TargetSize -> DwarfEndianReader -> Get [([(Word64, Word64)], Word64)]
getAddressRangeTable target64 odr = getWhileNotEmpty $ do
  (dr, debug_info_offset) <- getDebugInfoOffset target64 odr
  address_size      <- fromIntegral <$> getWord8
  _segment_size     <- getWord8
  bytes_read        <- Get.bytesRead
  Get.skip $ fromIntegral (2 * address_size - (bytes_read `mod` (2 * address_size)))
  address_ranges    <- case address_size of
                      4 -> whileM (/= (0, 0)) $ (,) <$> (fromIntegral <$> drGetW32 dr) <*> (fromIntegral <$> drGetW32 dr)
                      8 -> whileM (/= (0, 0)) $ (,) <$> drGetW64 dr <*> drGetW64 dr
                      n -> fail $ "Unrecognized address size " ++ show n ++ " in .debug_aranges section."
  pure (address_ranges, debug_info_offset)

-- | Parses  the .debug_aranges section (as ByteString) into a map from an address range to a debug info id that indexes the DwarfInfo.
parseDwarfAranges :: Endianess -> TargetSize -> B.ByteString -> [([(Word64, Word64)], Word64)]
parseDwarfAranges endianess target64 aranges_section =
    let dr = dwarfEndianReader endianess
    in runGet (getAddressRangeTable target64 dr) $ L.fromChunks [aranges_section]

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Section 7.21 - Line Number Information
data DW_LNI
    = DW_LNI_special Word64 Int64
    | DW_LNS_copy
    | DW_LNS_advance_pc Word64
    | DW_LNS_advance_line Int64
    | DW_LNS_set_file Word64
    | DW_LNS_set_column Word64
    | DW_LNS_negate_stmt
    | DW_LNS_set_basic_block
    | DW_LNS_const_add_pc Word64
    | DW_LNS_fixed_advance_pc Word64
    | DW_LNS_set_prologue_end
    | DW_LNS_set_epilogue_begin
    | DW_LNS_set_isa Word64
    | DW_LNE_end_sequence
    | DW_LNE_set_address Word64
    | DW_LNE_define_file String Word64 Word64 Word64
    deriving (Show, Eq)
getDW_LNI :: DwarfReader -> Int64 -> Word8 -> Word8 -> Word64 -> Get DW_LNI
getDW_LNI dr line_base line_range opcode_base minimum_instruction_length = fromIntegral <$> getWord8 >>= getDW_LNI_
    where getDW_LNI_ 0x00 = do
            rest <- getByteStringLen getULEB128
            pure $ runGet getDW_LNE $ L.fromChunks [rest]
                where getDW_LNE = getWord8 >>= getDW_LNE_
                      getDW_LNE_ 0x01 = pure DW_LNE_end_sequence
                      getDW_LNE_ 0x02 = pure DW_LNE_set_address <*> drGetDwarfTargetAddress dr
                      getDW_LNE_ 0x03 = pure DW_LNE_define_file <*> getNullTerminatedUTF8String <*> getULEB128 <*> getULEB128 <*> getULEB128
                      getDW_LNE_ n | 0x80 <= n && n <= 0xff = fail $ "User DW_LNE data requires extension of parser for code " ++ show n
                      getDW_LNE_ n = fail $ "Unexpected DW_LNE code " ++ show n
          getDW_LNI_ 0x01 = pure DW_LNS_copy
          getDW_LNI_ 0x02 = pure DW_LNS_advance_pc <*> (* minimum_instruction_length) <$> getULEB128
          getDW_LNI_ 0x03 = pure DW_LNS_advance_line <*> getSLEB128
          getDW_LNI_ 0x04 = pure DW_LNS_set_file <*> getULEB128
          getDW_LNI_ 0x05 = pure DW_LNS_set_column <*> getULEB128
          getDW_LNI_ 0x06 = pure DW_LNS_negate_stmt
          getDW_LNI_ 0x07 = pure DW_LNS_set_basic_block
          getDW_LNI_ 0x08 = pure $ DW_LNS_const_add_pc (minimum_instruction_length * fromIntegral ((255 - opcode_base) `div` line_range))
          getDW_LNI_ 0x09 = pure DW_LNS_fixed_advance_pc <*> fromIntegral <$> drGetW16 dr
          getDW_LNI_ 0x0a = pure DW_LNS_set_prologue_end
          getDW_LNI_ 0x0b = pure DW_LNS_set_epilogue_begin
          getDW_LNI_ 0x0c = pure DW_LNS_set_isa <*> getULEB128
          getDW_LNI_ n | n >= opcode_base =
            let addr_incr = minimum_instruction_length * fromIntegral ((n - opcode_base) `div` line_range)
                line_incr = line_base + fromIntegral ((n - opcode_base) `mod` line_range)
             in pure $ DW_LNI_special addr_incr line_incr
          getDW_LNI_ n = fail $ "Unexpected DW_LNI opcode " ++ show n

stepLineMachine :: Bool -> Word8 -> DW_LNE -> [DW_LNI] -> [DW_LNE]
stepLineMachine _ _ _ [] = []
stepLineMachine is_stmt mil lnm (DW_LNI_special addr_incr line_incr : xs) =
    let row = lnm { lnmAddress = lnmAddress lnm + addr_incr, lnmLine = lnmLine lnm + fromIntegral line_incr }
        new = row { lnmBasicBlock = False, lnmPrologueEnd = False, lnmEpilogueBegin = False }
    in row : stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_copy : xs) =
    let row = lnm
        new = row { lnmBasicBlock = False, lnmPrologueEnd = False, lnmEpilogueBegin = False }
    in row : stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_advance_pc incr : xs) =
    let new = lnm { lnmAddress = lnmAddress lnm + incr }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_advance_line incr : xs) =
    let new = lnm { lnmLine = lnmLine lnm + fromIntegral incr }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_set_file file : xs) =
    let new = lnm { lnmFile = file }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_set_column col : xs) =
    let new = lnm { lnmColumn = col }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_negate_stmt : xs) =
    let new = lnm { lnmStatement = not (lnmStatement lnm) }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_set_basic_block : xs) =
    let new = lnm { lnmBasicBlock = True }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_const_add_pc incr : xs) =
    let new = lnm { lnmAddress = lnmAddress lnm + incr }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_fixed_advance_pc incr : xs) =
    let new = lnm { lnmAddress = lnmAddress lnm + incr }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_set_prologue_end : xs) =
    let new = lnm { lnmPrologueEnd = True }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_set_epilogue_begin : xs) =
    let new = lnm { lnmEpilogueBegin = True }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNS_set_isa isa : xs) =
    let new = lnm { lnmISA = isa }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNE_end_sequence : xs) =
    let row = lnm { lnmEndSequence = True }
        new = defaultLNE is_stmt (lnmFiles lnm)
    in row : stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNE_set_address address : xs) =
    let new = lnm { lnmAddress = address }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNE_define_file name dir_index time len : xs) =
    let new = lnm { lnmFiles = lnmFiles lnm ++ [(name, dir_index, time, len)] }
    in stepLineMachine is_stmt mil new xs

data DW_LNE = DW_LNE
    { lnmAddress       :: Word64
    , lnmFile          :: Word64
    , lnmLine          :: Word64
    , lnmColumn        :: Word64
    , lnmStatement     :: Bool
    , lnmBasicBlock    :: Bool
    , lnmEndSequence   :: Bool
    , lnmPrologueEnd   :: Bool
    , lnmEpilogueBegin :: Bool
    , lnmISA           :: Word64
    , lnmFiles         :: [(String, Word64, Word64, Word64)]
    } deriving (Show, Eq)
defaultLNE :: Bool -> [(String, Word64, Word64, Word64)] -> DW_LNE
defaultLNE is_stmt files = DW_LNE
    { lnmAddress       = 0
    , lnmFile          = 1
    , lnmLine          = 1
    , lnmColumn        = 0
    , lnmStatement     = is_stmt
    , lnmBasicBlock    = False
    , lnmEndSequence   = False
    , lnmPrologueEnd   = False
    , lnmEpilogueBegin = False
    , lnmISA           = 0
    , lnmFiles         = files
    }
getDebugLineFileNames :: Get [(String, Word64, Word64, Word64)]
getDebugLineFileNames = do
    file_name <- getNullTerminatedUTF8String
    if file_name == [] then
        pure []
     else do
        dir_index   <- getULEB128
        last_mod    <- getULEB128
        file_length <- getULEB128
        ((file_name, dir_index, last_mod, file_length) :) <$> getDebugLineFileNames

-- | Retrieves the line information for a DIE from a given substring of the .debug_line section. The offset
-- into the .debug_line section is obtained from the DW_AT_stmt_list attribute of a DIE.
parseDwarfLine :: Endianess -> TargetSize -> B.ByteString -> ([String], [DW_LNE])
parseDwarfLine endianess target64 bs =
    let dr = dwarfEndianReader endianess
    in runGet (getDwarfLine target64 dr) (L.fromChunks [bs])
getDwarfLine :: TargetSize -> DwarfEndianReader -> Get ([String], [DW_LNE])
getDwarfLine target64 der = do
    (desr, sectLen)            <- getDwarfUnitLength der
    startLen <- Get.bytesRead
    let dr                     = dwarfReader target64 desr
    _version                   <- drGetW16 dr
    _header_length             <- drGetDwarfOffset dr
    minimum_instruction_length <- getWord8
    default_is_stmt            <- (/= 0) <$> getWord8
    line_base                  <- get :: Get Int8
    line_range                 <- getWord8
    opcode_base                <- getWord8
    _standard_opcode_lengths   <- replicateM (fromIntegral opcode_base - 1) getWord8
    _include_directories       <- whileM (/= "") getNullTerminatedUTF8String
    file_names                 <- getDebugLineFileNames
    endLen <- Get.bytesRead
    -- Check if we have reached the end of the section.
    if fromIntegral sectLen <= endLen - startLen
      then pure (map (\(name, _, _, _) -> name) file_names, [])
      else do
        line_program <-
          fmap (++ [DW_LNE_end_sequence]) .
          whileM (/= DW_LNE_end_sequence) .
            getDW_LNI dr (fromIntegral line_base) line_range opcode_base $
            fromIntegral minimum_instruction_length
        let initial_state = defaultLNE default_is_stmt file_names
            line_matrix = stepLineMachine default_is_stmt minimum_instruction_length initial_state line_program
         in pure (map (\(name, _, _, _) -> name) file_names, line_matrix)

-- Section 7.21 - Macro Information
data DW_MACINFO
    = DW_MACINFO_define Word64 String     -- ^ Line number and defined symbol with definition
    | DW_MACINFO_undef Word64 String      -- ^ Line number and undefined symbol
    | DW_MACINFO_start_file Word64 Word64 -- ^ Marks start of file with the line where the file was included from and a source file index
    | DW_MACINFO_end_file                 -- ^ Marks end of file
    | DW_MACINFO_vendor_ext Word64 String -- ^ Implementation defined
    deriving (Show, Eq)

-- | Retrieves the macro information for a compilation unit from a given substring of the .debug_macinfo section. The offset
-- into the .debug_macinfo section is obtained from the DW_AT_macro_info attribute of a compilation unit DIE.
parseDwarfMacInfo :: B.ByteString -> [DW_MACINFO]
parseDwarfMacInfo bs = runGet getDwarfMacInfo (L.fromChunks [bs])

getDwarfMacInfo :: Get [DW_MACINFO]
getDwarfMacInfo = do
    x <- getWord8
    case x of
        0x00 -> pure []
        0x01 -> pure (:) <*> (pure DW_MACINFO_define     <*> getULEB128 <*> getNullTerminatedUTF8String) <*> getDwarfMacInfo
        0x02 -> pure (:) <*> (pure DW_MACINFO_undef      <*> getULEB128 <*> getNullTerminatedUTF8String) <*> getDwarfMacInfo
        0x03 -> pure (:) <*> (pure DW_MACINFO_start_file <*> getULEB128 <*> getULEB128)              <*> getDwarfMacInfo
        0x04 -> pure (:) <*>  pure DW_MACINFO_end_file                                               <*> getDwarfMacInfo
        0xff -> pure (:) <*> (pure DW_MACINFO_vendor_ext <*> getULEB128 <*> getNullTerminatedUTF8String) <*> getDwarfMacInfo
        _ -> fail $ "Invalid MACINFO id: " ++ show x

-- Section 7.22 - Call Frame
data DW_CFA
    = DW_CFA_advance_loc Word8
    | DW_CFA_offset Word8 Word64
    | DW_CFA_restore Word8
    | DW_CFA_nop
    | DW_CFA_set_loc Word64
    | DW_CFA_advance_loc1 Word8
    | DW_CFA_advance_loc2 Word16
    | DW_CFA_advance_loc4 Word32
    | DW_CFA_offset_extended Word64 Word64
    | DW_CFA_restore_extended Word64
    | DW_CFA_undefined Word64
    | DW_CFA_same_value Word64
    | DW_CFA_register Word64 Word64
    | DW_CFA_remember_state
    | DW_CFA_restore_state
    | DW_CFA_def_cfa Word64 Word64
    | DW_CFA_def_cfa_register Word64
    | DW_CFA_def_cfa_offset Word64
    | DW_CFA_def_cfa_expression B.ByteString
    | DW_CFA_expression Word64 B.ByteString
    | DW_CFA_offset_extended_sf Word64 Int64
    | DW_CFA_def_cfa_sf Word64 Int64
    | DW_CFA_def_cfa_offset_sf Int64
    | DW_CFA_val_offset Word64 Word64
    | DW_CFA_val_offset_sf Word64 Int64
    | DW_CFA_val_expression Word64 B.ByteString
    deriving (Show, Eq)
getDW_CFA :: DwarfReader -> Get DW_CFA
getDW_CFA dr = do
    tag <- getWord8
    case tag `shiftR` 6 of
        0x1 -> pure $ DW_CFA_advance_loc $ tag .&. 0x3f
        0x2 -> pure (DW_CFA_offset (tag .&. 0x3f)) <*> getULEB128
        0x3 -> pure $ DW_CFA_restore $ tag .&. 0x3f
        0x0 -> case tag .&. 0x3f of
            0x00 -> pure DW_CFA_nop
            0x01 -> pure DW_CFA_set_loc <*> drGetDwarfTargetAddress dr
            0x02 -> pure DW_CFA_advance_loc1 <*> getWord8
            0x03 -> pure DW_CFA_advance_loc2 <*> drGetW16 dr
            0x04 -> pure DW_CFA_advance_loc4 <*> drGetW32 dr
            0x05 -> pure DW_CFA_offset_extended <*> getULEB128 <*> getULEB128
            0x06 -> pure DW_CFA_restore_extended <*> getULEB128
            0x07 -> pure DW_CFA_undefined <*> getULEB128
            0x08 -> pure DW_CFA_same_value <*> getULEB128
            0x09 -> pure DW_CFA_register <*> getULEB128 <*> getULEB128
            0x0a -> pure DW_CFA_remember_state
            0x0b -> pure DW_CFA_restore_state
            0x0c -> pure DW_CFA_def_cfa <*> getULEB128 <*> getULEB128
            0x0d -> pure DW_CFA_def_cfa_register <*> getULEB128
            0x0e -> pure DW_CFA_def_cfa_offset <*> getULEB128
            0x0f -> pure DW_CFA_def_cfa_expression <*> getByteStringLen getULEB128
            0x10 -> pure DW_CFA_expression <*> getULEB128 <*> getByteStringLen getULEB128
            0x11 -> pure DW_CFA_offset_extended_sf <*> getULEB128 <*> getSLEB128
            0x12 -> pure DW_CFA_def_cfa_sf <*> getULEB128 <*> getSLEB128
            0x13 -> pure DW_CFA_def_cfa_offset_sf <*> getSLEB128
            0x14 -> pure DW_CFA_val_offset <*> getULEB128 <*> getULEB128
            0x15 -> pure DW_CFA_val_offset_sf <*> getULEB128 <*> getSLEB128
            0x16 -> pure DW_CFA_val_expression <*> getULEB128 <*> getByteStringLen getULEB128
            _ -> fail $ "Invalid tag: " ++ show tag
        _ -> fail $ "Invalid tag: " ++ show tag

data DW_CIEFDE
    = DW_CIE
        { cieAugmentation          :: String
        , cieCodeAlignmentFactor   :: Word64
        , cieDataAlignmentFactor   :: Int64
        , cieReturnAddressRegister :: Word64
        , cieInitialInstructions   :: [DW_CFA]
        }
    | DW_FDE
        { fdeCiePointer      :: Word64
        , fdeInitialLocation :: Word64
        , fdeAddressRange    :: Word64
        , fdeInstructions    :: [DW_CFA]
        }
    deriving (Show, Eq)

getCIEFDE :: Endianess -> TargetSize -> Get DW_CIEFDE
getCIEFDE endianess target64 = do
    let der    = dwarfEndianReader endianess
    (dur, len) <- getDwarfUnitLength der
    let dr     = dwarfReader target64 dur
    begin      <- Get.bytesRead
    cie_id     <- drGetDwarfOffset dr
    if cie_id == drLargestOffset dr then do
        version                 <- getWord8
        augmentation            <- getNullTerminatedUTF8String
        code_alignment_factor   <- getULEB128
        data_alignment_factor   <- getSLEB128
        return_address_register <- case version of
                                    1 -> fromIntegral <$> getWord8
                                    3 -> getULEB128
                                    n -> fail $ "Unrecognized CIE version " ++ show n
        end                     <- Get.bytesRead
        raw_instructions        <- getByteString $ fromIntegral (fromIntegral len - (end - begin))
        let initial_instructions = runGet (getWhileNotEmpty (getDW_CFA dr)) $ L.fromChunks [raw_instructions]
        pure $ DW_CIE augmentation code_alignment_factor data_alignment_factor return_address_register initial_instructions
     else do
        initial_location        <- drGetDwarfTargetAddress dr
        address_range           <- drGetDwarfTargetAddress dr
        end                     <- Get.bytesRead
        raw_instructions        <- getByteString $ fromIntegral (fromIntegral len - (end - begin))
        let instructions        = runGet (getWhileNotEmpty (getDW_CFA dr)) $ L.fromChunks [raw_instructions]
        pure $ DW_FDE cie_id initial_location address_range instructions

-- | Parse the .debug_frame section into a list of DW_CIEFDE records.
parseDwarfFrame :: Endianess
                -> TargetSize
                -> B.ByteString -- ^ ByteString for the .debug_frame section.
                -> [DW_CIEFDE]
parseDwarfFrame endianess target64 bs =
  runGet (getWhileNotEmpty $ getCIEFDE endianess target64) (L.fromChunks [bs])

-- Section 7.23 - Non-contiguous Address Ranges
-- | Retrieves the non-contiguous address ranges for a compilation unit from a given substring of the .debug_ranges section. The offset
-- into the .debug_ranges section is obtained from the DW_AT_ranges attribute of a compilation unit DIE.
-- Left results are base address entries. Right results are address ranges.
parseDwarfRanges :: DwarfReader -> B.ByteString -> [Either Word64 (Word64, Word64)]
parseDwarfRanges dr bs = runGet (getDwarfRanges dr) (L.fromChunks [bs])

getDwarfRanges :: DwarfReader -> Get [Either Word64 (Word64, Word64)]
getDwarfRanges dr = do
    begin <- drGetDwarfTargetAddress dr
    end   <- drGetDwarfTargetAddress dr
    if begin == 0 && end == 0 then
        pure []
     else if begin == drLargestTargetAddress dr then
        pure (Left end :) <*> getDwarfRanges dr
     else
        pure (Right (begin, end) :) <*> getDwarfRanges dr

-- Section 7.7.3
-- | Retrieves the location list expressions from a given substring of the .debug_loc section. The offset
-- into the .debug_loc section is obtained from an attribute of class loclistptr for a given DIE.
-- Left results are base address entries. Right results are address ranges and a location expression.
parseDwarfLoc :: DwarfReader -> B.ByteString -> [Either Word64 (Word64, Word64, B.ByteString)]
parseDwarfLoc dr bs = runGet (getDwarfLoc dr) (L.fromChunks [bs])

getDwarfLoc :: DwarfReader -> Get [Either Word64 (Word64, Word64, B.ByteString)]
getDwarfLoc dr = do
    begin <- drGetDwarfTargetAddress dr
    end   <- drGetDwarfTargetAddress dr
    if begin == 0 && end == 0 then
        pure []
     else if begin == drLargestTargetAddress dr then
        pure (Left end :) <*> getDwarfLoc dr
      else do
        expr <- getByteStringLen (drGetW16 dr)
        pure (Right (begin, end, expr) :) <*> getDwarfLoc dr

data DW_TAG
    = DW_TAG_array_type
    | DW_TAG_class_type
    | DW_TAG_entry_point
    | DW_TAG_enumeration_type
    | DW_TAG_formal_parameter
    | DW_TAG_imported_declaration
    | DW_TAG_label
    | DW_TAG_lexical_block
    | DW_TAG_member
    | DW_TAG_pointer_type
    | DW_TAG_reference_type
    | DW_TAG_compile_unit
    | DW_TAG_string_type
    | DW_TAG_structure_type
    | DW_TAG_subroutine_type
    | DW_TAG_typedef
    | DW_TAG_union_type
    | DW_TAG_unspecified_parameters
    | DW_TAG_variant
    | DW_TAG_common_block
    | DW_TAG_common_inclusion
    | DW_TAG_inheritance
    | DW_TAG_inlined_subroutine
    | DW_TAG_module
    | DW_TAG_ptr_to_member_type
    | DW_TAG_set_type
    | DW_TAG_subrange_type
    | DW_TAG_with_stmt
    | DW_TAG_access_declaration
    | DW_TAG_base_type
    | DW_TAG_catch_block
    | DW_TAG_const_type
    | DW_TAG_constant
    | DW_TAG_enumerator
    | DW_TAG_file_type
    | DW_TAG_friend
    | DW_TAG_namelist
    | DW_TAG_namelist_item
    | DW_TAG_packed_type
    | DW_TAG_subprogram
    | DW_TAG_template_type_parameter
    | DW_TAG_template_value_parameter
    | DW_TAG_thrown_type
    | DW_TAG_try_block
    | DW_TAG_variant_part
    | DW_TAG_variable
    | DW_TAG_volatile_type
    | DW_TAG_dwarf_procedure
    | DW_TAG_restrict_type
    | DW_TAG_interface_type
    | DW_TAG_namespace
    | DW_TAG_imported_module
    | DW_TAG_unspecified_type
    | DW_TAG_partial_unit
    | DW_TAG_imported_unit
    | DW_TAG_condition
    | DW_TAG_shared_type
    deriving (Show, Eq)

-- TODO: Do we really want to maintain the siblings? We could go to
-- parent->children? or even always keep the parens in context and not
-- maintain that either?

data Tree ptr a = Tree
  { treeParent       :: Maybe ptr        -- ^ Unique identifier of this entry's parent.
  , treeSiblingLeft  :: Maybe ptr        -- ^ Unique identifier of the left sibling in the DIE tree, if one exists.
  , treeSiblingRight :: Maybe ptr        -- ^ Unique identifier of the right sibling in the DIE tree, if one exists.
  , treeData :: a
  }

instance Show a => Show (Tree ptr a) where
  show tree = show (treeData tree)

newtype DieID = DieID Word64
  deriving (Eq, Ord, Read, Show)

-- | The dwarf information entries form a graph of nodes tagged with attributes. Please refer to the DWARF specification
-- for semantics. Although it looks like a tree, there can be attributes which have adjacency information which will
-- introduce cross-branch edges.
data DIE = DIE
    { dieId           :: DieID              -- ^ Unique identifier for this entry.
    , dieTag          :: DW_TAG              -- ^ Type tag.
    , dieAttributes   :: [(DW_AT, DW_ATVAL)] -- ^ Attribute tag and value pairs.
    , dieReader       :: DwarfReader         -- ^ Decoder used to decode this entry. May be needed to further parse attribute values.
    } deriving (Show, Eq)

type DIETree = Tree DieID DIE

addSiblings :: Maybe DieID -> [DIE] -> [DIETree]
addSiblings mParent = go Nothing
  where
    go _lSibling [] = []
    go lSibling (die : xs) =
      Tree mParent lSibling (dieId <$> listToMaybe xs) die :
      go (Just (dieId die)) xs

concatSiblings :: Maybe DieID -> [(DIE, [DIETree])] -> [DIETree]
concatSiblings mParent diesAndDescendants =
  addSiblings mParent dies ++ descendants
  where
    dies = map fst diesAndDescendants
    descendants = concatMap snd diesAndDescendants

-- Decode a non-compilation unit DWARF information entry, its children and its siblings.
getDieAndSiblings ::
  DieID -> M.Map AbbrevId DW_ABBREV -> DwarfReader ->
  B.ByteString -> CUOffset -> Get [DIETree]
getDieAndSiblings parent abbrev_map dr str_section cu_offset =
  concatSiblings (Just parent) <$> go
  where
    go = do
      -- TODO: Move this including the "if" to getDIEAndDescendants,
      -- and have it return a Maybe
      offset <- DieID . fromIntegral <$> Get.bytesRead
      mAbbrid <- getMAbbrevId
      case mAbbrid of
        Nothing -> pure []
        Just abbrid -> do
          dieDescendants <- getDIEAndDescendants offset (abbrev_map M.! abbrid) abbrev_map dr str_section cu_offset
          siblings <- go
          pure $ dieDescendants : siblings

getDIEAndDescendants :: DieID -> DW_ABBREV -> M.Map AbbrevId DW_ABBREV -> DwarfReader -> B.ByteString -> CUOffset -> Get (DIE, [DIETree])
getDIEAndDescendants offset abbrev abbrev_map dr str_section cu_offset = do
  values    <- mapM (getForm dr str_section cu_offset) forms
  descendants <-
    if abbrevChildren abbrev
    then getDieAndSiblings offset abbrev_map dr str_section cu_offset
    else pure []
  pure $
    (DIE offset tag (zip attrs values) dr, descendants)
  where
    tag            = abbrevTag abbrev
    (attrs, forms) = unzip $ abbrevAttrForms abbrev

-- TODO: Why not return CUs rather than DIE's?
-- Decode the compilation unit DWARF information entries.
getDieCus :: DwarfEndianReader -> B.ByteString -> B.ByteString -> Get [DIETree]
getDieCus odr abbrev_section str_section =
  fmap (concatSiblings Nothing) .
  getWhileNotEmpty $ do
    cu_offset       <- CUOffset . fromIntegral <$> Get.bytesRead
    (desr, _)       <- getDwarfUnitLength odr
    _version        <- desrGetW16 desr
    abbrev_offset   <- desrGetDwarfOffset desr
    addr_size       <- getWord8
    dr              <- case addr_size of
                        4 -> pure $ dwarfReader TargetSize32 desr
                        8 -> pure $ dwarfReader TargetSize64 desr
                        _ -> fail $ "Invalid address size: " ++ show addr_size
    -- TODO: This duplicates getDieAndSiblings
    cudie_offset   <- DieID . fromIntegral <$> Get.bytesRead
    Just cu_abbr_num <- getMAbbrevId
    let abbrev_table         = B.drop (fromIntegral abbrev_offset) abbrev_section
        abbrev_map           = M.fromList . map (abbrevId &&& id) . runGet getAbbrevList $ L.fromChunks [abbrev_table]
        cu_abbrev            = abbrev_map M.! cu_abbr_num
        cu_tag               = abbrevTag cu_abbrev
        cu_has_children      = abbrevChildren cu_abbrev
        (cu_attrs, cu_forms) = unzip $ abbrevAttrForms cu_abbrev
    cu_values    <- mapM (getForm dr str_section cu_offset) cu_forms
    cu_descendants <-
      if cu_has_children
      then getDieAndSiblings cudie_offset abbrev_map dr str_section cu_offset
      else pure []
    -- TODO: YUCK!
    pure
      ( DIE cudie_offset cu_tag (zip cu_attrs cu_values) dr
      , cu_descendants )

-- | Parses the .debug_info section (as ByteString) using the .debug_abbrev and .debug_str sections.
parseDwarfInfo :: Endianess
               -> B.ByteString     -- ^ ByteString for the .debug_info section.
               -> B.ByteString     -- ^ ByteString for the .debug_abbrev section.
               -> B.ByteString     -- ^ ByteString for the .debug_str section.
               -> M.Map DieID DIETree -- ^ A map from the unique ids to their corresponding DWARF information entries.
parseDwarfInfo endianess info_section abbrev_section str_section =
    let dr = dwarfEndianReader endianess
        di = runGet (getDieCus dr abbrev_section str_section) $ L.fromChunks [info_section]
    in M.fromList $ map (dieId . treeData &&& id) di

data DW_OP
    = DW_OP_addr Word64
    | DW_OP_deref
    | DW_OP_const1u Word8
    | DW_OP_const1s Int8
    | DW_OP_const2u Word16
    | DW_OP_const2s Int16
    | DW_OP_const4u Word32
    | DW_OP_const4s Int32
    | DW_OP_const8u Word64
    | DW_OP_const8s Int64
    | DW_OP_constu  Word64
    | DW_OP_consts  Int64
    | DW_OP_dup
    | DW_OP_drop
    | DW_OP_over
    | DW_OP_pick Word8
    | DW_OP_swap
    | DW_OP_rot
    | DW_OP_xderef
    | DW_OP_abs
    | DW_OP_and
    | DW_OP_div
    | DW_OP_minus
    | DW_OP_mod
    | DW_OP_mul
    | DW_OP_neg
    | DW_OP_not
    | DW_OP_or
    | DW_OP_plus
    | DW_OP_plus_uconst Word64
    | DW_OP_shl
    | DW_OP_shr
    | DW_OP_shra
    | DW_OP_xor
    | DW_OP_skip Int16
    | DW_OP_bra Int16
    | DW_OP_eq
    | DW_OP_ge
    | DW_OP_gt
    | DW_OP_le
    | DW_OP_lt
    | DW_OP_ne
    | DW_OP_lit0
    | DW_OP_lit1
    | DW_OP_lit2
    | DW_OP_lit3
    | DW_OP_lit4
    | DW_OP_lit5
    | DW_OP_lit6
    | DW_OP_lit7
    | DW_OP_lit8
    | DW_OP_lit9
    | DW_OP_lit10
    | DW_OP_lit11
    | DW_OP_lit12
    | DW_OP_lit13
    | DW_OP_lit14
    | DW_OP_lit15
    | DW_OP_lit16
    | DW_OP_lit17
    | DW_OP_lit18
    | DW_OP_lit19
    | DW_OP_lit20
    | DW_OP_lit21
    | DW_OP_lit22
    | DW_OP_lit23
    | DW_OP_lit24
    | DW_OP_lit25
    | DW_OP_lit26
    | DW_OP_lit27
    | DW_OP_lit28
    | DW_OP_lit29
    | DW_OP_lit30
    | DW_OP_lit31
    | DW_OP_reg0
    | DW_OP_reg1
    | DW_OP_reg2
    | DW_OP_reg3
    | DW_OP_reg4
    | DW_OP_reg5
    | DW_OP_reg6
    | DW_OP_reg7
    | DW_OP_reg8
    | DW_OP_reg9
    | DW_OP_reg10
    | DW_OP_reg11
    | DW_OP_reg12
    | DW_OP_reg13
    | DW_OP_reg14
    | DW_OP_reg15
    | DW_OP_reg16
    | DW_OP_reg17
    | DW_OP_reg18
    | DW_OP_reg19
    | DW_OP_reg20
    | DW_OP_reg21
    | DW_OP_reg22
    | DW_OP_reg23
    | DW_OP_reg24
    | DW_OP_reg25
    | DW_OP_reg26
    | DW_OP_reg27
    | DW_OP_reg28
    | DW_OP_reg29
    | DW_OP_reg30
    | DW_OP_reg31
    | DW_OP_breg0 Int64
    | DW_OP_breg1 Int64
    | DW_OP_breg2 Int64
    | DW_OP_breg3 Int64
    | DW_OP_breg4 Int64
    | DW_OP_breg5 Int64
    | DW_OP_breg6 Int64
    | DW_OP_breg7 Int64
    | DW_OP_breg8 Int64
    | DW_OP_breg9 Int64
    | DW_OP_breg10 Int64
    | DW_OP_breg11 Int64
    | DW_OP_breg12 Int64
    | DW_OP_breg13 Int64
    | DW_OP_breg14 Int64
    | DW_OP_breg15 Int64
    | DW_OP_breg16 Int64
    | DW_OP_breg17 Int64
    | DW_OP_breg18 Int64
    | DW_OP_breg19 Int64
    | DW_OP_breg20 Int64
    | DW_OP_breg21 Int64
    | DW_OP_breg22 Int64
    | DW_OP_breg23 Int64
    | DW_OP_breg24 Int64
    | DW_OP_breg25 Int64
    | DW_OP_breg26 Int64
    | DW_OP_breg27 Int64
    | DW_OP_breg28 Int64
    | DW_OP_breg29 Int64
    | DW_OP_breg30 Int64
    | DW_OP_breg31 Int64
    | DW_OP_regx Word64
    | DW_OP_fbreg Int64
    | DW_OP_bregx Word64 Int64
    | DW_OP_piece Word64
    | DW_OP_deref_size Word8
    | DW_OP_xderef_size Word8
    | DW_OP_nop
    | DW_OP_push_object_address
    | DW_OP_call2 Word16
    | DW_OP_call4 Word32
    | DW_OP_call_ref Word64
    | DW_OP_form_tls_address
    | DW_OP_call_frame_cfa
    | DW_OP_bit_piece Word64 Word64
    deriving (Show, Eq)
-- | Parse a ByteString into a DWARF opcode. This will be needed for further decoding of DIE attributes.
parseDW_OP :: DwarfReader -> B.ByteString -> DW_OP
parseDW_OP dr bs = runGet (getDW_OP dr) (L.fromChunks [bs])
getDW_OP :: DwarfReader -> Get DW_OP
getDW_OP dr = getWord8 >>= getDW_OP_
    where getDW_OP_ 0x03 = pure DW_OP_addr <*> drGetDwarfTargetAddress dr
          getDW_OP_ 0x06 = pure DW_OP_deref
          getDW_OP_ 0x08 = pure DW_OP_const1u <*> fromIntegral <$> getWord8
          getDW_OP_ 0x09 = pure DW_OP_const1s <*> fromIntegral <$> getWord8
          getDW_OP_ 0x0a = pure DW_OP_const2u <*> fromIntegral <$> drGetW16 dr
          getDW_OP_ 0x0b = pure DW_OP_const2s <*> fromIntegral <$> drGetW16 dr
          getDW_OP_ 0x0c = pure DW_OP_const4u <*> fromIntegral <$> drGetW32 dr
          getDW_OP_ 0x0d = pure DW_OP_const4s <*> fromIntegral <$> drGetW32 dr
          getDW_OP_ 0x0e = pure DW_OP_const8u <*> drGetW64 dr
          getDW_OP_ 0x0f = pure DW_OP_const8s <*> fromIntegral <$> drGetW64 dr
          getDW_OP_ 0x10 = pure DW_OP_constu  <*> getULEB128
          getDW_OP_ 0x11 = pure DW_OP_consts  <*> getSLEB128
          getDW_OP_ 0x12 = pure DW_OP_dup
          getDW_OP_ 0x13 = pure DW_OP_drop
          getDW_OP_ 0x14 = pure DW_OP_over
          getDW_OP_ 0x15 = pure DW_OP_pick <*> getWord8
          getDW_OP_ 0x16 = pure DW_OP_swap
          getDW_OP_ 0x17 = pure DW_OP_rot
          getDW_OP_ 0x18 = pure DW_OP_xderef
          getDW_OP_ 0x19 = pure DW_OP_abs
          getDW_OP_ 0x1a = pure DW_OP_and
          getDW_OP_ 0x1b = pure DW_OP_div
          getDW_OP_ 0x1c = pure DW_OP_minus
          getDW_OP_ 0x1d = pure DW_OP_mod
          getDW_OP_ 0x1e = pure DW_OP_mul
          getDW_OP_ 0x1f = pure DW_OP_neg
          getDW_OP_ 0x20 = pure DW_OP_not
          getDW_OP_ 0x21 = pure DW_OP_or
          getDW_OP_ 0x22 = pure DW_OP_plus
          getDW_OP_ 0x23 = pure DW_OP_plus_uconst <*> getULEB128
          getDW_OP_ 0x24 = pure DW_OP_shl
          getDW_OP_ 0x25 = pure DW_OP_shr
          getDW_OP_ 0x26 = pure DW_OP_shra
          getDW_OP_ 0x27 = pure DW_OP_xor
          getDW_OP_ 0x2f = pure DW_OP_skip <*> fromIntegral <$> drGetW16 dr
          getDW_OP_ 0x28 = pure DW_OP_bra  <*> fromIntegral <$> drGetW16 dr
          getDW_OP_ 0x29 = pure DW_OP_eq
          getDW_OP_ 0x2a = pure DW_OP_ge
          getDW_OP_ 0x2b = pure DW_OP_gt
          getDW_OP_ 0x2c = pure DW_OP_le
          getDW_OP_ 0x2d = pure DW_OP_lt
          getDW_OP_ 0x2e = pure DW_OP_ne
          getDW_OP_ 0x30 = pure DW_OP_lit0
          getDW_OP_ 0x31 = pure DW_OP_lit1
          getDW_OP_ 0x32 = pure DW_OP_lit2
          getDW_OP_ 0x33 = pure DW_OP_lit3
          getDW_OP_ 0x34 = pure DW_OP_lit4
          getDW_OP_ 0x35 = pure DW_OP_lit5
          getDW_OP_ 0x36 = pure DW_OP_lit6
          getDW_OP_ 0x37 = pure DW_OP_lit7
          getDW_OP_ 0x38 = pure DW_OP_lit8
          getDW_OP_ 0x39 = pure DW_OP_lit9
          getDW_OP_ 0x3a = pure DW_OP_lit10
          getDW_OP_ 0x3b = pure DW_OP_lit11
          getDW_OP_ 0x3c = pure DW_OP_lit12
          getDW_OP_ 0x3d = pure DW_OP_lit13
          getDW_OP_ 0x3e = pure DW_OP_lit14
          getDW_OP_ 0x3f = pure DW_OP_lit15
          getDW_OP_ 0x40 = pure DW_OP_lit16
          getDW_OP_ 0x41 = pure DW_OP_lit17
          getDW_OP_ 0x42 = pure DW_OP_lit18
          getDW_OP_ 0x43 = pure DW_OP_lit19
          getDW_OP_ 0x44 = pure DW_OP_lit20
          getDW_OP_ 0x45 = pure DW_OP_lit21
          getDW_OP_ 0x46 = pure DW_OP_lit22
          getDW_OP_ 0x47 = pure DW_OP_lit23
          getDW_OP_ 0x48 = pure DW_OP_lit24
          getDW_OP_ 0x49 = pure DW_OP_lit25
          getDW_OP_ 0x4a = pure DW_OP_lit26
          getDW_OP_ 0x4b = pure DW_OP_lit27
          getDW_OP_ 0x4c = pure DW_OP_lit28
          getDW_OP_ 0x4d = pure DW_OP_lit29
          getDW_OP_ 0x4e = pure DW_OP_lit30
          getDW_OP_ 0x4f = pure DW_OP_lit31
          getDW_OP_ 0x50 = pure DW_OP_reg0
          getDW_OP_ 0x51 = pure DW_OP_reg1
          getDW_OP_ 0x52 = pure DW_OP_reg2
          getDW_OP_ 0x53 = pure DW_OP_reg3
          getDW_OP_ 0x54 = pure DW_OP_reg4
          getDW_OP_ 0x55 = pure DW_OP_reg5
          getDW_OP_ 0x56 = pure DW_OP_reg6
          getDW_OP_ 0x57 = pure DW_OP_reg7
          getDW_OP_ 0x58 = pure DW_OP_reg8
          getDW_OP_ 0x59 = pure DW_OP_reg9
          getDW_OP_ 0x5a = pure DW_OP_reg10
          getDW_OP_ 0x5b = pure DW_OP_reg11
          getDW_OP_ 0x5c = pure DW_OP_reg12
          getDW_OP_ 0x5d = pure DW_OP_reg13
          getDW_OP_ 0x5e = pure DW_OP_reg14
          getDW_OP_ 0x5f = pure DW_OP_reg15
          getDW_OP_ 0x60 = pure DW_OP_reg16
          getDW_OP_ 0x61 = pure DW_OP_reg17
          getDW_OP_ 0x62 = pure DW_OP_reg18
          getDW_OP_ 0x63 = pure DW_OP_reg19
          getDW_OP_ 0x64 = pure DW_OP_reg20
          getDW_OP_ 0x65 = pure DW_OP_reg21
          getDW_OP_ 0x66 = pure DW_OP_reg22
          getDW_OP_ 0x67 = pure DW_OP_reg23
          getDW_OP_ 0x68 = pure DW_OP_reg24
          getDW_OP_ 0x69 = pure DW_OP_reg25
          getDW_OP_ 0x6a = pure DW_OP_reg26
          getDW_OP_ 0x6b = pure DW_OP_reg27
          getDW_OP_ 0x6c = pure DW_OP_reg28
          getDW_OP_ 0x6d = pure DW_OP_reg29
          getDW_OP_ 0x6e = pure DW_OP_reg30
          getDW_OP_ 0x6f = pure DW_OP_reg31
          getDW_OP_ 0x70 = pure DW_OP_breg0  <*> getSLEB128
          getDW_OP_ 0x71 = pure DW_OP_breg1  <*> getSLEB128
          getDW_OP_ 0x72 = pure DW_OP_breg2  <*> getSLEB128
          getDW_OP_ 0x73 = pure DW_OP_breg3  <*> getSLEB128
          getDW_OP_ 0x74 = pure DW_OP_breg4  <*> getSLEB128
          getDW_OP_ 0x75 = pure DW_OP_breg5  <*> getSLEB128
          getDW_OP_ 0x76 = pure DW_OP_breg6  <*> getSLEB128
          getDW_OP_ 0x77 = pure DW_OP_breg7  <*> getSLEB128
          getDW_OP_ 0x78 = pure DW_OP_breg8  <*> getSLEB128
          getDW_OP_ 0x79 = pure DW_OP_breg9  <*> getSLEB128
          getDW_OP_ 0x7a = pure DW_OP_breg10 <*> getSLEB128
          getDW_OP_ 0x7b = pure DW_OP_breg11 <*> getSLEB128
          getDW_OP_ 0x7c = pure DW_OP_breg12 <*> getSLEB128
          getDW_OP_ 0x7d = pure DW_OP_breg13 <*> getSLEB128
          getDW_OP_ 0x7e = pure DW_OP_breg14 <*> getSLEB128
          getDW_OP_ 0x7f = pure DW_OP_breg15 <*> getSLEB128
          getDW_OP_ 0x80 = pure DW_OP_breg16 <*> getSLEB128
          getDW_OP_ 0x81 = pure DW_OP_breg17 <*> getSLEB128
          getDW_OP_ 0x82 = pure DW_OP_breg18 <*> getSLEB128
          getDW_OP_ 0x83 = pure DW_OP_breg19 <*> getSLEB128
          getDW_OP_ 0x84 = pure DW_OP_breg20 <*> getSLEB128
          getDW_OP_ 0x85 = pure DW_OP_breg21 <*> getSLEB128
          getDW_OP_ 0x86 = pure DW_OP_breg22 <*> getSLEB128
          getDW_OP_ 0x87 = pure DW_OP_breg23 <*> getSLEB128
          getDW_OP_ 0x88 = pure DW_OP_breg24 <*> getSLEB128
          getDW_OP_ 0x89 = pure DW_OP_breg25 <*> getSLEB128
          getDW_OP_ 0x8a = pure DW_OP_breg26 <*> getSLEB128
          getDW_OP_ 0x8b = pure DW_OP_breg27 <*> getSLEB128
          getDW_OP_ 0x8c = pure DW_OP_breg28 <*> getSLEB128
          getDW_OP_ 0x8d = pure DW_OP_breg29 <*> getSLEB128
          getDW_OP_ 0x8e = pure DW_OP_breg30 <*> getSLEB128
          getDW_OP_ 0x8f = pure DW_OP_breg31 <*> getSLEB128
          getDW_OP_ 0x90 = pure DW_OP_regx   <*> getULEB128
          getDW_OP_ 0x91 = pure DW_OP_fbreg  <*> getSLEB128
          getDW_OP_ 0x92 = pure DW_OP_bregx  <*> getULEB128 <*> getSLEB128
          getDW_OP_ 0x93 = pure DW_OP_piece  <*> getULEB128
          getDW_OP_ 0x94 = pure DW_OP_deref_size <*> getWord8
          getDW_OP_ 0x95 = pure DW_OP_xderef_size <*> getWord8
          getDW_OP_ 0x96 = pure DW_OP_nop
          getDW_OP_ 0x97 = pure DW_OP_push_object_address
          getDW_OP_ 0x98 = pure DW_OP_call2 <*> drGetW16 dr
          getDW_OP_ 0x99 = pure DW_OP_call4 <*> drGetW32 dr
          getDW_OP_ 0x9a = pure DW_OP_call_ref <*> drGetDwarfTargetAddress dr
          getDW_OP_ 0x9b = pure DW_OP_form_tls_address
          getDW_OP_ 0x9c = pure DW_OP_call_frame_cfa
          getDW_OP_ 0x9d = pure DW_OP_bit_piece <*> getULEB128 <*> getULEB128
          getDW_OP_ n | 0xe0 <= n && n <= 0xff = fail $ "User DW_OP data requires extension of parser for code " ++ show n
          getDW_OP_ n = fail $ "Unrecognized DW_OP code " ++ show n

data DW_ATE
    = DW_ATE_address
    | DW_ATE_boolean
    | DW_ATE_complex_float
    | DW_ATE_float
    | DW_ATE_signed
    | DW_ATE_signed_char
    | DW_ATE_unsigned
    | DW_ATE_unsigned_char
    | DW_ATE_imaginary_float
    | DW_ATE_packed_decimal
    | DW_ATE_numeric_string
    | DW_ATE_edited
    | DW_ATE_signed_fixed
    | DW_ATE_unsigned_fixed
    | DW_ATE_decimal_float
    deriving (Show, Eq)
dw_ate :: Word64 -> DW_ATE
dw_ate 0x01 = DW_ATE_address
dw_ate 0x02 = DW_ATE_boolean
dw_ate 0x03 = DW_ATE_complex_float
dw_ate 0x04 = DW_ATE_float
dw_ate 0x05 = DW_ATE_signed
dw_ate 0x06 = DW_ATE_signed_char
dw_ate 0x07 = DW_ATE_unsigned
dw_ate 0x08 = DW_ATE_unsigned_char
dw_ate 0x09 = DW_ATE_imaginary_float
dw_ate 0x0a = DW_ATE_packed_decimal
dw_ate 0x0b = DW_ATE_numeric_string
dw_ate 0x0c = DW_ATE_edited
dw_ate 0x0d = DW_ATE_signed_fixed
dw_ate 0x0e = DW_ATE_unsigned_fixed
dw_ate 0x0f = DW_ATE_decimal_float
dw_ate n = error $ "Unrecognized DW_ATE encoding " ++ show n

data DW_DS
    = DW_DS_unsigned
    | DW_DS_leading_overpunch
    | DW_DS_trailing_overpunch
    | DW_DS_leading_separate
    | DW_DS_trailing_separate
    deriving (Show, Eq)
dw_ds :: Word64 -> DW_DS
dw_ds 0x01 = DW_DS_unsigned
dw_ds 0x02 = DW_DS_leading_overpunch
dw_ds 0x03 = DW_DS_trailing_overpunch
dw_ds 0x04 = DW_DS_leading_separate
dw_ds 0x05 = DW_DS_trailing_separate
dw_ds tag = error $ "Invalid DW_DS tag: " ++ show tag

data DW_END
    = DW_END_default
    | DW_END_big
    | DW_END_little
    deriving (Show, Eq)
dw_end :: Word64 -> DW_END
dw_end 0x00 = DW_END_default
dw_end 0x01 = DW_END_big
dw_end 0x02 = DW_END_little
dw_end n = error $ "Unrecognized DW_END value " ++ show n

data DW_ACCESS
    = DW_ACCESS_public
    | DW_ACCESS_protected
    | DW_ACCESS_private
    deriving (Show, Eq)
dw_access :: Word64 -> DW_ACCESS
dw_access 0x01 = DW_ACCESS_public
dw_access 0x02 = DW_ACCESS_protected
dw_access 0x03 = DW_ACCESS_private
dw_access tag = error $ "Invalid dw_access tag: " ++ show tag

data DW_VIS
    = DW_VIS_local
    | DW_VIS_exported
    | DW_VIS_qualified
    deriving (Show, Eq)
dw_vis :: Word64 -> DW_VIS
dw_vis 0x01 = DW_VIS_local
dw_vis 0x02 = DW_VIS_exported
dw_vis 0x03 = DW_VIS_qualified
dw_vis tag = error $ "Invalid DW_VIS tag: " ++ show tag

data DW_VIRTUALITY
    = DW_VIRTUALITY_none
    | DW_VIRTUALITY_virtual
    | DW_VIRTUALITY_return_virtual
    deriving (Show, Eq)
dw_virtuality :: Word64 -> DW_VIRTUALITY
dw_virtuality 0x00 = DW_VIRTUALITY_none
dw_virtuality 0x01 = DW_VIRTUALITY_virtual
dw_virtuality 0x02 = DW_VIRTUALITY_return_virtual
dw_virtuality tag = error $ "Invalid tag for DW_VIRTUALITY: " ++ show tag

data DW_LANG
    = DW_LANG_C89
    | DW_LANG_C
    | DW_LANG_Ada83
    | DW_LANG_C_plus_plus
    | DW_LANG_Cobol74
    | DW_LANG_Cobol85
    | DW_LANG_Fortran77
    | DW_LANG_Fortran90
    | DW_LANG_Pascal83
    | DW_LANG_Modula2
    | DW_LANG_Java
    | DW_LANG_C99
    | DW_LANG_Ada95
    | DW_LANG_Fortran95
    | DW_LANG_PLI
    | DW_LANG_ObjC
    | DW_LANG_ObjC_plus_plus
    | DW_LANG_UPC
    | DW_LANG_D
    deriving (Show, Eq)
dw_lang :: Word64 -> DW_LANG
dw_lang 0x0001 = DW_LANG_C89
dw_lang 0x0002 = DW_LANG_C
dw_lang 0x0003 = DW_LANG_Ada83
dw_lang 0x0004 = DW_LANG_C_plus_plus
dw_lang 0x0005 = DW_LANG_Cobol74
dw_lang 0x0006 = DW_LANG_Cobol85
dw_lang 0x0007 = DW_LANG_Fortran77
dw_lang 0x0008 = DW_LANG_Fortran90
dw_lang 0x0009 = DW_LANG_Pascal83
dw_lang 0x000a = DW_LANG_Modula2
dw_lang 0x000b = DW_LANG_Java
dw_lang 0x000c = DW_LANG_C99
dw_lang 0x000d = DW_LANG_Ada95
dw_lang 0x000e = DW_LANG_Fortran95
dw_lang 0x000f = DW_LANG_PLI
dw_lang 0x0010 = DW_LANG_ObjC
dw_lang 0x0011 = DW_LANG_ObjC_plus_plus
dw_lang 0x0012 = DW_LANG_UPC
dw_lang 0x0013 = DW_LANG_D
dw_lang n = error $ "Unrecognized DW_LANG " ++ show n

data DW_ID
    = DW_ID_case_sensitive
    | DW_ID_up_case
    | DW_ID_down_case
    | DW_ID_case_insensitive
    deriving (Show, Eq)
dw_id :: Word64 -> DW_ID
dw_id 0x00 = DW_ID_case_sensitive
dw_id 0x01 = DW_ID_up_case
dw_id 0x02 = DW_ID_down_case
dw_id 0x03 = DW_ID_case_insensitive
dw_id n = error $ "Unrecognized DW_ID " ++ show n

data DW_CC
    = DW_CC_normal
    | DW_CC_program
    | DW_CC_nocall
    deriving (Show, Eq)
dw_cc :: Word64 -> DW_CC
dw_cc 0x01 = DW_CC_normal
dw_cc 0x02 = DW_CC_program
dw_cc 0x03 = DW_CC_nocall
dw_cc n = error $ "Unrecognized calling convention " ++ show n

data DW_INL
    = DW_INL_not_inlined
    | DW_INL_inlined
    | DW_INL_declared_not_inlined
    | DW_INL_declared_inlined
    deriving (Show, Eq)
dw_inl :: Word64 -> DW_INL
dw_inl 0x00 = DW_INL_not_inlined
dw_inl 0x01 = DW_INL_inlined
dw_inl 0x02 = DW_INL_declared_not_inlined
dw_inl 0x03 = DW_INL_declared_inlined
dw_inl n = error $ "Unrecognized DW_INL " ++ show n

data DW_ORD
    = DW_ORD_row_major
    | DW_ORD_col_major
    deriving (Show, Eq)
dw_ord :: Word64 -> DW_ORD
dw_ord 0x00 = DW_ORD_row_major
dw_ord 0x01 = DW_ORD_col_major
dw_ord n = error $ "Unrecognized DW_ORD " ++ show n

data DW_DSC
    = DW_DSC_label
    | DW_DSC_range
    deriving (Show, Eq)
dw_dsc :: Word64 -> DW_DSC
dw_dsc 0x00 = DW_DSC_label
dw_dsc 0x01 = DW_DSC_range
dw_dsc n = error $ "Unrecognized DW_DSC " ++ show n
