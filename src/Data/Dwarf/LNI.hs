{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Dwarf.LNI where

import           Control.Monad (replicateM)
import           Data.Binary (Binary(..), Get)
import           Data.Binary.Get (getWord8)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as B
import           Data.Dwarf.Reader
import           Data.Dwarf.Utils
import           Data.Int (Int8, Int64)
import           Data.Text (Text)
import           Data.Word (Word8, Word64)
import           GHC.Generics (Generic)

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
    | DW_LNE_define_file Text Word64 Word64 Word64
    | DW_LNE_set_discriminator Word64
    deriving (Eq, Ord, Read, Show, Generic)


getDW_LNI :: Reader -> Int64 -> Word8 -> Word8 -> Word64 -> Get DW_LNI
getDW_LNI dr line_base line_range opcode_base minimum_instruction_length = getWord8 >>= getDW_LNI_
    where getDW_LNI_ 0x00 = do
            rest <- getByteStringLen getULEB128
            pure $ strictGet getDW_LNE rest
                where getDW_LNE = getWord8 >>= getDW_LNE_
                      getDW_LNE_ 0x01 = pure DW_LNE_end_sequence
                      getDW_LNE_ 0x02 = pure DW_LNE_set_address <*> drGetTargetAddress dr
                      getDW_LNE_ 0x03 = pure DW_LNE_define_file <*> getUTF8Str0 <*> getULEB128 <*> getULEB128 <*> getULEB128
                      getDW_LNE_ 0x04 = pure DW_LNE_set_discriminator <*> getULEB128
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
stepLineMachine is_stmt mil lnm (DW_LNE_set_discriminator d : xs) =
    let new = lnm { lnmDiscriminator = d }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNE_end_sequence : xs) =
    let row = lnm { lnmEndSequence = True }
        new = defaultLNE is_stmt (lnmFiles lnm)
    in row : stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNE_set_address address : xs) =
    let new = lnm { lnmAddress = address }
    in stepLineMachine is_stmt mil new xs
stepLineMachine is_stmt mil lnm (DW_LNE_define_file name dir_index time len : xs) =
    let new = lnm { lnmFiles = lnmFiles lnm ++ [LNEFile name (fromIntegral dir_index) time len] }
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
    , lnmDiscriminator :: Word64
    , lnmFiles         :: [LNEFile]
    } deriving (Eq, Ord, Read, Show, Generic)


defaultLNE :: Bool -> [LNEFile] -> DW_LNE
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
    , lnmDiscriminator = 0
    , lnmFiles         = files
    }

-- | Retrieves the line information for a DIE from a given substring of the .debug_line section. The offset
-- into the .debug_line section is obtained from the DW_AT_stmt_list attribute of a DIE.
parseLNE :: Endianess -> TargetSize -> Word64 -> B.ByteString -> LNE
parseLNE endianess target64 offset bs =
    let dr = endianReader endianess
    in getAt (getLNE target64 dr) offset bs

data LNEFile = LNEFile
  { lneFileName :: Text
  , lneDirIndex :: Int
  , lneLastMod  :: Word64
  , lneLength   :: Word64
  } deriving (Eq, Ord, Read, Show, Generic)

data LNE = LNE
  { lneDirs :: [Text]
  , lneFiles :: [LNEFile]
  , lneLines :: [DW_LNE]
  } deriving (Eq, Ord, Read, Show, Generic)

getDebugLineFileNames :: Get [LNEFile]
getDebugLineFileNames = whileJust $ traverse entry =<< getNonEmptyUTF8Str0
  where
    entry file_name = do
      dir_index   <- getULEB128
      last_mod    <- getULEB128
      file_length <- getULEB128
      pure (LNEFile file_name (fromIntegral dir_index) last_mod file_length)

getLNE :: TargetSize -> EndianReader -> Get LNE
getLNE target64 der = do
    (desr, endPos)             <- getUnitLength der
    let dr                      = reader target64 desr
    _version                   <- drGetW16 dr
    _header_length             <- drGetOffset dr
    minimum_instruction_length <- getWord8
    default_is_stmt            <- (/= 0) <$> getWord8
    line_base                  <- get :: Get Int8
    line_range                 <- getWord8
    opcode_base                <- getWord8
    _standard_opcode_lengths   <- replicateM (fromIntegral opcode_base - 1) getWord8
    include_directories       <- whileM (/= "") getUTF8Str0
    file_names                 <- getDebugLineFileNames
    curPos <- fromIntegral <$> Get.bytesRead
    -- Check if we have reached the end of the section.
    if endPos <= curPos
      then pure (LNE include_directories file_names [])
      else do
        line_program <-
          fmap (++ [DW_LNE_end_sequence]) .
          whileM (/= DW_LNE_end_sequence) .
            getDW_LNI dr (fromIntegral line_base) line_range opcode_base $
            fromIntegral minimum_instruction_length
        let initial_state = defaultLNE default_is_stmt file_names
            line_matrix = stepLineMachine default_is_stmt minimum_instruction_length initial_state line_program
         in pure (LNE include_directories file_names line_matrix)
