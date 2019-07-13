{-# LANGUAGE DeriveGeneric #-}
module Data.Dwarf.Form where

import Data.Word (Word64)
import GHC.Generics (Generic)

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
    | DW_FORM_sec_offset          -- ^ (Dwarf 4)
    | DW_FORM_exprloc             -- ^ (Dwarf 4)
    | DW_FORM_flag_present        -- ^ (Dwarf 4)
    | DW_FORM_ref_sig8            -- ^ (Dwarf 4)
    deriving (Eq, Ord, Read, Show, Generic)


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
dw_form 0x17 = DW_FORM_sec_offset
dw_form 0x18 = DW_FORM_exprloc
dw_form 0x19 = DW_FORM_flag_present
dw_form 0x20 = DW_FORM_ref_sig8
dw_form n    = error $ "Unrecognized DW_FORM " ++ show n
