{-# LANGUAGE DeriveGeneric #-}
module Data.Dwarf.ATE where

import Data.Word (Word64)
import GHC.Generics (Generic)

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
    deriving (Eq, Ord, Read, Show, Generic)


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
