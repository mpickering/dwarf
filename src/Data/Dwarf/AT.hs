{-# LANGUAGE DeriveGeneric #-}
module Data.Dwarf.AT where

import qualified Data.ByteString as B
import           Data.Dwarf.Types
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

data DW_ATVAL
    = DW_ATVAL_INT    Int64
    | DW_ATVAL_UINT   Word64
    | DW_ATVAL_REF    DieID
    | DW_ATVAL_STRING Text
    | DW_ATVAL_BLOB   B.ByteString
    | DW_ATVAL_BOOL   Bool
    deriving (Eq, Ord, Show, Generic)


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
    | DW_AT_language             -- ^ constant (DW_LANG)
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
    | DW_AT_signature
    | DW_AT_main_subprogram
    | DW_AT_data_bit_offset
    | DW_AT_const_expr
    | DW_AT_enum_class
    | DW_AT_linkage_name

    -- DWARF 5
    | DW_AT_string_length_bit_size
    | DW_AT_string_length_byte_size
    | DW_AT_rank
    | DW_AT_str_offsets_base
    | DW_AT_addr_base
    | DW_AT_rnglists_base
    | DW_AT_dwo_name
    | DW_AT_reference
    | DW_AT_rvalue_reference
    | DW_AT_macros
    | DW_AT_call_all_calls
    | DW_AT_call_all_source_calls
    | DW_AT_call_all_tail_calls
    | DW_AT_call_return_pc
    | DW_AT_call_value
    | DW_AT_call_origin
    | DW_AT_call_parameter
    | DW_AT_call_pc
    | DW_AT_call_tail_call
    | DW_AT_call_target
    | DW_AT_call_target_clobbered
    | DW_AT_call_data_location
    | DW_AT_call_data_value
    | DW_AT_noreturn
    | DW_AT_alignment
    | DW_AT_export_symbols
    | DW_AT_deleted
    | DW_AT_defaulted
    | DW_AT_loclists_base
    | DW_AT_user Word64          -- ^ user extension
    deriving (Eq, Ord, Read, Show, Generic)


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
dw_at 0x69 = DW_AT_signature
dw_at 0x6a = DW_AT_main_subprogram
dw_at 0x6b = DW_AT_data_bit_offset
dw_at 0x6c = DW_AT_const_expr
dw_at 0x6d = DW_AT_enum_class
dw_at 0x6e = DW_AT_linkage_name
dw_at 0x6f = DW_AT_string_length_bit_size
dw_at 0x70 = DW_AT_string_length_byte_size
dw_at 0x71 = DW_AT_rank
dw_at 0x72 = DW_AT_str_offsets_base
dw_at 0x73 = DW_AT_addr_base
dw_at 0x74 = DW_AT_rnglists_base
dw_at 0x76 = DW_AT_dwo_name
dw_at 0x77 = DW_AT_reference
dw_at 0x78 = DW_AT_rvalue_reference
dw_at 0x79 = DW_AT_macros
dw_at 0x7a = DW_AT_call_all_calls
dw_at 0x7b = DW_AT_call_all_source_calls
dw_at 0x7c = DW_AT_call_all_tail_calls
dw_at 0x7d = DW_AT_call_return_pc
dw_at 0x7e = DW_AT_call_value
dw_at 0x7f = DW_AT_call_origin
dw_at 0x80 = DW_AT_call_parameter
dw_at 0x81 = DW_AT_call_pc
dw_at 0x82 = DW_AT_call_tail_call
dw_at 0x83 = DW_AT_call_target
dw_at 0x84 = DW_AT_call_target_clobbered
dw_at 0x85 = DW_AT_call_data_location
dw_at 0x86 = DW_AT_call_data_value
dw_at 0x87 = DW_AT_noreturn
dw_at 0x88 = DW_AT_alignment
dw_at 0x89 = DW_AT_export_symbols
dw_at 0x8a = DW_AT_deleted
dw_at 0x8b = DW_AT_defaulted
dw_at 0x8c = DW_AT_loclists_base
dw_at n | 0x2000 <= n && n <= 0x3fff = DW_AT_user n
dw_at n = error $ "Unrecognized DW_AT " ++ show n
