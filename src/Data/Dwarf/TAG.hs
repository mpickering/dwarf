{-# LANGUAGE DeriveGeneric #-}
module Data.Dwarf.TAG where

import Data.Binary.Get (Get)
import Data.Dwarf.Utils
import Data.Word (Word64)
import GHC.Generics (Generic)

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
    | DW_TAG_user Word64 -- index into the user range of tags 0x4080 becomes 0
    deriving (Eq, Ord, Read, Show, Generic)


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
          dw_tag n | 0x4080 <= n && n <= 0xffff = pure . DW_TAG_user $ n - 0x4080
          dw_tag n = fail $ "Unrecognized DW_TAG " ++ show n
