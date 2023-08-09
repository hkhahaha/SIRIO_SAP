FUNCTION zmm_bp_maintain.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  ZDT_OA2SAP_BP
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  ZDT_OA2SAP_BP_RET
*"----------------------------------------------------------------------

  IF i_input IS INITIAL AND sy-uname(6) = 'ELAINE'.
    PERFORM frm_import_data_from_excel CHANGING i_input.
  ENDIF.
  CLEAR: e_output-type, e_output-message.
  g_mcflag = i_input-mcflag.
  PERFORM frm_insert_log USING i_input-oaid cns_bptype_supplier i_input-mcflag
                         CHANGING i_input-bpartner e_output-type e_output-message.
  IF e_output-type = 'E'.
    RETURN.
  ENDIF.

  e_output-type = 'S'.
  PERFORM frm_check_input USING    i_input
                          CHANGING e_output.
  IF e_output-type NE 'S'.
    RETURN.
  ENDIF.

  PERFORM frm_maintain_supply USING    i_input
                              CHANGING e_output.

  PERFORM frm_update_log USING i_input-oaid
                               cns_bptype_supplier
                               i_input-mcflag
                               g_partner
                               e_output-type
                               e_output-message.

ENDFUNCTION.