FUNCTION zsd_maintain_customer_bp.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  ZDT_OA2OTHER_CUSTOM
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  ZDT_OA2OTHER_CUSTOM_RET
*"----------------------------------------------------------------------

  IF i_input IS INITIAL AND sy-uname(6) = 'ELAINE'.
    PERFORM frm_import_customer_from_excel CHANGING i_input.
  ENDIF.
  CLEAR: e_output-type, e_output-message.
  g_mcflag = i_input-mcflag.
  PERFORM frm_insert_log USING i_input-oaid cns_bptype_customer i_input-mcflag
                         CHANGING i_input-bpartner e_output-type e_output-message.
  IF e_output-type = 'E'.
    RETURN.
  ENDIF.

  e_output-type = 'S'.
  PERFORM frm_check_custom_input USING    i_input
                                 CHANGING e_output.
  IF e_output-type NE 'S'.
    RETURN.
  ENDIF.

  PERFORM frm_maintain_customer USING    i_input
                                CHANGING e_output.

  PERFORM frm_update_log USING i_input-oaid
                               cns_bptype_customer
                               i_input-mcflag
                               g_partner
                               e_output-type
                               e_output-message.



ENDFUNCTION.