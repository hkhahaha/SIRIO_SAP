*----------------------------------------------------------------------*
***INCLUDE LZMM006F04.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_CUSTOM_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_INPUT
*&      <-- E_OUTPUT
*&---------------------------------------------------------------------*
FORM frm_check_custom_input  USING    u_input TYPE zdt_oa2other_custom
                             CHANGING c_output TYPE zdt_oa2other_custom_ret.

  PERFORM frm_get_g_partner USING u_input-bpartner .
  PERFORM frm_check_bp_exist USING    u_input-bpartner g_mcflag
                             CHANGING c_output-type c_output-message.
  IF c_output-type = 'E'.
    RETURN.
  ENDIF.
  PERFORM frm_check_bp_group USING    u_input-grouping g_mcflag
                             CHANGING c_output-type c_output-message.
  IF c_output-type = 'E'.
    RETURN.
  ENDIF.

  PERFORM frm_check_taxnumber USING g_partner u_input-taxtype u_input-taxnumxl
                              CHANGING c_output-type c_output-message.
  IF c_output-type = 'E'.
    RETURN.
  ENDIF.

ENDFORM.