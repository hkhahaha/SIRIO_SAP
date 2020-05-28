*&---------------------------------------------------------------------*
*& Report ZSDR0023
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0023.
INCLUDE zsdr0023_top.
INCLUDE zsdr0023_src.
INCLUDE zsdr0023_f01.

INITIALIZATION.
  gv_functxt-icon_id       = icon_xls.
  gv_functxt-icon_text     = '下载模板'.
  sscrfields-functxt_01 = gv_functxt.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_file_input.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'FC01'.
    PERFORM frm_downdload_template .
  ENDIF.

START-OF-SELECTION.

  IF p_file <> ''.
    PERFORM frm_upload_data.
    PERFORM frm_check_data.
    PERFORM frm_get_text.
  ELSE.
    MESSAGE '没有选择导入文件！'  TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*----------------------------------------------------------------------*
*          END-OF-SELECTION                                          *
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM frm_display_data.