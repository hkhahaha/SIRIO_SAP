*&---------------------------------------------------------------------*
*& Report ZPPR0058
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr0058 MESSAGE-ID zpp.
INCLUDE zppr0058_top.
INCLUDE zppr0058_sel_screen.
INCLUDE zppr0058_o01.
INCLUDE zppr0058_i01.
INCLUDE zppr0058_c01.
INCLUDE zppr0058_frm.
INITIALIZATION.
PERFORM frm_set_initial_value.
START-OF-SELECTION.
  PERFORM frm_auth_check."工厂权限检查
  PERFORM frm_set_pdatu."设置12个月的期间范围及字段标签
  PERFORM frm_get_pir."获取PIR
  CALL SCREEN '9000'."显示报表