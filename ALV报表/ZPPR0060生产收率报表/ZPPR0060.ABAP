*&---------------------------------------------------------------------*
*& Report ZPPR0058
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zppr0060 MESSAGE-ID zpp.
INCLUDE zppr0060_top.
INCLUDE zppr0060_sel_screen.
INCLUDE zppr0060_o01.
INCLUDE zppr0060_i01.
INCLUDE zppr0060_c01.
INCLUDE zppr0060_frm.
INITIALIZATION.
  PERFORM frm_set_initial_value.

START-OF-SELECTION.
  PERFORM frm_auth_check."工厂权限检查
  PERFORM frm_reset_selcondition.
  PERFORM frm_get_data."流程订单抬头信息
  CALL SCREEN '9000'."显示报表