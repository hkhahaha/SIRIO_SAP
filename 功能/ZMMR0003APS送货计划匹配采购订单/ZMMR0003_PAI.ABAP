*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_PAI
*&---------------------------------------------------------------------*


*&SPWIZARD: INPUT MODULE FOR TS 'TAB1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tab1_active_tab_get INPUT.
  gv_ok_code = sy-ucomm.
  CASE gv_ok_code.
    WHEN c_tab1-tab1.
      g_tab1-pressed_tab = c_tab1-tab1.
    WHEN c_tab1-tab2.
      g_tab1-pressed_tab = c_tab1-tab2.
    WHEN c_tab1-tab3.
      g_tab1-pressed_tab = c_tab1-tab3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  MOD_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mod_exit INPUT.
  PERFORM frm_leave_screen.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'OK'.
      PERFORM frm_change_from_detail.
      PERFORM frm_leave_screen.
    WHEN 'CANCEL'.
      PERFORM frm_leave_screen.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       响应用户按钮
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM frm_save_data.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2000 INPUT.

ENDMODULE.