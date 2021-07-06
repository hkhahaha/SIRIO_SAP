*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  GET_TABSTRIP_ACTIVE_TAB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_tabstrip_active_tab INPUT.
  gv_savecode = gv_okcode ."tabstrip的function code
  "设置激活的TAB
  IF gv_savecode = gc_tab_funs-zfc_tab1 "
    OR gv_savecode = gc_tab_funs-zfc_tab2.
    gs_tab_appl-pressed_tab = gv_savecode.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  PERFORM d0100_user_command.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  gv_savecode = gv_okcode.
  CASE gv_savecode.
    WHEN 'ZBN_CONVERT'."转换产品收率表
      "设置gs_tab_appl-pressed_tab后，程序会重新执行9000主屏幕的动态子屏幕9200
      IF gv_converted_flag IS INITIAL.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在计算...'.
        PERFORM frm_conv_data_for_9200."准备数据
        gv_converted_flag = 'X'.
      ENDIF.
      gs_tab_appl-pressed_tab = gc_tab_funs-zfc_tab2."激活TAB-产品收率表
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.