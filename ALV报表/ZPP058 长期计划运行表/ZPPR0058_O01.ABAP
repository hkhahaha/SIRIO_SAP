*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module SET_TABSTRIP_ACTIVE_TAB OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_tabstrip_active_tab OUTPUT.
  ztab_strip-activetab = gs_tab_appl-pressed_tab."active tab
  CASE gs_tab_appl-pressed_tab."active tab
    WHEN gc_tab_funs-zfc_pir_gross."PIR毛需求
      gs_tab_appl-subscreen = '9100'.
    WHEN gc_tab_funs-zfc_pir_raw_gross. "PIR原辅料毛需求
      gs_tab_appl-subscreen = '9200'.
    WHEN gc_tab_funs-zfc_pir_net. "PIR净需求
      gs_tab_appl-subscreen = '9300'.
    WHEN gc_tab_funs-zfc_pir_raw_net. "PIR原辅料净需求
      gs_tab_appl-subscreen = '9400'.
    WHEN gc_tab_funs-zfc_warehouse. "PIR计算仓储地点维护
      gs_tab_appl-subscreen = '9500'.
    WHEN OTHERS.
  ENDCASE.
  SET PF-STATUS 'D0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'D0100'.
  SET TITLEBAR 'RPT_TITLE'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module DISPLAY_PIR_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE display_pir_gross_alv OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  PERFORM frm_display_pir_gross_alv.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV_WAREHOUSE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE display_alv_warehouse OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  PERFORM frm_get_zppt0011.
  PERFORM frm_display_alv_wh.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_PIR_RAW_GROSS_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE display_pir_raw_gross_alv OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在加载...'.
  PERFORM frm_display_pir_raw_gross_alv.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_PIR_NET_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE display_pir_net_alv OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在加载...'.
  IF gt_pir_net IS INITIAL.
    PERFORM frm_get_pir_net."准备数据
  ENDIF.
  PERFORM frm_display_alv_pir_net."显示ALV
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_PIR_RAW_NET_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE display_pir_raw_net_alv OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在加载...'.
  PERFORM frm_display_pir_raw_net_alv.
ENDMODULE.