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
    WHEN gc_tab_funs-zfc_tab1."tab1
      gs_tab_appl-subscreen = '9100'.
    WHEN gc_tab_funs-zfc_tab2. "tab2
      gs_tab_appl-subscreen = '9200'.
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
MODULE display_pro_alv OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  PERFORM frm_display_pro_alv.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module DISPLAY_ALV_WAREHOUSE OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE display_alv_warehouse OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
*  PERFORM frm_get_zppt0011.
*  PERFORM frm_display_alv_wh.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DISPLAY_PIR_RAW_GROSS_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE display_grid_alv OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING percentage = 50 text = '正在加载...'.
  PERFORM frm_display_grid_alv.
ENDMODULE.