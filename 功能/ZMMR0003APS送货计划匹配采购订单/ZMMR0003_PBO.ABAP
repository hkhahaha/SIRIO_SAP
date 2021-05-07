*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_PBO
*&---------------------------------------------------------------------*
*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tab1_active_tab_set OUTPUT.
  tab1-activetab = g_tab1-pressed_tab.
  CASE g_tab1-pressed_tab.
    WHEN c_tab1-tab1.
      g_tab1-subscreen = '0102'.
    WHEN c_tab1-tab2.
      g_tab1-subscreen = '0103'.
    WHEN c_tab1-tab3.
      g_tab1-subscreen = '0103'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100' EXCLUDING gt_menu.
  SET TITLEBAR 'T0100'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module MOD_HEAD_ALV_INIT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_head_alv_init OUTPUT.
  PERFORM frm_head_alv_init.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module MOD_PO_ALV_INIT OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mod_po_alv_init OUTPUT.
  PERFORM frm_po_alv_init.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  PERFORM frm_detail_alv_init.
  SET PF-STATUS 'PF0200'.
* SET TITLEBAR 'xxx'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0010 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0010 OUTPUT.
  SET PF-STATUS 'PF0100' EXCLUDING gt_menu.
  SET TITLEBAR 'T0100'.
  PERFORM frm_split_init.
ENDMODULE.