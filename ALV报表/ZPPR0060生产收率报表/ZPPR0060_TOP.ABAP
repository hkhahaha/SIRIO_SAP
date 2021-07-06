*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_TOP
*&---------------------------------------------------------------------*
TABLES:zppsprofis.
* TAB_TRIPE的tab function
TYPES ty_tab_zppsprofis TYPE TABLE OF zppsprofis.
CONSTANTS:BEGIN OF gc_tab_funs,
            zfc_tab1 LIKE sy-ucomm  VALUE 'ZFC_TAB1', "tab1的TAB FUNCTION
            zfc_tab2 LIKE sy-ucomm VALUE 'ZFC_TAB2', "tab2的TAB FUNCTION
          END OF gc_tab_funs.
CONTROLS ztab_strip TYPE TABSTRIP. "与屏幕9000中的tabstrip的名称一致
DATA:BEGIN OF gs_tab_appl,
       subscreen   LIKE sy-dynnr,
       prog        LIKE sy-repid VALUE 'ZPPR0060',
       pressed_tab LIKE sy-ucomm VALUE gc_tab_funs-zfc_tab1, "默认激活TAB1
     END OF gs_tab_appl.
DATA:BEGIN OF gs_charg,
       charg TYPE afpo-charg,
     END OF gs_charg.
DATA:
  gv_converted_flag TYPE char1,
  gv_okcode         LIKE sy-ucomm,
  gv_savecode       LIKE sy-ucomm,
  go_con_tab1       TYPE REF TO cl_gui_custom_container,
  go_con_tab2       TYPE REF TO cl_gui_custom_container,
  go_salv_columns   TYPE REF TO cl_salv_columns_table,
  go_salv_table     TYPE REF TO  cl_salv_table,
  go_alv_grid       TYPE REF TO cl_gui_alv_grid,
  gt_alv_fidcat     TYPE lvc_t_fcat,
  gs_layout         TYPE lvc_s_layo,
  gs_stable         TYPE lvc_s_stbl,
  gs_bapiret        TYPE bapiret2,
  gt_pro            LIKE TABLE OF zppsprofis WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY sk_charg COMPONENTS charg,
  gt_pro_sl         LIKE TABLE OF zppsprofis.