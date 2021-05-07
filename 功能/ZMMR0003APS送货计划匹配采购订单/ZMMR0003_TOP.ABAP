*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS:icon.    "图标池
*&---------------------------------------------------------------------*
*  Tables
*&---------------------------------------------------------------------*
TABLES: zmmaps_pl,
        zmmaps_temp,
        zmmwms_pol,
        zmmwms_poh,
        mara,
        marc,
        ekko,
        sscrfields.

*&---------------------------------------------------------------------*
*  Types
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_key,
         werks   LIKE ekpo-werks,
         lgort   LIKE ekpo-lgort,
         version LIKE zmmaps_pl-version,
         matnr   LIKE ekpo-matnr,
         st_code LIKE zmmaps_pl-st_code,
         reqdate LIKE zmmaps_pl-reqdate,
       END OF ty_key.

TYPES: BEGIN OF ty_key2,
         werks   LIKE ekpo-werks,
*         lgort   LIKE ekpo-lgort,
         matnr   LIKE ekpo-matnr,
         st_code LIKE zmmaps_pl-st_code,
       END OF ty_key2.

TYPES: BEGIN OF ty_aps_pl.
         INCLUDE TYPE zmmaps_pl.
         TYPES:   key            TYPE ty_key,
         key2           TYPE ty_key2,
         maktx          LIKE makt-maktx,
         st_code_txt    TYPE char20,
         groes          LIKE mara-groes,
         ekgrp          LIKE marc-ekgrp,
         meins          LIKE mara-meins,
         mfrnr          LIKE mara-mfrnr,
         producer_name  LIKE lfa1-name1,
         mseht          LIKE t006a-mseht,
         pdlv_menge     LIKE zmmwms_pol-pdlv_menge,
         received_quan  LIKE zmmwms_pol-received_quan,
         sy_menge       LIKE zmmaps_temp-assign_menge,
         assigned_menge LIKE zmmaps_temp-assign_menge,
         zpr_ststxt     TYPE char20,
         lifnr1         LIKE lfa1-lifnr,
         name1_1        LIKE lfa1-name1,
         vmenge1        LIKE zmmwms_pol-pdlv_menge,
         lifnr2         LIKE lfa1-lifnr,
         name1_2        LIKE lfa1-name1,
         vmenge2        LIKE zmmwms_pol-pdlv_menge,
         lifnr3         LIKE lfa1-lifnr,
         name1_3        LIKE lfa1-name1,
         vmenge3        LIKE zmmwms_pol-pdlv_menge,
         lifnr4         LIKE lfa1-lifnr,
         name1_4        LIKE lfa1-name1,
         vmenge4        LIKE zmmwms_pol-pdlv_menge,
         maktxl         TYPE char255,
         xh(3),
         sel,
         zjsyul         TYPE zmmaps_pl-remain_days , "计算的保留日期
         zzwdfr         TYPE sy-datum , "计算的最晚回复日期
         versiom        TYPE zmmaps_pl-version , "计算最大版本号
       END OF ty_aps_pl.

TYPES : BEGIN OF ty_zplta .
          INCLUDE TYPE zmmaps_pl .
          TYPES : matkl TYPE mara-matkl,
        END OF ty_zplta .

TYPES : BEGIN OF ty_zjfsdt ,
          matnr  TYPE mara-matnr,
          zjfsdt TYPE zmmaps_pl-remain_days,
        END OF ty_zjfsdt .

TYPES: BEGIN OF ty_po_data,
         ebeln          LIKE ekpo-ebeln,
         ebelp          LIKE ekpo-ebelp,
         uniqueid       LIKE ekpo-uniqueid,
         lifnr          LIKE ekko-lifnr,
         frgke          LIKE ekko-frgke,
         frgsx          LIKE ekko-frgsx,
         name1          LIKE lfa1-name1,
         key            TYPE ty_key,
         key2           TYPE ty_key2,
         werks          LIKE ekpo-werks,
         lgort          LIKE ekpo-lgort,
         matnr          LIKE ekpo-matnr,
         maktx          TYPE makt-maktx,
         ematn          LIKE ekpo-ematn,
         st_code        LIKE zmmaps_pl-st_code,
         mfrnr          LIKE mara-mfrnr,
         producer_name  LIKE lfa1-name1,
         po_date        LIKE ekko-bedat,
         menge          LIKE ekpo-menge,
         meins          LIKE ekpo-meins,
         kzwi1          LIKE ekpo-kzwi1,
         price          LIKE ekpo-kzwi1,
         waers          LIKE ekko-waers,
         base_menge     LIKE ekpo-menge,
         lmein          LIKE ekpo-lmein,
         arrive_menge   LIKE ekpo-menge,
         confirm_menge  LIKE ekpo-menge,
         sgfp_menge     LIKE ekpo-menge,
         assigned_menge LIKE ekpo-menge,
         match_menge    LIKE ekpo-menge,
         left_quan      LIKE ekpo-menge,
         sort,
         cell_styles    TYPE lvc_t_styl,
         sel,
       END OF ty_po_data.

TYPES: BEGIN OF ty_match_data,
         key           TYPE ty_key,
         werks         LIKE ekpo-werks,
         lgort         LIKE ekpo-lgort,
         ebeln         LIKE ekpo-ebeln,
         ebelp         LIKE ekpo-ebelp,
         uniqueid      LIKE ekpo-uniqueid,
         reqdate       LIKE zmmaps_pl-reqdate,
         guid          LIKE zmmaps_pl-guid,
         lifnr         LIKE ekko-lifnr,
         vendor_name   LIKE lfa1-name1,
         producer      LIKE ekko-lifnr,
         producer_name LIKE lfa1-name1,
         matnr         LIKE ekpo-matnr,
         maktx         LIKE makt-maktx,
         version       LIKE zmmaps_temp-version,
         pdlv_date     LIKE zmmaps_temp-pdlv_date,
         match_menge   LIKE ekpo-menge,
         meins         LIKE mara-meins,
         st_code       LIKE zmmaps_pl-st_code,
         pdlv_date_ext LIKE zmmaps_temp-pdlv_date_ext,
         released      LIKE zmmaps_temp-released,
         xh(3),
         cell_styles   TYPE lvc_t_styl,
         update_flag,
         light         TYPE icon_d, "指示灯
       END OF ty_match_data.

TYPES  : BEGIN OF ty_zmatdoc ,
           dlv_planno TYPE zmatdoc-dlv_planno,
           dlv_itemno TYPE zmatdoc-dlv_itemno,
           menge      TYPE zmatdoc-menge,
         END OF ty_zmatdoc .

TYPES: BEGIN OF ty_aps_tmp.
         INCLUDE TYPE zmmaps_temp.
         TYPES:   key TYPE ty_key,
       END OF ty_aps_tmp.

TYPES: BEGIN OF ty_pdlv_data.
         INCLUDE TYPE zmmwms_pol.
         TYPES:   key             TYPE ty_key,
         lifnr           LIKE zmmaps_poh-lifnr,
         name1_gp        LIKE zmmaps_poh-name1_gp,
         name1_p         LIKE lfa1-name1,
         ernam           LIKE zmmaps_poh-ernam,
         note            LIKE zmmaps_poh-note,
         reqdate         LIKE zmmaps_temp-reqdate,
         send_date       LIKE zmmaps_temp-send_date,
         assign_menge    LIKE zmmaps_temp-assign_menge,
         wjh_qty         LIKE zmmaps_temp-assign_menge,            "未交货数量
         released        LIKE zmmaps_temp-released,
         version         LIKE zmmaps_temp-version,  "版本号
         remain_days     LIKE zmmaps_pl-remain_days, "保留天数
         menge           LIKE ekpo-menge,                          "采购订单数量
         ematn           LIKE ekpo-ematn,
         maktx           TYPE makt-maktx,                          "物料描述
         frgsx           LIKE ekko-frgsx,                          "审批策略
         frgke           LIKE ekko-frgke,                          "批准标识：采购凭证
         pz,
         poreceived_quan TYPE zreceived_quan, "PO已收数量
         sel,
         light           TYPE icon_d, "##
         message(200),                        "##
         cell_styles     TYPE lvc_t_styl,
         update_flag,
         update_temp,
         zbeiz           TYPE zmmaps_temp-zbeiz  , "add by lanjc 20200426 增加备注
         zjsyul          TYPE zmmaps_pl-remain_days , "add by lanjc 20200426 计算的保留日期
         zzwdfr          TYPE sy-datum , "add by lanjc 20200426 计算的最晚回复日期
         versiom         TYPE zmmaps_pl-version , "add by lanjc 20200426 计算最大版本号
         zkshsl          TYPE ekpo-menge, "add by lanjc 20200529 增加可收货数量
         index           TYPE i,
       END OF ty_pdlv_data.

TYPES: BEGIN OF ty_struct,
         position  LIKE dd03l-position,
         tabname   LIKE dd03l-tabname,
         fieldname LIKE dd03l-fieldname,
         datatype  LIKE dd03l-datatype,
         ddleng    LIKE dd03l-leng,
         decimals  LIKE dd03l-decimals,
         ddtext    LIKE dd03t-ddtext,
         eddtext   LIKE dd03t-ddtext,
         dddtext   LIKE dd03t-ddtext,
       END OF ty_struct.

TYPES: BEGIN OF ty_excel_stru,
         guid(32),          "APS唯一标识
         ebeln(10),         "采购订单
         ebelp(5),          "采购订单行
         po_menge(20),      "采购数量
         bprme(4),          "采购订单单位
         po_dlvdate(10),    "订单交货日期
         matnr(18),         "物料编号
         maktx(40),         "物料描述
         vonder(10),        "供应商
         vonder_name(35),   "供应商名称
         assign_menge(20),  "匹配数量
         meins(4),          "单位
         plan_date(10),     "计划日期
         reqdate(10),       "日期
         st_code(30),       "指定标准
         version(8),        "版本
         werks(4),          "工厂
         werks_name(4),     "送货至工厂
         lgort(4),          "库位
         lgobe(16),         "送货至工厂
         plant_area(20),    "送货厂区
         ack_date(10),      "确认送货日期
         ack_menge(20),     "确认数量

       END OF ty_excel_stru.

TYPES: BEGIN OF ty_excel_alv.
         INCLUDE TYPE ty_excel_stru.
         TYPES: light        TYPE icon_d, "##
         icon         TYPE icon_d,     "##
         message(200),                        "##
         menge        LIKE ekpo-menge,
         eindt        LIKE eket-eindt,
         cell_styles  TYPE lvc_t_styl,
         ebel1        TYPE ekpo-ebeln,
         ebep1        TYPE ekpo-ebelp,
       END OF ty_excel_alv.

TYPES: BEGIN OF ty_pol_temp,
         dlv_planno LIKE zmmwms_pol-dlv_planno,
         dlv_itemno LIKE zmmwms_pol-dlv_itemno,
         guid       LIKE zmmwms_pol-guid,
         uniqueid   LIKE zmmwms_pol-uniqueid,
       END OF ty_pol_temp.

TYPES : BEGIN OF ty_ekbe1 ,
          ebeln TYPE ekbe-ebeln,
          ebelp TYPE ekbe-ebelp,
*          zekkn TYPE ekbe-zekkn,
*          vgabe TYPE ekbe-vgabe,
*          gjahr TYPE ekbe-gjahr,
*          belnr TYPE ekbe-belnr,
*          buzei TYPE ekbe-buzei,
*          bwart TYPE ekbe-bwart,
          menge TYPE ekbe-menge,
*          shkzg TYPE ekbe-shkzg,
        END OF ty_ekbe1 .

TYPES : BEGIN OF ty_ekpo ,
          ebeln TYPE ekpo-ebeln,
          ebelp TYPE ekpo-ebelp,
          menge TYPE ekpo-menge,
          uebto TYPE ekpo-uebto,
        END OF ty_ekpo .

*&---------------------------------------------------------------------*
*  Intertables
*&---------------------------------------------------------------------*
DATA : gs_ekbe1 TYPE ty_ekbe1 .
DATA : gt_ekbe1 TYPE TABLE OF ty_ekbe1 .

DATA : gs_zmmwms_pol TYPE zmmwms_pol .
DATA : gt_zmmwms_pol TYPE TABLE OF zmmwms_pol .

DATA : gs_zmmaps_temp TYPE zmmaps_temp .
DATA : gt_zmmaps_temp TYPE TABLE OF zmmaps_temp .

DATA : gs_ekpo TYPE ty_ekpo .
DATA : gt_ekpo TYPE TABLE OF ty_ekpo .

DATA : gs_zmatdoc1 TYPE zmatdoc .
DATA : gt_zmatdoc1 TYPE TABLE OF zmatdoc .

DATA: gt_aps_pl      TYPE TABLE OF ty_aps_pl,
      gs_aps_pl      TYPE ty_aps_pl,
      gt_po_show     TYPE TABLE OF ty_po_data,
      gt_po_data     TYPE TABLE OF ty_po_data,
      gs_po_data     TYPE ty_po_data,
      gt_match_show  TYPE TABLE OF ty_match_data,
      gt_match_data  TYPE TABLE OF ty_match_data,
      gs_match_data  TYPE ty_match_data,
      gt_mmaps_del   TYPE TABLE OF ty_aps_tmp,
      gt_mmaps_temp  TYPE TABLE OF ty_aps_tmp,
      gs_mmaps_temp  TYPE ty_aps_tmp,
      gt_temp_export TYPE TABLE OF zmmaps_temp,
      gs_temp_export TYPE zmmaps_temp,
      gt_pdlv_data   TYPE TABLE OF ty_pdlv_data,
      gt_pdlv_data_m TYPE TABLE OF ty_pdlv_data, "add by lanjingcun 20200420期初pol
      gs_pdlv_data   TYPE ty_pdlv_data.

DATA: gt_mmaps_temp_po TYPE TABLE OF ty_aps_tmp,
      gs_mmaps_temp_po TYPE ty_aps_tmp,
      gt_pdlv_data_po  TYPE TABLE OF ty_pdlv_data,
      gs_pdlv_data_po  TYPE ty_pdlv_data.

DATA: gt_makt  TYPE TABLE OF makt,
      gs_makt  TYPE makt,
      gt_marc  TYPE TABLE OF marc,
      gs_marc  TYPE marc,
      gt_t006a TYPE TABLE OF t006a,
      gs_t006a TYPE t006a,
      gt_lfa1  TYPE TABLE OF lfa1,
      gs_lfa1  TYPE lfa1,
      gt_ekbe  TYPE TABLE OF ekbe,
      gs_ekbe  TYPE ekbe,
      gt_dd07v TYPE TABLE OF dd07v,
      gs_dd07v TYPE dd07v.

DATA : gt_zmatdoc TYPE TABLE OF ty_zmatdoc .

DATA: gt_menu TYPE slis_t_extab,
      gs_menu TYPE slis_extab.

DATA: gt_outtab TYPE TABLE OF ty_struct,
      gs_outtab TYPE ty_struct.

*EXCEL上传功能全局变量
DATA: gt_excel TYPE TABLE OF alsmex_tabline, "excel####
      gs_excel TYPE alsmex_tabline,
      gt_stru  TYPE TABLE OF ty_excel_stru,
      gs_stru  TYPE ty_excel_stru,
      gt_alv   TYPE TABLE OF ty_excel_alv,
      gs_alv   TYPE ty_excel_alv.

RANGES: gr_ekgrp FOR marc-ekgrp.

DATA : gs_zplta TYPE ty_zplta .
DATA : gs_zjfsdt TYPE ty_zjfsdt .

DATA : gt_zplta TYPE TABLE OF ty_zplta .
DATA : gt_zjfsdt TYPE TABLE OF ty_zjfsdt .

*&---------------------------------------------------------------------*
*  全局变量
*&---------------------------------------------------------------------*
DATA: gv_ok_code       LIKE sy-ucomm,
      ok_code          LIKE sy-ucomm,
      gv_change        TYPE char1,
      gv_need_save     TYPE char1,
      gv_screen        LIKE sy-dynnr,
      gv_split_height  TYPE i VALUE 90,
      gv_detail_modify.

*&---------------------------------------------------------------------*
*  ALV变量
*&---------------------------------------------------------------------*
DATA: gs_variat          TYPE disvariant,
      gs_layout_head     TYPE lvc_s_layo,
      gt_fieldcat_head   TYPE lvc_t_fcat,
      gs_layout_po       TYPE lvc_s_layo,
      gt_fieldcat_po     TYPE lvc_t_fcat,
      gs_layout_detail   TYPE lvc_s_layo,
      gt_fieldcat_detail TYPE lvc_t_fcat.

DATA: gcl_alv_grid_head    TYPE REF TO cl_gui_alv_grid,
      gcl_container_head   TYPE REF TO cl_gui_custom_container,
      gcl_container_head2  TYPE REF TO cl_gui_container,
      gcl_alv_grid_po      TYPE REF TO cl_gui_alv_grid,
      gcl_container_po     TYPE REF TO cl_gui_custom_container,
      gcl_container_po2    TYPE REF TO cl_gui_container,
      gcl_alv_grid_detail  TYPE REF TO cl_gui_alv_grid,
      gcl_container_detail TYPE REF TO cl_gui_custom_container.

" cl_gui_custom_container container for hosting splitter
DATA: gcl_container_split TYPE REF TO cl_gui_custom_container,
      gcl_splitter        TYPE REF TO cl_gui_splitter_container.

DATA: gt_fieldcat TYPE lvc_t_fcat,
      gs_layout   TYPE lvc_s_layo,
      gt_events   TYPE slis_t_event,
      gv_status   TYPE slis_formname VALUE 'FRM_STANDARD_ALV',
      gv_comand   TYPE slis_formname VALUE 'FRM_USER_COMMAND'.

DATA : gt_filt TYPE lvc_t_filt .
DATA : gs_filt TYPE LINE OF lvc_t_filt .
*
DATA: gt_status_tab LIKE TABLE OF rsmpe-func,
      gs_status_tab LIKE rsmpe-func.

*&---------------------------------------------------------------------*
*  EXCEL变量
*&---------------------------------------------------------------------*
DATA :gole_excel        TYPE ole2_object,
      gole_sheet        TYPE ole2_object,
      gole_book         TYPE ole2_object,
      gole_cell         TYPE ole2_object,
      gole_sheet_number TYPE i.

*&---------------------------------------------------------------------*
*  Constant
*&---------------------------------------------------------------------*
CONSTANTS: cns_x VALUE 'X'.

*&---------------------------------------------------------------------*
*  Macro
*&---------------------------------------------------------------------*
DEFINE append_range.
  IF &3 = ''.
    &1-sign = 'I'.
    &1-option = 'EQ'.
    &1-low = &2.
    APPEND &1.
    CLEAR  &1.
  ELSEIF &2 <> '' AND &3 <> ''.
    &1-sign = 'I'.
    &1-option = 'BT'.
    &1-low = &2.
    &1-high = &3.
    APPEND &1.
    CLEAR  &1.
  ENDIF.
END-OF-DEFINITION.

DATA: gs_toolbar TYPE stb_button.

DEFINE set_button.
  CLEAR gs_toolbar.
  MOVE &2 TO gs_toolbar-function.      "功能码
* --> This function code is evaluated in 'handle_menu_button'
  MOVE &3 TO gs_toolbar-text.             "显示文本
  MOVE &3 TO gs_toolbar-quickinfo.   "提示文本
  MOVE &4 TO gs_toolbar-butn_type.               "按钮类型--0：普通按钮
  MOVE space TO gs_toolbar-disabled.
  APPEND gs_toolbar TO &1.
END-OF-DEFINITION.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB1'
CONSTANTS: BEGIN OF c_tab1,
             tab1 LIKE sy-ucomm VALUE 'TAB1_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB1_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB1_FC3',
           END OF c_tab1.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB1'
CONTROLS:  tab1 TYPE TABSTRIP.
DATA: BEGIN OF g_tab1,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZMMR0003',
        pressed_tab LIKE sy-ucomm VALUE c_tab1-tab1,
      END OF g_tab1.

DATA:BEGIN OF gs_planno_item,
       dlv_planno TYPE  zmatdoc-dlv_planno,
       dlv_itemno TYPE zmatdoc-dlv_itemno,
       pdlv_menge TYPE zpdlv_menge,
     END OF      gs_planno_item.

DATA:BEGIN OF gs_planno_menge,
       dlv_planno TYPE  zmatdoc-dlv_planno,
       dlv_itemno TYPE zmatdoc-dlv_itemno,
       menge_s    TYPE zmatdoc-menge,
       menge_h    TYPE zmatdoc-menge,
     END OF      gs_planno_menge.

DATA:BEGIN OF gs_ebe,
       ebeln TYPE ebeln,
       ebelp TYPE ebelp,
     END OF gs_ebe.

DATA:BEGIN OF gs_ebe_menge,
       ebeln    TYPE ebeln,
       ebelp    TYPE ebelp,
       menge101 TYPE matdoc-menge,
       menge102 TYPE matdoc-menge,
       menge122 TYPE matdoc-menge,
       menge123 TYPE matdoc-menge,
     END OF gs_ebe_menge.

CONSTANTS:c_line_length TYPE i VALUE 4000.
DATA : g_pr_con   TYPE REF TO cl_gui_custom_container .
DATA : g_pr_ltxt  TYPE REF TO cl_gui_textedit .
DATA : g_pv_cnam  TYPE c .
DATA : g_pv_edit  TYPE c VALUE 'X'. "‘X’-可编辑，‘’不可编辑
DATA : g_pv_value TYPE c.