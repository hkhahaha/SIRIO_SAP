*======================================================================*
*  Program ID             :  ZSDR0121
*  Program Description    :  销售订单同步CRM系统
*  Functional Consultant  :  linxiaoli
*  T-code                 :
*  Created By             :  LIUWUKUN
*  Start Date             :  18.08.2022 09:41:37
*  End Date               :
*======================================================================*
*                     Modify Log History.
*----------------------------------------------------------------------*
*
*No. Modified by      Date          Description
*--- -----------      ----          -----------
*1.    LIUWUKUN     18.08.2022 09:41:37      Create the first version
*
*======================================================================*

REPORT zsdr0121.

TABLES:vbak,vbap,vbpa,zsdt0121.

TYPES: BEGIN OF ty_data,
         dest_system TYPE zsdt0121-dest_system,        "目标系统
         vbeln       TYPE zsdt0121-vbeln,              "销售凭证

         sel         TYPE char1,                       "选择
         zstate      TYPE icon_d,                      "分发状态
         zcrmzt      TYPE zsdt0121-zcrmzt,             "分发CRM状态
         message     TYPE zsdt0121-message,            "返回消息
         vkbur       TYPE zsdt0121-vkbur,              "销售办公室
         tag         TYPE zsdt0121-tag,                "新增/修改标识
         kunnr_sp    TYPE zsdt0121-kunnr_sp,           "售达方
         kunnr_sh    TYPE zsdt0121-kunnr_sh,           "送达方
         kunnr_z1    TYPE zsdt0121-kunnr_z1,           "实际客户
         netwr       TYPE zsdt0121-netwr,              "净值
         waerk       TYPE zsdt0121-waerk,              "货币
         kunnr_zz3   TYPE zsdt0121-kunnr_z3,           "业务员
         sortl_z3    TYPE zsdt0121-sortl_z3,           "业务员工号
         del_flag    TYPE zsdt0121-del_flag,           "归档标记
         del_date    TYPE zsdt0121-del_date,           "归档日期
         kunnr_z3    TYPE zsdt0121-kunnr_z3,           "业务员
         erdat       TYPE zsdt0121-erdat,              "创建时间
         zblxy       TYPE zsdt0121-zblxy,              "是否有备料协议
         zsfypo      TYPE zsdt0121-zsfypo,             "是否有PO
         zterm       TYPE zsdt0121-zterm,              "付款条款
         zhtqk       TYPE zsdt0121-zhtqk,              "合同情况
         zsfxkh      TYPE zsdt0121-zsfxkh,             "是否新客户
         zckgj       TYPE zsdt0121-zckgj,              "出口国家
         zbldlx      TYPE zsdt0121-zbldlx,             "备料单类型
         bstkd_e     TYPE zsdt0121-bstkd_e,            "采购订单编号
         bstdk_e     TYPE zsdt0121-bstdk_e,            "采购订单日期
         posnr       TYPE zsdt0121-posnr,              "订单行项目编号
         matnr       TYPE zsdt0121-matnr,              "物料
         arktx       TYPE zsdt0121-arktx,              "项目描述
         kwmeng      TYPE zsdt0121-kwmeng,             "销售单位数量
         state       TYPE zsdt0121-state,              "订单行状态
         vrkme       TYPE zsdt0121-vrkme,              "销售单位
         werks       TYPE zsdt0121-werks,              "工厂
         zbzgg       TYPE zsdt0121-zbzgg,              "包装规格
         zypsl       TYPE zsdt0121-zypsl,              "样品数量
         zsfxcp      TYPE zsdt0121-zsfxcp,             "是否新产品
         zsfbj       TYPE zsdt0121-zsfbj,              "是否报检
         zsfxbz      TYPE zsdt0121-zsfxbz,             "是否新包装
         zatpdfrq    TYPE zsdt0121-zatpdfrq,           "ATP答复日期
         zysjhq      TYPE zsdt0121-zysjhq,             "原始交货期
         znbjhq      TYPE zsdt0121-znbjhq,             "内部交货期
         inco1       TYPE zsdt0121-inco1,              "国际贸易条款
         cmpre       TYPE zsdt0121-cmpre,              "含税单价
         netpr_i     TYPE zsdt0121-netpr_i,            "净价
         waerk_i     TYPE zsdt0121-waerk_i,            "货币
         netwr_i     TYPE zsdt0121-netwr_i,            "净值
         vkorg       TYPE vbak-vkorg,                  "销售组织
         vkgrp       TYPE vbak-vkgrp,                  "销售组织
         vtweg       TYPE vbak-vtweg,                  "分销渠道
         spart       TYPE vbak-spart,                  "产品组
         auart       TYPE zsdt0121-auart,              "订单类型
         timestap    TYPE zsdt0121-timestap,           "时间戳
         zsyncuser   TYPE zsdt0121-zsyncuser,          "同步账号
         zsyncdate   TYPE zsdt0121-zsyncdate,          "同步日期
         zsynctime   TYPE zsdt0121-zsynctime.          "同步时间
TYPES: END OF ty_data.

TYPES: BEGIN OF ty_so,
         vbeln TYPE vbak-vbeln,
         udate TYPE cdhdr-udate,
       END OF ty_so.
DATA: gt_so TYPE TABLE OF ty_so.

DATA: gt_data TYPE TABLE OF ty_data,
      gs_data TYPE ty_data,
      gt_fcat TYPE lvc_t_fcat,
      gs_fcat TYPE lvc_s_fcat,
      gs_layo TYPE lvc_s_layo.

SELECTION-SCREEN BEGIN OF BLOCK bk0 WITH FRAME TITLE TEXT-000.

PARAMETERS: p_rd1 RADIOBUTTON GROUP rd DEFAULT 'X' USER-COMMAND rd,
            p_rd2 RADIOBUTTON GROUP rd,
            p_rd3 RADIOBUTTON GROUP rd.

SELECTION-SCREEN END OF BLOCK bk0.

SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: s_aedat FOR vbak-aedat.             "订单日期
SELECT-OPTIONS: s_zzgdr FOR vbak-aedat.             "归档日期
SELECT-OPTIONS: s_vbeln FOR vbak-vbeln.             "订单号
SELECT-OPTIONS: s_kunnr FOR vbpa-kunnr.             "实际客户
SELECT-OPTIONS: s_zzywy FOR vbpa-kunnr.             "业务员编号
SELECT-OPTIONS: s_vkbur FOR vbak-vkbur.             "销售办公室
SELECT-OPTIONS: s_sendd FOR zsdt0121-zsyncdate.     "下发日期
SELECT-OPTIONS: s_erdat FOR zsdt0121-erdat.         "创建日期
SELECT-OPTIONS: s_zdest FOR zsdt0121-dest_system.   "分发CRM系统

SELECTION-SCREEN END OF BLOCK bk1.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    CASE abap_true.
      WHEN p_rd1.
        IF screen-name CS 'S_ZZGDR' OR screen-name CS 'S_SENDD'
          OR screen-name CS 'S_ERDAT' OR screen-name CS 'S_ZDEST'.
          screen-active = 0.
        ENDIF.
      WHEN p_rd2.
        IF screen-name CS 'S_AEDAT' OR screen-name CS 'S_KUNNR'
          OR screen-name CS 'S_ZZYWY' OR screen-name CS 'S_VKBUR'
          OR screen-name CS 'S_SENDD' OR screen-name CS 'S_ERDAT'
          OR screen-name CS 'S_ZDEST'.
          screen-active = 0.
        ENDIF.
      WHEN p_rd3.
        IF screen-name CS 'S_AEDAT' .
          screen-active = 0.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

START-OF-SELECTION.

  PERFORM frm_get_data.

  IF gt_data IS INITIAL.
    MESSAGE '查询无数据'(002) TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF sy-batch IS NOT INITIAL.
    PERFORM frm_send_data.
  ELSE.
    PERFORM frm_show_data.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .

  REFRESH gt_data.

  CASE abap_true.
    WHEN p_rd1.
      PERFORM frm_rd1_data.
    WHEN p_rd2.
      PERFORM frm_get_del_data.
    WHEN p_rd3.
      PERFORM frm_get_log.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_RD1_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_rd1_data .

  "获取订单号
  REFRESH: gt_so[].

  PERFORM frm_get_change_so TABLES gt_so[]. "获取更改的so-vbak -vbap

  PERFORM frm_cdhdr_data TABLES gt_so[].   "cdhdr 记录更改的单据

  "排序去重
  SORT gt_so BY vbeln.
  DELETE ADJACENT DUPLICATES FROM gt_so COMPARING vbeln.

  PERFORM get_data.   "获取订单数据

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_CHANGE_SO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SO[]
*&---------------------------------------------------------------------*
FORM frm_get_change_so  TABLES pt_so.

  SELECT vbak~vbeln
    INTO TABLE pt_so
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
   WHERE ( vbak~aedat IN s_aedat
      OR vbak~erdat IN s_aedat
      OR vbap~aedat IN s_aedat )
     AND vbak~vbeln IN s_vbeln.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CDHDR_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SO[]
*&---------------------------------------------------------------------*
FORM frm_cdhdr_data  TABLES   pt_so LIKE gt_so.

  SELECT cdhdr~*,cdpos~*
    INTO TABLE @DATA(lt_change)
    FROM cdhdr
    INNER JOIN cdpos ON cdhdr~objectclas = cdpos~objectclas
                    AND cdhdr~changenr   = cdpos~changenr
   WHERE cdhdr~objectclas = 'VERKBELEG'
     AND cdhdr~udate      IN @s_aedat
     AND cdhdr~objectid   IN @s_vbeln.

  LOOP AT lt_change INTO DATA(ls_change).
    IF ls_change-cdpos-tabname = 'VBAK' AND ( ls_change-cdpos-chngind  = 'D' OR ls_change-cdhdr-change_ind  = 'D' ).
      DELETE lt_change WHERE cdhdr-objectid = ls_change-cdpos-objectid.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_change INTO ls_change.
    pt_so-vbeln = ls_change-cdhdr-objectid.
    APPEND pt_so.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  DATA it_so TYPE STANDARD TABLE OF zsds127.
  DATA ot_so TYPE STANDARD TABLE OF zsds127a.

  REFRESH: gt_data.

  SELECT vbak~vbeln,
         vbak~kunnr AS kunnr_sp,
         vbak~netwr,
         vbak~waerk,
         vbak~erdat,
         vbak~zblxy,
         vbak~zsfypo,
         vbkd~zterm,
         vbak~zhtqk,
         vbak~zsfxkh,
         vbak~zckgj,
         vbak~zbldlx,
         vbak~auart,
         vbkd~bstkd_e,
         vbkd~bstdk_e,
         vbap~posnr,
         vbap~matnr,
         vbap~arktx,
         we~kunnr AS kunnr_sh,
         z1~kunnr AS kunnr_z1,
         z1~kunnr AS kunnr_zz3,
         vbap~kwmeng,
         vbap~vrkme,
         vbap~werks,
         vbap~zbzgg,
         vbap~zypsl,
         vbap~zsfxcp,
         vbap~zsfbj,
         vbap~zsfxbz,
         vbap~zatpdfrq,
         vbap~zysjhq,
         vbap~znbjhq,
         vbkd~inco1,
         vbak~vkorg,
         vbak~vkgrp,
         vbak~vtweg,
         vbak~spart,
         vbap~cmpre,
         vbap~netpr AS netpr_i,
         vbap~waerk AS waerk_i,
         vbap~netwr AS netwr_i
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
    INNER JOIN @gt_so AS a ON a~vbeln = vbak~vbeln
    LEFT  JOIN vbkd ON vbkd~vbeln = vbak~vbeln
                   AND vbkd~posnr = ''
    LEFT  JOIN vbpa AS z1 ON z1~vbeln = vbak~vbeln
                         AND z1~posnr = ''
                         AND z1~parvw = 'Z1'
    LEFT  JOIN vbpa AS we ON we~vbeln = vbak~vbeln
                         AND we~posnr = ''
                         AND we~parvw = 'WE'
*    LEFT  JOIN vbpa AS z3 ON z3~vbeln = vbak~vbeln
*                         AND z3~posnr = ''
*                         AND z3~parvw = 'Z1'
    WHERE z1~kunnr IN @s_kunnr
      AND vbap~kwmeng <> 0
      AND EXISTS ( SELECT objnr
                         FROM jest
                        WHERE objnr = vbak~objnr
                          AND ( ( stat = 'E0003' ) ) )
      AND vbak~auart LIKE 'Z%'
      AND vbak~auart <> 'ZA10'
      AND vbak~auart <> 'ZA12'
      AND vbap~kwmeng <> 0
     INTO CORRESPONDING FIELDS OF TABLE @gt_data
    .

  SORT gt_data BY vbeln posnr.
  DELETE ADJACENT DUPLICATES FROM gt_data COMPARING vbeln posnr.

  IF gt_data IS NOT INITIAL.

    MOVE-CORRESPONDING gt_data TO it_so.

    CALL FUNCTION 'ZSD_CHECK_SO_STATUS'
      TABLES
        it_so = it_so
        ot_so = ot_so.

    DELETE ot_so WHERE zstatus = '' OR zstatus = 'SO1'.
    SORT ot_so BY vbeln posnr.

    LOOP AT ot_so INTO DATA(ls_so).
      DELETE gt_data WHERE vbeln = ls_so-vbeln.
    ENDLOOP.

    SELECT * INTO TABLE @DATA(lt_zsdt_vkbur) FROM zsdt_vkbur ORDER BY vkbur.

    SELECT DISTINCT knvp~*
      FROM knvp
      INNER JOIN @gt_data AS a ON a~vkorg     = knvp~vkorg
                              AND a~vtweg     = knvp~vtweg
                              AND a~spart     = knvp~spart
                              AND a~kunnr_zz3 = knvp~kunnr
     WHERE knvp~parvw = 'Z3'
      INTO TABLE @DATA(lt_knvp).
    SORT lt_knvp BY kunnr vkorg vtweg spart.

    SELECT DISTINCT knvv~*
      FROM knvv
      INNER JOIN @gt_data AS a ON a~vkorg    = knvv~vkorg
                              AND a~vtweg    = knvv~vtweg
                              AND a~spart    = knvv~spart
                              AND a~kunnr_z1 = knvv~kunnr
      INTO TABLE @DATA(lt_knvv).

    SORT lt_knvv BY kunnr vkorg vtweg spart.

    "判断是否下发过
    SELECT zsdt0121~*
      FROM zsdt0121
      INNER JOIN @gt_data AS a ON a~vbeln = zsdt0121~vbeln
     WHERE zsdt0121~zcrmzt = 'S'
      INTO TABLE @DATA(lt_zsdt0121).
    SORT lt_zsdt0121 BY vbeln dest_system.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      "业务员
      READ TABLE lt_knvp INTO DATA(ls_knvp)
                               WITH KEY  kunnr = <fs_data>-kunnr_zz3
                                         vkorg = <fs_data>-vkorg
                                         vtweg = <fs_data>-vtweg
                                         spart = <fs_data>-spart BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_data>-kunnr_z3  = ls_knvp-kunn2.
        SELECT SINGLE sortl INTO <fs_data>-sortl_z3 FROM kna1 WHERE kunnr = <fs_data>-kunnr_z3.
      ENDIF.


      READ TABLE lt_knvv INTO DATA(ls_knvv)
                               WITH KEY  kunnr = <fs_data>-kunnr_z1
                                         vkorg = <fs_data>-vkorg
                                         vtweg = <fs_data>-vtweg
                                         spart = <fs_data>-spart BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_data>-vkbur = ls_knvv-vkbur.
      ENDIF.

      READ TABLE lt_zsdt_vkbur INTO DATA(ls_zsdt_vkbur) WITH KEY vkbur = <fs_data>-vkbur BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_data>-dest_system = ls_zsdt_vkbur-crmsys.
      ENDIF.

      IF <fs_data>-dest_system IS NOT INITIAL.
        <fs_data>-sel = sy-batch.
      ENDIF.

      "新增-0 /修改-1 标识
      "判断是否下发过crm
      "判断Dest_System是否在ZSDT0121表中且TYPE=S,是则赋值1否则赋值0
      READ TABLE lt_zsdt0121 TRANSPORTING NO FIELDS
                                        WITH KEY vbeln = <fs_data>-vbeln
                                                 dest_system = <fs_data>-dest_system BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_data>-tag = 1.
      ELSE.
        <fs_data>-tag = 0.
      ENDIF.

      ""判断KWMENG=0.01.若为0.01赋值STATE=100000001，否则STATE=100000000"
      IF <fs_data>-kwmeng = '0.01'.
        <fs_data>-state = '100000001'.
      ELSE.
        <fs_data>-state = '100000000'.
      ENDIF.

      IF <fs_data>-auart+0(2) = 'ZR'.  "退货   * -1
        <fs_data>-kwmeng = 0 - <fs_data>-kwmeng.
        <fs_data>-netpr_i = 0 - <fs_data>-netpr_i.
        <fs_data>-netwr_i = 0 - <fs_data>-netwr_i.
      ENDIF.

    ENDLOOP.

    SORT gt_data BY vbeln posnr.

    DELETE gt_data WHERE vkbur NOT IN s_vkbur.
    DELETE gt_data WHERE kunnr_z3 NOT IN s_zzywy.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_data .

  PERFORM frm_add_fieldcatlog.

  CLEAR gs_layo.

  gs_layo-cwidth_opt = 'X'.
  gs_layo-zebra = 'X'.
  gs_layo-sel_mode = 'A'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout_lvc            = gs_layo
      it_fieldcat_lvc          = gt_fcat
      i_callback_pf_status_set = 'FRM_PF_STATUS_SET'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PF_STATUS_SET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_pf_status_set USING lt_extab TYPE slis_t_extab.

  REFRESH: lt_extab.

  IF p_rd3 IS NOT INITIAL.
    APPEND 'SEND' TO lt_extab.
  ENDIF.

  SET PF-STATUS '0100' EXCLUDING lt_extab.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_user_command USING r_ucomm ls_selfield TYPE slis_selfield.

  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lt_filt TYPE         lvc_t_fidx,
        ls_filt TYPE LINE OF lvc_t_fidx,
        BEGIN OF ls_index,
          index TYPE sy-tabix,
        END OF ls_index,
        lt_index LIKE STANDARD TABLE OF ls_index.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  CALL METHOD lr_grid->check_changed_data.

  CASE r_ucomm.
    WHEN 'SEND'.   "发送数据

      PERFORM frm_send_data.

    WHEN  '&ZALL'.   "全选按钮逻辑

      "全选排除过滤数据
      CALL METHOD lr_grid->get_filtered_entries
        IMPORTING
          et_filtered_entries = lt_filt.

      LOOP AT lt_filt INTO ls_filt.
        CLEAR ls_index.
        ls_index-index = ls_filt.
        APPEND ls_index TO lt_index.
      ENDLOOP.

      LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_header>).

        READ TABLE lt_index TRANSPORTING NO FIELDS
                                         WITH KEY index = sy-tabix.
        IF NOT sy-subrc IS INITIAL AND
          <fs_header>-sel = ''.
          <fs_header>-sel = 'X'.
        ELSEIF sy-subrc IS INITIAL.
          <fs_header>-sel = ''.
        ENDIF.

      ENDLOOP.

    WHEN  '&ZSAL'.   "取消全选逻辑

      gs_data-sel = ''.

      MODIFY gt_data FROM gs_data TRANSPORTING sel WHERE sel = 'X'.

    WHEN OTHERS.
  ENDCASE.

  lr_grid->get_frontend_layout(
    IMPORTING
      es_layout = gs_layo
  ).

  gs_layo-cwidth_opt = 'X'.

  lr_grid->set_frontend_layout( is_layout = gs_layo ).

  ls_selfield-refresh = 'X'.
  ls_selfield-row_stable = 'X'.
  ls_selfield-col_stable = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_ADD_FIELDCATLOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_add_fieldcatlog .

  DEFINE add_field.

    CLEAR gs_fcat.
    gs_fcat-fieldname  = &1.
    gs_fcat-scrtext_l  = &2.
    gs_fcat-scrtext_m  = &2.
    gs_fcat-scrtext_s  = &2.
    gs_fcat-seltext    = &2.
    gs_fcat-tooltip    = &2.
    gs_fcat-coltext    = &2.
    gs_fcat-reptext    = &2.
    gs_fcat-edit       = &3.
    gs_fcat-checkbox   = &4.
    gs_fcat-just       = &5.
    gs_fcat-ref_table  = &6.
    gs_fcat-ref_field  = &7.
    gs_fcat-convexit   = &8.

    APPEND gs_fcat TO gt_fcat.

  END-OF-DEFINITION.

  REFRESH: gt_fcat.

  IF p_rd3 IS NOT INITIAL.
    add_field:
     'TIMESTAP'     TEXT-t50    ''    ''   ''   ''           ''                ''.
  ENDIF.

  IF p_rd2 IS NOT INITIAL.
    add_field:
        'SEL'          TEXT-t00    'X'   'X'   ''   ''           ''              '',
        'ZSTATE'       TEXT-t01    ''   ''   'C'  ''           ''                '',
        'ZCRMZT'       TEXT-t02    ''   ''   ''   'ZSDT0121'   'ZCRMZT'          '',
        'MESSAGE'      TEXT-t03    ''   ''   ''   'ZSDT0121'   'MESSAGE'         '',
        'DEST_SYSTEM'  TEXT-t04    ''   ''   ''   'ZSDT0121'   'DEST_SYSTEM'     '',
        'VKBUR'        TEXT-t05    ''   ''   ''   'ZSDT0121'   'VKBUR'           '',
        'VBELN'        TEXT-t06    ''   ''   ''   'ZSDT0121'   'VBELN'           '',
        'KUNNR_Z1'     TEXT-t10    ''   ''   ''   'ZSDT0121'   'KUNNR_Z1'        '',
        'KUNNR_Z3'     TEXT-t13    ''   ''   ''   'ZSDT0121'   'KUNNR_Z3'        '',
        'DEL_FLAG'     TEXT-t51    ''   ''   ''   'ZSDT0121'   'DEL_FLAG'        '',
        'DEL_DATE'     TEXT-t52    ''   ''   ''   'ZSDT0121'   'DEL_FLAG'        '',
        'ZSYNCUSER'    TEXT-t44    ''   ''   ''   'ZSDT0121'   'ZSYNCUSER'       'USER',
        'ZSYNCDATE'    TEXT-t45    ''   ''   ''   'ZSDT0121'   'ZSYNCDATE'       '',
        'ZSYNCTIME'    TEXT-t46    ''   ''   ''   'ZSDT0121'   'ZSYNCTIME'       ''.
  ELSE.
    add_field:
        'SEL'          TEXT-t00    'X'   'X'   ''   ''           ''              '',
        'ZSTATE'       TEXT-t01    ''   ''   'C'  ''           ''                '',
        'ZCRMZT'       TEXT-t02    ''   ''   ''   'ZSDT0121'   'ZCRMZT'          '',
        'MESSAGE'      TEXT-t03    ''   ''   ''   'ZSDT0121'   'MESSAGE'         '',
        'DEST_SYSTEM'  TEXT-t04    ''   ''   ''   'ZSDT0121'   'DEST_SYSTEM'     '',
        'VKBUR'        TEXT-t05    ''   ''   ''   'ZSDT0121'   'VKBUR'           '',
        'VBELN'        TEXT-t06    ''   ''   ''   'ZSDT0121'   'VBELN'           '',
        'AUART'        TEXT-t54    ''   ''   ''   'ZSDT0121'   'AUART'           '',
        'TAG'          TEXT-t07    ''   ''   ''   'ZSDT0121'   'TAG'             '',
        'DEL_FLAG'     TEXT-t51    ''   ''   ''   'ZSDT0121'   'DEL_FLAG'        '',
        'DEL_DATE'     TEXT-t52    ''   ''   ''   'ZSDT0121'   'DEL_FLAG'        '',
        'VKORG'        TEXT-t47    ''   ''   ''   'ZSDT0121'   'VKORG'           '',
        'VTWEG'        TEXT-t48    ''   ''   ''   'ZSDT0121'   'VTWEG'           '',
        'SPART'        TEXT-t49    ''   ''   ''   'ZSDT0121'   'SPART'           '',
        'KUNNR_SP'     TEXT-t08    ''   ''   ''   'ZSDT0121'   'KUNNR_SP'        '',
        'KUNNR_SH'     TEXT-t09    ''   ''   ''   'ZSDT0121'   'KUNNR_SH'        '',
        'KUNNR_Z1'     TEXT-t10    ''   ''   ''   'ZSDT0121'   'KUNNR_Z1'        '',
        'NETWR'        TEXT-t11    ''   ''   ''   'ZSDT0121'   'NETWR'           '',
        'WAERK'        TEXT-t12    ''   ''   ''   'ZSDT0121'   'WAERK'           '',
        'KUNNR_Z3'     TEXT-t13    ''   ''   ''   'ZSDT0121'   'KUNNR_Z3'        '',
        'SORTL_Z3'     TEXT-t53    ''   ''   ''   'ZSDT0121'   'SORTL_Z3'        '',
        'ERDAT'        TEXT-t14    ''   ''   ''   'ZSDT0121'   'ERDAT'           '',
        'ZBLXY'        TEXT-t15    ''   ''   ''   'ZSDT0121'   'ZBLXY'           '',
        'ZSFYPO'       TEXT-t16    ''   ''   ''   'ZSDT0121'   'ZSFYPO'          '',
        'ZTERM'        TEXT-t17    ''   ''   ''   'ZSDT0121'   'ZTERM'           '',
        'ZHTQK'        TEXT-t18    ''   ''   ''   'ZSDT0121'   'ZHTQK'           '',
        'ZSFXKH'       TEXT-t19    ''   ''   ''   'ZSDT0121'   'ZSFXKH'          '',
        'ZCKGJ'        TEXT-t20    ''   ''   ''   'ZSDT0121'   'ZCKGJ'           '',
        'ZBLDLX'       TEXT-t21    ''   ''   ''   'ZSDT0121'   'ZBLDLX'          '',
        'BSTKD_E'      TEXT-t22    ''   ''   ''   'ZSDT0121'   'BSTKD_E'         '',
        'BSTDK_E'      TEXT-t23    ''   ''   ''   'ZSDT0121'   'BSTDK_E'         '',
        'POSNR'        TEXT-t24    ''   ''   ''   'ZSDT0121'   'POSNR'           '',
        'MATNR'        TEXT-t25    ''   ''   ''   'ZSDT0121'   'MATNR'           '',
        'ARKTX'        TEXT-t26    ''   ''   ''   'ZSDT0121'   'ARKTX'           '',
        'KWMENG'       TEXT-t27    ''   ''   ''   'ZSDT0121'   'KWMENG'          '',
        'STATE'        TEXT-t28    ''   ''   ''   'ZSDT0121'   'STATE'           '',
        'VRKME'        TEXT-t29    ''   ''   ''   'ZSDT0121'   'VRKME'           '',
        'WERKS'        TEXT-t30    ''   ''   ''   'ZSDT0121'   'WERKS'           '',
        'ZBZGG'        TEXT-t31    ''   ''   ''   'ZSDT0121'   'ZBZGG'           '',
        'ZYPSL'        TEXT-t32    ''   ''   ''   'ZSDT0121'   'ZYPSL'           '',
        'ZSFXCP'       TEXT-t33    ''   ''   ''   'ZSDT0121'   'ZSFXCP'          '',
        'ZSFBJ'        TEXT-t34    ''   ''   ''   'ZSDT0121'   'ZSFBJ'           '',
        'ZSFXBZ'       TEXT-t35    ''   ''   ''   'ZSDT0121'   'ZSFXBZ'          '',
        'ZATPDFRQ'     TEXT-t36    ''   ''   ''   'ZSDT0121'   'ZATPDFRQ'        '',
        'ZYSJHQ'       TEXT-t37    ''   ''   ''   'ZSDT0121'   'ZYSJHQ'          '',
        'ZNBJHQ'       TEXT-t38    ''   ''   ''   'ZSDT0121'   'ZNBJHQ'          '',
        'INCO1'        TEXT-t39    ''   ''   ''   'ZSDT0121'   'INCO1'           '',
        'CMPRE'        TEXT-t40    ''   ''   ''   'ZSDT0121'   'CMPRE'           '',
        'NETPR_I'      TEXT-t41    ''   ''   ''   'ZSDT0121'   'NETPR_I'         '',
        'WAERK_I'      TEXT-t42    ''   ''   ''   'ZSDT0121'   'WAERK_I'         '',
        'NETWR_I'      TEXT-t43    ''   ''   ''   'ZSDT0121'   'NETWR_I'         '',
        'ZSYNCUSER'    TEXT-t44    ''   ''   ''   'ZSDT0121'   'ZSYNCUSER'       'USER',
        'ZSYNCDATE'    TEXT-t45    ''   ''   ''   'ZSDT0121'   'ZSYNCDATE'       '',
        'ZSYNCTIME'    TEXT-t46    ''   ''   ''   'ZSDT0121'   'ZSYNCTIME'       ''.

    IF p_rd3 IS NOT INITIAL.
      DELETE gt_fcat WHERE fieldname = 'SEL'.
    ENDIF.

    IF p_rd1 IS NOT INITIAL.
      DELETE gt_fcat WHERE fieldname = 'DEL_DATE' OR fieldname = 'DEL_FLAG'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SEND_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_send_data .

  DATA: lt_send TYPE TABLE OF ty_data.

  DATA(lt_data) = gt_data.
  DELETE lt_data WHERE sel = ''.
  IF lt_data IS INITIAL.
    MESSAGE '未选择数据，请勾选' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE lt_data TRANSPORTING NO FIELDS WITH KEY dest_system = ''.
  IF sy-subrc IS INITIAL.
    MESSAGE '选择数据存在目标系统为空，请检查' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SORT lt_data BY dest_system vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING dest_system vbeln.
  SORT lt_data BY dest_system vbeln.

  "按系统分发数据
  LOOP AT lt_data INTO DATA(ls_data).

    DATA(ls_data_t) = ls_data.

    AT NEW dest_system.
      REFRESH lt_send.
    ENDAT.

    LOOP AT gt_data INTO gs_data WHERE dest_system = ls_data_t-dest_system
                                    AND vbeln = ls_data_t-vbeln.
      APPEND gs_data TO lt_send.
    ENDLOOP.

    AT END OF dest_system.

      CASE abap_true.
        WHEN p_rd1.
          PERFORM frm_call_sproxy TABLES lt_send USING ls_data_t-dest_system.   "正常数据
        WHEN p_rd2.
          PERFORM frm_call_sproxy_sd129 TABLES lt_send USING ls_data_t-dest_system.  "归档数据
        WHEN OTHERS.
      ENDCASE.

    ENDAT.

    CLEAR ls_data_t.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_SPROXY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DATA
*&---------------------------------------------------------------------*
FORM frm_call_sproxy  TABLES pt_data LIKE gt_data
                       USING pv_dest_system TYPE zsdt0121-dest_system.

  DATA: lr_sproxy  TYPE REF TO zco_si_sap2crm_sales_order_out,
        l_intid    TYPE ze_intf_id VALUE 'SD121',
        lv_message TYPE bapi_msg,
        output     TYPE zmt_sap2crm_sales_order_outpu1,
        ls_header  TYPE zdt_sap2crm_sales_order_outpu1,
        ls_item    TYPE zdt_sap2crm_sales_order_output,
        input      TYPE zmt_sap2crm_sales_order_output,
        lt_data    TYPE TABLE OF ty_data.

  DATA: lt_tmp        TYPE TABLE OF ty_data,
        lv_index_from TYPE sy-index VALUE 1,
        lv_index_to   TYPE sy-index,
        lv_lines      TYPE int4,
        lv_number     TYPE int4,
        lv_time       TYPE i,
        lv_package    TYPE i.

  DATA: lt_zsdt0121 TYPE TABLE OF zsdt0121,
        ls_zsdt0121 TYPE zsdt0121,
        lv_vbeln    TYPE vbak-vbeln,
        lv_timestap TYPE zsdt0121-timestap,
        lv_zstate   TYPE icon_d,
        lv_zcrmzt   TYPE zsdt0121-zcrmzt.

  DATA(l_guid) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
*  "接口开关检查
  CALL FUNCTION 'Z_PI_CK'
    EXPORTING
      intf_id  = 'SD121'
    IMPORTING
      err_info = lv_message.
*  接口未打开
  IF lv_message  IS NOT INITIAL.
    MESSAGE 'SD121接口未启用！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  "整理接口数据
  "每20个单据发送一次接口数据
  lt_data = pt_data[].
  SORT lt_data BY vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING vbeln.
  lv_package = 20.

  lv_number = lv_package.  "每次发送条目数
  lv_lines = lines( lt_data ).
  lv_index_to = lv_number.

  CREATE OBJECT lr_sproxy.

  DO.

    lv_time = lv_time + 1.

    "加入要处理的条目
    REFRESH: lt_tmp.

    IF lv_index_to >= lv_lines.
      APPEND LINES OF lt_data FROM lv_index_from TO lv_lines TO lt_tmp.
      lv_index_to = lv_lines.
    ELSE.
      APPEND LINES OF lt_data FROM lv_index_from TO lv_index_to TO lt_tmp.
    ENDIF.

    "转换数据--将拆分的20个单据的明细数据整合
    REFRESH: output-mt_sap2crm_sales_order_output-listof_so.
    REFRESH: lt_zsdt0121.

    LOOP AT lt_tmp INTO DATA(ls_tmp).

      CLEAR: ls_header.
      REFRESH: ls_header-order_details_infos.

      lv_vbeln = |{ ls_tmp-vbeln ALPHA = OUT }| .

      ls_header = VALUE #(
                            vbeln     = lv_vbeln      "销售凭证
                            tag       = ls_tmp-tag         "新增/修改标识
                            auart     = ls_tmp-auart         "销售订单类型
                            kunnr_sp  = |{ ls_tmp-kunnr_sp ALPHA = OUT }|    "售达方
                            kunnr_sh  = |{ ls_tmp-kunnr_sh ALPHA = OUT }|    "送达方
                            kunnr_z1  = |{ ls_tmp-kunnr_z1 ALPHA = OUT }|    "实际客户
                            netwr     = ls_tmp-netwr       "净值
                            waerk     = ls_tmp-waerk       "货币
                            kunnr_z3  = ls_tmp-sortl_z3    "业务员
                            erdat     = ls_tmp-erdat+0(4) && '-' && ls_tmp-erdat+4(2) && '-' && ls_tmp-erdat+6(2)       "创建时间
                            zblxy     = ls_tmp-zblxy       "是否有备料协议
                            zsfypo    = ls_tmp-zsfypo      "是否有PO
                            zterm     = ls_tmp-zterm       "付款条款
                            zhtqk     = ls_tmp-zhtqk       "合同情况
                            zsfxkh    = ls_tmp-zsfxkh      "是否新客户
                            zckgj     = ls_tmp-zckgj       "出口国家
                            zbldlx    = ls_tmp-zbldlx      "备料单类型
                            bstkd_e   = ls_tmp-bstkd_e     "采购订单编号
                            bstdk_e   = ls_tmp-bstdk_e+0(4) && '-' && ls_tmp-bstdk_e+4(2) && '-' && ls_tmp-bstdk_e+6(2)      "采购订单日期
                  ).

      CONDENSE ls_header-tag NO-GAPS.
      CONDENSE ls_header-netwr NO-GAPS.
      CONDENSE ls_header-kunnr_sp NO-GAPS.
      CONDENSE ls_header-kunnr_sh NO-GAPS.
      CONDENSE ls_header-kunnr_z1 NO-GAPS.

      IF ls_tmp-erdat IS INITIAL.
        CLEAR ls_header-erdat.
      ENDIF.

      IF ls_tmp-bstdk_e IS INITIAL.
        CLEAR ls_header-bstdk_e.
      ENDIF.

      SELECT SINGLE ddtext INTO ls_header-zhtqk FROM dd07t WHERE domname = 'ZD_HTQK' AND domvalue_l = ls_tmp-zhtqk.   "合同情况
      SELECT SINGLE ddtext INTO ls_header-zbldlx FROM dd07t WHERE domname = 'ZBLDLX' AND domvalue_l = ls_tmp-zbldlx.  "备料单类型

      LOOP AT gt_data INTO gs_data WHERE vbeln = ls_tmp-vbeln.

        CLEAR ls_item.
        ls_item = VALUE #(
                            posnr       = gs_data-posnr         "订单行项目编号
                            matnr       = gs_data-matnr         "物料
                            arktx       = gs_data-arktx         "项目描述
                            kwmeng      = gs_data-kwmeng        "销售单位数量
                            state       = gs_data-state         "订单行状态
                            vrkme       = gs_data-vrkme         "销售单位
                            werks       = gs_data-werks         "工厂
                            zbzgg       = gs_data-zbzgg         "包装规格
                            zypsl       = gs_data-zypsl         "样品数量
                            zsfxcp      = gs_data-zsfxcp        "是否新产品
                            zsfbj       = gs_data-zsfbj         "是否报检
                            zsfxbz      = gs_data-zsfxbz        "是否新包装
                            zatpdfrq    = gs_data-zatpdfrq+0(4) && '-' && gs_data-zatpdfrq+4(2) && '-' && gs_data-zatpdfrq+6(2)      "ATP答复日期
                            zysjhq      = gs_data-zysjhq+0(4) && '-' && gs_data-zysjhq+4(2) && '-' && gs_data-zysjhq+6(2)         "原始交货期
                            znbjhq      = gs_data-znbjhq+0(4) && '-' && gs_data-znbjhq+4(2) && '-' && gs_data-znbjhq+6(2)        "内部交货期
                            inco1       = gs_data-inco1         "国际贸易条款
                            cmpre       = gs_data-cmpre         "含税单价
                            netpr       = gs_data-netpr_i       "净价
                            waerk       = gs_data-waerk_i       "货币
                            netwr       = gs_data-netwr_i       "净值
                         ).

        SHIFT ls_item-posnr LEFT DELETING LEADING '0'.

        CONDENSE ls_item-netpr NO-GAPS.
        CONDENSE ls_item-netwr NO-GAPS.
        CONDENSE ls_item-cmpre NO-GAPS.
        CONDENSE ls_item-kwmeng NO-GAPS.

        IF gs_data-zatpdfrq IS INITIAL.
          CLEAR ls_item-zatpdfrq.
        ENDIF.

        IF gs_data-znbjhq IS INITIAL.
          CLEAR ls_item-znbjhq.
        ENDIF.

        IF gs_data-zysjhq IS INITIAL.
          CLEAR ls_item-zysjhq.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = ls_item-matnr
          IMPORTING
            output = ls_item-matnr.

        APPEND ls_item TO ls_header-order_details_infos.

        CLEAR ls_zsdt0121.
        MOVE-CORRESPONDING gs_data TO ls_zsdt0121.
        APPEND ls_zsdt0121 TO lt_zsdt0121.

        CLEAR gs_data.

      ENDLOOP.

      APPEND ls_header TO output-mt_sap2crm_sales_order_output-listof_so.

    ENDLOOP.

    "调用接口发送数据
    TRY.

        output-mt_sap2crm_sales_order_output-dest_system = pv_dest_system.

        DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'I' is_data = output ). "记录输入参数

        lv_timestap = sy-datum && sy-uzeit.

        CLEAR input-mt_sap2crm_sales_order_output.
        REFRESH: input-mt_sap2crm_sales_order_output-data.

        CALL METHOD lr_sproxy->si_sap2crm_sales_order_output
          EXPORTING
            output = output
          IMPORTING
            input  = input.
        COMMIT WORK AND WAIT.

        "整理回传结果
        LOOP AT output-mt_sap2crm_sales_order_output-listof_so INTO ls_header.

          CLEAR lv_vbeln.
          lv_vbeln = ls_header-vbeln.
          lv_vbeln = |{ lv_vbeln ALPHA = IN }|.
          CLEAR: lv_zstate,lv_zcrmzt,lv_message.

          IF input-mt_sap2crm_sales_order_output-type = 'S'.   "所有的数据都成功

            lv_zstate = icon_led_green.
            lv_zcrmzt = 'S'.
            lv_message = '传输成功'.

          ELSE.

            READ TABLE input-mt_sap2crm_sales_order_output-data INTO DATA(ls_data) WITH KEY vbeln = ls_header-vbeln.  "判断是否在错误列表
            IF sy-subrc IS INITIAL.

              lv_zstate = icon_led_red.
              lv_zcrmzt = 'E'.
              lv_message = ls_data-message.

            ELSE.

              lv_zstate = icon_led_green.
              lv_zcrmzt = 'S'.
              lv_message = '传输成功'.

            ENDIF.

          ENDIF.

          gs_data-zstate = lv_zstate.
          gs_data-zcrmzt = lv_zcrmzt.
          gs_data-message = lv_message.
          gs_data-zsyncuser = sy-uname.
          gs_data-zsyncdate = sy-datum.
          gs_data-zsynctime = sy-uzeit.

          ls_zsdt0121-timestap = lv_timestap.
          ls_zsdt0121-zsyncuser = sy-uname.
          ls_zsdt0121-zsyncdate = sy-datum.
          ls_zsdt0121-zsynctime = sy-uzeit.
          ls_zsdt0121-zcrmzt  = gs_data-zcrmzt.
          ls_zsdt0121-message = gs_data-message.

          MODIFY gt_data FROM gs_data
                        TRANSPORTING zstate zcrmzt message
                                     zsyncuser zsyncdate zsynctime
                        WHERE vbeln = lv_vbeln.

          MODIFY lt_zsdt0121 FROM ls_zsdt0121 TRANSPORTING zcrmzt message timestap
                                                           zsyncuser zsyncdate zsynctime
                                              WHERE vbeln = lv_vbeln.
        ENDLOOP.

        MODIFY zsdt0121 FROM TABLE lt_zsdt0121.

        COMMIT WORK AND WAIT.

        zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'E' is_data = input ). "记录输出参数

      CATCH cx_root INTO DATA(lr_root).
        MESSAGE lr_root->get_text(  ) TYPE 'I'.
        EXIT.
    ENDTRY.

* 计算下一个task的起止索引
    lv_index_from = lv_index_from + lv_number.
    lv_index_to   = lv_index_from + lv_number - 1.
    IF lv_index_from > lv_lines.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_log .

  "获取接口历史数据

  SELECT zsdt0121~*,
         CASE zsdt0121~zcrmzt
         WHEN 'S' THEN  '@5B@' "  Green LED; go; OK    ICON_LED_GREEN
         WHEN 'E' THEN  '@5C@' "  Red LED; stop; error ICON_LED_RED
         END AS zstate
    INTO CORRESPONDING FIELDS OF TABLE @gt_data
    FROM zsdt0121
   WHERE zsyncdate   IN @s_sendd
     AND erdat       IN @s_erdat
     AND del_date    IN @s_zzgdr
     AND vbeln       IN @s_vbeln
     AND kunnr_z1    IN @s_kunnr
     AND kunnr_z3    IN @s_zzywy
     AND vkbur       IN @s_vkbur
     AND dest_system IN @s_zdest.

  SORT gt_data BY timestap vbeln posnr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DEL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_del_data .

  "取已经删除的销售订单   并且已经下发给crm的销售订单
  "销售订单下发时已经判断SO1  这里不需要重新判断

  REFRESH: gt_so.

  SELECT cdhdr~*,cdpos~*
    INTO TABLE @DATA(lt_change)
    FROM cdhdr
    INNER JOIN cdpos ON cdhdr~objectclas = cdpos~objectclas
                    AND cdhdr~changenr   = cdpos~changenr
   WHERE cdhdr~objectclas = 'VERKBELEG'
     AND cdpos~tabname    = 'VBAK'
     AND cdpos~chngind    = 'D'
     AND cdhdr~udate      IN @s_zzgdr
     AND cdhdr~objectid   IN @s_vbeln.

  LOOP AT lt_change INTO DATA(ls_change).
    APPEND VALUE #( vbeln = ls_change-cdhdr-objectid
                    udate = ls_change-cdhdr-udate )
           TO gt_so.
  ENDLOOP.
  SORT gt_so BY vbeln.
  DELETE ADJACENT DUPLICATES FROM gt_so COMPARING vbeln.

  SELECT zsdt0121~*
    FROM zsdt0121
    INNER JOIN @gt_so AS a ON a~vbeln = zsdt0121~vbeln
    WHERE zsdt0121~zcrmzt = 'S'
    INTO TABLE @DATA(lt_zsdt0121).
  SORT lt_zsdt0121 BY dest_system vbeln timestap DESCENDING .
  DELETE ADJACENT DUPLICATES FROM lt_zsdt0121 COMPARING dest_system vbeln.

  LOOP AT lt_zsdt0121 INTO DATA(ls_zsdt0121).
    CLEAR gs_data.
    gs_data-dest_system = ls_zsdt0121-dest_system.
    gs_data-vkbur       = ls_zsdt0121-vkbur.
    gs_data-kunnr_z3    = ls_zsdt0121-kunnr_z3.
    gs_data-kunnr_z1    = ls_zsdt0121-kunnr_z1.
    gs_data-vbeln       = ls_zsdt0121-vbeln.
    gs_data-del_flag    = 'X'.
    gs_data-sel         = sy-batch.
    READ TABLE gt_so INTO DATA(ls_so) WITH KEY vbeln = gs_data-vbeln.
    IF sy-subrc IS INITIAL.
      gs_data-del_date = ls_so-udate.
    ENDIF.
    APPEND gs_data TO gt_data.
  ENDLOOP.

  SORT gt_data BY dest_system vbeln.
  DELETE ADJACENT DUPLICATES FROM gt_data COMPARING dest_system vbeln.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_SPROXY_SD129
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SEND
*&      --> LS_DATA_T_DEST_SYSTEM
*&---------------------------------------------------------------------*
FORM frm_call_sproxy_sd129  TABLES pt_data LIKE gt_data
                       USING pv_dest_system TYPE zsdt0121-dest_system.

  DATA: lr_sproxy  TYPE REF TO zco_si_sap2crm_order_gui_dang,
        l_intid    TYPE ze_intf_id VALUE 'SD129',
        lv_message TYPE bapi_msg,
        output     TYPE zmt_sap2crm_order_gui_dang,
        ls_item    TYPE zdt_sap2crm_order_gui_dang_lin,
        input      TYPE zmt_sap2crm_order_gui_dang_ret,
        lt_data    TYPE TABLE OF ty_data.

  DATA: lt_tmp        TYPE TABLE OF ty_data,
        lv_index_from TYPE sy-index VALUE 1,
        lv_index_to   TYPE sy-index,
        lv_lines      TYPE int4,
        lv_number     TYPE int4,
        lv_time       TYPE i,
        lv_package    TYPE i.

  DATA: lt_zsdt0121 TYPE TABLE OF zsdt0121,
        ls_zsdt0121 TYPE zsdt0121,
        lv_vbeln    TYPE vbak-vbeln,
        lv_timestap TYPE zsdt0121-timestap,
        lv_zstate   TYPE icon_d,
        lv_zcrmzt   TYPE zsdt0121-zcrmzt.

  DATA(l_guid) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
*  "接口开关检查
  CALL FUNCTION 'Z_PI_CK'
    EXPORTING
      intf_id  = 'SD129'
    IMPORTING
      err_info = lv_message.
*  接口未打开
  IF lv_message  IS NOT INITIAL.
    MESSAGE 'SD129接口未启用！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  "整理接口数据
  "每20个单据发送一次接口数据
  lt_data = pt_data[].
  SORT lt_data BY vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING vbeln.
  lv_package = 20.

  lv_number = lv_package.  "每次发送条目数
  lv_lines = lines( lt_data ).
  lv_index_to = lv_number.

  CREATE OBJECT lr_sproxy.

  DO.

    lv_time = lv_time + 1.

    "加入要处理的条目
    REFRESH: lt_tmp.

    IF lv_index_to >= lv_lines.
      APPEND LINES OF lt_data FROM lv_index_from TO lv_lines TO lt_tmp.
      lv_index_to = lv_lines.
    ELSE.
      APPEND LINES OF lt_data FROM lv_index_from TO lv_index_to TO lt_tmp.
    ENDIF.

    "转换数据--将拆分的20个单据的明细数据整合
    REFRESH: output-mt_sap2crm_order_gui_dang-linesof_so.
    REFRESH: lt_zsdt0121.

    LOOP AT lt_tmp INTO DATA(ls_tmp).

      ls_item-vbeln = |{ ls_tmp-vbeln ALPHA = OUT }|.
      CONDENSE ls_item-vbeln NO-GAPS.
      ls_item-vbtyp = 1.
      CONDENSE ls_item-vbtyp NO-GAPS.
      APPEND ls_item TO output-mt_sap2crm_order_gui_dang-linesof_so.

      CLEAR ls_zsdt0121.
      MOVE-CORRESPONDING ls_tmp TO ls_zsdt0121.
      APPEND ls_zsdt0121 TO lt_zsdt0121.

      CLEAR ls_tmp.
    ENDLOOP.

    "调用接口发送数据
    TRY.

        output-mt_sap2crm_order_gui_dang-dest_system = pv_dest_system.

        DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'I' is_data = output ). "记录输入参数

        lv_timestap = sy-datum && sy-uzeit.

        CLEAR input-mt_sap2crm_order_gui_dang_ret.
        REFRESH: input-mt_sap2crm_order_gui_dang_ret-data.

        CALL METHOD lr_sproxy->si_sap2crm_order_gui_dang_out
          EXPORTING
            output = output
          IMPORTING
            input  = input.

        COMMIT WORK AND WAIT.

        "整理回传结果
        LOOP AT output-mt_sap2crm_order_gui_dang-linesof_so INTO ls_item.

          CLEAR lv_vbeln.
          lv_vbeln = ls_item-vbeln.
          lv_vbeln = |{ lv_vbeln ALPHA = IN }|.
          CLEAR: lv_zstate,lv_zcrmzt,lv_message.

          IF input-mt_sap2crm_order_gui_dang_ret-type = 'S'.   "所有的数据都成功

            lv_zstate = icon_led_green.
            lv_zcrmzt = 'S'.
            lv_message = '传输成功'.

          ELSE.

            READ TABLE input-mt_sap2crm_order_gui_dang_ret-data INTO DATA(ls_data) WITH KEY vbeln = ls_item-vbeln.  "判断是否在错误列表
            IF sy-subrc IS INITIAL.

              lv_zstate = icon_led_red.
              lv_zcrmzt = 'E'.
              lv_message = ls_data-message.

            ELSE.

              lv_zstate = icon_led_green.
              lv_zcrmzt = 'S'.
              lv_message = '传输成功'.

            ENDIF.

          ENDIF.

          gs_data-zstate = lv_zstate.
          gs_data-zcrmzt = lv_zcrmzt.
          gs_data-message = lv_message.
          gs_data-zsyncuser = sy-uname.
          gs_data-zsyncdate = sy-datum.
          gs_data-zsynctime = sy-uzeit.

          ls_zsdt0121-timestap = lv_timestap.
          ls_zsdt0121-zsyncuser = sy-uname.
          ls_zsdt0121-zsyncdate = sy-datum.
          ls_zsdt0121-zsynctime = sy-uzeit.
          ls_zsdt0121-zcrmzt  = gs_data-zcrmzt.
          ls_zsdt0121-message = gs_data-message.

          MODIFY gt_data FROM gs_data
                        TRANSPORTING zstate zcrmzt message
                                     zsyncuser zsyncdate zsynctime
                        WHERE vbeln = lv_vbeln.

          MODIFY lt_zsdt0121 FROM ls_zsdt0121 TRANSPORTING zcrmzt message timestap
                                                           zsyncuser zsyncdate zsynctime
                                              WHERE vbeln = lv_vbeln.
        ENDLOOP.

        MODIFY zsdt0121 FROM TABLE lt_zsdt0121.

        COMMIT WORK AND WAIT.

        zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'E' is_data = input ). "记录输出参数

      CATCH cx_root INTO DATA(lr_root).
        MESSAGE lr_root->get_text(  ) TYPE 'I'.
        EXIT.
    ENDTRY.

* 计算下一个task的起止索引
    lv_index_from = lv_index_from + lv_number.
    lv_index_to   = lv_index_from + lv_number - 1.
    IF lv_index_from > lv_lines.
      EXIT.
    ENDIF.

  ENDDO.
ENDFORM.