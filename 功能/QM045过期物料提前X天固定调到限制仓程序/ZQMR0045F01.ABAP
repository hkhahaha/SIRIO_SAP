*&---------------------------------------------------------------------*
*& 包含               ZQMR0045F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_GET_EXT_ST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

FORM frm_get_ext_st .
  CLEAR: gt_ext_st.
  SELECT * FROM zmmwms_ext_st INTO TABLE gt_ext_st.

  t001w-werks = '8100'.
  IF s_matnr IS NOT INITIAL.
    SELECT
    mara~matnr,
    mard~werks,
    mchb~charg,
    mard~lgort

    FROM mara
    INNER JOIN mard
    ON mard~matnr = mara~matnr
    INNER JOIN mchb
    ON mchb~matnr = mara~matnr
    AND mchb~werks = mard~werks
    AND mchb~lgort = mard~lgort
    WHERE mara~matnr IN @s_matnr
    AND mtart IN @s_mtart
    AND mtart IN ('Z010','Z020','Z050')
    AND mard~werks = @p_werks
    AND mard~lgort IN ( SELECT lgort FROM zmm_lgort )
    AND mchb~charg IN @s_charg
    INTO TABLE @DATA(lt_mara).

    SELECT
    a~matnr,
    a~werks,
    a~charg,
    a~lgort
    FROM @lt_mara AS a
    INNER JOIN mseg
    ON mseg~matnr = a~matnr
    AND mseg~werks = a~werks
    AND mseg~lgort = a~lgort
    AND mseg~charg = a~charg
    WHERE mseg~bwart = @p_bwart
    INTO TABLE @DATA(lt_all).

    LOOP AT lt_all INTO DATA(ls_all).
      ls_all-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_all-matnr ).
      gs_item-matnr = ls_all-matnr.
      APPEND gs_item TO gt_item.
      CLEAR gs_item.
      CLEAR ls_all.
    ENDLOOP.
  ELSE.
*    MESSAGE '物料号为空' TYPE 'I'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_AUTHORITY_CHECK_01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_authority_check_01 .
*  IF p_bwart IS INITIAL.
*    MESSAGE '创建的时候，移动类型必输输入' TYPE 'S' DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.
*
*  IF p_werks IS INITIAL.
*    MESSAGE '创建的时候，工厂必输输入' TYPE 'S' DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.
*
**  AUTHORITY-CHECK OBJECT 'ZMM030'
**  ID 'WERKS' FIELD p_werks
**  ID 'BWART' FIELD p_bwart.
*  IF sy-subrc <> 0.
*    DATA(lv_message) = |{ p_werks }/{ p_bwart }|.
*    MESSAGE s002(zauthority) DISPLAY LIKE 'E' WITH '工厂/移动类型' lv_message.
*    STOP.
*  ENDIF.
  CLEAR: gt_wms_custid_help[].
  SELECT *
  INTO TABLE gt_wms_custid_help
  FROM zmmwms_sys
  WHERE werks = p_werks.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_save .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADDRECORD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM addrecord.
  CLEAR gs_item.
  APPEND gs_item TO gt_item.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM catalog .
  w_repid = sy-repid.
  CLEAR fieldcat.
  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = sy-tabix."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.


  fieldcatset 'LIGHT' '指示灯'  .
  fieldcatset 'ZREASON' '报错原因'  .
  fieldcatset 'ZSAPNOTE_NO' '通知单号'  .
  fieldcatset 'ZSAPNOTE_LINE' '通知单行号'  .
  fieldcatset 'WMS_CUSTID' 'WMS系统号'  .
  fieldcatset 'WERKS' '工厂'  .
  fieldcatset 'NOTE' '抬头备注'  .
  fieldcatset 'MATNR' '物料号'  .
  fieldcatset 'MAKTX' '物料名'  .
  fieldcatset 'CHARG' 'SAP批号'  .
  fieldcatset 'UNAME' '用户名'  .
  fieldcatset 'DATUM' '日期'  .
  fieldcatset 'UZEIT' '时间'  .
  fieldcatset 'MENGE' '数量'  .
  fieldcatset 'MEINS' '单位'  .
  fieldcatset 'MSEHT' '单位文本'  .
  fieldcatset 'BWART' '移动类型'  .
  fieldcatset 'BTEXT' '移动类型文本'  .
  fieldcatset 'STOCK_ST' '库存状态'  .
  fieldcatset 'LGORT' '库存地点'  .
  fieldcatset 'DLGORT' '变更后库存地点'  .
  fieldcatset 'DSTOCK_ST' '变更后库存状态'  .
  fieldcatset 'SOBKZ' '特殊标识'  .
  fieldcatset 'LIFNR' '供应商'  .
  fieldcatset 'KDAUF' '销售订单'  .
  fieldcatset 'KDPOS' '销售订单项目'  .

*layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'."斑马线的样式
  layout-box_fieldname = 'sel'.
  layout-colwidth_optimize = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_authority_check .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SCREEN_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_screen_check .
  CLEAR lt_zmm_changeno.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GETDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getdata .
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_alv
  FROM zmm_changeno AS a
  WHERE a~zsapnote_no IN s_no
    AND a~werks IN s_werks
    AND a~matnr IN s_matnr2
    AND a~charg IN s_charg2
    AND a~lgort IN s_lgort2
    AND a~datum IN s_date.

  IF p_no IS INITIAL.
    DELETE lt_alv WHERE zstatus = 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOWALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM showalv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = w_repid "程序名称
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     i_grid_title             = '会计凭证'"标题名
*     I_GRID_SETTINGS          =
      is_layout                = layout "程序所定义的layout名称
      it_fieldcat              = fieldcat[] "定义fieldcat数据
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = lt_alv
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.

FORM frm_user_command USING i_ucomm       TYPE sy-ucomm
      i_wa_selfield TYPE slis_selfield.
  DATA:
    ls_layout TYPE lvc_s_layo,
    lv_grid   TYPE REF TO cl_gui_alv_grid.
  DATA ls_stable TYPE lvc_s_stbl.
  CLEAR:gt_item,gs_head.

  CASE i_ucomm.
    WHEN '&CHANGE'.
      LOOP AT lt_alv INTO ls_alv WHERE sel = 'X'.
        MOVE-CORRESPONDING ls_alv TO gs_item.
        gs_head-zsapnote_no = ls_alv-zsapnote_no.
        wms_custid = ls_alv-wms_custid.
        gs_head-bwart = ls_alv-bwart.
        gs_head-note = ls_alv-note.
        "物料描述
        SELECT SINGLE
          maktx
        INTO gs_item-maktx
        FROM makt
        WHERE makt~matnr = gs_item-matnr
          AND makt~spras = '1'.
        "变更之后的库存地点
        IF p_bwart = 325.
          DATA(lv_flag) = gs_item-lgort+0(1).
          IF gs_item-werks = 8100 AND lv_flag = '1'.
            gs_item-dlgort = 1180.
          ELSEIF gs_item-werks = 8100 AND lv_flag = '2'.
            gs_item-dlgort = 2180.
          ENDIF.
          IF gs_item-werks = 8200.
            gs_item-dlgort = 3180.
          ENDIF.
        ELSEIF p_bwart = 344.
          gs_item-dlgort = gs_item-lgort.
          gs_item-dstock_st = 'B'.
        ENDIF.

        APPEND gs_item TO gt_item.
        CLEAR:gs_item,ls_alv.
      ENDLOOP.
      CALL SCREEN 100.
    WHEN '&DELETE'.
      CLEAR:ls_alv.
      LOOP AT lt_alv INTO ls_alv WHERE sel = 'X'.
        DELETE TABLE lt_alv FROM ls_alv.
        DELETE FROM zmm_changeno WHERE zsapnote_no = ls_alv-zsapnote_no AND zsapnote_line = ls_alv-zsapnote_line.
        CLEAR ls_alv.
      ENDLOOP.
    WHEN '&DOWNLOAD'.
      PERFORM frm_send_to_wms.
    WHEN OTHERS.
  ENDCASE.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
    IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
      e_grid = lv_grid
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .

  i_wa_selfield-refresh = 'X'.
  i_wa_selfield-col_stable = 'X'.
  i_wa_selfield-row_stable = 'X'.

  CALL METHOD lv_grid->get_frontend_layout
    IMPORTING
      es_layout = ls_layout.

  ls_layout-cwidth_opt = 'x'.

  CALL METHOD lv_grid->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.

ENDFORM.

FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZGUI2'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  MOD_WMS_CUSTID_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mod_wms_custid_f4 INPUT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'WMS_CUSTID'            "大写,可选值内表的字段名
      value_org       = 'S'                     "就写'S'
      dynpprog        = sy-repid                "返回的输入框所在的main program
      dynpnr          = sy-dynnr                "返回的输入框所在屏幕
      dynprofield     = 'GS_HEAD-WMS_CUSTID'    "返回的输入框名
    TABLES
      value_tab       = gt_wms_custid_help       "可选值的内表
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HELP_MODULE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_module INPUT.
  CALL FUNCTION 'HELP_OBJECT_SHOW'
    EXPORTING
      dokclass         = 'DE'
*     DOKLANGU         = SY-LANGU
      dokname          = 'SYDATUM'
*     DOKTITLE         = ' '
*     CALLED_BY_PROGRAM                   = ' '
*     CALLED_BY_DYNP   = ' '
*     CALLED_FOR_TAB   = ' '
*     CALLED_FOR_FIELD = ' '
*     CALLED_FOR_TAB_FLD_BTCH_INPUT       = ' '
*     MSG_VAR_1        = ' '
*     MSG_VAR_2        = ' '
*     MSG_VAR_3        = ' '
*     MSG_VAR_4        = ' '
*     CALLED_BY_CUAPROG                   = ' '
*     CALLED_BY_CUASTAT                   =
*     SHORT_TEXT       = ' '
*     CLASSIC_SAPSCRIPT                   = ' '
*     MES_PROGRAM_NAME = ' '
*     MES_INCLUDE_NAME = ' '
*     MES_LINE_NUMBER  =
*     MES_EXCEPTION    = ' '
    TABLES
      links            = t_link
    EXCEPTIONS
      object_not_found = 1
      sapscript_error  = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MO_ITEM OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE mo_item OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_data .
  DATA:lv_line    TYPE zmm_changeno-zsapnote_line,
       lv_no      TYPE zmm_changeno-zsapnote_no,
       lv_num     TYPE zesapnote_no,
       zout_x     TYPE zmmwms_ext_st-zout_x,
       zst_code   TYPE zmmwms_ext_st-zst_code,
       xconsign   TYPE zmmwms_ext_st-xconsign,
       wms_ext_st TYPE zmmwms_ext_st-wms_ext_st.
  DATA:lv_atinn1(15),
       lv_atinn2(15).
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'ZOUT_X'
    IMPORTING
      output = lv_atinn1.
*
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'ZST_CODE'
    IMPORTING
      output = lv_atinn2.
  READ TABLE gt_item INTO gs_item INDEX 1.
  IF gs_item-zsapnote_no IS INITIAL.
    lv_num = zcl_cm_public=>get_bus_no( iv_bwart = gs_head-bwart ).
    lv_noall = lv_num.
    IF lv_noall IS INITIAL.
      CLEAR s_no.
      s_no-sign = 'I'.
      s_no-option = 'EQ'.
      s_no-low = lv_noall.
      APPEND s_no.
    ENDIF.
  ENDIF.
  lv_line = 1.

  SELECT
  *
  FROM zmmwms_ext_st
  INTO TABLE @DATA(lt_zmmwms_ext_st).

  SELECT
  matnr,
  charg,
  werks,
  cuobj_bm,
  atinn,
  atwrt,
  atflv
  FROM mcha
  INNER JOIN ausp
  ON ausp~objek = mcha~cuobj_bm
  FOR ALL ENTRIES IN @gt_item
  WHERE mcha~werks = @gt_item-werks
  AND mcha~charg = @gt_item-charg
  AND mcha~matnr = @gt_item-matnr
*    AND ausp~atinn IN ( @lv_atinn1,@lv_atinn2 )
  INTO TABLE @DATA(lt_mcha).


  SELECT
  matnr,
  werks,
  charg,
  sobkz
  FROM mkol
  INTO TABLE @DATA(lt_mkol)
  FOR ALL ENTRIES IN @gt_item
  WHERE mkol~werks = @gt_item-werks
  AND mkol~charg = @gt_item-charg
  AND mkol~matnr = @gt_item-matnr.

*  SELECT
*    *
*  INTO TABLE @DATA(lt_makt)
*  FROM makt
*  FOR ALL ENTRIES IN @gt_item
*  WHERE makt~spras = '1'
*    AND makt~matnr = @gt_item-matnr.
  CLEAR lt_zmm_changeno.
  LOOP AT gt_item INTO gs_item.
    CLEAR ls_zmm_changeno.
    "首先先把抬头的数据存在LT_zmm_changeno内表里面，抬头的数据一般是固定的
    "货主ID
    ls_zmm_changeno-wms_custid = wms_custid.
    "抬头备注
    ls_zmm_changeno-note = gs_head-note.
    "工厂
    ls_zmm_changeno-werks = t001w-werks.
    "移动类型
    ls_zmm_changeno-bwart = gs_head-bwart.
    "通知单号
    IF gs_item-zsapnote_no IS INITIAL.
      ls_zmm_changeno-zsapnote_no = lv_num.
    ELSE.
      ls_zmm_changeno-zsapnote_no = gs_item-zsapnote_no.
    ENDIF.
    "然后把行项的数据一一存在上面的内表中
    IF gs_item-zsapnote_line IS INITIAL.
      ls_zmm_changeno-zsapnote_line = lv_line.
    ELSE.
      ls_zmm_changeno-zsapnote_line = gs_item-zsapnote_line.
    ENDIF.
    "用户名
    ls_zmm_changeno-uname = sy-uname.
    "物料
    ls_zmm_changeno-matnr = gs_item-matnr.
    "批次
    ls_zmm_changeno-charg = gs_item-charg.
    "销售订单号
    ls_zmm_changeno-kdauf = gs_item-kdauf.
    "销售订单行号
    ls_zmm_changeno-kdpos = gs_item-kdpos.
    "数量
    ls_zmm_changeno-menge = gs_item-menge.
    "基本计量单位
    ls_zmm_changeno-meins = gs_item-meins.
    "商业
    ls_zmm_changeno-mseh3 = gs_item-mseh3.
    "技术
    ls_zmm_changeno-mseh6 = gs_item-mseh6.
    "度量单位文本
    ls_zmm_changeno-mseht = gs_item-mseht.
    "库存状态
    ls_zmm_changeno-stock_st = gs_item-stock_st.
    "库存地点
    ls_zmm_changeno-lgort = gs_item-lgort.
*    "变更后的物料
    ls_zmm_changeno-dmatnr = gs_item-matnr.
*    "变更后的批次
    ls_zmm_changeno-dcharg = gs_item-charg.
    "变更后的库存状态
    ls_zmm_changeno-dstock_st = gs_item-dstock_st.
    ls_zmm_changeno-datum = sy-datum.
    ls_zmm_changeno-uzeit = sy-uzeit.
    ls_zmm_changeno-sobkz = gs_item-sobkz.
    ls_zmm_changeno-lifnr = gs_item-lifnr.
    ls_zmm_changeno-dlgort = gs_item-dlgort.
    "WMS库存控制字
*    ls_zmm_changeno-wms_ext_st = ''.

    IF gs_head-bwart = '344'.
      ls_zmm_changeno-dstock_st = 'B'.
      ls_zmm_changeno-stock_st = ''.
      CLEAR:zout_x,zst_code,xconsign,wms_ext_st.
      READ TABLE lt_mcha INTO DATA(ls_mcha) WITH KEY matnr = gs_item-matnr
                                                        charg = gs_item-charg
                                                        werks = gs_item-werks
                                                        atinn = lv_atinn1.
      "ZOUT_X外寄标识
      zout_x = ls_mcha-atwrt.

      CLEAR ls_mcha.
      READ TABLE lt_mcha INTO ls_mcha WITH KEY matnr = gs_item-matnr
                                              charg = gs_item-charg
                                              werks = gs_item-werks
                                              atinn = lv_atinn2.
      "ZOUT_X外寄标识
      zst_code = ls_mcha-atwrt.
      CLEAR ls_mcha.
      "XCONSIGN寄售库存标记
      READ TABLE lt_mkol INTO DATA(ls_mkol) WITH KEY matnr = gs_item-matnr
                                                    charg = gs_item-charg
                                                    werks = gs_item-werks.
      xconsign = ls_mkol-sobkz.
      CLEAR ls_mkol.

      READ TABLE lt_zmmwms_ext_st INTO DATA(ls_zmmwms_ext_st) WITH KEY zout_x = zout_x
                                                                      zst_code = zst_code
                                                                      xconsign = xconsign.
      ls_zmm_changeno-wms_ext_st = ls_zmmwms_ext_st-wms_ext_st.
      "变更后的库存地点
      ls_zmm_changeno-dlgort = gs_item-dlgort.
    ENDIF.



    "行号累加，不清空
    IF gs_item-zsapnote_line IS INITIAL.
      lv_line  = lv_line + 1.
    ENDIF.
    IF lt_alv IS NOT INITIAL.
      MOVE-CORRESPONDING gs_item TO ls_alv.
*      MODIFY TABLE lt_alv FROM ls_alv .
      MODIFY lt_alv FROM ls_alv TRANSPORTING dlgort kdauf kdpos matnr meins charg menge lgort dlgort sobkz lifnr
      WHERE zsapnote_no = gs_item-zsapnote_no AND zsapnote_line = gs_item-zsapnote_line.
    ENDIF.
    APPEND ls_zmm_changeno TO lt_zmm_changeno.

    CLEAR:gs_item,ls_zmm_changeno.
  ENDLOOP.
  PERFORM check_item.
  MODIFY zmm_changeno FROM TABLE lt_zmm_changeno.
  IF lv_num IS NOT INITIAL.
    MESSAGE '通知单:' && lv_num && '创建成功' TYPE 'S' DISPLAY LIKE 'S'.
  ELSE.
    MESSAGE '修改成功' TYPE 'S' DISPLAY LIKE 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  TC_100_01_MARK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_100_01_mark INPUT.
  DATA: g_tc_100_01_wa2 LIKE LINE OF gt_item.
  IF tc_100-line_sel_mode = 1
  AND gs_item-sel = 'X'.
    LOOP AT gt_item INTO g_tc_100_01_wa2
    WHERE sel = 'X'.
      g_tc_100_01_wa2-sel = ''.
      MODIFY gt_item
      FROM g_tc_100_01_wa2
      TRANSPORTING sel.
    ENDLOOP.
  ENDIF.
  MODIFY gt_item FROM gs_item INDEX tc_100-current_line TRANSPORTING sel.
*  MODIFY gt_item
*  FROM gt_item
*  INDEX tc_100-current_line
*  TRANSPORTING sel.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form ALLSELECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM allselect .
  CLEAR gs_item.
  DATA lv_flag(5) VALUE '1'.
  LOOP AT gt_item INTO gs_item.
    gs_item-sel = 'X'.
    MODIFY gt_item FROM gs_item INDEX lv_flag TRANSPORTING sel.
    lv_flag = lv_flag + 1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DALLSELECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM dallselect .
  CLEAR gs_item.
  DATA lv_flag(5) VALUE '1'.
  LOOP AT gt_item INTO gs_item.
    gs_item-sel = ''.
    MODIFY gt_item FROM gs_item INDEX lv_flag TRANSPORTING sel.
    lv_flag = lv_flag + 1.
  ENDLOOP.
ENDFORM.