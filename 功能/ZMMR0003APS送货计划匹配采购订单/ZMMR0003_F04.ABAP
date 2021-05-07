*&---------------------------------------------------------------------*
*& 包含               ZMMR0003_F04
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  frm_SHOW_DATA
*&---------------------------------------------------------------------*
*       显示ALV
*----------------------------------------------------------------------*
FORM FRM_SHOW_DATA TABLES PT_TABLE.
    CLEAR:GT_FIELDCAT[],GS_LAYOUT.
 *ALV布局
    PERFORM FRM_ALV_LIST_LAYOUT_PT.
 * ALV事件
    PERFORM FRM_EVENT_BUILD USING GT_EVENTS[].
 *ALV字段设置
    PERFORM FRM_ALV_FILL_NAME_PT USING GT_FIELDCAT.
 * ALV排序
 *  PERFORM frm_sort_field CHANGING GT_sort[].
 *ALV显示
    PERFORM FRM_ALV_LIST_DISPLY TABLES PT_TABLE.
  ENDFORM. " frm_SHOW_DATA
 
 *&---------------------------------------------------------------------*
 *&      Form  frm_ALV_LIST_LAYOUT_pt
 *&---------------------------------------------------------------------*
 *       LAYOUT##
 *----------------------------------------------------------------------*
  FORM FRM_ALV_LIST_LAYOUT_PT .
    IF P_R06 = 'X'.
      GS_LAYOUT-BOX_FNAME   = 'SEL'.
    ENDIF.
    GS_LAYOUT-CWIDTH_OPT    = 'X'.
    GS_LAYOUT-ZEBRA                = 'X'.
 *   gs_layout-detail_initial_lines = 'X'.
    GS_LAYOUT-STYLEFNAME = 'CELL_STYLES'.
 *  gs_layout-numc_sum = 'X'.
 *  gs_layout-coltab_fieldname = 'CELL_COLOR'.
  ENDFORM. " frm_ALV_LIST_LAYOUT
 
 *&---------------------------------------------------------------------*
 *&      Form  frm_alv_fill_name_pt
 *&---------------------------------------------------------------------*
 *       ####
 *----------------------------------------------------------------------*
  FORM FRM_ALV_FILL_NAME_PT USING IT_FIELDCAT TYPE LVC_T_FCAT.
 
    IF P_R05 = 'X'.
      PERFORM FRM_ALV_SET_FIELDCAT_PT USING:
           SPACE 'light   ' SPACE SPACE SPACE SPACE 'Light' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'message ' SPACE SPACE SPACE SPACE 'Message' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'ebeln' SPACE 'zmmaps_temp' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'ebelp' SPACE 'zmmaps_temp' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'po_menge' SPACE '' SPACE SPACE '采购数量' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'bprme' SPACE 'EKPO' SPACE SPACE '采购订单单位' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'po_dlvdate' SPACE '' SPACE SPACE '订单交货日期' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'matnr' SPACE 'EKPO' SPACE SPACE '物料编号' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'maktx' SPACE 'MAKT' SPACE SPACE '物料描述' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'vonder' SPACE '' SPACE SPACE '供应商' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'vonder_name' SPACE '' SPACE SPACE '供应商名称' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'assign_menge' SPACE '' SPACE SPACE '匹配数量' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'meins' SPACE 'EKPO' SPACE SPACE '单位' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'plan_date' SPACE '' SPACE SPACE '计划日期' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'reqdate' SPACE '' SPACE SPACE '日期' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'st_code' SPACE '' SPACE SPACE '指定标准' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'version' SPACE '' SPACE SPACE '版本' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'werks' SPACE '' SPACE SPACE '工厂' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'werks_name' SPACE '' SPACE SPACE '送货至工厂' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'lgort' SPACE '' SPACE SPACE '库位' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'lgobe' SPACE '' SPACE SPACE '送货地点' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'ack_date' SPACE '' SPACE SPACE '确认送货日期' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           SPACE 'ack_menge' SPACE '' SPACE SPACE '确认数量' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT.
 *     IF p_r06 = 'X'.
 *       PERFORM frm_alv_set_fieldcat_pt USING:
 *           space 'PDLV_DATE_EXT' space 'ZMMAPS_TEMP' space space space space space space space space space space space space 'X' it_fieldcat.
 *
 *     ENDIF.
    ELSEIF P_R06 = 'X'.
      PERFORM FRM_ALV_SET_FIELDCAT_PT USING:
           'X' 'light   ' SPACE SPACE SPACE SPACE 'Light' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           'X' 'message ' SPACE SPACE SPACE SPACE 'Message' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           'X' 'DLV_PLANNO' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           'X' 'DLV_ITEMNO' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           'X' 'XCLOSED' SPACE 'ZMMWMS_POL' SPACE SPACE '关闭标识' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' IT_FIELDCAT,
           'X' 'update_flag' SPACE SPACE SPACE SPACE '修改标识' SPACE SPACE SPACE SPACE SPACE SPACE 'X' SPACE SPACE SPACE IT_FIELDCAT,
           'X' 'PZ' SPACE SPACE SPACE SPACE '已批准' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'REQDATE' SPACE 'zmmaps_temp' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'lifnr' SPACE 'ZMMWMS_POH' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'name1_gp' SPACE 'ZMMWMS_POH' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'ernam' SPACE 'ZMMWMS_POH' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'note' SPACE 'ZMMWMS_POH' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'EBELN' SPACE 'EKPO' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'EBELP' SPACE 'EKPO' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'MATNR' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'MAKTX' SPACE 'MAKT' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'MENGE' SPACE 'EKPO' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'RECEIVED_QUAN' SPACE '' SPACE SPACE '送货行已收数量' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'PORECEIVED_QUAN' SPACE '' SPACE SPACE 'PO收货数量' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'PDLV_DATE' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' SPACE SPACE IT_FIELDCAT,
           SPACE 'PDLV_DATE_EXT' SPACE 'zmmaps_temp' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' SPACE SPACE IT_FIELDCAT,
           SPACE 'PDLV_MENGE' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'ASSIGN_MENGE' SPACE 'zmmaps_temp' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE 'X' SPACE SPACE IT_FIELDCAT,
           SPACE 'ZKSHSL' SPACE SPACE SPACE SPACE TEXT-048 SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'VERSION' SPACE 'zmmaps_temp' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'REMAIN_DAYS' SPACE 'zmmaps_pl' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
             "ADD BY LANJINGCUN 20200426 最晚答复日期显示-start
             SPACE 'VERSIOM' SPACE SPACE SPACE SPACE TEXT-047 SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
             SPACE 'ZJSYUL' SPACE SPACE SPACE SPACE TEXT-045 SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
             SPACE 'ZZWDFR' SPACE SPACE SPACE SPACE TEXT-046 SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
            "ADD BY LANJINGCUN 20200426 最晚答复日期显示-end
           SPACE 'MEINS' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'WERKS' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'LGORT' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'PRODUCER' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'name1_p' 'NAME1' 'LFA1' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'MFRPN' SPACE 'ZMMWMS_POL' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'UEBTO' SPACE SPACE SPACE SPACE '过量交货(%)' SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT,
           SPACE 'ZBEIZ' 'ZBEIZ' 'ZMMWMS_TEMP' SPACE SPACE '备注' SPACE SPACE SPACE SPACE '1' SPACE SPACE SPACE SPACE SPACE IT_FIELDCAT."增加备注字段 add by lanjingcun 20200426
 *          space 'ZAPS_STS' space 'ZMMWMS_POL' space space space space space space space space space space space space space it_fieldcat.
 
    ENDIF.
 
  ENDFORM. " frm_alv_fill_name
 
 *&-------------------------------------------------------------------*
 *&      Form  frm_alv_set_fieldcat
 *&-------------------------------------------------------------------*
 *       Adds an entry to the field catalog.
 *--------------------------------------------------------------------*
  FORM              FRM_ALV_SET_FIELDCAT_PT USING UV_KEY TYPE ANY
                                   UV_FIELDNAME     TYPE ANY
                                   UV_REF_FIELD TYPE ANY
                                   UV_REF_TABLE   TYPE ANY
                                   UV_OUTPUTLEN     TYPE ANY
                                   UV_NOOUT         TYPE ANY
                                   UV_SCRTEXT_M     TYPE ANY
                                   UV_SCRTEXT_L     TYPE ANY
                                   UV_SCRTEXT_S     TYPE ANY
                                   UV_REPTEXT_DDIC  TYPE ANY
                                   UV_DDICTXT       TYPE ANY
                                   UV_HOTSPOT       TYPE ANY
                                   UV_SHOWASICON    TYPE ANY
                                   UV_CHECKBOX      TYPE ANY
                                   UV_EDIT          TYPE ANY
                                   UV_DOSUM         TYPE ANY
                                   UV_NO_ZERO       TYPE ANY
                                   CT_FIELDCAT      TYPE LVC_T_FCAT.
    DATA: LS_FIELDCAT  TYPE LVC_S_FCAT,
          LV_FNAME(20).
    CLEAR LS_FIELDCAT.
 
 * General settings
    IF UV_FIELDNAME IS NOT INITIAL.
      LV_FNAME = UV_FIELDNAME.
      TRANSLATE LV_FNAME TO UPPER CASE.
      LS_FIELDCAT-FIELDNAME = LV_FNAME.
    ELSE.
      EXIT.
    ENDIF.
 
    LS_FIELDCAT-KEY = UV_KEY.
 
    LS_FIELDCAT-NO_OUT = UV_NOOUT.
    LS_FIELDCAT-HOTSPOT = UV_HOTSPOT.
    LS_FIELDCAT-CHECKBOX = UV_CHECKBOX.
    LS_FIELDCAT-ICON = UV_SHOWASICON.
    LS_FIELDCAT-DO_SUM = UV_DOSUM.
 *  ls_fieldcat-fix_column = uv_fix_column.
    LS_FIELDCAT-NO_ZERO = UV_NO_ZERO.
 
    IF UV_REF_TABLE IS INITIAL.
      LS_FIELDCAT-ROLLNAME =   UV_REF_FIELD.
    ELSE.
      LS_FIELDCAT-REF_TABLE = UV_REF_TABLE.
      IF UV_REF_FIELD EQ SPACE.
        LS_FIELDCAT-REF_FIELD =   LS_FIELDCAT-FIELDNAME.
      ELSE.
        LS_FIELDCAT-REF_FIELD =   UV_REF_FIELD.
      ENDIF.
      TRANSLATE LS_FIELDCAT-REF_TABLE TO UPPER CASE.
    ENDIF.
 
 * Set output length.
    IF NOT UV_OUTPUTLEN IS INITIAL.
      LS_FIELDCAT-OUTPUTLEN = UV_OUTPUTLEN.
    ENDIF.
 
 * Set text headers.
    IF NOT UV_SCRTEXT_M IS INITIAL.
      LS_FIELDCAT-SCRTEXT_M = UV_SCRTEXT_M.
    ENDIF.
    IF NOT UV_SCRTEXT_L IS INITIAL.
      LS_FIELDCAT-SCRTEXT_L = UV_SCRTEXT_L.
    ENDIF.
    IF NOT UV_SCRTEXT_S IS INITIAL.
      LS_FIELDCAT-SCRTEXT_S = UV_SCRTEXT_S.
    ENDIF.
 *   IF NOT uv_reptext_ddic IS INITIAL.
 *     ls_fieldcat-reptext_ddic = uv_reptext_ddic.
 *   ENDIF.
 *   IF NOT uv_ddictxt IS INITIAL.
 *     ls_fieldcat-ddictxt = uv_ddictxt.
 *   ENDIF.
 
 * Set as editable or not.
    IF UV_EDIT IS NOT INITIAL.
 *     ls_fieldcat-input     = 'X'.
      LS_FIELDCAT-EDIT     = 'X'.
    ENDIF.
    APPEND LS_FIELDCAT TO CT_FIELDCAT.
  ENDFORM. "frm_alv_set_fieldcat
 
 **&---------------------------------------------------------------------*
 **&      Form  frm_sort_field
 **&---------------------------------------------------------------------*
 **       text
 **----------------------------------------------------------------------*
 **      -->PT_SOTR    text
 **----------------------------------------------------------------------*
 * FORM frm_sort_field CHANGING pt_sort TYPE slis_t_sortinfo_alv.
 *
 *   DEFINE set_sort.
 *     gs_sort-fieldname = &1.
 *     gs_sort-up        = &2.
 *     gs_sort-down      = &3.
 *     APPEND gs_sort TO &4.
 *     CLEAR  gs_sort.
 *   END-OF-DEFINITION.
 *
 **  set_sort:'ZDATE'  space 'X' pt_sort,
 **           'ZTIME'  space 'X' pt_sort,
 **           'DOCNUM' space 'X' pt_sort.
 *
 * ENDFORM. "frm_sort_field
 
 *&---------------------------------------------------------------------*
 *&      Form  frm_ALV_LIST_DISPLY
 *&---------------------------------------------------------------------*
 *       ALV显示
 *----------------------------------------------------------------------*
  FORM FRM_ALV_LIST_DISPLY TABLES PT_TABLE.
 
 *ALV显示
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        I_CALLBACK_PROGRAM       = SY-REPID
        I_CALLBACK_PF_STATUS_SET = GV_STATUS
        I_CALLBACK_USER_COMMAND  = GV_COMAND
        IT_FIELDCAT_LVC          = GT_FIELDCAT
        IS_LAYOUT_LVC            = GS_LAYOUT
        IT_EVENTS                = GT_EVENTS
        IT_FILTER_LVC            = GT_FILT "add by lanjingcun 20200522 设置筛选器
 *      i_callback_html_top_of_page = gv_top_of_page "HTML_TOP_OF_PAGE'
 *      i_grid_title             = gv_title
        I_SAVE                   = 'A'
      TABLES
        T_OUTTAB                 = PT_TABLE.
  ENDFORM. " frm_ALV_LIST_DISPLY
 
 *&---------------------------------------------------------------------*
 *&      Form  FRM_STANDARD_ALV
 *&---------------------------------------------------------------------*
 *       设置菜单
 *----------------------------------------------------------------------*
  FORM FRM_STANDARD_ALV USING UT_EXTAB TYPE SLIS_T_EXTAB.
 
    DEFINE SET_MENU.
      gs_status_tab = &1.
      APPEND gs_status_tab TO gt_status_tab.
    END-OF-DEFINITION.
 
    CASE 'X'.
      WHEN P_R05.
        SET_MENU 'SEND_SAVE'.
        SET_MENU 'CLOSE_POL'.
        SET_MENU 'SEND'.
        SET_MENU 'CLOSE_TEMP'.
      WHEN P_R06.
        SET_MENU 'COMMIT'.
        IF R_03 = ''.
          SET_MENU 'CLOSE_POL'.
        ENDIF.
        IF R_02 IS INITIAL.
          SET_MENU 'SEND'.
          SET_MENU 'CLOSE_TEMP'.
        ENDIF.
        IF R_01 = 'X'.
          SET_MENU 'SEND_SAVE'.
        ENDIF .
    ENDCASE.
 
    SET PF-STATUS 'PF001' EXCLUDING GT_STATUS_TAB.
 
 *  SET TITLEBAR 'TITLE_0100' WITH lv_title.
 
  ENDFORM. "FRM_STANDARD_ALV
 
 *&---------------------------------------------------------------------*
 *&      Form  FRM_USER_COMMAND
 *&---------------------------------------------------------------------*
 *       用户响应事件
 *----------------------------------------------------------------------*
  FORM FRM_USER_COMMAND USING UV_UCOMM LIKE SY-UCOMM
                                 US_SELFIELD  TYPE SLIS_SELFIELD.
 
    DATA : LV_LENG TYPE I .
 
    US_SELFIELD-REFRESH = 'X'.     "刷新
    US_SELFIELD-COL_STABLE = 'X'.  "列位置
    US_SELFIELD-ROW_STABLE = 'X'.  "行位置
 
    CASE UV_UCOMM.
      WHEN 'CLOSE_TEMP'.
        PERFORM FRM_CLOSE_TEMP.        "关闭匹配数据
      WHEN 'CLOSE_POL'.
        PERFORM FRM_CLOSE_POL.         "关闭交货计划
      WHEN 'COMMIT'.
        PERFORM FRM_COMMIT_TMP_DATA.   "提交临时数据
 *  双击
      WHEN '&IC1'.
        "ADD BY LANJINGCUN 20200426 增加备注 star
        IF P_R06 = 'X' .
          IF US_SELFIELD-FIELDNAME = 'ZBEIZ' .
            CLEAR GS_PDLV_DATA .
            READ TABLE GT_PDLV_DATA INTO GS_PDLV_DATA INDEX US_SELFIELD-TABINDEX .
            IF SY-SUBRC = 0 .
              PERFORM FRM_CALL_LONG  .
 
              PERFORM FRM_GET_LONGTEXT  USING
                      G_PR_LTXT
                      GS_PDLV_DATA-ZBEIZ .
              MODIFY GT_PDLV_DATA FROM GS_PDLV_DATA INDEX US_SELFIELD-TABINDEX .
              LV_LENG = STRLEN( GS_PDLV_DATA-ZBEIZ ) .
              IF LV_LENG > 200 .
                MESSAGE '注意：备注长度超过200，保存时会被截断' TYPE 'W' .
              ENDIF .
 
            ENDIF .
          ENDIF .
        ENDIF .
        "ADD BY LANJINGCUN 20200426 增加备注 end
      WHEN 'SEND_SAVE'.
        "UP BY LANJINGCUN 作废保存按钮 'SEND' -S
 *       PERFORM FRM_SEND_WMS_AND_SAVE. "发送WMS并保存数据
        IF P_R05 = 'X' .
          PERFORM FRM_SEND_WMS_AND_SAVE. "发送WMS并保存数据
        ELSE .
          IF R_01 = 'X'
              OR R_03 = 'X' .
            PERFORM FRM_SEND_WMS_AND_SAVE. "发送WMS并保存数据
          ELSE .
            PERFORM FRM_SEND_WMS. "下传WMS  "在该逻辑进行处理 下发的同时进行保存
          ENDIF .
        ENDIF .
        "UP BY LANJINGCUN 作废保存按钮 'SEND' -E
        "ADD BY LANJINGCUN 20200426 增加备注 star
        IF P_R06 = 'X' .
 
          LOOP AT GT_PDLV_DATA INTO DATA(LS_PDLV_DATA) WHERE SEL = 'X' .
            LS_PDLV_DATA-ZBEIZ = LS_PDLV_DATA-ZBEIZ+0(200) ."目前只获取200长度
            UPDATE ZMMAPS_TEMP
               SET ZBEIZ    = LS_PDLV_DATA-ZBEIZ
             WHERE GUID = LS_PDLV_DATA-GUID
               AND UNIQUEID = LS_PDLV_DATA-UNIQUEID.
          ENDLOOP .
 
        ENDIF .
        "ADD BY LANJINGCUN 20200426 增加备注 end
      WHEN 'SEND'.
        PERFORM FRM_SEND_WMS. "下传WMS
 
    ENDCASE.
 
  ENDFORM. "FRM_USER_COMMAND
 
 *&---------------------------------------------------------------------*
 *&      Form  frm_EVENT_BUILD
 *&---------------------------------------------------------------------*
 *       ALV事件
 *----------------------------------------------------------------------*
  FORM FRM_EVENT_BUILD USING UT_EVENTS TYPE SLIS_T_EVENT.
 
    CHECK P_R06 = 'X'.
 
    DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
 **添加客户化事件
    LS_EVENT-NAME = 'CALLER_EXIT'.
    LS_EVENT-FORM = 'FRM_DATA_CHANGE2'.
    APPEND LS_EVENT TO UT_EVENTS.
  ENDFORM. " frm_EVENT_BUILD
 
 *&---------------------------------------------------------------------*
 *&      Form  PF_DATA_CHANGE2
 *&---------------------------------------------------------------------*
 *      事件与类方法映射
 *----------------------------------------------------------------------*
  FORM FRM_DATA_CHANGE2 USING RS_DATA TYPE SLIS_DATA_CALLER_EXIT.
    DATA LR_GRID TYPE REF TO CL_GUI_ALV_GRID.
    IF LR_GRID IS INITIAL.
      DATA LR_EVENT TYPE REF TO CL_EVENT_RECEIVER.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          E_GRID = LR_GRID.
 
      CALL METHOD LR_GRID->REGISTER_EDIT_EVENT
        EXPORTING
          I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED
        EXCEPTIONS
          ERROR      = 1
          OTHERS     = 2.
 
      CREATE OBJECT LR_EVENT.
      SET HANDLER LR_EVENT->HANDLE_DATA_CHANGED FOR LR_GRID.
      SET HANDLER LR_EVENT->HANDLE_DATA_CHANGED_FINISHED FOR LR_GRID.
    ENDIF.
  ENDFORM.                    "PF_DATA_CHANGE
 *&---------------------------------------------------------------------*
 *& Form FRM_CALL_LONG
 *&---------------------------------------------------------------------*
 *& text
 *&---------------------------------------------------------------------*
 *&      <-- LS_DATA
 *&---------------------------------------------------------------------*
  FORM FRM_CALL_LONG .
 
 
    CALL   SCREEN 2000 STARTING AT 20 10
                         ENDING AT 60 20 .
 
  ENDFORM.
 *&---------------------------------------------------------------------*
 *&      Form  FRM_SET_LONGTEXT
 *&---------------------------------------------------------------------*
 *       显示长文本空间
 *----------------------------------------------------------------------*
  FORM FRM_SET_LONGTEXT
                      USING    PR_CON  TYPE REF TO CL_GUI_CUSTOM_CONTAINER
                               PR_LTXT TYPE REF TO CL_GUI_TEXTEDIT
 *                             PV_CNAM TYPE C
                               PV_EDIT TYPE C  "‘X’-可编辑，‘’不可编辑
                               PV_VALUE.
    DATA:LV_LENGTH TYPE I,
         LV_POS    TYPE I,
         LV_MODE   TYPE I.
    DATA:BEGIN OF LS_TABLE,
           LINE(C_LINE_LENGTH) TYPE C,
         END OF LS_TABLE,
         LT_TABLE LIKE TABLE OF LS_TABLE.
    DESCRIBE FIELD PV_VALUE LENGTH LV_LENGTH IN CHARACTER MODE.
    IF PR_CON IS INITIAL.
 *   创建容器
      CREATE OBJECT PR_CON
        EXPORTING
          CONTAINER_NAME              = 'ZZ01' "屏幕上创建自定义区域
        EXCEPTIONS
          CNTL_ERROR                  = 1
          CNTL_SYSTEM_ERROR           = 2
          CREATE_ERROR                = 3
          LIFETIME_ERROR              = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5
          OTHERS                      = 6.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
 *   创建文本框 ==
      CREATE OBJECT PR_LTXT
        EXPORTING
          MAX_NUMBER_CHARS           = LV_LENGTH      "可输入文本长度
          WORDWRAP_MODE              =
                                       CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
          WORDWRAP_POSITION          = '10' "行长度
          WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE
          PARENT                     = PR_CON
        EXCEPTIONS
          ERROR_CNTL_CREATE          = 1
          ERROR_CNTL_INIT            = 2
          ERROR_CNTL_LINK            = 3
          ERROR_DP_CREATE            = 4
          GUI_TYPE_NOT_SUPPORTED     = 5
          OTHERS                     = 6.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
    CHECK PR_LTXT IS NOT INITIAL.
    IF PV_EDIT EQ 'X'.
      LV_MODE   = 0.
    ELSE.
      LV_MODE   = 1.
    ENDIF.
    PR_LTXT->SET_READONLY_MODE(
      EXPORTING
        READONLY_MODE          = LV_MODE
      EXCEPTIONS
        ERROR_CNTL_CALL_METHOD = 1
        INVALID_PARAMETER      = 2
           ).
    IF SY-SUBRC <> 0.
 *   Implement suitable error handling here
    ENDIF.
 *设置显示文本 根据行长度进行拆分
    CLEAR:LT_TABLE[],LV_POS.
    DO.
      CLEAR:LS_TABLE.
      IF LV_POS >= LV_LENGTH.
        EXIT.
      ELSEIF ( LV_POS + C_LINE_LENGTH ) > LV_LENGTH.
        LS_TABLE-LINE = PV_VALUE+LV_POS.
      ELSE.
        LS_TABLE-LINE = PV_VALUE+LV_POS(C_LINE_LENGTH).
      ENDIF.
      APPEND LS_TABLE TO LT_TABLE.
      ADD C_LINE_LENGTH TO LV_POS.
    ENDDO.
    PR_LTXT->SET_TEXT_AS_R3TABLE(
      EXPORTING
        TABLE           = LT_TABLE
      EXCEPTIONS
        ERROR_DP        = 1
        ERROR_DP_CREATE = 2
           ).
    IF SY-SUBRC <> 0.
 *   Implement suitable error handling here
    ENDIF.
  ENDFORM.
 *&---------------------------------------------------------------------*
 *&      Form  FRM_GET_LONGTEXT
 *&---------------------------------------------------------------------*
 *       读取长文本
 *----------------------------------------------------------------------*
  FORM FRM_GET_LONGTEXT  USING
                               PR_LTXT TYPE REF TO CL_GUI_TEXTEDIT
                               PV_VALUE TYPE C.
    DATA:BEGIN OF LS_TABLE,
           LINE(C_LINE_LENGTH) TYPE C,
         END OF LS_TABLE,
         LT_TABLE LIKE TABLE OF LS_TABLE.
    PR_LTXT->GET_TEXT_AS_R3TABLE(
     IMPORTING
       TABLE           = LT_TABLE
     EXCEPTIONS
       ERROR_DP        = 1
       ERROR_DP_CREATE = 2
          ).
    IF SY-SUBRC <> 0.
 *   Implement suitable error handling here
    ELSE.
      CLEAR:PV_VALUE.
      LOOP AT LT_TABLE INTO LS_TABLE.
        CONCATENATE PV_VALUE LS_TABLE-LINE INTO PV_VALUE.
      ENDLOOP.
    ENDIF.
  ENDFORM.
 
 *&---------------------------------------------------------------------*
 *& Module STATUS_2000 OUTPUT
 *&---------------------------------------------------------------------*
 *&
 *&---------------------------------------------------------------------*
  MODULE STATUS_2000 OUTPUT.
    SET PF-STATUS 'STATUS_2000'.
 * SET TITLEBAR 'xxx'.
 
    PERFORM FRM_SET_LONGTEXT USING G_PR_CON
                                   G_PR_LTXT
 *                               'Z' "G_PV_CNAM
                                   G_PV_EDIT
                                   GS_PDLV_DATA-ZBEIZ ."传入的值内容
  ENDMODULE.