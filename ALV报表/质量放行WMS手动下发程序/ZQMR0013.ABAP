*&---------------------------------------------------------------------*
*& Report ZQMR0013
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zqmr0013.
*&---------------------------------------------------------------------*
*& 程序名：ZQMR0013
*&作者：Seashell Huang
*&模块：QM模块
*&创建日期：11.10.2019 14:48:51
*&功能描述：接口异步小程序
*&---------------------------------------------------------------------*
*&修改记录：
*&
*&---------------------------------------------------------------------*
TABLES:mseg,zqmrel.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名

" 选择屏幕所需要的字段列表
TYPES:BEGIN OF lv_select,
        pmblnr TYPE    mseg-mblnr  , "  物料凭证
        pmjahr TYPE    mseg-mjahr  , "  物料凭证
        pzeile TYPE    mseg-zeile  , "  物料凭证
      END OF lv_select.
"ALV界面所需要的字段列表
TYPES:BEGIN OF ty_out.
TYPES:wms_custid TYPE    string  , "  WMS货主ID
      mblnr      TYPE    string  , "  物料凭证编号
      mjahr      TYPE    string  , "  物料凭证的年份
      zeile      TYPE    string  , "  物料凭证中的项目
      bus_type   TYPE    string  , "  业务类型
      bwart      TYPE    string  , "  移动类型
      werks      TYPE    string  , "  工厂
      lgort      TYPE    string  , "  发出库存地点
      note       TYPE    string  , "  备注
      matnr      TYPE    string  , "  物料号
      maktx      TYPE    string  , "  物料名称
      charg      TYPE    string  , "  SAP批号
      wms_ext_st TYPE    string  , "  WMS状态控制字
      qmrel_quan TYPE    string  , "  数量
      dlgort     TYPE    string  , "  目标库存地点
      stock_st   TYPE    string  , "  库存状态
      meins      TYPE    string  , "  计量单位
      mseht      TYPE    string  , "  计量单位文本
      END OF ty_out.
DATA:lt_mseg TYPE TABLE OF mseg,
     ls_mseg TYPE mseg.
DATA:lv_e_objek TYPE  cuobn,
     lv_e_obtab TYPE  tabelle,
     lv_e_klart TYPE  klassenart,
     lv_e_class TYPE  klasse_d.

DATA:lv_objectkey      TYPE  bapi1003_key-object,
     lv_objecttable    TYPE  bapi1003_key-objecttable,
     lv_classnum       TYPE  bapi1003_key-classnum,
     lv_classtype      TYPE  bapi1003_key-classtype,
     lv_objectkey_long TYPE  bapi1003_key-object_long.

DATA: ls_key             TYPE bapi1003_key,
      lv_status          TYPE bapi1003_key-status,
      lt_allocvaluesnum  TYPE TABLE OF bapi1003_alloc_values_num,
      ls_allocvaluesnum  TYPE bapi1003_alloc_values_num,
      lt_allocvalueschar TYPE TABLE OF bapi1003_alloc_values_char,
      ls_allocvalueschar TYPE bapi1003_alloc_values_char,
      lt_allocvaluescurr TYPE TABLE OF bapi1003_alloc_values_curr,
      ls_allocvaluescurr TYPE bapi1003_alloc_values_curr,
      lt_return          TYPE TABLE OF bapiret2,
      ls_return          TYPE bapiret2.
DATA:lt_out    TYPE TABLE OF ty_out,
     ls_out    TYPE ty_out,
     lt_makt   TYPE TABLE OF makt,
     ls_makt   TYPE makt,
     ls_zqmrel TYPE zqmrel,
     lt_zqmrel TYPE TABLE OF zqmrel,
     ls_t006a  TYPE t006a.
DATA:ls_zmmwms_ext_st TYPE zmmwms_ext_st.
DATA: lv_intf_id(6) TYPE c,
      lv_guid       TYPE sxmsguid,
      lv_err_info   TYPE bapi_msg.
DATA:ls_output      TYPE zdt_wms_qmrel_ret,
     lr_service     TYPE REF TO zco_si_sap2wms_qmrel_out,
     ls_output_main TYPE zmt_wms_qmrel,
     ls_output2     TYPE zdt_wms_subk,
     ls_input       TYPE zmt_wms_qmrel_ret,
     lt_output      TYPE zdt_wms_qmrel_linesofqm_tab,
     ls_output_line TYPE zdt_wms_qmrel_linesofqm.
CONSTANTS: gco_callback_status     TYPE slis_formname  VALUE 'FRM_USER_STATUS'.

START-OF-SELECTION.
  SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
  SELECT-OPTIONS:pmblnr FOR zqmrel-mblnr  , "  物料凭证
                 pmjahr FOR zqmrel-mjahr  , "  物料凭证年度
                 pzeile FOR zqmrel-zeile  , "  物料凭证项目
                 pdatum FOR zqmrel-datum,"调用接口的日期
                 puzeit FOR zqmrel-uzeit."调用接口的时间

  SELECTION-SCREEN END OF BLOCK blk.
  SET PF-STATUS '0100'.
  PERFORM get_data.
  PERFORM catalog.
  PERFORM alvshow.

END-OF-SELECTION.


FORM get_data.
*  SELECT
*    *
*  INTO CORRESPONDING FIELDS OF TABLE lt_mseg
*  FROM mseg
*  WHERE mseg~mblnr IN pmblnr
*  AND mseg~mjahr IN pmjahr
*  AND mseg~zeile IN pzeile.
*
*
*  LOOP AT lt_mseg INTO ls_mseg.
*    "当移动类型为321/350/331时
*    IF ls_mseg-bwart = '321' OR ls_mseg-bwart = '350' OR ls_mseg-bwart = '331'.
*      "当移动类型为321或者350时，取SHKZG = 'S''
*      IF ls_mseg-bwart = '321' OR ls_mseg-bwart = '350'.
*        ls_mseg-shkzg = 'S'.
*      ENDIF.
*
*      " WMS_CUSTID  WMS货主ID
*      ls_out-wms_custid = ls_mseg-werks.
*      "  MBLNR 物料凭证编号
*      ls_out-mblnr = ls_mseg-mblnr.
*      "  MJAHR 物料凭证的年份
*      ls_out-mjahr = ls_mseg-mjahr.
*      "  ZEILE 物料凭证中的项目
*      ls_out-zeile = ls_mseg-zeile.
*      " BUS_TYPE  业务类型
*      ls_out-bus_type = 'C03'.
*      "移动类型BWART
*      ls_out-bwart = ls_mseg-bwart.
*
*      "  WERKS 工厂
*      ls_out-werks = ls_mseg-werks.
*      "  LGORT 发出库存地点
*      ls_out-lgort = ls_mseg-lgort.
*      " NOTE  备注
*
*      "  MAKTX 物料名称
*      SELECT SINGLE
*      maktx
*      INTO CORRESPONDING FIELDS OF ls_makt
*      FROM makt
*      WHERE makt~spras = '1'
*      AND makt~matnr = ls_mseg-matnr.
*      ls_out-maktx = ls_makt-maktx.
*      "  MATNR 物料号
*      ls_mseg-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_mseg-matnr i_out = 'X' ).
*      ls_out-matnr = ls_mseg-matnr.
*      "  CHARG SAP批号
*      ls_out-charg = ls_mseg-charg.
*      " WMS_EXT_ST  WMS状态控制字
*      "外寄标识ZOUT_X
*      "质量标准ZST_CODE
*      "XCONSIGN寄售库存标记
*      IF ls_mseg-matnr IS NOT INITIAL AND ls_mseg-werks IS NOT INITIAL AND ls_mseg-charg IS NOT INITIAL.
*        CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
*          EXPORTING
*            i_matnr = ls_mseg-matnr
*            i_charg = ls_mseg-charg
*            i_werks = ls_mseg-werks
*          IMPORTING
*            e_objek = lv_e_objek
*            e_obtab = lv_e_obtab
*            e_klart = lv_e_klart
*            e_class = lv_e_class.
***          获取批次对象的属性信息
*        IF sy-subrc = 0.
*          lv_objectkey  =    lv_e_objek.
*          lv_objecttable   = lv_e_obtab.
*          lv_classnum  = lv_e_class.
*          lv_classtype  = lv_e_klart.
*          lv_objectkey_long = lv_e_objek.
*          CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*            EXPORTING
*              objectkey       = lv_objectkey
*              objecttable     = lv_objecttable
*              classnum        = lv_classnum
*              classtype       = lv_classtype
*              objectkey_long  = lv_objectkey_long
*            TABLES
*              allocvaluesnum  = lt_allocvaluesnum
*              allocvalueschar = lt_allocvalueschar
*              allocvaluescurr = lt_allocvaluescurr
*              return          = lt_return.
*          IF lt_return IS NOT INITIAL AND lt_return[ 1 ]-type = 'S'.
*          ENDIF.
*        ENDIF.
*        "质量标识
*        READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZST_CODE'.
*        IF sy-subrc = 0.
*          ls_zmmwms_ext_st-zst_code = ls_allocvalueschar-value_char.
*        ENDIF.
*        "外寄标识
*        READ TABLE lt_allocvalueschar INTO ls_allocvalueschar WITH KEY charact = 'ZOUT_X'.
*        IF sy-subrc = 0.
*          ls_zmmwms_ext_st-zout_x = ls_allocvalueschar-value_char.
*        ENDIF.
*        " 获取寄售标识
*        SELECT SINGLE
*        sobkz
*        INTO ls_zmmwms_ext_st-xconsign
*        FROM mkol
*        WHERE mkol~matnr = ls_mseg-matnr
*        AND mkol~werks = ls_mseg-werks
*        AND mkol~charg = ls_mseg-charg.
*
*        SELECT SINGLE
*        wms_ext_st
*        INTO ls_zmmwms_ext_st-wms_ext_st
*        FROM zmmwms_ext_st
*        WHERE zmmwms_ext_st~zout_x = ls_zmmwms_ext_st-zout_x
*        AND zmmwms_ext_st~zst_code = ls_zmmwms_ext_st-zst_code
*        AND zmmwms_ext_st~xconsign = ls_zmmwms_ext_st-xconsign.
*
*        ls_out-wms_ext_st = ls_zmmwms_ext_st-wms_ext_st.
*      ENDIF.
*
*      " QMREL_QUAN  数量
*      ls_out-qmrel_quan = ls_mseg-menge.
*      " DLGORT  目标库存地点
*      ls_out-dlgort = ls_mseg-umlgo.
*      " STOCK_ST  库存状态
*      "空-非限制321
*      IF ls_mseg-bwart = '321'.
*        ls_out-stock_st = ''.
*        "B-冻结350
*      ELSEIF ls_mseg-bwart = '331'.
*        ls_out-stock_st = 'B'.
*        "331-C
*      ELSEIF ls_mseg-bwart = '331'.
*        ls_out-stock_st = 'C'.
*      ENDIF.
*      "  MEINS 计量单位
*      ls_out-meins = ls_mseg-meins.
*      "  MSEHT 计量单位文本
*      SELECT SINGLE
*      mseht
*      INTO CORRESPONDING FIELDS OF ls_t006a
*      FROM t006a
*      WHERE t006a~spras = '1'
*      AND t006a~msehi = ls_mseg-meins.
*      ls_out-mseht = ls_t006a-mseht.
*      APPEND ls_out TO lt_out.
*      MOVE-CORRESPONDING ls_out TO ls_output_line.
*      APPEND ls_output_line TO ls_output_main-mt_wms_qmrel-linesofqm.
*
*    ENDIF.
*  ENDLOOP.
*



  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_zqmrel
    FROM zqmrel
  WHERE zqmrel~mjahr IN pmjahr
  AND zqmrel~zeile IN pzeile
  AND zqmrel~datum IN pdatum
  AND zqmrel~uzeit IN puzeit
  AND zqmrel~flag = '失败'.
  LOOP AT lt_zqmrel INTO ls_zqmrel.
    MOVE-CORRESPONDING ls_zqmrel TO ls_output_line.
    APPEND ls_output_line TO ls_output_main-mt_wms_qmrel-linesofqm.
    CLEAR ls_zqmrel.
  ENDLOOP.

ENDFORM.

FORM catalog.
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'L'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.

  fieldcatset     'wms_custid'    'WMS货主ID' sy-tabix.
  fieldcatset     'mblnr'    '物料凭证编号' sy-tabix.
  fieldcatset     'mjahr'    '物料凭证的年份' sy-tabix.
  fieldcatset     'zeile'    '物料凭证中的项目' sy-tabix.
  fieldcatset     'bus_type'    '业务类型' sy-tabix.
  fieldcatset     'bwart'    '移动类型' sy-tabix.
  fieldcatset     'werks'    '工厂' sy-tabix.
  fieldcatset     'lgort'    '发出库存地点' sy-tabix.
  fieldcatset     'note '    '备注' sy-tabix.
  fieldcatset     'matnr'    '物料号' sy-tabix.
  fieldcatset     'maktx'    '物料名称' sy-tabix.
  fieldcatset     'charg'    'SAP批号' sy-tabix.
  fieldcatset     'wms_ext_st'    'WMS状态控制字' sy-tabix.
  fieldcatset     'qmrel_quan'    '数量' sy-tabix.
  fieldcatset     'dlgort'    '目标库存地点' sy-tabix.
  fieldcatset     'stock_st'    '库存状态' sy-tabix.
  fieldcatset     'meins'    '计量单位' sy-tabix.
  fieldcatset     'ZRSV01'    '备用字段1' sy-tabix.
  fieldcatset     'ZRSV02'    '备用字段2' sy-tabix.
  fieldcatset     'ZRSV03'    '备用字段3' sy-tabix.
  fieldcatset     'ZRSV04'    '备用字段4' sy-tabix.
  fieldcatset     'ZRSV05'    '备用字段5' sy-tabix.
  fieldcatset     'FLAG'    '成功失败标识' sy-tabix.
  fieldcatset     'DATUM'    '放行日期' sy-tabix.
  fieldcatset     'UZEIT'    '放行时间' sy-tabix.




ENDFORM.

FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = w_repid "程序名称
      i_callback_pf_status_set = 'FRM_USER_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = '质量放行接口报表' "标题名
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
      t_outtab                 = lt_zqmrel
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
FORM frm_user_status  USING i_it_extab TYPE slis_t_extab.
  SET PF-STATUS '0100' .
ENDFORM.                    " FRM_SET_PF_STATUS

FORM frm_user_command USING i_ucomm       TYPE sy-ucomm
      i_wa_selfield TYPE slis_selfield.
  CASE i_ucomm.
    WHEN 'POP'."调用接口
      DATA: lv_error           TYPE c,

            lo_api             TYPE REF TO zco_si_sap2wms_qmrel_out,
            lo_system_fault    TYPE REF TO cx_ai_system_fault,
            lo_async_messaging TYPE REF TO if_wsprotocol_async_messaging,
            l_ack_request      TYPE prx_ack_request_details.
      CREATE OBJECT lo_api.
      TRY.
*      CALL METHOD lo_api->si_wms_subk_ret
          IF NOT zcl_bc_public=>int_actived('MM041').
            MESSAGE '接口未启用' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
          CALL METHOD lo_api->si_sap2wms_qmrel_out
            EXPORTING
              output = ls_output_main
            IMPORTING
              input  = ls_input.
        CATCH cx_ai_system_fault INTO lo_system_fault.
          lv_error = 'E'.
      ENDTRY.
      IF lv_error <> 'E' AND ls_input-mt_wms_qmrel_ret-returnflag = '1'.
        READ TABLE lt_zqmrel INTO ls_zqmrel INDEX 1.
        DELETE zqmrel FROM ls_zqmrel.
        MESSAGE '数据下发成功！'&& i_ucomm TYPE 'S'.

      ENDIF.
    WHEN OTHERS.
      WRITE i_ucomm.
  ENDCASE.
ENDFORM.