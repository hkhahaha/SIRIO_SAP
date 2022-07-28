*----------------------------------------------------------------------*
***INCLUDE LZPP001F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_SAVE_LONG_TEXT
*&---------------------------------------------------------------------*
*& 创建订单调用
*&---------------------------------------------------------------------*
*&      --> LV_AUFNR
*&      --> is_input_ZTEXT
*&---------------------------------------------------------------------*
FORM frm_save_long_text  USING iv_aufnr TYPE aufnr
                                iv_ztext TYPE string
                         CHANGING cs_log TYPE zpitlog0004.
  DATA ls_header TYPE thead.
  DATA lt_lines TYPE TABLE OF tline.
  DATA lt_text TYPE TABLE OF tdline.
  DATA lv_message TYPE char200.
  DATA lt_return TYPE TABLE OF bdcmsgcoll.
  DATA lv_subrc TYPE syst_subrc.
  DATA lv_bdc_aufnr TYPE bdcdata-fval.
  DATA lv_bdc_textline2 TYPE bdcdata-fval.
  DATA lv_bdc_textline3 TYPE bdcdata-fval.
  DATA lv_bdc_textline4 TYPE bdcdata-fval.
  DATA lv_flag_changeable TYPE flag.

  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      i_string         = iv_ztext
      i_tabline_length = '79'
    TABLES
      et_table         = lt_text.

  LOOP AT lt_text INTO DATA(lv_text).
    CASE sy-tabix.
      WHEN '1'.
        lv_bdc_textline2 = lv_text.
      WHEN '2'.
        lv_bdc_textline3 = lv_text.
      WHEN '3'.
        lv_bdc_textline4 = lv_text.
    ENDCASE.
  ENDLOOP.

  lv_bdc_aufnr = iv_aufnr.
  "检查确认订单可以编辑
  DO 10 TIMES.
    CALL FUNCTION 'CO_ZF_ORDER_LOCK'
      EXPORTING
        aufnr                = iv_aufnr
      EXCEPTIONS
        order_already_locked = 1
        system_failure       = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      WAIT UP TO '0.1' SECONDS.
    ELSE.
      CALL FUNCTION 'CO_ZF_ORDER_DELOCK'
        EXPORTING
          aufnr                = iv_aufnr
        EXCEPTIONS
          order_already_locked = 1
          system_failure       = 2
          OTHERS               = 3.
      lv_flag_changeable = 'X'.
      EXIT.
    ENDIF.
  ENDDO.

  IF lv_flag_changeable <> 'X'.
    cs_log-type = 'W'.
    IF cs_log-message IS INITIAL.
      cs_log-message = '订单' && iv_aufnr && '长文本更新失败'.
    ELSE.
      lv_message = '订单' && iv_aufnr && '长文本更新失败'.
      CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
    ENDIF.
    RETURN."无法更改文本，退出当前程序段
  ENDIF.

  "调用COR2编辑保存长文本
  CALL FUNCTION 'Z_PP_BDC_PROORDER_LONGTEXT'
    EXPORTING
      aufnr_001     = lv_bdc_aufnr
      txline_03_003 = lv_bdc_textline2
      txline_04_005 = lv_bdc_textline3
      txline_05_007 = lv_bdc_textline4
    IMPORTING
      subrc         = lv_subrc
    TABLES
      messtab       = lt_return.

  IF lv_subrc <> 0.
    cs_log-type = 'W'.
    IF cs_log-message IS INITIAL.
      cs_log-message = '订单' && iv_aufnr && '长文本更新失败'.
    ELSE.
      lv_message = '订单' && iv_aufnr && '长文本更新失败'.
      CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
    ENDIF.
  ELSE.
    LOOP AT lt_return INTO DATA(ls_return) WHERE msgtyp = 'E' OR msgtyp = 'A'.
      cs_log-type = 'W'.
      IF cs_log-message IS INITIAL.
        cs_log-message = '订单' && iv_aufnr && '长文本更新失败'.
      ELSE.
        lv_message = '订单' && iv_aufnr && '长文本更新失败'.
        CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
      ENDIF.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SAVE_LONG_TEXT
*&---------------------------------------------------------------------*
*& 创建子订单调用
*&---------------------------------------------------------------------*
*&      --> LV_AUFNR
*&      --> is_input_ZTEXT
*&---------------------------------------------------------------------*
FORM frm_save_suborder_longtext  USING iv_aufnr TYPE aufnr
                                iv_ztext TYPE string
                         CHANGING cs_log TYPE zppt0001_subs.

  "本子程序和frm_save_long_text 的区别在于CS_LOG结构不同
  DATA ls_header TYPE thead.
  DATA lt_lines TYPE TABLE OF tline.
  DATA lt_text TYPE TABLE OF tdline.
  DATA lv_message TYPE char200.
  DATA lt_return TYPE TABLE OF bdcmsgcoll.
  DATA lv_subrc TYPE syst_subrc.
  DATA lv_bdc_aufnr TYPE bdcdata-fval.
  DATA lv_bdc_textline2 TYPE bdcdata-fval.
  DATA lv_bdc_textline3 TYPE bdcdata-fval.
  DATA lv_bdc_textline4 TYPE bdcdata-fval.
  DATA lv_flag_changeable TYPE flag.

  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      i_string         = iv_ztext
      i_tabline_length = '79'
    TABLES
      et_table         = lt_text.

  LOOP AT lt_text INTO DATA(lv_text).
    CASE sy-tabix.
      WHEN '1'.
        lv_bdc_textline2 = lv_text.
      WHEN '2'.
        lv_bdc_textline3 = lv_text.
      WHEN '3'.
        lv_bdc_textline4 = lv_text.
    ENDCASE.
  ENDLOOP.

  lv_bdc_aufnr = iv_aufnr.
  "检查确认订单可以编辑
  DO 10 TIMES.
    CALL FUNCTION 'CO_ZF_ORDER_LOCK'
      EXPORTING
        aufnr                = iv_aufnr
      EXCEPTIONS
        order_already_locked = 1
        system_failure       = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      WAIT UP TO '0.1' SECONDS.
    ELSE.
      CALL FUNCTION 'CO_ZF_ORDER_DELOCK'
        EXPORTING
          aufnr                = iv_aufnr
        EXCEPTIONS
          order_already_locked = 1
          system_failure       = 2
          OTHERS               = 3.
      lv_flag_changeable = 'X'.
      EXIT.
    ENDIF.
  ENDDO.

  IF lv_flag_changeable <> 'X'.
    cs_log-type = 'W'.
    IF cs_log-message IS INITIAL.
      cs_log-message = '订单' && iv_aufnr && '长文本更新失败'.
    ELSE.
      lv_message = '订单' && iv_aufnr && '长文本更新失败'.
      CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
    ENDIF.
    RETURN."无法更改文本，退出当前程序段
  ENDIF.

  "调用COR2编辑保存长文本
  CALL FUNCTION 'Z_PP_BDC_PROORDER_LONGTEXT'
    EXPORTING
      aufnr_001     = lv_bdc_aufnr
      txline_03_003 = lv_bdc_textline2
      txline_04_005 = lv_bdc_textline3
      txline_05_007 = lv_bdc_textline4
    IMPORTING
      subrc         = lv_subrc
    TABLES
      messtab       = lt_return.

  IF lv_subrc <> 0.
    cs_log-type = 'W'.
    IF cs_log-message IS INITIAL.
      cs_log-message = '订单' && iv_aufnr && '长文本更新失败'.
    ELSE.
      lv_message = '订单' && iv_aufnr && '长文本更新失败'.
      CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
    ENDIF.
  ELSE.
    LOOP AT lt_return INTO DATA(ls_return) WHERE msgtyp = 'E' OR msgtyp = 'A'.
      cs_log-type = 'W'.
      IF cs_log-message IS INITIAL.
        cs_log-message = '订单' && iv_aufnr && '长文本更新失败'.
      ELSE.
        lv_message = '订单' && iv_aufnr && '长文本更新失败'.
        CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
      ENDIF.
      EXIT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SAVE_LONG_TEXT
*&---------------------------------------------------------------------*
*& 修改订单接口调用
*&---------------------------------------------------------------------*
*&      --> LV_AUFNR
*&      --> is_input_ZTEXT
*&---------------------------------------------------------------------*
FORM frm_save_long_text_change  USING iv_aufnr TYPE aufnr"订单号
                                      iv_aufnr_ref TYPE aufnr"相关的（S母或拆分前m）订单
                                      iv_ztext TYPE string
                                      is_log TYPE zpitlog0005
                         CHANGING cs_log TYPE zpitlog0005.
  DATA ls_header TYPE thead.
  DATA lt_lines TYPE TABLE OF tline.
  DATA lt_text TYPE TABLE OF tdline.
  DATA lv_message TYPE char200.

************  APPEND INITIAL LINE TO ct_log ASSIGNING FIELD-SYMBOL(<fs_log>).
  ls_header-tdobject = 'AUFK'.
  ls_header-tdname = sy-mandt && iv_aufnr.
  ls_header-tdid = 'KOPF'.
  ls_header-tdspras = sy-langu.

  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      i_string         = iv_ztext
      i_tabline_length = '79'
    TABLES
      et_table         = lt_text.
  "将新的文本第一行初始化为空
  APPEND INITIAL LINE TO lt_lines ASSIGNING FIELD-SYMBOL(<fs_line>).
  <fs_line>-tdformat = '*'.

  LOOP AT lt_text INTO DATA(lv_text).
    APPEND INITIAL LINE TO lt_lines ASSIGNING <fs_line>.
    <fs_line>-tdformat = '*'.
    <fs_line>-tdline = lv_text.
  ENDLOOP.
*---初始化相关订单消息行
***********  CHECK <fs_log> IS ASSIGNED.
***********  MOVE-CORRESPONDING is_log TO <fs_log>.
  cs_log-aufnr_ref = iv_aufnr_ref."相关（母）订单

*---保存长文本更新
  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = ls_header
      savemode_direct = 'X'
    TABLES
      lines           = lt_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.
  IF sy-subrc <> 0."长文本更新失败，日志记录为警告，不影响回传APS的正确消息
    cs_log-type = 'W'.
    lv_message = '订单' && iv_aufnr && '更新成功但长文本更新失败'.
    CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
  ELSE."长文本更新成功
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_RELEASE_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> cs_log_AUFNR
*&      <-- cs_log
*&---------------------------------------------------------------------*
FORM frm_release_order  USING   iv_aufnr TYPE aufnr
                        CHANGING cs_log TYPE zpitlog0004.
  DATA ls_order  TYPE bapi_order_key.
  DATA lt_order  TYPE STANDARD TABLE OF bapi_order_key.
  DATA ls_return TYPE bapiret2.
  DATA lv_message TYPE char200.
  DATA lv_objnr TYPE j_objnr.
  DATA lv_stat TYPE j_status.

  ls_order-order_number = iv_aufnr.
  APPEND ls_order TO lt_order.
  CALL FUNCTION 'BAPI_PROCORD_RELEASE'
    IMPORTING
      return = ls_return
    TABLES
      orders = lt_order.
  IF ls_return-type = 'E' OR ls_return-type = 'A' .
    cs_log-type = 'W'.
    IF cs_log-message IS INITIAL.
      cs_log-message = '订单' && iv_aufnr && '下达失败'.
    ELSE.
      lv_message = '订单' && iv_aufnr && '下达失败'.
      CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    "检查确保释放成功
    lv_objnr = 'OR' && iv_aufnr.
    SELECT SINGLE stat FROM jest INTO lv_stat
      WHERE objnr = lv_objnr
        AND stat = 'I0002'."已释放
    IF sy-subrc <> 0.
      DO 10 TIMES.
        WAIT UP TO '0.1' SECONDS.
        CALL FUNCTION 'BAPI_PROCORD_RELEASE'
          IMPORTING
            return = ls_return
          TABLES
            orders = lt_order.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        SELECT SINGLE stat FROM jest INTO lv_stat
          WHERE objnr = lv_objnr
            AND stat = 'I0002'."已释放
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDDO.
      IF sy-subrc <> 0.
        cs_log-type = 'W'.
        IF cs_log-message IS INITIAL.
          cs_log-message = '订单' && iv_aufnr && '下达失败'.
        ELSE.
          lv_message = '订单' && iv_aufnr && '下达失败'.
          CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
FORM frm_release_suborder  USING   iv_aufnr TYPE aufnr
                        CHANGING cs_log TYPE zppt0001_subs.
  DATA ls_order  TYPE bapi_order_key.
  DATA lt_order  TYPE STANDARD TABLE OF bapi_order_key.
  DATA ls_return TYPE bapiret2.
  DATA lv_message TYPE char200.

  ls_order-order_number = iv_aufnr.
  APPEND ls_order TO lt_order.
  CALL FUNCTION 'BAPI_PROCORD_RELEASE'
    IMPORTING
      return = ls_return
    TABLES
      orders = lt_order.
  IF ls_return-type = 'E' OR ls_return-type = 'A' .
    cs_log-type = 'W'.
    IF cs_log-message IS INITIAL.
      cs_log-message = '订单' && iv_aufnr && '下达失败'.
    ELSE.
      lv_message = '订单' && iv_aufnr && '下达失败'.
      CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_RELEASE_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> cs_log_AUFNR
*&      <-- cs_log
*&---------------------------------------------------------------------*
FORM frm_release_order_chg  USING   iv_aufnr TYPE aufnr
                        CHANGING cs_log TYPE zpitlog0005.
  DATA ls_order  TYPE bapi_order_key.
  DATA lt_order  TYPE STANDARD TABLE OF bapi_order_key.
  DATA ls_return TYPE bapiret2.
  DATA lv_message TYPE char200.
  DATA lv_objnr TYPE j_objnr.
  DATA lv_stat TYPE j_status.

  ls_order-order_number = iv_aufnr.
  APPEND ls_order TO lt_order.
  CALL FUNCTION 'BAPI_PROCORD_RELEASE'
    IMPORTING
      return = ls_return
    TABLES
      orders = lt_order.
  IF ls_return-type = 'E' OR ls_return-type = 'A' .
    cs_log-type = 'W'.
    IF cs_log-message IS INITIAL.
      cs_log-message = '订单' && iv_aufnr && '下达失败'.
    ELSE.
      lv_message = '订单' && iv_aufnr && '下达失败'.
      CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    "检查确保释放成功
    lv_objnr = 'OR' && iv_aufnr.
    SELECT SINGLE stat FROM jest INTO lv_stat
      WHERE objnr = lv_objnr
        AND stat = 'I0002'."已释放
    IF sy-subrc <> 0.
      DO 10 TIMES.
        WAIT UP TO '0.1' SECONDS.
        CALL FUNCTION 'BAPI_PROCORD_RELEASE'
          IMPORTING
            return = ls_return
          TABLES
            orders = lt_order.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        SELECT SINGLE stat FROM jest INTO lv_stat
          WHERE objnr = lv_objnr
            AND stat = 'I0002'."已释放
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDDO.
      IF sy-subrc <> 0.
        cs_log-type = 'W'.
        IF cs_log-message IS INITIAL.
          cs_log-message = '订单' && iv_aufnr && '下达失败'.
        ELSE.
          lv_message = '订单' && iv_aufnr && '下达失败'.
          CONCATENATE cs_log-message lv_message INTO cs_log-message SEPARATED BY ','.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_PROCESS_ORDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> is_input
*&      <-- cs_output
*&      <-- cs_log
*&---------------------------------------------------------------------*
FORM frm_create_process_order  USING  is_input TYPE zdt_aps2sap_pro_pro_req
                               CHANGING cv_error TYPE flag
                                        cv_aufnr TYPE aufnr
                                        cs_output TYPE zdt_aps2sap_pro_ret_pro_ret
                                        cs_log TYPE zpitlog0004.
  DATA ls_return TYPE bapiret2.
  DATA lv_aufnr TYPE aufnr.
  DATA ls_order_data TYPE bapi_pi_order_create.
  DATA lv_vbeln TYPE vbeln.
  DATA lv_ausss TYPE ausss.
  DATA lv_andec TYPE andec.
  CALL FUNCTION 'CONVERSION_EXIT_MATN5_INPUT'
    EXPORTING
      input        = is_input-matnr
    IMPORTING
      output       = ls_order_data-material
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  ls_order_data-plant = is_input-werks.
  ls_order_data-order_type = is_input-auart.
  ls_order_data-basic_start_date = is_input-gstrp.
  ls_order_data-basic_end_date = is_input-gltrp.
  SELECT SINGLE ausss FROM marc INTO lv_ausss
    WHERE matnr = ls_order_data-material
      AND werks = ls_order_data-plant.
  SELECT SINGLE andec
    FROM t006 INNER JOIN mara ON t006~msehi = mara~meins
    INTO lv_andec
   WHERE mara~matnr = ls_order_data-material.
  IF lv_ausss <> 0 AND is_input-zsfs = 'S'."有装配报废
*    IF lv_andec = 0."该单位不支持小数位,计算结果向上取整
*      ls_order_data-scrap_quantity = ceil( is_input-gamng * lv_ausss / 100 ).
*      ls_order_data-quantity = ceil( is_input-gamng + ls_order_data-scrap_quantity ).
*      cs_log-gamng = ls_order_data-quantity."更新S原始母订单的数量
*    ELSE."该单位支持小数位,直接计算即可
*      ls_order_data-scrap_quantity = is_input-gamng * lv_ausss / 100.
*      ls_order_data-quantity = is_input-gamng + ls_order_data-scrap_quantity.
*      cs_log-gamng = ls_order_data-quantity."更新S原始母订单的数量
*    ENDIF.
    ls_order_data-scrap_quantity = is_input-gamng * lv_ausss / 100.
    ls_order_data-quantity = is_input-gamng + ls_order_data-scrap_quantity.
    cs_log-gamng = ls_order_data-quantity."更新S原始母订单的数量
  ELSE.
    ls_order_data-quantity = is_input-gamng.
  ENDIF.
  IF lv_andec = 0."单位没有小数点位数，数量向上取整
    ls_order_data-scrap_quantity = ceil(  ls_order_data-scrap_quantity ).
    ls_order_data-quantity = ceil( ls_order_data-quantity ) .
    cs_log-gamng = ls_order_data-quantity."更新S原始母订单的数量
  ENDIF.

*  ls_order_data-quantity = is_input-gamng.
*  ls_order_data-scrap_quantity = 0."装配损耗已经包含在了quantity中
  ls_order_data-quantity_uom = is_input-gmein.
  ls_order_data-prod_version = is_input-verid.
  IF is_input-zsfs <> 'S'."S时不需写入销售订单数据
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = is_input-kdauf
      IMPORTING
        output = ls_order_data-sales_order.
    ls_order_data-sales_order_item = is_input-kdpos.
  ELSE."S订单
    "传输销售订单信息给BAPI质检出口ZXQPAU04(S订单BAPI不填销售订单)
    lv_vbeln = is_input-kdauf.
    SET PARAMETER ID 'ZAPS_VBELN' FIELD ''."首先清空
    SET PARAMETER ID 'ZAPS_VBELN' FIELD lv_vbeln.
  ENDIF.
  CALL FUNCTION 'BAPI_PROCORD_CREATE'
    EXPORTING
      orderdata    = ls_order_data
    IMPORTING
      return       = ls_return
      order_number = lv_aufnr.
  IF ls_return-type = 'E' OR ls_return-type = 'A'.
    cv_error = 'X'.
    cs_output-type = 'E'.
    cs_output-message = ls_return-message.
    cs_log-type = 'E'.
    cs_log-message = ls_return-message.
  ELSE."S流程订单创建成功
    "保存日志记录
    cs_log-aufnr = lv_aufnr.
    cs_output-aufnr = lv_aufnr.
    cs_log-message = '流程订单' && lv_aufnr && '创建成功'.
    cv_aufnr = lv_aufnr.
  ENDIF.
  IF cs_log-type = 'E'.
    cv_error = 'X'.
  ENDIF.
ENDFORM.
FORM frm_create_process_order_simu  USING  is_input TYPE zdt_aps2sap_pro_pro_req
                               CHANGING cv_error TYPE flag
                                        cv_aufnr TYPE aufnr
                                        cs_output TYPE zdt_aps2sap_pro_ret_pro_ret
                                        cs_log TYPE zpitlog0004.
  DATA ls_return TYPE bapiret2.
  DATA lv_aufnr TYPE aufnr.
  DATA ls_order_data TYPE bapi_pi_order_create.
  DATA lv_vbeln TYPE vbeln.
  CALL FUNCTION 'CONVERSION_EXIT_MATN5_INPUT'
    EXPORTING
      input        = is_input-matnr
    IMPORTING
      output       = ls_order_data-material
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  ls_order_data-plant = is_input-werks.
  ls_order_data-order_type = is_input-auart.
  ls_order_data-basic_start_date = is_input-gstrp.
  ls_order_data-basic_end_date = is_input-gltrp.
  ls_order_data-quantity = ceil( is_input-gamng ).
*  ls_order_data-scrap_quantity = 0."装配损耗已经包含在了quantity中
  ls_order_data-quantity_uom = is_input-gmein.
  ls_order_data-prod_version = is_input-verid.
  IF is_input-zsfs <> 'S'."S时不需写入销售订单数据
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = is_input-kdauf
      IMPORTING
        output = ls_order_data-sales_order.
    ls_order_data-sales_order_item = is_input-kdpos.
  ENDIF.
  CALL FUNCTION 'ZBAPI_PROCORD_CREATE'
    EXPORTING
      orderdata    = ls_order_data
    IMPORTING
      return       = ls_return
      order_number = lv_aufnr.
  IF ls_return-type = 'E' OR ls_return-type = 'A'.
    cv_error = 'X'.
    cs_output-type = 'E'.
    cs_output-message = '订单模拟创建失败' && ls_return-message.
    cs_log-type = 'E'.
    cs_log-message = '订单模拟创建失败' && ls_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_ORDER_ITSELF
*&---------------------------------------------------------------------*
*& 修改M订单
*&---------------------------------------------------------------------*
*&      --> LS_INPUT
*&      --> LV_AUFNR_IN
*&---------------------------------------------------------------------*
FORM frm_change_order_capital_m  USING is_input TYPE zdt_aps2sap_pro_u_pro_req
                                     iv_aufnr_in TYPE aufnr
                                 CHANGING ct_log TYPE tt_zpitlog0005.
  DATA ls_input TYPE zdt_aps2sap_pro_u_pro_req.
  DATA ls_log TYPE zpitlog0005.
  DATA lv_longtext TYPE string.

  ls_input = is_input.
  MOVE-CORRESPONDING ls_input TO ls_log.
  ls_log-uname = sy-uname.
  ls_log-datum = sy-datum.
  ls_log-uzeit = sy-uzeit.
  CLEAR ls_input-charg."不允许修改M和m的批号
  PERFORM frm_change_order USING ls_input iv_aufnr_in '' '0' CHANGING ls_log.
  IF ls_log-type = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    APPEND ls_log TO ct_log.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*    WAIT UP TO 1 SECONDS.
    READ TABLE ct_log ASSIGNING FIELD-SYMBOL(<fs_log>) WITH KEY aufnr = iv_aufnr_in aufnr_ref = iv_aufnr_in.
    IF sy-subrc <> 0."本订单log行还未生成，需要生成
      APPEND INITIAL LINE TO ct_log ASSIGNING <fs_log>.
      CHECK <fs_log> IS ASSIGNED.
      MOVE-CORRESPONDING ls_log TO <fs_log>.
    ENDIF.
    lv_longtext = '/内部订单号:' && ls_input-zrsv01 && '/成本中心号:' && ls_input-zrsv02 && '/备注（半成品报废）:' && ls_input-zrsv03 && '/长文本：' && ls_input-ztext.
    IF lv_longtext IS NOT INITIAL."修改长文本
*      PERFORM frm_save_long_text_change USING iv_aufnr_in iv_aufnr_in lv_longtext ls_log CHANGING <fs_log>.
      DATA(lt_text) = VALUE zsds_so_text_t( tdobject = 'AUFK' tdname = sy-mandt && iv_aufnr_in ( tdid = 'KOPF' text = lv_longtext ) ). "创建长文本 add by litao 20191202
      PERFORM frm_create_text USING lt_text.
    ENDIF.
*---------下达生产订单
    IF ls_input-status = 'REL'.
      PERFORM frm_release_order_chg USING iv_aufnr_in CHANGING <fs_log>.
*      PERFORM frm_set_release USING iv_aufnr_in CHANGING <fs_log>-type <fs_log>-message. "下达订单 add by litao 20191202
    ENDIF.
  ENDIF.
ENDFORM.
FORM frm_chg_order_capital_m_simu  USING is_input TYPE zdt_aps2sap_pro_u_pro_req
                                     iv_aufnr_in TYPE aufnr
                                CHANGING cs_output TYPE zdt_aps2sap_pro_u_ret_pro_ret
                                          cv_error TYPE flag.
  DATA ls_input TYPE zdt_aps2sap_pro_u_pro_req.

  ls_input = is_input.

  CLEAR ls_input-charg."不允许修改M和m的批号
  PERFORM frm_change_order_simu USING is_input iv_aufnr_in '' CHANGING cv_error cs_output-message.
  IF cv_error = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    cs_output-aufnr = iv_aufnr_in.
    cs_output-type = 'E'.
    cs_output-message = '订单模拟更新失败' && cs_output-message.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_ORDER_ITSELF
*&---------------------------------------------------------------------*
*& 修改M订单
*&---------------------------------------------------------------------*
*&      --> LS_INPUT
*&      --> LV_AUFNR_IN
*&---------------------------------------------------------------------*
FORM frm_change_order USING is_input TYPE zdt_aps2sap_pro_u_pro_req
                                     iv_aufnr_in TYPE aufnr
                                     iv_s_flag TYPE flag"S订单或S子订单
                                     iv_ausss TYPE ausss
                                 CHANGING cs_log TYPE zpitlog0005.

  DATA ls_order_data TYPE bapi_pi_order_change.
  DATA ls_order_datax TYPE bapi_pi_order_changex.
  DATA ls_return TYPE bapiret2.
  DATA lt_return TYPE TABLE OF bapiret2.

  ls_order_data-basic_start_date = is_input-gstrp.
  ls_order_data-basic_end_date = is_input-gltrp.
  IF iv_s_flag = 'X' .
    ls_order_data-quantity = is_input-gamng.
    ls_order_data-scrap_quantity = ceil( ls_order_data-quantity - ls_order_data-quantity / ( 1 + iv_ausss / 100 ) ).
  ELSE.
    ls_order_data-quantity = is_input-gamng.
    ls_order_data-scrap_quantity = 0.
  ENDIF.
  ls_order_data-quantity_uom = is_input-gmein.
  ls_order_data-prod_version = is_input-verid.
  ls_order_data-zzcharg = is_input-charg."批次（通过增强修改）

  IF ls_order_data-basic_start_date IS NOT INITIAL.
    ls_order_datax-basic_start_date = 'X'.
  ENDIF.
  IF ls_order_data-basic_end_date IS NOT INITIAL.
    ls_order_datax-basic_end_date = 'X'.
  ENDIF.
  IF ls_order_data-quantity IS NOT INITIAL.
    ls_order_datax-quantity = 'X'.
    IF iv_s_flag = 'X'.
      ls_order_datax-scrap_quantity = 'X'."更新报废量
    ENDIF.
  ENDIF.
  IF ls_order_data-quantity_uom IS NOT INITIAL.
    ls_order_datax-quantity_uom = 'X'.
  ENDIF.
  IF ls_order_data-prod_version IS NOT INITIAL.
    ls_order_datax-prod_version = 'X'.
  ENDIF.
  IF ls_order_data-zzcharg IS NOT INITIAL.
    ls_order_datax-zzcharg = 'X'.
  ENDIF.

  SELECT SINGLE vbap~zdytk FROM vbap
    INNER JOIN afpo ON vbap~vbeln = afpo~kdauf
                       AND vbap~posnr = afpo~kdpos
    INTO @DATA(lv_dytk)
  WHERE afpo~aufnr = @iv_aufnr_in.
  IF sy-subrc = 0."获取并拆分短溢条款
    PERFORM frm_get_dytk USING lv_dytk CHANGING ls_order_data-zzuntto ls_order_data-zzuebto.
    ls_order_datax-zzuntto = 'X'."交货不足限度
    ls_order_datax-zzuebto = 'X'."交货过量限度
  ENDIF.

  CALL FUNCTION 'BAPI_PROCORD_CHANGE'
    EXPORTING
      number     = iv_aufnr_in
      orderdata  = ls_order_data
      orderdatax = ls_order_datax
    IMPORTING
      return     = ls_return.
  cs_log-aufnr = iv_aufnr_in.
  IF ls_return-type = 'E' OR ls_return-type = 'A'."更新失败
    cs_log-type = 'E'.
    cs_log-message = ls_return-message.
  ELSE."更新成功
    IF ls_order_data-zzcharg IS NOT INITIAL."批次需要更新
      SELECT SINGLE matnr,dwerk,charg FROM afpo
      INTO @DATA(ls_afpo)
      WHERE aufnr = @iv_aufnr_in.
      IF sy-subrc = 0.
        SELECT SINGLE matnr, werks, charg FROM mcha
          INTO @DATA(ls_mcha)
          WHERE matnr = @ls_afpo-matnr
            AND werks = @ls_afpo-dwerk
            AND charg = @ls_order_data-zzcharg.
        IF sy-subrc <> 0."批次不存在需要新建
          gs_batchcontrolfields-doclassify = 'X'."分类对象需设置为X,创建批次时分类特性才能展开
          CALL FUNCTION 'BAPI_BATCH_CREATE'
            EXPORTING
              batch              = ls_order_data-zzcharg
              plant              = ls_afpo-dwerk
              batchcontrolfields = gs_batchcontrolfields
              material_long      = ls_afpo-matnr
            TABLES
              return             = lt_return.
          LOOP AT lt_return INTO ls_return WHERE type = ' E' OR type = 'A'.
            cs_log-type = 'E'.
            cs_log-message = '订单' && iv_aufnr_in && '批次更新失败'.
            sy-subrc = 0.
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            cs_log-type = 'S'.
            cs_log-message = '订单' && iv_aufnr_in && '更新成功'.
          ENDIF.
        ELSE."批次存在不需要新建
          cs_log-type = 'S'.
          cs_log-message = '订单' && iv_aufnr_in && '更新成功'.
        ENDIF.
      ENDIF.
    ELSE."批次不需要更新
      cs_log-type = 'S'.
      cs_log-message = '订单' && iv_aufnr_in && '更新成功'.
    ENDIF.
  ENDIF.

ENDFORM.
*M流程订单已拆单后，APS再上传修改 add by ljm 20191208
FORM frm_change_order1 USING is_input TYPE zdt_aps2sap_pro_u_pro_req
                                     iv_aufnr_in TYPE aufnr
                                 CHANGING cs_log TYPE zpitlog0005.

  DATA ls_order_data TYPE bapi_pi_order_change.
  DATA ls_order_datax TYPE bapi_pi_order_changex.
  DATA ls_return TYPE bapiret2.
  DATA lt_return TYPE TABLE OF bapiret2.

  ls_order_data-basic_start_date = is_input-gstrp.
  ls_order_data-basic_end_date = is_input-gltrp.
  IF ls_order_data-basic_start_date IS NOT INITIAL.
    ls_order_datax-basic_start_date = 'X'.
  ENDIF.
  IF ls_order_data-basic_end_date IS NOT INITIAL.
    ls_order_datax-basic_end_date = 'X'.
  ENDIF.
  SELECT SINGLE vbap~zdytk FROM vbap
    INNER JOIN afpo ON vbap~vbeln = afpo~kdauf
                       AND vbap~posnr = afpo~kdpos
    INTO @DATA(lv_dytk)
  WHERE afpo~aufnr = @iv_aufnr_in.
  IF sy-subrc = 0."获取并拆分短溢条款
    PERFORM frm_get_dytk USING lv_dytk CHANGING ls_order_data-zzuntto ls_order_data-zzuebto.
    ls_order_datax-zzuntto = 'X'."交货不足限度
    ls_order_datax-zzuebto = 'X'."交货过量限度
  ENDIF.
  CALL FUNCTION 'BAPI_PROCORD_CHANGE'
    EXPORTING
      number     = iv_aufnr_in
      orderdata  = ls_order_data
      orderdatax = ls_order_datax
    IMPORTING
      return     = ls_return.
  cs_log-aufnr = iv_aufnr_in.
  IF ls_return-type = 'E' OR ls_return-type = 'A'."更新失败
    cs_log-type = 'E'.
    cs_log-message = ls_return-message.
  ELSE."更新成功
    cs_log-type = 'S'.
    cs_log-message = '订单' && iv_aufnr_in && '更新成功'.
  ENDIF.

ENDFORM.
FORM frm_change_order_simu USING is_input TYPE zdt_aps2sap_pro_u_pro_req
                                     iv_aufnr_in TYPE aufnr
                                     iv_subs_flag TYPE flag"S子订单
                                 CHANGING cv_error TYPE flag
                                          cv_message TYPE string.

  DATA ls_order_data TYPE bapi_pi_order_change.
  DATA ls_order_datax TYPE bapi_pi_order_changex.
  DATA ls_bapibatchctrl TYPE bapibatchctrl.
  DATA ls_return TYPE bapiret2.
  DATA lt_return TYPE TABLE OF bapiret2.

  ls_order_data-basic_start_date = is_input-gstrp.
  ls_order_data-basic_end_date = is_input-gltrp.
  ls_order_data-quantity = ceil( is_input-gamng ).
  ls_order_data-quantity_uom = is_input-gmein.
  ls_order_data-prod_version = is_input-verid.
  ls_order_data-zzcharg = is_input-charg."批次（通过增强修改）

  IF ls_order_data-basic_start_date IS NOT INITIAL.
    ls_order_datax-basic_start_date = 'X'.
  ENDIF.
  IF ls_order_data-basic_end_date IS NOT INITIAL.
    ls_order_datax-basic_end_date = 'X'.
  ENDIF.
  IF ls_order_data-quantity IS NOT INITIAL.
    ls_order_datax-quantity = 'X'.
*    IF iv_subs_flag <> 'X'.
    ls_order_datax-scrap_quantity = 'X'."数量中已经包含了报废量，所以报废量设为0
*    ENDIF.
  ENDIF.
  IF ls_order_data-quantity_uom IS NOT INITIAL.
    ls_order_datax-quantity_uom = 'X'.
  ENDIF.
  IF ls_order_data-prod_version IS NOT INITIAL.
    ls_order_datax-prod_version = 'X'.
  ENDIF.
  IF ls_order_data-zzcharg IS NOT INITIAL.
    ls_order_datax-zzcharg = 'X'.
  ENDIF.

  CALL FUNCTION 'ZBAPI_PROCORD_CHANGE'
    EXPORTING
      number     = iv_aufnr_in
      orderdata  = ls_order_data
      orderdatax = ls_order_datax
    IMPORTING
      return     = ls_return.

  IF ls_return-type = 'E' OR ls_return-type = 'A'."更新失败
    cv_error = 'X'.
    cv_message = ls_return-message.
  ELSE."更新成功
    IF ls_order_data-zzcharg IS NOT INITIAL."批次需要更新
      SELECT SINGLE matnr,dwerk,charg FROM afpo
      INTO @DATA(ls_afpo)
      WHERE aufnr = @iv_aufnr_in.
      IF sy-subrc = 0.
        SELECT SINGLE matnr, werks, charg FROM mcha
          INTO @DATA(ls_mcha)
          WHERE matnr = @ls_afpo-matnr
            AND werks = @ls_afpo-dwerk
            AND charg = @ls_order_data-zzcharg.
        IF sy-subrc <> 0."批次不存在需要新建
          ls_bapibatchctrl-nocommit = 'X'.
          ls_bapibatchctrl-doclassify = 'X'."分类对象需设置为X,创建批次时分类特性才能展开
          CALL FUNCTION 'BAPI_BATCH_CREATE'
            EXPORTING
              batch              = ls_order_data-zzcharg
              plant              = ls_afpo-dwerk
              batchcontrolfields = ls_bapibatchctrl
              material_long      = ls_afpo-matnr
            TABLES
              return             = lt_return.
          LOOP AT lt_return INTO ls_return WHERE type = ' E' OR type = 'A'.
            cv_error = 'X'.
            cv_message = ls_return-message.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'."不更新数据库
ENDFORM.
***FORM frm_change_order_remove_scrap USING iv_qty TYPE string
***                                     iv_aufnr_in TYPE aufnr
***                                 CHANGING cs_log TYPE zpitlog0004.
***
***  DATA ls_order_data TYPE bapi_pi_order_change.
***  DATA ls_order_datax TYPE bapi_pi_order_changex.
***  DATA ls_return TYPE bapiret2.
***  DATA lt_return TYPE TABLE OF bapiret2.
***
***  ls_order_data-quantity = iv_qty.
***
***  IF ls_order_data-quantity IS NOT INITIAL.
***    ls_order_datax-quantity = 'X'.
***    ls_order_datax-scrap_quantity = 'X'."数量中已经包含了报废量，所以报废量设为0
***  ENDIF.
***
***  CALL FUNCTION 'BAPI_PROCORD_CHANGE'
***    EXPORTING
***      number     = iv_aufnr_in
***      orderdata  = ls_order_data
***      orderdatax = ls_order_datax
***    IMPORTING
***      return     = ls_return.
***  cs_log-aufnr = iv_aufnr_in.
***  IF ls_return-type = 'E' OR ls_return-type = 'A'."更新失败
***    cs_log-type = 'E'.
***    cs_log-message = '订单创建成功但报废量更新删除失败'.
***  ENDIF.
***
***ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_ORDER_ITSELF
*&---------------------------------------------------------------------*
*& 修改S订单
*&---------------------------------------------------------------------*
FORM frm_change_order_s  USING is_input TYPE zdt_aps2sap_pro_u_pro_req
                                     iv_aufnr_in TYPE aufnr
                                 CHANGING ct_log TYPE tt_zpitlog0005.
  DATA ls_log TYPE zpitlog0005.
  DATA lv_error TYPE flag.
  DATA lt_stb_merge_final TYPE tt_stpox.
  DATA ls_bom_cal TYPE zdt_aps2sap_pro_pro_req.
  DATA ls_input TYPE zdt_aps2sap_pro_u_pro_req.
  DATA lv_ausss TYPE ausss.
  DATA lv_andec TYPE andec.
  DATA lv_longtext TYPE string.
  DATA lv_menge TYPE menge_d.
  DATA lv_menge_s_upd_new TYPE menge_d.
  ls_input = is_input.
  "获取S订单物料工厂信息
  SELECT SINGLE matnr, werks,gstrp,gamng FROM zppt0001
    INTO @DATA(ls_zppt0001)
  WHERE aufnr = @iv_aufnr_in.
  IF sy-subrc = 0.
    ls_bom_cal-matnr = ls_zppt0001-matnr.
    ls_bom_cal-werks = ls_zppt0001-werks.
  ENDIF.
  "因为发现过流程订单创建了但是没有写入到zppt0001,所以这里需要验证下
  "如果没有查询就得到数据就从AFPO中重读下---by ljm 2019-12-04
  IF  ls_zppt0001-matnr IS INITIAL OR ls_zppt0001-werks IS INITIAL.
    SELECT SINGLE afpo~matnr,afpo~dwerk AS werks,afko~gstrp,afpo~psmng AS gamng
      FROM afko
      INNER JOIN afpo ON afko~aufnr = afpo~aufnr
      WHERE afko~aufnr =  @iv_aufnr_in
      INTO @ls_zppt0001.
  ENDIF.
  IF is_input-gamng IS NOT INITIAL.
    ls_bom_cal-gamng = is_input-gamng.
  ELSE.
    ls_bom_cal-gamng = ls_zppt0001-gamng.
  ENDIF.
  IF is_input-gstrp IS NOT INITIAL.
    ls_bom_cal-gstrp = is_input-gstrp.
  ELSE.
    ls_bom_cal-gstrp = ls_zppt0001-gstrp.
  ENDIF.
  ls_bom_cal-verid = is_input-verid.
*---首先处理S本身
  MOVE-CORRESPONDING is_input TO ls_log.
  "APS传值不包含装配报废率，需计算
  SELECT SINGLE ausss FROM marc INTO lv_ausss
  WHERE matnr = ls_zppt0001-matnr
    AND werks = ls_zppt0001-werks.
  SELECT SINGLE andec FROM t006 INNER JOIN mara
                                      ON t006~msehi = mara~meins
                                            INTO lv_andec
                                      WHERE mara~matnr = ls_zppt0001-matnr.
  IF lv_ausss <> 0."有装配报废
    ls_input-gamng = ls_input-gamng + ( is_input-gamng * lv_ausss / 100 ).
  ENDIF.
  IF lv_andec = 0."单位没有小数点，数量就向上取整
    ls_input-gamng = ceil( ls_input-gamng ).
  ENDIF.
  ls_log-uname = sy-uname.
  ls_log-datum = sy-datum.
  ls_log-uzeit = sy-uzeit.
  PERFORM frm_change_order USING ls_input iv_aufnr_in 'X' lv_ausss CHANGING ls_log.
  IF ls_log-type = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    APPEND ls_log TO ct_log."返回错误消息
  ELSE.
    ls_log-aufnr_ref = iv_aufnr_in.
    APPEND ls_log TO ct_log."返回S订单日志正确处理消息
*---没有错误，继续处理S的子订单
    SELECT * FROM zppt0001_subs INTO TABLE @DATA(lt_zppt0001_subs) WHERE aufnr_s = @iv_aufnr_in.
    IF sy-subrc = 0.
      lv_menge_s_upd_new  = ls_input-gamng."S本身修改后的数量 add by ljm ----20191211
      LOOP AT lt_zppt0001_subs ASSIGNING FIELD-SYMBOL(<fs_zppt0001_subs>).
        REFRESH lt_stb_merge_final.
        "根据BOM重新获取S订单子订单的组件需求量
        PERFORM frm_calculate_bom_req USING ls_bom_cal 'X' CHANGING lt_stb_merge_final.
        "S订单子订单物料重展BOM后的数量
        READ TABLE lt_stb_merge_final INTO DATA(ls_stb_merge_final) WITH KEY idnrk = <fs_zppt0001_subs>-matnr.
        IF sy-subrc = 0.
          ls_input-gamng = ls_stb_merge_final-mnglg.
        ELSE."订单物料在BOM不存在,不继续修改
          lv_error = 'X'.
          ls_log-type = 'E'.
          ls_log-message = |{ '子订单' }{ <fs_zppt0001_subs>-aufnr ALPHA = OUT }{ '的物料在BOM不存在' } |.
          APPEND ls_log TO ct_log.
          EXIT.
        ENDIF.
        SELECT SINGLE andec
         FROM t006 INNER JOIN mara ON t006~msehi = mara~meins
         WHERE mara~matnr = @<fs_zppt0001_subs>-matnr
         INTO @lv_andec.
        IF lv_andec = 0.
          ls_input-gamng = ceil( ls_input-gamng ).
        ENDIF.
        SELECT SINGLE ausss FROM marc INTO lv_ausss"计算小S装配报废
        WHERE matnr = <fs_zppt0001_subs>-matnr
           AND werks = <fs_zppt0001_subs>-werks.
* S流程订单的下阶C、G流程订单直接根据zppt0001_subs的比率计算 add by ljm 20191210------
        "物料装配报废率
*        SELECT SINGLE ausss FROM marc INTO lv_ausss"计算小S装配报废
*        WHERE matnr = <fs_zppt0001_subs>-matnr
*          AND werks = <fs_zppt0001_subs>-werks.
        "物料基本单位小数点尾数
*        CLEAR lv_ausss.
*        SELECT SINGLE andec
*          FROM t006 INNER JOIN mara ON t006~msehi = mara~meins
*          WHERE mara~matnr = @<fs_zppt0001_subs>-matnr
*          INTO @lv_andec.
*        "计算C、G子流程订单的需求量lv_menge
*        IF <fs_zppt0001_subs>-menge_s <> 0."S流程订单需求量
*          "下阶子订单的需求量 = 接口传来的S更新后的数量 * （  下阶订单数量 / S数量  ）
*          lv_menge = <fs_zppt0001_subs>-menge / <fs_zppt0001_subs>-menge_s * lv_menge_s_upd_new.
*        ENDIF.
*        IF lv_andec = 0."物料基本单位小数点位数
*          lv_menge = ceil( lv_menge )."向上取整
*        ENDIF.
*        CHECK lv_menge > 0.
*        ls_input-gamng = lv_menge.
* S流程订单的下阶C、G流程订单直接根据zppt0001_subs的比率计算 add by ljm 20191210------

        PERFORM frm_change_order USING ls_input <fs_zppt0001_subs>-aufnr 'X'  lv_ausss CHANGING ls_log.
        ls_log-aufnr_ref = iv_aufnr_in.
        ls_log-gamng = ls_input-gamng.
        APPEND ls_log TO ct_log."返回日志消息
        IF ls_log-type = 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          lv_error = 'X'.
          DELETE ct_log WHERE aufnr_ref = iv_aufnr_in.
          RETURN."任一相关订单更新失败，结束当前子程序
        ENDIF.
      ENDLOOP.
      IF lv_error = 'X'.
        RETURN.
      ENDIF.
*-------无错误发生，提交更新
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
*      WAIT UP TO 1 SECONDS.
**-----完善日志记录(母订单)
      READ TABLE ct_log ASSIGNING FIELD-SYMBOL(<fs_log>) WITH KEY aufnr = iv_aufnr_in aufnr_ref = iv_aufnr_in.
      IF sy-subrc <> 0."本订单log行还未生成，需要生成
        APPEND INITIAL LINE TO ct_log ASSIGNING <fs_log>.
        CHECK <fs_log> IS ASSIGNED.
        MOVE-CORRESPONDING ls_log TO <fs_log>.
        <fs_log>-aufnr = iv_aufnr_in.
        <fs_log>-aufnr_ref = iv_aufnr_in.
      ENDIF.
*-------为修改成功的所有流程订单更新长文本
      lv_longtext = '/内部订单号:' && ls_input-zrsv01 && '/成本中心号:' && ls_input-zrsv02 && '/备注（半成品报废）:' && ls_input-zrsv03 && '/长文本：' && ls_input-ztext.
      IF lv_longtext IS NOT INITIAL."长文本需要更新
*        PERFORM frm_save_long_text_change USING iv_aufnr_in iv_aufnr_in lv_longtext ls_log CHANGING <fs_log>.
*        LOOP AT lt_zppt0001_subs ASSIGNING <fs_zppt0001_subs>.
*          PERFORM frm_save_long_text_change USING <fs_zppt0001_subs>-aufnr <fs_zppt0001_subs>-aufnr_s lv_longtext ls_log CHANGING <fs_log>.
*        ENDLOOP.
        DATA(lt_text) = VALUE zsds_so_text_t( tdobject = 'AUFK' tdname = sy-mandt && iv_aufnr_in ( tdid = 'KOPF' text = lv_longtext ) ). "创建长文本 add by litao 20191202
        LOOP AT lt_zppt0001_subs ASSIGNING FIELD-SYMBOL(<fs_temp>).
          APPEND VALUE #( tdobject = 'AUFK' tdid = 'KOPF' tdname = sy-mandt && <fs_temp>-aufnr text = lv_longtext ) TO lt_text.
        ENDLOOP.
        PERFORM frm_create_text USING lt_text.
      ENDIF.
*---------下达生产订单
      IF is_input-status = 'REL'.
        PERFORM frm_release_order_chg USING iv_aufnr_in CHANGING <fs_log>.
*        PERFORM frm_set_release USING iv_aufnr_in CHANGING <fs_log>-type <fs_log>-message. "下达订单 add by litao 20191202
      ENDIF.
      LOOP AT lt_zppt0001_subs ASSIGNING <fs_zppt0001_subs>.
        READ TABLE ct_log ASSIGNING <fs_log> WITH KEY aufnr = <fs_zppt0001_subs>-aufnr aufnr_ref = <fs_zppt0001_subs>-aufnr_s.
        IF sy-subrc <> 0."本订单log行还未生成，需要生成
          APPEND INITIAL LINE TO ct_log ASSIGNING <fs_log>."完善日志记录(子订单)
          CHECK <fs_log> IS ASSIGNED.
          MOVE-CORRESPONDING ls_log TO <fs_log>.
          <fs_log>-aufnr_ref = <fs_zppt0001_subs>-aufnr_s.
          <fs_log>-aufnr = <fs_zppt0001_subs>-aufnr.
        ENDIF.
*---------下达生产订单
        IF is_input-status = 'REL'.
          PERFORM frm_release_order_chg USING iv_aufnr_in CHANGING <fs_log>.
*          PERFORM frm_set_release USING iv_aufnr_in CHANGING <fs_log>-type <fs_log>-message. "下达订单 add by litao 20191202
        ENDIF.
      ENDLOOP.
    ELSE."无子订单需要处理
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'      "无错误发生，提交更新
        EXPORTING
          wait = 'X'.
*      WAIT UP TO 1 SECONDS.
      READ TABLE ct_log ASSIGNING <fs_log> WITH KEY aufnr = iv_aufnr_in aufnr_ref = iv_aufnr_in.
      IF sy-subrc <> 0."本订单log行还未生成，需要生成
        APPEND INITIAL LINE TO ct_log ASSIGNING <fs_log>.
        CHECK <fs_log> IS ASSIGNED.
        MOVE-CORRESPONDING ls_log TO <fs_log>.
        <fs_log>-aufnr = iv_aufnr_in.
        <fs_log>-aufnr_ref = iv_aufnr_in.
      ENDIF.
      lv_longtext = '/内部订单号:' && ls_input-zrsv01 && '/成本中心号:' && ls_input-zrsv02 && '/备注（半成品报废）:' && ls_input-zrsv03 && '/长文本：' && ls_input-ztext.
      IF lv_longtext IS NOT INITIAL."修改长文本
*        PERFORM frm_save_long_text_change USING iv_aufnr_in iv_aufnr_in lv_longtext ls_log CHANGING <fs_log>.
        lt_text = VALUE #( tdobject = 'AUFK' tdname = sy-mandt && iv_aufnr_in ( tdid = 'KOPF' text = lv_longtext ) ). "创建长文本 add by litao 20191202
        PERFORM frm_create_text USING lt_text.
      ENDIF.
*---------下达生产订单
      IF is_input-status = 'REL'.
        PERFORM frm_release_order_chg USING iv_aufnr_in CHANGING <fs_log>.
*        PERFORM frm_set_release USING iv_aufnr_in CHANGING <fs_log>-type <fs_log>-message. "下达订单 add by litao 20191202
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
FORM frm_change_order_s_simu  USING is_input TYPE zdt_aps2sap_pro_u_pro_req
                                     iv_aufnr_in TYPE aufnr
                                 CHANGING cs_output TYPE zdt_aps2sap_pro_u_ret_pro_ret
                                          cv_error TYPE flag.
  DATA lv_error TYPE flag.
  DATA lt_stb_merge_final TYPE tt_stpox.
  DATA ls_bom_cal TYPE zdt_aps2sap_pro_pro_req.
  DATA ls_input TYPE zdt_aps2sap_pro_u_pro_req.
  ls_input = is_input.
  "获取S订单物料工厂信息
  SELECT SINGLE matnr, werks,gstrp,gamng FROM zppt0001
    INTO @DATA(ls_zppt0001)
    WHERE aufnr = @iv_aufnr_in.
  IF sy-subrc = 0.
    ls_bom_cal-matnr = ls_zppt0001-matnr.
    ls_bom_cal-werks = ls_zppt0001-werks.
  ENDIF.
  IF is_input-gamng IS NOT INITIAL.
    ls_bom_cal-gamng = is_input-gamng.
  ELSE.
    ls_bom_cal-gamng = ls_zppt0001-gamng.
  ENDIF.
  IF is_input-gstrp IS NOT INITIAL.
    ls_bom_cal-gstrp = is_input-gstrp.
  ELSE.
    ls_bom_cal-gstrp = ls_zppt0001-gstrp.
  ENDIF.
  ls_bom_cal-verid = is_input-verid.
*---首先处理S本身
  PERFORM frm_change_order_simu USING is_input iv_aufnr_in '' CHANGING cv_error cs_output-message.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  IF cv_error = 'X'.
    cs_output-aufnr = iv_aufnr_in.
    cs_output-type = 'E'.
    cs_output-message = '订单模拟更新失败' && cs_output-message.
  ELSE.
*---没有错误，继续模拟处理S的子订单
    SELECT *  FROM zppt0001_subs INTO TABLE @DATA(lt_zppt0001_subs) WHERE aufnr_s = @iv_aufnr_in.
    IF sy-subrc = 0.
      LOOP AT lt_zppt0001_subs ASSIGNING FIELD-SYMBOL(<fs_zppt0001_subs>).
        REFRESH lt_stb_merge_final.
        "根据BOM重新获取S订单子订单的组件需求量
        PERFORM frm_calculate_bom_req_simu USING ls_bom_cal CHANGING lt_stb_merge_final cv_error.
        IF cv_error = 'X'.
          cs_output-aufnr = iv_aufnr_in.
          cs_output-type = 'E'.
          cs_output-message = '订单的子订单' && <fs_zppt0001_subs>-aufnr && '模拟BOM展开失败'.
          EXIT.
        ENDIF.
        "S订单的子订单物料号,如果未读取到不能继续修改
        READ TABLE lt_stb_merge_final WITH KEY idnrk = <fs_zppt0001_subs>-matnr  INTO DATA(ls_stb_merge_final) .
        IF sy-subrc = 0.
          ls_input-gamng = ls_stb_merge_final-mnglg.
        ELSE.
          cv_error = 'X'.
          cs_output-aufnr = iv_aufnr_in.
          cs_output-type = 'E'.
          cs_output-message = '订单的子订单' && <fs_zppt0001_subs>-aufnr && '的物料号在BOM中不存在'.
          EXIT.
        ENDIF.
        PERFORM frm_change_order_simu USING ls_input <fs_zppt0001_subs>-aufnr '' CHANGING cv_error cs_output-message.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        IF cv_error = 'X'.
          cs_output-aufnr = iv_aufnr_in.
          cs_output-type = 'E'.
          cs_output-message = '订单的子订单' && <fs_zppt0001_subs>-aufnr && '模拟更新失败' && cs_output-message.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATE_ORDER_BATCH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_AUFNR
*&      --> IS_INPUT_CHARG
*&---------------------------------------------------------------------*
FORM frm_update_suborder_batch  USING iv_aufnr TYPE aufnr
                                    iv_matnr TYPE matnr
                                    iv_werks TYPE werks_d
                                    iv_charg TYPE charg_d
                                CHANGING cs_log TYPE zppt0001_subs.
  DATA ls_order_data TYPE bapi_pi_order_change.
  DATA ls_order_datax TYPE bapi_pi_order_changex.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA ls_return TYPE bapiret2.

  ls_order_data-zzcharg = iv_charg."批次（通过增强修改）
  ls_order_datax-zzcharg = 'X'.

  CALL FUNCTION 'BAPI_PROCORD_CHANGE'
    EXPORTING
      number     = iv_aufnr
      orderdata  = ls_order_data
      orderdatax = ls_order_datax
    IMPORTING
      return     = ls_return.

  IF ls_return-type = 'E' OR ls_return-type = 'A'."更新失败
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    cs_log-type = 'W'.
    cs_log-message = cs_log-message && '，订单' && iv_aufnr && '批次更新失败'.
  ELSE."更新成功
    IF ls_order_data-zzcharg IS NOT INITIAL."批次需要更新
      SELECT SINGLE matnr,dwerk,charg FROM afpo
      INTO @DATA(ls_afpo)
      WHERE aufnr = @iv_aufnr.
      IF sy-subrc = 0.
        SELECT SINGLE matnr, werks, charg FROM mcha
          INTO @DATA(ls_mcha)
          WHERE matnr = @ls_afpo-matnr
            AND werks = @ls_afpo-dwerk
            AND charg = @ls_order_data-zzcharg.
        IF sy-subrc <> 0."批次不存在需要新建
          gs_batchcontrolfields-doclassify = 'X'."分类对象需设置为X,创建批次时分类特性才能展开
          CALL FUNCTION 'BAPI_BATCH_CREATE'
            EXPORTING
              batch              = ls_order_data-zzcharg
              plant              = iv_werks
              batchcontrolfields = gs_batchcontrolfields
              material_long      = iv_matnr
            TABLES
              return             = lt_return.
          LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
            cs_log-type = 'W'.
            cs_log-message = cs_log-message && '，订单' && iv_aufnr && '批次更新失败'.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            sy-subrc = 0.
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            cs_log-type = 'S'.
            cs_log-message = cs_log-message && '，订单' && iv_aufnr && '批次更新成功'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATE_ORDER_BATCH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_AUFNR
*&      --> IS_INPUT_CHARG
*&---------------------------------------------------------------------*
FORM frm_update_order_batch  USING iv_aufnr TYPE aufnr
                                    iv_matnr TYPE matnr
                                    iv_werks TYPE werks_d
                                    iv_charg TYPE charg_d
                            CHANGING cs_log TYPE zpitlog0004.
  DATA ls_order_data TYPE bapi_pi_order_change.
  DATA ls_order_datax TYPE bapi_pi_order_changex.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA ls_return TYPE bapiret2.
  DATA lv_vbeln TYPE vbeln.
  DATA lv_posnr TYPE posnr.

  ls_order_data-zzcharg = iv_charg."批次（通过增强修改）
  ls_order_datax-zzcharg = 'X'.

* APS传过来的物料如果标识为S码物料即ls_input-zsfs = 'S'，
*不管有没有带销售订单号及行项目，S码物料的流程订单交货容差上下限都取自物料主数据的容差值，不再取销售订单上的短溢值作为容差
* 注释---- by ljm 2019-12-01
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = cs_log-kdauf
*    IMPORTING
*      output = lv_vbeln.
*  lv_posnr = cs_log-kdpos.
*
*  SELECT SINGLE zdytk FROM vbap INTO @DATA(lv_dytk)
*    WHERE vbeln = @lv_vbeln
*      AND posnr = @lv_posnr.
*  IF sy-subrc = 0."获取并拆分短溢条款
*    PERFORM frm_get_dytk USING lv_dytk CHANGING ls_order_data-zzuntto ls_order_data-zzuebto.
*    ls_order_datax-zzuntto = 'X'."交货不足限度
*    ls_order_datax-zzuebto = 'X'."交货过量限度
*  ENDIF.
* 注释---- by ljm 2019-12-01

  CALL FUNCTION 'BAPI_PROCORD_CHANGE'
    EXPORTING
      number     = iv_aufnr
      orderdata  = ls_order_data
      orderdatax = ls_order_datax
    IMPORTING
      return     = ls_return.

  IF ls_return-type = 'E' OR ls_return-type = 'A'."更新失败
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    cs_log-type = 'W'.
    cs_log-message = cs_log-message && '，订单' && iv_aufnr && '批次更新失败'.
  ELSE."更新成功
    IF ls_order_data-zzcharg IS NOT INITIAL."批次需要更新
      SELECT SINGLE matnr,dwerk,charg FROM afpo
      INTO @DATA(ls_afpo)
      WHERE aufnr = @iv_aufnr.
      IF sy-subrc = 0.
        SELECT SINGLE matnr, werks, charg FROM mcha
          INTO @DATA(ls_mcha)
          WHERE matnr = @ls_afpo-matnr
            AND werks = @ls_afpo-dwerk
            AND charg = @ls_order_data-zzcharg.
        IF sy-subrc <> 0."批次不存在需要新建
          "S批次的生产日期在流程订单收货的时候手动输入
          gs_batchcontrolfields-doclassify = 'X'."分类对象需设置为X,创建批次时分类特性才能展开
          CALL FUNCTION 'BAPI_BATCH_CREATE'
            EXPORTING
              batch              = ls_order_data-zzcharg
              plant              = iv_werks
              batchcontrolfields = gs_batchcontrolfields
              material_long      = iv_matnr
            TABLES
              return             = lt_return.
          LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
            cs_log-type = 'W'.
            cs_log-message = cs_log-message && '，订单' && iv_aufnr && '批次更新失败'.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            sy-subrc = 0.
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            cs_log-type = 'S'.
            cs_log-message = cs_log-message && '，订单' && iv_aufnr && '批次更新成功'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

FORM frm_upd_order_under_over_dlv  USING iv_aufnr TYPE aufnr
                                    iv_matnr TYPE matnr
                                    iv_werks TYPE werks_d
                                    iv_charg TYPE charg_d
                            CHANGING cs_log TYPE zpitlog0004.
  DATA ls_order_data TYPE bapi_pi_order_change.
  DATA ls_order_datax TYPE bapi_pi_order_changex.
  DATA lt_return TYPE TABLE OF bapiret2.
  DATA ls_return TYPE bapiret2.
  DATA lv_vbeln TYPE vbeln.
  DATA lv_posnr TYPE posnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = cs_log-kdauf
    IMPORTING
      output = lv_vbeln.
  lv_posnr = cs_log-kdpos.

  SELECT SINGLE zdytk FROM vbap INTO @DATA(lv_dytk)
    WHERE vbeln = @lv_vbeln
      AND posnr = @lv_posnr.
  IF sy-subrc = 0."获取并拆分短溢条款
    PERFORM frm_get_dytk USING lv_dytk CHANGING ls_order_data-zzuntto ls_order_data-zzuebto.
    ls_order_datax-zzuntto = 'X'."交货不足限度
    ls_order_datax-zzuebto = 'X'."交货过量限度
  ENDIF.

  CALL FUNCTION 'BAPI_PROCORD_CHANGE'
    EXPORTING
      number     = iv_aufnr
      orderdata  = ls_order_data
      orderdatax = ls_order_datax
    IMPORTING
      return     = ls_return.

  IF ls_return-type = 'E' OR ls_return-type = 'A'."更新失败
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    cs_log-type = 'W'.
    cs_log-message = cs_log-message && '，订单' && iv_aufnr && '容差更新失败'.
  ELSE."更新成功
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    cs_log-type = 'S'.
    cs_log-message = cs_log-message && '，订单' && iv_aufnr && '容差更新成功'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SAVE_APS_RECORD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ZPITLOG0004
*&      --> LT_ZPPT0001_SUBS
*&---------------------------------------------------------------------*
FORM frm_save_aps_create_record  USING it_zpitlog0004 TYPE tt_zpitlog0004
                                 it_zppt0001_subs TYPE tt_zppt0001_subs.
  DATA lt_zppt0001 TYPE TABLE OF zppt0001. "APS接口创建/修改流程订单最终记录
  LOOP AT it_zpitlog0004 ASSIGNING FIELD-SYMBOL(<fs_log>) WHERE type <> 'E' AND aufnr IS NOT INITIAL."已生成的M，m，S订单
    APPEND INITIAL LINE TO lt_zppt0001 ASSIGNING FIELD-SYMBOL(<fs_ppt0001>).
    IF <fs_ppt0001> IS ASSIGNED.
      MOVE-CORRESPONDING <fs_log> TO <fs_ppt0001>.
      IF <fs_log>-zmpc = '1'."m需要赋值，此值m拆单用
        <fs_ppt0001>-gamng_ori = <fs_log>-gamng.
      ENDIF.
    ENDIF.
    LOOP AT it_zppt0001_subs ASSIGNING FIELD-SYMBOL(<fs_zppt0001_subs>) WHERE type <> 'E'."S的子订单
      APPEND INITIAL LINE TO lt_zppt0001 ASSIGNING <fs_ppt0001>.
      IF <fs_ppt0001> IS ASSIGNED.
        MOVE-CORRESPONDING <fs_log> TO <fs_ppt0001>.
        <fs_ppt0001>-aufnr = <fs_zppt0001_subs>-aufnr."S子订单号
        <fs_ppt0001>-matnr = <fs_zppt0001_subs>-matnr.
        <fs_ppt0001>-gamng = <fs_zppt0001_subs>-menge.
        CLEAR: <fs_ppt0001>-zmpc ,<fs_ppt0001>-zsfs.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  MODIFY zppt0001 FROM TABLE lt_zppt0001.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALCULATE_BOM_REQ
*&---------------------------------------------------------------------*
*& 计算考虑损耗的BOM需求量
*&---------------------------------------------------------------------*
*&      --> LS_INPUT
*&      <-- LT_STB_MERGE_FINAL
*&---------------------------------------------------------------------*
FORM frm_calculate_bom_req  USING   ls_input TYPE zdt_aps2sap_pro_pro_req
                                     iv_change TYPE flag
                            CHANGING ct_stb_merge_final TYPE tt_stpox.
  DATA ls_topmat TYPE cstmat.
  DATA lt_stb TYPE TABLE OF stpox.
  DATA lt_stb_merge TYPE TABLE OF stpox.
  DATA lt_stb_merge_final TYPE TABLE OF stpox.
  DATA lv_matnr TYPE matnr.
  DATA lv_werks TYPE werks_d.
  DATA lv_verid TYPE verid.
  DATA lv_datuv TYPE sy-datum.
  DATA lv_emeng TYPE basmn.
  DATA lv_mtart TYPE mtart.
  DATA ls_order_data TYPE bapi_pi_order_create.
  DATA ls_return TYPE bapiret2.
  DATA lv_ausss TYPE ausss.

*-------首先根据物料主数据的装配报废率（MARC-AUSSS）计算完整需求量
  lv_matnr = ls_input-matnr.
  lv_werks = ls_input-werks.
  lv_emeng = ls_input-gamng."需求量
  lv_datuv = ls_input-gstrp."基本开始日期
  lv_verid = ls_input-verid.

  IF iv_change = 'X'."修改模式
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        aumng                  = '0'              "计算成品装配报废
        auskz                  = 'X'              "计算所有损耗量（主数据损耗MARC-AUSSS & BOM行损耗STPO-AUSCH）
        capid                  = 'PI01'           " 流程制造
        datuv                  = lv_datuv         " 有效起始日
        ehndl                  = '1'              "考虑使用可能性
        emeng                  = lv_emeng         " 需求数量
        mehrs                  = 'X'              "多层展开
        mmory                  = '1'
        mtnrv                  = lv_matnr        "展开物料
        stlan                  = '1'              " BOM用途
        werks                  = lv_werks         "物料所在工厂
        verid                  = lv_verid         "生产版本
      IMPORTING
        topmat                 = ls_topmat
      TABLES
        stb                    = lt_stb
      EXCEPTIONS
        alt_not_found          = 1
        call_invalid           = 2
        material_not_found     = 3
        missing_authorizatizon = 4
        no_bom_found           = 5
        no_plant_data          = 6
        no_suitable_bom_found  = 7
        conversion_error       = 8
        OTHERS                 = 9.
  ELSE."创建模式
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        aumng                 = '1'              "不计算成品装配报废
        auskz                 = 'X'              "计算所有损耗量（主数据损耗MARC-AUSSS & BOM行损耗STPO-AUSCH）
        capid                 = 'PI01'           " 流程制造
        datuv                 = lv_datuv         " 有效起始日
        ehndl                 = '1'              "考虑使用可能性
        emeng                 = lv_emeng         " 需求数量
        mehrs                 = 'X'              "多层展开
        mmory                 = '1'
        mtnrv                 = lv_matnr        "展开物料
        stlan                 = '1'              " BOM用途
        werks                 = lv_werks         "物料所在工厂
        verid                 = lv_verid         "生产版本
      IMPORTING
        topmat                = ls_topmat
      TABLES
        stb                   = lt_stb
      EXCEPTIONS
        alt_not_found         = 1
        call_invalid          = 2
        material_not_found    = 3
        missing_authorization = 4
        no_bom_found          = 5
        no_plant_data         = 6
        no_suitable_bom_found = 7
        conversion_error      = 8
        OTHERS                = 9.
  ENDIF.
*----------重组生产S所需的所有半成品需求量（合并相同物料避免重复计算固定损耗）
  SORT lt_stb BY idnrk fmeng.
  DELETE lt_stb WHERE mnglg < 0.
  LOOP AT lt_stb INTO DATA(ls_stb) WHERE mtart = 'Z030' OR mtart = 'Z050' ."Z030-半成品或Z050-成品
    "若物料类型为Z040，则判断此物料的物料组是否为4010开头，若是则展开BOM，若否则不展开
*    IF ls_stb-mtart = 'Z040' AND ls_stb-matmk(4) <> '4010'.
*      CONTINUE.
*    ENDIF.
    READ TABLE lt_stb_merge ASSIGNING FIELD-SYMBOL(<fs_stb_merge>) WITH KEY idnrk = ls_stb-idnrk."合并计算组件
    IF sy-subrc = 0."固定损耗
      IF ls_stb-fmeng = 'X'."固定损耗
        READ TABLE lt_stb_merge TRANSPORTING NO FIELDS
                    WITH KEY idnrk = ls_stb-idnrk fmeng = 'X'.
        IF sy-subrc <> 0."合并计算组件报废率（STPO-AUSCH）的组件需求数
          APPEND INITIAL LINE TO lt_stb_merge ASSIGNING FIELD-SYMBOL(<fs_stb_merge_fixed>).
          IF <fs_stb_merge_fixed> IS ASSIGNED.
            <fs_stb_merge_fixed> = ls_stb.
          ENDIF.
        ENDIF.
      ELSE."非固定损耗
        <fs_stb_merge>-mnglg = <fs_stb_merge>-mnglg + ls_stb-mnglg.
      ENDIF.
    ELSE."非固定损耗
      APPEND INITIAL LINE TO lt_stb_merge ASSIGNING <fs_stb_merge>.
      IF <fs_stb_merge> IS ASSIGNED.
        <fs_stb_merge> = ls_stb.
      ENDIF.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_stb_merge ASSIGNING <fs_stb_merge>.
    READ TABLE lt_stb_merge_final ASSIGNING FIELD-SYMBOL(<fs_stb_merge_final>)
    WITH KEY idnrk = <fs_stb_merge>-idnrk."合并计算组件
    IF sy-subrc = 0.
      <fs_stb_merge_final>-mnglg = <fs_stb_merge_final>-mnglg + <fs_stb_merge>-mnglg.
    ELSE.
      APPEND INITIAL LINE TO lt_stb_merge_final ASSIGNING <fs_stb_merge_final>.
      IF <fs_stb_merge_final> IS ASSIGNED.
        <fs_stb_merge_final> = <fs_stb_merge>.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF iv_change = 'X'."修改模式，需要自己计算下阶物料的物料装配报废
    LOOP AT lt_stb_merge_final ASSIGNING <fs_stb_merge_final>.
      SELECT SINGLE ausss FROM marc INTO lv_ausss
        WHERE matnr = <fs_stb_merge_final>-idnrk
          AND werks = <fs_stb_merge_final>-werks.
      IF sy-subrc = 0.
        <fs_stb_merge_final>-mnglg = <fs_stb_merge_final>-mnglg * ( 1 + lv_ausss / 100 ).
      ENDIF.
    ENDLOOP.
  ENDIF.

  ct_stb_merge_final = lt_stb_merge_final.

ENDFORM.
FORM frm_calculate_bom_req_simu USING ls_input TYPE zdt_aps2sap_pro_pro_req
                             CHANGING ct_stb_merge_final TYPE tt_stpox
                                      cv_error TYPE flag.
  DATA ls_topmat TYPE cstmat.
  DATA lt_stb TYPE TABLE OF stpox.
  DATA lt_stb_merge TYPE TABLE OF stpox.
  DATA lt_stb_merge_final TYPE TABLE OF stpox.
  DATA lv_matnr TYPE matnr.
  DATA lv_werks TYPE werks_d.
  DATA lv_verid TYPE verid.
  DATA lv_datuv TYPE sy-datum.
  DATA lv_emeng TYPE basmn.
  DATA lv_mtart TYPE mtart.
  DATA ls_order_data TYPE bapi_pi_order_create.
  DATA ls_return TYPE bapiret2.
  DATA lv_ausss TYPE ausss.

*-------首先根据物料主数据的装配报废率（MARC-AUSSS）计算完整需求量
  lv_matnr = ls_input-matnr.
  lv_werks = ls_input-werks.
  lv_emeng = ls_input-gamng."需求量
  lv_datuv = ls_input-gstrp."基本开始日期
  lv_verid = ls_input-verid.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      aumng                 = '1'              "不考虑展开物料报废率
      auskz                 = 'X'              "计算所有损耗量（主数据损耗MARC-AUSSS & BOM行损耗STPO-AUSCH）
      capid                 = 'PI01'           " 流程制造
      datuv                 = lv_datuv         " 有效起始日
      ehndl                 = '1'              "考虑使用可能性
      emeng                 = lv_emeng         " 需求数量
      mehrs                 = 'X'              "多层展开
      mmory                 = '1'
      mtnrv                 = lv_matnr        "展开物料
      stlan                 = '1'              " BOM用途
      werks                 = lv_werks         "物料所在工厂
      verid                 = lv_verid         "生产版本
    IMPORTING
      topmat                = ls_topmat
    TABLES
      stb                   = lt_stb
    EXCEPTIONS
      alt_not_found         = 1
      call_invalid          = 2
      material_not_found    = 3
      missing_authorization = 4
      no_bom_found          = 5
      no_plant_data         = 6
      no_suitable_bom_found = 7
      conversion_error      = 8
      OTHERS                = 9.
  IF sy-subrc <> 0.
    cv_error = 'X'.
    RETURN.
  ENDIF.
*----------重组生产S所需的所有半成品需求量（合并相同物料避免重复计算固定损耗）
  SORT lt_stb BY idnrk fmeng.
  DELETE lt_stb WHERE mnglg < 0.
  LOOP AT lt_stb INTO DATA(ls_stb) WHERE mtart = 'Z030'."半成品
    READ TABLE lt_stb_merge ASSIGNING FIELD-SYMBOL(<fs_stb_merge>) WITH KEY idnrk = ls_stb-idnrk."合并计算组件
    IF sy-subrc = 0."固定损耗
      IF ls_stb-fmeng = 'X'."固定损耗
        READ TABLE lt_stb_merge   WITH KEY idnrk = ls_stb-idnrk fmeng = 'X' TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0."合并计算组件报废率（STPO-AUSCH）的组件需求数
          APPEND INITIAL LINE TO lt_stb_merge ASSIGNING FIELD-SYMBOL(<fs_stb_merge_fixed>).
          IF <fs_stb_merge_fixed> IS ASSIGNED.
            <fs_stb_merge_fixed> = ls_stb.
          ENDIF.
        ENDIF.
      ELSE."非固定损耗
        <fs_stb_merge>-mnglg = <fs_stb_merge>-mnglg + ls_stb-mnglg.
      ENDIF.
    ELSE."非固定损耗
      APPEND INITIAL LINE TO lt_stb_merge ASSIGNING <fs_stb_merge>.
      IF <fs_stb_merge> IS ASSIGNED.
        <fs_stb_merge> = ls_stb.
      ENDIF.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_stb_merge ASSIGNING <fs_stb_merge>.
    READ TABLE lt_stb_merge_final ASSIGNING FIELD-SYMBOL(<fs_stb_merge_final>)
    WITH KEY idnrk = <fs_stb_merge>-idnrk."合并计算组件
    IF sy-subrc = 0.
      <fs_stb_merge_final>-mnglg = <fs_stb_merge_final>-mnglg + <fs_stb_merge>-mnglg.
    ELSE.
      APPEND INITIAL LINE TO lt_stb_merge_final ASSIGNING <fs_stb_merge_final>.
      IF <fs_stb_merge_final> IS ASSIGNED.
        <fs_stb_merge_final> = <fs_stb_merge>.
      ENDIF.
    ENDIF.
  ENDLOOP.
  ct_stb_merge_final = lt_stb_merge_final.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_ORDER_SMALL_M
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_INPUT
*&      --> LV_AUFNR_IN
*&      <-- LT_LOG
*&---------------------------------------------------------------------*
FORM frm_change_order_small_m   USING is_input TYPE zdt_aps2sap_pro_u_pro_req
                                     iv_aufnr_in TYPE aufnr
                                 CHANGING ct_log TYPE tt_zpitlog0005.
*m订单和S订单不同，S订单需要处理S本身以及S下阶的c和g订单，
*但m已经拆分为一层结构（原m和拆分出的新m在同一层级），因此处理m只需直接处理zppt0002里的拆分完的m即可
  DATA ls_log TYPE zpitlog0005.
  DATA ls_input TYPE zdt_aps2sap_pro_u_pro_req.
  DATA lv_longtext TYPE string.

  ls_input = is_input.

  SELECT zmaufnr_ori, zmaufnr_new, zmgsmng_ori, zmgsmng_new FROM zppt0002
    INTO TABLE @DATA(lt_zppt0002)
    WHERE zmaufnr_ori = @iv_aufnr_in.
  IF sy-subrc = 0.
    SORT lt_zppt0002 BY zmaufnr_new ASCENDING."原始m在第一行
    LOOP AT lt_zppt0002 INTO DATA(ls_zppt0002).
      MOVE-CORRESPONDING ls_input TO ls_log.
      ls_log-aufnr_ref = ls_zppt0002-zmaufnr_ori.
      ls_log-gamng = ls_zppt0002-zmgsmng_ori.
      ls_log-uname = sy-uname.
      ls_log-datum = sy-datum.
      ls_log-uzeit = sy-uzeit.
******如果m已拆单，m数量以拆分结果为准，不允许通过接口修改********************************
      ls_input-gamng = ls_zppt0002-zmgsmng_ori.
      "不允许修改原始m单的批号，由m拆分出的新m批号也不可修改
      CLEAR ls_input-charg.
      PERFORM frm_change_order1 USING ls_input ls_zppt0002-zmaufnr_new CHANGING ls_log.
      IF ls_log-type = 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        DELETE ct_log WHERE aufnr_ref = iv_aufnr_in."删除与该m相关的正确消息
        APPEND ls_log TO ct_log.
*        EXIT."更新出错，不再继续更新
      ELSE.
        APPEND ls_log TO ct_log."返回m订单日志正确处理消息
      ENDIF.
    ENDLOOP.
    IF ls_log-type <> 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
*      WAIT UP TO 1 SECONDS.
      LOOP AT lt_zppt0002 INTO ls_zppt0002.
        READ TABLE ct_log ASSIGNING FIELD-SYMBOL(<fs_log>) WITH KEY aufnr = ls_zppt0002-zmaufnr_new aufnr_ref = ls_zppt0002-zmaufnr_ori.
        IF sy-subrc = 0.
          lv_longtext = '/内部订单号:' && ls_input-zrsv01 && '/成本中心号:' && ls_input-zrsv02 && '/备注（半成品报废）:' && ls_input-zrsv03 && '/长文本：' && ls_input-ztext.
          IF lv_longtext IS NOT INITIAL."修改长文本
*            PERFORM frm_save_long_text_change USING ls_zppt0002-zmaufnr_new ls_zppt0002-zmaufnr_ori lv_longtext ls_log CHANGING <fs_log>.
            DATA(lt_text) = VALUE zsds_so_text_t( tdobject = 'AUFK' tdname = sy-mandt && ls_zppt0002-zmaufnr_ori ( tdid = 'KOPF' text = lv_longtext ) ). "创建长文本 add by litao 20191202
            PERFORM frm_create_text USING lt_text.
          ENDIF.
*---------下达生产订单
          IF ls_input-status = 'REL'.
            PERFORM frm_release_order_chg USING ls_zppt0002-zmaufnr_new CHANGING <fs_log>.
*            PERFORM frm_set_release USING ls_zppt0002-zmaufnr_new CHANGING <fs_log>-type <fs_log>-message. "下达订单 add by litao 20191202
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
FORM frm_change_order_small_m_simu USING is_input TYPE zdt_aps2sap_pro_u_pro_req
                                     iv_aufnr_in TYPE aufnr
                                CHANGING cs_output TYPE zdt_aps2sap_pro_u_ret_pro_ret
                                          cv_error TYPE flag.
*m订单和S订单不同，S订单需要处理S本身以及S下阶的c和g订单，
*但m已经拆分为一层结构（原m和拆分出的新m在同一层级），因此处理m只需直接处理zppt0002里的拆分完的m即可
  DATA ls_input TYPE zdt_aps2sap_pro_u_pro_req.
  DATA lv_gamng_ori TYPE gamng.

  ls_input = is_input.

  SELECT SINGLE gamng_ori FROM zppt0001 INTO lv_gamng_ori
    WHERE aufnr = iv_aufnr_in.
  IF sy-subrc = 0 AND lv_gamng_ori <> is_input-gamng.
    cv_error = 'X'.
    cs_output-type = 'E'.
    cs_output-message = '订单' && iv_aufnr_in  && '已拆分所以数量不允许更改,原数量' && lv_gamng_ori.
    RETURN.
  ENDIF.

  SELECT zmaufnr_ori, zmaufnr_new, zmgsmng_new FROM zppt0002
    INTO TABLE @DATA(lt_zppt0002)
    WHERE zmaufnr_ori = @iv_aufnr_in.
  IF sy-subrc = 0.
    SORT lt_zppt0002 BY zmaufnr_new ASCENDING."原始m在第一行
    LOOP AT lt_zppt0002 INTO DATA(ls_zppt0002).
******如果m已拆单，m数量以拆分结果为准，不允许通过接口修改********************************
      ls_input-gamng = ls_zppt0002-zmgsmng_new.
      "不允许修改原始m单的批号，由m拆分出的新m批号也不可修改
      CLEAR ls_input-charg.
      PERFORM frm_change_order_simu USING is_input ls_zppt0002-zmaufnr_new  '' CHANGING cv_error cs_output-message.
      IF cv_error = 'X'.
        cs_output-aufnr = iv_aufnr_in.
        cs_output-type = 'E'.
        cs_output-message = '订单的拆分订单' && ls_zppt0002-zmaufnr_new  && '模拟更新失败' && cs_output-message.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'."不更新数据库
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_ORDER_CREATE_DATA_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> INPUT
*&      <-- OUTPUT
*&      <-- LV_ERROR
*&---------------------------------------------------------------------*
FORM frm_order_create_data_check  USING   input TYPE zmt_aps2sap_pro
                                  CHANGING output TYPE zmt_aps2sap_pro_ret
                                           cv_error TYPE flag.
  DATA lv_len TYPE i.
  LOOP AT input-mt_aps2sap_pro-pro_req INTO DATA(ls_input).
    APPEND INITIAL LINE TO output-mt_aps2sap_pro_ret-pro_ret ASSIGNING FIELD-SYMBOL(<fs_output>).
    IF <fs_output> IS ASSIGNED.
      MOVE-CORRESPONDING ls_input TO <fs_output>.
      IF ls_input-guid = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'guid唯一标识码必填'.
      ELSEIF ls_input-matnr = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'matnr物料号必填'.
      ELSEIF ls_input-werks = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'werks工必填厂'.
      ELSEIF ls_input-auart = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'auart订单类型必填'.
      ELSEIF ls_input-gamng = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'gamng流程订单数量必填'.
      ELSEIF ls_input-gstrp = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'gstrp基本开始日期必填'.
      ELSEIF ls_input-gltrp = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'gltrp基本完成日期必填'.
      ELSEIF ls_input-verid = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'verid生产版本必填'.
      ELSEIF ls_input-status = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'status状态必填'.
      ELSEIF ls_input-zstcq = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'zstcq厂区必填'.
      ELSEIF ls_input-zchej = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'zchej车间必填'.
      ELSEIF ls_input-zchanx = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'zchanx产线必填'.
      ENDIF.
      "销售订单号长度超过10位，后面的 CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'会抛出异常
      lv_len = strlen( ls_input-kdauf ).
      IF lv_len > 10.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'kdauf销售订单号长度不能超过10位'.
      ENDIF.
      IF <fs_output>-type = 'E'.
        cv_error = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF cv_error <> 'X'.
    REFRESH output-mt_aps2sap_pro_ret-pro_ret.
  ENDIF.
ENDFORM.

FORM frm_order_change_data_check  USING   input TYPE zmt_aps2sap_pro_u
                                  CHANGING output TYPE zmt_aps2sap_pro_u_ret
                                           cv_error TYPE flag.
  LOOP AT input-mt_aps2sap_pro_u-pro_req INTO DATA(ls_input).
    APPEND INITIAL LINE TO output-mt_aps2sap_pro_u_ret-pro_ret ASSIGNING FIELD-SYMBOL(<fs_output>).
    IF <fs_output> IS ASSIGNED.
      MOVE-CORRESPONDING ls_input TO <fs_output>.
      IF ls_input-aufnr = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'aufnr流程订单号识码必填'.
      ELSEIF ls_input-gamng = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'gamng流程订单数量必填'.
      ELSEIF ls_input-gstrp = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'gstrp基本开始日期必填'.
      ELSEIF ls_input-gltrp = ''.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'gltrp基本完成日期必填'.
      ELSEIF ls_input-verid IS INITIAL.
        <fs_output>-type = 'E'.
        <fs_output>-message = 'verid生产版本必填'.
      ENDIF.
      IF <fs_output>-type = 'E'.
        cv_error = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF cv_error <> 'X'.
    REFRESH output-mt_aps2sap_pro_u_ret-pro_ret.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_S_CREATE_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_INPUT
*&      <-- LV_ERROR
*&      <-- LV_AUFNR
*&      <-- <FS_OUTPUT>
*&      <-- <FS_LOG>
*&---------------------------------------------------------------------*
FORM frm_s_create_simu  USING  is_input TYPE zdt_aps2sap_pro_pro_req
                               CHANGING cv_error TYPE flag
                                        cv_aufnr TYPE aufnr
                                        cs_output TYPE zdt_aps2sap_pro_ret_pro_ret
                                        cs_log TYPE zpitlog0004.
  DATA lt_stb TYPE TABLE OF stpox.
  DATA ls_stb TYPE stpox.
  DATA lt_stb_merge TYPE TABLE OF stpox.
  DATA lt_stb_merge_final TYPE TABLE OF stpox.
  DATA ls_order_data TYPE bapi_pi_order_create.
  DATA ls_return TYPE bapiret2.
  DATA lv_aufnr TYPE aufnr.
  DATA lv_vbeln TYPE vbeln.
  DATA ls_input TYPE zdt_aps2sap_pro_pro_req.
  ls_input = is_input.

  PERFORM frm_create_process_order_simu USING ls_input CHANGING cv_error cv_aufnr cs_output cs_log.
  IF cv_error IS INITIAL.
*-------------------------------处理S展开的子订单-------------------------------
    PERFORM frm_calculate_bom_req_simu USING ls_input CHANGING lt_stb_merge_final cv_error.
****modify by hj 20191027
*    IF cv_error = 'X' OR lt_stb_merge_final IS INITIAL.
****end by
    IF cv_error = 'X'.
      cv_error = 'X'.
      cs_output-type = 'E'.
      cs_output-message = 'S订单预展开BOM失败'.
      cs_log-type = 'E'.
      cs_log-message = 'S订单预展开BOM失败'.
      RETURN.
    ENDIF.
*----------为生产S所需的所有半成品创建流程订单
    LOOP AT lt_stb_merge_final INTO ls_stb."重组好的半成品
      CLEAR: ls_order_data,ls_return,lv_aufnr.
      ls_order_data-material = ls_stb-idnrk.
      ls_order_data-plant = ls_stb-werks.
      ls_order_data-order_type = ls_input-auart.
      ls_order_data-basic_start_date = ls_input-gstrp.
      ls_order_data-basic_end_date = ls_input-gltrp.
      ls_order_data-quantity = ls_stb-mnglg.
      ls_order_data-quantity_uom = ls_stb-meins.
      ls_order_data-prod_version = ls_input-verid.

      CALL FUNCTION 'ZBAPI_PROCORD_CREATE'
        EXPORTING
          orderdata    = ls_order_data
        IMPORTING
          return       = ls_return
          order_number = lv_aufnr.
      IF ls_return-type = 'E' OR ls_return-type = 'A'.
        cv_error = 'X'.
        cs_output-type = 'E'.
        cs_output-message = 'S子订单预创建失败' && ls_return-message..
        CLEAR: cs_log-aufnr.
        cs_log-type = 'E'.
        cs_log-message = 'S子订单预创建失败' && ls_return-message.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
      IF cv_error = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DYTK
*&---------------------------------------------------------------------*
*& 读取并转换短溢条款
*&---------------------------------------------------------------------*
*&      --> LV_DYTK
*&      <-- LS_ORDER_DATA_ZZUNTTO
*&      <-- LS_ORDER_DATA_ZZUEBTO
*&---------------------------------------------------------------------*
FORM frm_get_dytk  USING    iv_dytk TYPE zsd_dytk
                   CHANGING cv_zzuntto TYPE untto
                            cv_zzuebto TYPE uebto.

  DATA lt_dd07v TYPE TABLE OF dd07v.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZD_DYTK' "需求类型
      text           = 'X'
      langu          = '1'
      bypass_buffer  = 'X'
    TABLES
      dd07v_tab      = lt_dd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  IF sy-subrc = 0.
    READ TABLE lt_dd07v INTO DATA(ls_dd07v)
    WITH KEY domvalue_l = iv_dytk.
    IF sy-subrc = 0 AND ls_dd07v-ddtext IS NOT INITIAL.
      CASE ls_dd07v-ddtext(1).
        WHEN '+'.
          SHIFT ls_dd07v-ddtext.
          IF ls_dd07v-ddtext CO '1234567890. '.
            cv_zzuebto = ls_dd07v-ddtext."交货过量限度
          ENDIF.
          cv_zzuntto = 0."交货不足限度
        WHEN '-'.
          SHIFT ls_dd07v-ddtext.
          IF ls_dd07v-ddtext CO '1234567890. '.
            cv_zzuntto = ls_dd07v-ddtext."交货不足限度
          ENDIF.
          cv_zzuebto = 0."交货过量限度
        WHEN '±'.
          SHIFT ls_dd07v-ddtext.
          IF ls_dd07v-ddtext CO '1234567890. '.
            cv_zzuntto = ls_dd07v-ddtext."交货不足限度
          ENDIF.
          IF ls_dd07v-ddtext CO '1234567890. '.
            cv_zzuebto = ls_dd07v-ddtext."交货过量限度
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_create_text
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_create_text USING it_data TYPE zsds_so_text_t.

  CONSTANTS: lc_size TYPE i VALUE 132.

  DATA: lt_lines TYPE tline_tab,
        l_text   TYPE string.

  LOOP AT it_data INTO DATA(ls_data) WHERE text NE space.
    CLEAR lt_lines.
    l_text = ls_data-text.
    DATA(l_count) = strlen( l_text ) DIV lc_size + 1.
    DO l_count TIMES.
      DATA(l_size) = strlen( l_text ).
      IF l_size GT lc_size. l_size = lc_size. ENDIF.
      APPEND VALUE #( tdline = l_text(l_size) ) TO lt_lines.
      SHIFT l_text BY l_size PLACES.
    ENDDO.

    CALL FUNCTION 'CREATE_TEXT'
      EXPORTING
        flanguage = sy-langu
        fid       = ls_data-tdid
        fname     = ls_data-tdname
        fobject   = ls_data-tdobject
        fformat   = space
      TABLES
        flines    = lt_lines
      EXCEPTIONS
        no_init   = 1
        no_save   = 2
        OTHERS    = 3.

    IF sy-subrc EQ 0.
      UPDATE aufk SET ltext = sy-langu ktext = ls_data-text WHERE aufnr EQ ls_data-tdname+3(12).
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_release
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_release USING i_aufnr TYPE aufnr CHANGING c_type c_message.

  DATA l_objnr TYPE jest-objnr.

  l_objnr = 'OR' && i_aufnr.
  CALL FUNCTION 'I_CHANGE_STATUS'
    EXPORTING
      objnr          = l_objnr
      estat_inactive = 'I0001' "CRTD
      estat_active   = 'I0002' "REL
    EXCEPTIONS
      cannot_update  = 1
      OTHERS         = 2.

  IF sy-subrc NE 0.
    c_type = 'W'.
    c_message = '订单' && i_aufnr && '下达失败'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CREATE_JOB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_create_job .
  DATA ls_job TYPE tbtcjob.

  ls_job-jobname = 'Z_PP_PI_PROCESS_ORDER_CREATE'.
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = ls_job-jobname
    IMPORTING
      jobcount         = ls_job-jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'JOB_SUBMIT'
    EXPORTING
      authcknam       = sy-uname
      jobcount        = ls_job-jobcount
      jobname         = ls_job-jobname
      report          = 'ZPPR0030'
    IMPORTING
      step_number     = ls_job-stepcount
    EXCEPTIONS
      bad_priparams   = 1
      bad_xpgflags    = 2
      invalid_jobdata = 3
      OTHERS          = 4.

  CHECK sy-subrc EQ 0.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = ls_job-jobcount
      jobname              = ls_job-jobname
      strtimmed            = 'X'
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      OTHERS               = 4.

ENDFORM.