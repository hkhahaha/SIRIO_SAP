FUNCTION z_mm_send_reservatio_to_wms2.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  EXPORTING
*"     REFERENCE(ERROR) TYPE  ZDT_SAP2WMS_OTHER_RET
*"  TABLES
*"      TI_RKPF STRUCTURE  RKPF
*"      TI_RESB STRUCTURE  RESB
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& DEFINE CONSTANTS
*&---------------------------------------------------------------------*
  CONSTANTS: gc_msgty_success      TYPE c VALUE 'S',
             gc_msgty_error        TYPE c VALUE 'E',
             gc_zdwxz_gongsi       TYPE c VALUE '1',
             gc_otype_organization TYPE text02 VALUE 'O',
             gc_inif_id(6)         TYPE c VALUE 'MM026', "接口编号
             gc_src_system(3)      TYPE c VALUE 'SAP',    "源系统
             gc_dest_system(3)     TYPE c VALUE 'WMS'.    "目标系统

  DATA:lo_clientproxy       TYPE REF TO zco_si_sap2wms_other_out,
       lo_system_fault      TYPE REF TO cx_ai_system_fault,
       lo_ack               TYPE REF TO if_ws_acknowledgment,
       lo_ack_status_simple TYPE prx_ack_status,
       lo_ack_status_detail TYPE prx_ack_status_detail_table,
       lo_ack_request       TYPE prx_ack_request_details,
       lo_async_messaging   TYPE REF TO if_wsprotocol_async_messaging,
       lo_msg_id_protocol   TYPE REF TO if_wsprotocol_message_id,
       lv_msg_id            TYPE sxmsguid,
       lv_error             TYPE c,
       lv_msgty             TYPE sy-msgty,
       lv_msg               TYPE string,
       ls_output            TYPE zmt_sap2wms_other,
       ls_head              TYPE zdt_sap2wms_other,
       lt_item              TYPE zdt_sap2wms_other_list_of_tab1,
       ls_item              TYPE zdt_sap2wms_other_list_of_oth1.
*
  DATA:is_resb TYPE resb,
       is_rkpf TYPE rkpf,
       lt_char TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
       ls_char TYPE bapi1003_alloc_values_char,
       lt_num  TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
       ls_num  TYPE bapi1003_alloc_values_num,
       lt_curr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
       lt_ret  TYPE STANDARD TABLE OF bapiret2,
       wa_char TYPE zdt_sap2wms_other_list_of_othe.
  DATA: ls_record TYPE zmm026_record.
  DATA:ls_input TYPE zmt_sap2wsm_other_ret.
  CLEAR:lv_error,
        lv_msgty,
        lv_msg,
        ls_output,
        lv_msg_id,
        ls_head.
  DATA:l_objek TYPE cuobn.
  DATA: ls_outx   TYPE ausp-atwrt,
        ls_stcode TYPE ausp-atwrt,
        ls_date   TYPE ausp-atwrt.
  DATA:l_flag TYPE i.
  DATA: l_wms_sys   TYPE zmm_lgort-wms_sys,
        l_wms_sys_x TYPE zmm_lgort-wms_sys.
  DATA: lt_conv TYPE TABLE OF zmm_conv_charact.
*&---------------------------------------------------------------------*
* 1实例化proxy
*&---------------------------------------------------------------------*
  TRY.
      CREATE OBJECT lo_clientproxy.
      lo_async_messaging ?= lo_clientproxy->get_protocol( if_wsprotocol=>async_messaging ).
*     ask for transport acknowledgment
      CLEAR lo_ack_request.
      lo_ack_request = if_wsprotocol_async_messaging=>co_transport_acknowledgment.
      lo_async_messaging->set_acknowledgment_requested( lo_ack_request ).
    CATCH cx_ai_system_fault INTO lo_system_fault.
      lv_msg   = lo_system_fault->get_text( ).
      lv_msgty = gc_msgty_error.
      lv_error = abap_true.
  ENDTRY.
*&---------------------------------------------------------------------*
* 1赋值部分
*&---------------------------------------------------------------------*
*  DATA:l_noteno(10) TYPE c.
*  WAIT UP TO 2 SECONDS.
*  IF ti_rkpf[] IS INITIAL.
*    SELECT * FROM rkpf
*     INTO TABLE ti_rkpf
*      FOR ALL ENTRIES IN ti_resb
*      WHERE rsnum = ti_resb-rsnum.
*  ENDIF.

  DATA: l_lines TYPE i.
  DATA: lv_num TYPE zesapnote_no.
  DATA: x_a05 TYPE c.
  DATA: lv_time TYPE sy-uzeit.
  DATA: lv_werks TYPE mseg-werks.
  DATA lv_zsap_note TYPE rkpf-zsap_note.
*  BREAK abap05.


  SELECT *
  INTO TABLE lt_conv
  FROM zmm_conv_charact.

  LOOP AT   ti_rkpf INTO is_rkpf.

    LOOP AT ti_resb WHERE rsnum = is_rkpf-rsnum
                     AND ( lgort+0(1) = '7'
                        OR lgort+0(2) = '97').
      x_a05 = 'X'.
    ENDLOOP.
    "MB21参照预留号创建新预留单据时，通知单号也会被复制过来，所以这里需要清空再重新生成
    IF sy-tcode = 'MB21' .
      CLEAR is_rkpf-zsap_note.
    ENDIF.
    IF is_rkpf-zsap_note IS  INITIAL.
      IF is_rkpf-bwart = '311' AND x_a05 = 'X'.
        SELECT SINGLE  wms_sys
         INTO l_wms_sys_x
         FROM zmm_lgort
         WHERE lgort = is_rkpf-umlgo.
*          READ TABLE it_resb with key rsnum = is_rkpf-rsnum
        IF l_wms_sys_x IS NOT INITIAL .
          lv_num = zcl_cm_public=>get_bus_no( iv_bwart = '399' ).
        ENDIF.
      ELSE.
        lv_num = zcl_cm_public=>get_bus_no( iv_bwart = is_rkpf-bwart ).
      ENDIF.
      ls_record-zreserv_no = lv_num.
      is_rkpf-zsap_note = lv_num.
      ls_record-rsnum = is_rkpf-rsnum.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      is_rkpf-zsap_note = lv_num.
      MODIFY ti_rkpf FROM is_rkpf  TRANSPORTING zsap_note.

    ELSE.

    ENDIF.
    ls_head-rsnum = is_rkpf-rsnum.

    ls_head-sapnote_no =  is_rkpf-zsap_note.
    ls_head-bwart = is_rkpf-bwart.


***************** 抬头文本修改 add by sujx 20190111****************************
*    SELECT SINGLE btext INTO is_rkpf-bktxt FROM t156ht
*      WHERE bwart = is_rkpf-bwart
*      AND   spras = '1'.


    ls_head-werks = is_rkpf-umwrk.
    ls_head-usnam = is_rkpf-usnam.
    ls_head-note  = is_rkpf-bktxt.

    IF is_rkpf-bwart = '311' AND x_a05 = 'X'.
      SELECT SINGLE bus_typ shkzg FROM ztbus_mvt
    INTO ( ls_head-bus_type ,ls_head-shkzg )
    WHERE bwart = '399'.

    ELSE.
      SELECT SINGLE bus_typ FROM ztbus_mvt
         INTO ls_head-bus_type
         WHERE bwart = is_rkpf-bwart.

      SELECT SINGLE shkzg FROM ztbus_mvt
        INTO ls_head-shkzg
        WHERE bwart = is_rkpf-bwart.
    ENDIF.

    CHECK ls_head-bus_type IS NOT INITIAL.
    LOOP AT ti_resb INTO is_resb WHERE rsnum = is_rkpf-rsnum .
      l_lines =  l_lines + 1.
*********************检查仓库是否与WMS相关  不相关 不下法***************************
      IF is_rkpf-bwart = 'Z61'
        OR is_rkpf-bwart = 'Y61'
        OR is_rkpf-bwart = '511'
        OR is_rkpf-bwart = '512'
        OR is_rkpf-bwart = '201'
        OR is_rkpf-bwart = '291' "addby ljm20191218
        OR is_rkpf-bwart = 'Z55' "addby waymon 2020/01/10.
        OR is_rkpf-bwart = '311'.
        SELECT SINGLE  wms_sys
          INTO l_wms_sys
          FROM zmm_lgort
          WHERE lgort = is_resb-lgort.


        SELECT SINGLE  wms_sys
         INTO l_wms_sys_x
         FROM zmm_lgort
         WHERE lgort = is_resb-umlgo.
        IF l_wms_sys_x IS NOT  INITIAL OR l_wms_sys IS NOT INITIAL.
          l_flag = l_flag + 1.
        ENDIF.
      ELSE.
        l_flag = l_flag + 1.
      ENDIF.
      CHECK l_flag  IS NOT INITIAL.
      ls_item-sapnote_line = is_resb-rspos.
*      ls_head-xclosed_h = is_resb-xloek.
      ls_item-matnr = zcl_bc_public=>conv_by_ddic( i_input = is_resb-matnr i_out = 'X' ) .

      SELECT SINGLE maktx INTO ls_item-maktx
        FROM makt
        WHERE matnr = is_resb-matnr
        AND   spras = '1'.
      ls_item-charg = is_resb-charg.
      CLEAR: lt_char[],lt_char.
      IF is_resb-charg IS NOT INITIAL.
********************** 8199 工厂转换为 8100 工厂 ADD by sujx   20191101 ***************************
        IF is_resb-werks = '8199'.
*          lv_werks = '8100'.
          lv_werks = '8199'.
        ELSE.
          lv_werks = is_resb-werks.
        ENDIF.

        CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
          EXPORTING
            i_matnr = is_resb-matnr
            i_charg = is_resb-charg
            i_werks = lv_werks
          IMPORTING
            e_objek = l_objek.

        CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
          EXPORTING
            objecttable     = 'MCHA'
            classnum        = 'ZBATCH_SIRIO'
            classtype       = '022'
            objectkey_long  = l_objek
          TABLES
            allocvaluesnum  = lt_num
            allocvalueschar = lt_char
            allocvaluescurr = lt_curr
            return          = lt_ret.

        LOOP AT lt_char INTO ls_char .
          wa_char-atnam  = ls_char-charact .
          IF ls_char-value_char IS INITIAL.
            wa_char-atwrt  = ls_char-value_char_long.
          ELSE.
            wa_char-atwrt  = ls_char-value_char.
          ENDIF.

          APPEND wa_char TO ls_item-lines_of_character.
          CLEAR :ls_char,wa_char.
        ENDLOOP.
        LOOP AT lt_num INTO ls_num .
          IF ls_num-charact = 'ZHSDAT' OR ls_num-charact = 'ZVFDAT'
            OR ls_num-charact = 'ZLWEDT'.
            CALL FUNCTION 'CTCV_CONVERT_FLOAT_TO_DATE'
              EXPORTING
                float = ls_num-value_from
              IMPORTING
                date  = ls_date.
            wa_char-atwrt = ls_date.
          ELSE.
            wa_char-atwrt  = ls_num-value_from.
          ENDIF.
          wa_char-atnam  = ls_num-charact .
          APPEND wa_char TO ls_item-lines_of_character.
          CLEAR :ls_num,wa_char.
        ENDLOOP.
        BREAK abap05.

        IF ls_item-matnr+0(2)  = '50' OR ls_item-matnr+0(2)  = '80'."20200114 waymon top.
****************** 追加 BATCH_CP 批次特性  add by sujx 20191101 ************************************
          CLEAR:lt_num,lt_char,lt_curr,lt_ret.
          CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
            EXPORTING
              objecttable     = 'MCHA'
              classnum        = 'BATCH_CP'
              classtype       = '022'
              objectkey_long  = l_objek
            TABLES
              allocvaluesnum  = lt_num
              allocvalueschar = lt_char
              allocvaluescurr = lt_curr
              return          = lt_ret.

          LOOP AT lt_char INTO ls_char .
            READ TABLE lt_conv INTO DATA(ls_conv) WITH KEY zsource_field = ls_char-charact.
            IF sy-subrc = 0.
              wa_char-atnam  = ls_conv-ztarget_field .
            ELSE.
              wa_char-atnam  = ls_char-charact .
            ENDIF.
            wa_char-atwrt  = ls_char-value_char.
            APPEND wa_char TO ls_item-lines_of_character.
            CLEAR :ls_char,wa_char.
          ENDLOOP.
          LOOP AT lt_num INTO ls_num .
            IF ls_num-charact = 'Z_BATCH_SCRQ' OR ls_num-charact = 'Z_BATCH_YXQZ'.
              CALL FUNCTION 'CTCV_CONVERT_FLOAT_TO_DATE'
                EXPORTING
                  float = ls_num-value_from
                IMPORTING
                  date  = ls_date.
              wa_char-atwrt = ls_date.
            ELSE.
              wa_char-atwrt  = ls_num-value_from.
            ENDIF.
            READ TABLE lt_conv INTO ls_conv WITH KEY zsource_field = ls_num-charact.
            IF sy-subrc = 0.
              wa_char-atnam  = ls_conv-ztarget_field .
            ELSE.
              wa_char-atnam  = ls_num-charact .
            ENDIF.
            APPEND wa_char TO ls_item-lines_of_character.
            CLEAR :ls_num,wa_char.
          ENDLOOP.

        ENDIF."20200114 waymon end.
      ENDIF.


      SELECT SINGLE mseht INTO ls_item-mseht
         FROM t006a
        WHERE msehi = is_resb-meins
        AND   spras = '1'.
      IF  is_rkpf-bwart = 'Y11'
         OR is_rkpf-bwart = 'Y44'
         OR is_rkpf-bwart = '511'.
        ls_item-lgort = is_resb-umlgo.
        ls_item-dlgort = is_resb-lgort.
      ELSE.
        ls_item-lgort = is_resb-lgort.
        ls_item-dlgort = is_resb-umlgo.
      ENDIF.
      IF  is_rkpf-bwart = '511'.
        CLEAR ls_item-lgort.
      ENDIF.
      CASE is_rkpf-bwart .
        WHEN '201' OR '291'."add by ljm20191218
          ls_item-dlgort = 'OT'.
        WHEN 'Z55'."addby waymon 2020/01/10.
          ls_item-dlgort = 'OT'."addby waymon 2020/01/10.
          ls_item-stock_st = 'B'.     "addby waymon 2020/01/14.
        WHEN 'Z61' .
          ls_item-dlgort = 'OT'.
        WHEN '512'.
          ls_item-dlgort = 'OT'.
        WHEN 'Y61'.
          ls_item-dlgort = 'OT'.
        WHEN 'X11' .
          ls_item-dstock_st = 'I'.
        WHEN OTHERS.
      ENDCASE.
      ls_item-lifnr =  zcl_bc_public=>conv_by_ddic( i_input = is_resb-lifnr i_out = 'X' ).

      ls_item-menge = is_resb-erfmg.
      ls_item-meins = is_resb-meins.
      ls_item-bdter = is_resb-bdter.
      IF  is_rkpf-bwart = '311'
        AND x_a05 IS NOT INITIAL
        AND is_resb-sgtxt IS NOT INITIAL.
        CONDENSE is_resb-sgtxt NO-GAPS.
        SPLIT is_resb-sgtxt AT '/' INTO ls_item-zrsv02 ls_item-zrsv03.
*        item-zrsv02 = is_resb-sgtxt.
      ENDIF.
      IF is_rkpf-bktxt IS NOT INITIAL
        AND  is_rkpf-bwart = 'Z11'
        AND ( sy-tcode = 'MB21' OR sy-tcode = 'MB22' ).

        lv_time = is_rkpf-bktxt.

        CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
          EXPORTING
            time                      = lv_time
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc = 0. "如果返回非0，则时间不合法
          ls_item-bdztp =  is_rkpf-bktxt.
        ELSE.
*          MESSAGE 'Z11抬头日期非法' TYPE 'E'.
          error-returncode =   '001'.
          error-returndesc =   'Z11抬头日期非法' .
          error-returnflag =   'E'.
        ENDIF.
      ELSE.
        ls_item-bdztp = is_resb-bdztp.
      ENDIF.

*去除is_resb-kdauf的前导零 20200304 add by waymon.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = is_resb-kdauf
        IMPORTING
          output = is_resb-kdauf.

*      IS_RESB-wempf
      ls_item-ablad = is_resb-ablad.
*      ls_item-sobkz = is_rkpf-sobkz.
      ls_item-kdauf = is_resb-kdauf.
      ls_item-kdpos = is_resb-kdpos.
      ls_item-shkzg = is_resb-shkzg.
      ls_item-dwerks = is_resb-umwrk.
      ls_item-aufnr = is_resb-aufnr.
      ls_item-kostl = is_rkpf-kostl.
      ls_item-sgtxt = is_resb-sgtxt.
      ls_item-bdter = is_resb-bdter.
      READ TABLE lt_char INTO ls_char WITH KEY charact = 'ZOUT_X'.
      IF sy-subrc = 0.
        ls_outx =  ls_char-value_char.
      ENDIF.
      READ TABLE lt_char INTO ls_char WITH KEY charact = 'ZST_CODE'.
      IF sy-subrc = 0.
        ls_stcode = ls_char-value_char.
      ENDIF.

      SELECT SINGLE wms_ext_st INTO ls_item-wms_ext_st
        FROM zmmwms_ext_st
        WHERE zout_x = ls_outx
        AND  zst_code = ls_stcode
        AND  xconsign = is_rkpf-sobkz.
      CLEAR:ls_outx,ls_stcode.
      "如果bus_type为B02或B03且charg批次为空时，不传wms_ext_st外寄+质量标识+寄售 ADD BY LJM 20191219
      IF ( ls_head-bus_type = 'B02' OR ls_head-bus_type = 'B03' ) AND ls_item-charg IS INITIAL.
        CLEAR ls_item-wms_ext_st.
      ENDIF.
      SELECT SINGLE wms_custid INTO ls_head-wms_custid
        FROM zmmwms_sys
        WHERE werks = is_resb-werks.
      ls_head-werks = is_resb-werks.
      IF is_rkpf-bwart = 'Y44' OR is_rkpf-bwart = '325'.
        ls_item-stock_st = 'B'.
      ENDIF.
      IF  is_rkpf-bwart = '325'..
        ls_item-dstock_st = 'B'.
      ENDIF.
      IF   is_resb-xloek = ''.
        APPEND ls_item TO lt_item.
      ENDIF.
      CLEAR:ls_item,is_resb.

    ENDLOOP.
    ls_head-list_of_others = lt_item.
    IF lt_item IS INITIAL..
      ls_head-xclosed_h = 'X'.
    ENDIF.

    MODIFY zmm026_record FROM ls_record.
    ls_output-mt_sap2wms_other = ls_head.
**&---------------------------------------------------------------------*
** 3数据发送
**&---------------------------------------------------------------------*

    CHECK l_flag = l_lines.
    TRY.
        CALL METHOD lo_clientproxy->si_sap2wms_other_out
          EXPORTING
            output = ls_output
          IMPORTING
            input  = ls_input.
      CATCH cx_ai_system_fault INTO lo_system_fault.
        lv_msg   = lo_system_fault->get_text( ).
        lv_error = abap_true.
        lv_msgty = gc_msgty_error.
    ENDTRY.
    BREAK abap05.
    IF ls_input-mt_sap2wsm_other_ret-returnflag <> '1'.


      IF sy-tcode = 'ZMM007'.
        error-returncode =   ls_input-mt_sap2wsm_other_ret-returncode.
        error-returndesc =   ls_input-mt_sap2wsm_other_ret-returndesc.
        error-returnflag =   ls_input-mt_sap2wsm_other_ret-returnflag.
      ELSE.
        LOOP AT ti_resb INTO is_resb.

          is_resb-xloek = 'X'.
          MODIFY ti_resb FROM is_resb INDEX sy-tabix TRANSPORTING xloek.
        ENDLOOP.

        CONCATENATE 'WMS消息:' ls_input-mt_sap2wsm_other_ret-returndesc  INTO error-returndesc  .
        error-returncode =   ls_input-mt_sap2wsm_other_ret-returncode  .
        error-returnflag =   'E'.

      ENDIF.
    ENDIF.

*      MESSAGE '' TYPE 'W'.
*  COMMIT WORK.
**&---------------------------------------------------------------------*
** 4获取message ID
**&---------------------------------------------------------------------*
    TRY .
*     get message id of sent message
        lo_msg_id_protocol ?=  lo_clientproxy->get_protocol( if_wsprotocol=>message_id ).
        lv_msg_id = lo_msg_id_protocol->get_message_id( ).
      CATCH cx_ai_system_fault INTO lo_system_fault.
        EXIT.
    ENDTRY.
*  error = 'E'.
**&---------------------------------------------------------------------*
** 5返回消息--保存日志--如必要

    CLEAR:
     ls_output           ,
     ls_head             ,
     lt_item             ,
     ls_item             .
*
    CLEAR:is_resb,
         is_rkpf ,
         lt_char ,
         ls_char ,
         lt_num  ,
         ls_num  ,
         lt_curr ,
         lt_ret  ,
         wa_char ,
         ls_record ,
        ls_input.

    CLEAR:l_objek.
    CLEAR: ls_outx   ,
          ls_stcode ,
          ls_date  .
    CLEAR:l_flag ,l_lines.
    CLEAR: l_wms_sys  ,
          l_wms_sys_x .
    CLEAR x_a05.
  ENDLOOP.
**&---------------------------------------------------------------------*
ENDFUNCTION.
