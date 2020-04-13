FUNCTION z_mm_send_note_to_wms2.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IV_NOTE_NO) LIKE  ZMM_CHANGENO-ZSAPNOTE_NO
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  ZSMESSAGE_OUT
*"----------------------------------------------------------------------

  DATA: lr_service      TYPE REF TO zco_si_sap2wms_other_out,
        lr_system_fault TYPE REF TO cx_ai_system_fault,
        ls_output       TYPE zmt_sap2wms_other,
        lt_item         TYPE zdt_sap2wms_other_list_of_tab1,
        ls_item         TYPE zdt_sap2wms_other_list_of_oth1,
        ls_input        TYPE zmt_sap2wsm_other_ret,
        lt_charg        TYPE zdt_sap2wms_other_list_of__tab,
        ls_charg        TYPE zdt_sap2wms_other_list_of_othe.

  DATA:lv_message_id TYPE zbc_guid,
       lv_intf_id    TYPE ze_intf_id.
  DATA:lv_atinn1(15),
       lv_atinn2(15),
       zhsdat(15),
       zvfdat(15),
       zlwedt(15),
       z_batch_scrq(15),
       z_batch_yxqz(15),
       wms_ext_st       TYPE zmmwms_ext_st-wms_ext_st,
       zout_x           TYPE zmmwms_ext_st-zout_x,
       zst_code         TYPE zmmwms_ext_st-zst_code,
       xconsign         TYPE zmmwms_ext_st-xconsign.
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
*
  DEFINE chang_value.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = &1
    IMPORTING
      output = &2.
  END-OF-DEFINITION.
  chang_value 'ZHSDAT' zhsdat.
  chang_value 'ZVFDAT' zvfdat.
  chang_value 'ZLWEDT' zlwedt.
  chang_value 'Z_BATCH_SCRQ' z_batch_scrq.
  chang_value 'Z_BATCH_YXQZ' z_batch_yxqz.

  DEFINE message_exit.
    output-success = &1.
    output-message = &2.
    IF &3 = 'X'.
      EXIT.
    ENDIF.
  END-OF-DEFINITION.

  CLEAR: lv_message_id,lv_intf_id.

  lv_intf_id = 'MM026'. "OA传输SAP规格标准

  CHECK zcl_bc_public=>int_actived( lv_intf_id ).

*移动类型文本


  SELECT *
  FROM zmm_changeno
  WHERE zsapnote_no = @iv_note_no
  INTO TABLE @DATA(lt_notes).

  SELECT
    *
  FROM zmmwms_ext_st
  INTO TABLE @DATA(lt_zmmwms_ext_st).




  IF lt_notes[] IS INITIAL.
    message_exit 'E' '没有找到要发送的数据' 'X'.
  ENDIF.
*      IF ls_note-bwart = '325' OR ls_note-bwart = 'X11'.
*        ls_output-mt_sap2wms_matchn-bus_type   = 'B06'.
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
  FOR ALL ENTRIES IN @lt_notes
  WHERE mcha~werks = @lt_notes-werks
    AND mcha~charg = @lt_notes-charg
    AND mcha~matnr = @lt_notes-matnr
*    AND ausp~atinn IN ( @lv_atinn1,@lv_atinn2 )
  INTO TABLE @DATA(lt_mcha).

  SELECT
  matnr,
  werks,
  charg,
  sobkz
  FROM mkol
  INTO TABLE @DATA(lt_mkol)
  FOR ALL ENTRIES IN @lt_notes
  WHERE mkol~werks = @lt_notes-werks
    AND mkol~charg = @lt_notes-charg
    AND mkol~matnr = @lt_notes-matnr.


  "首先先录入抬头的数据
  READ TABLE lt_notes INTO DATA(ls_note) INDEX 1.
  "SAP出入库号
  ls_output-mt_sap2wms_other-sapnote_no = ls_note-zsapnote_no.
  "预留单号
  ls_output-mt_sap2wms_other-rsnum = ls_note-zsapnote_no.
  "业务类型
  ls_output-mt_sap2wms_other-bus_type = 'B06'.
  "发出工厂
  ls_output-mt_sap2wms_other-werks = ls_note-werks.
  "移动类型
  ls_output-mt_sap2wms_other-bwart = '325'.
  "制单员
  ls_output-mt_sap2wms_other-usnam = sy-uname.
  "抬头备注
  ls_output-mt_sap2wms_other-note = ls_note-note.

  "填充无用的必填数据
  ls_output-mt_sap2wms_other-wms_custid = ls_note-werks.
  ls_output-mt_sap2wms_other-shkzg = 'H'.
  CLEAR ls_note.
  LOOP AT lt_notes INTO ls_note.
    CLEAR:zout_x,zst_code,xconsign,wms_ext_st.
    READ TABLE lt_mcha INTO DATA(ls_mcha) WITH KEY matnr = ls_note-matnr
                                                   charg = ls_note-charg
                                                   werks = ls_note-werks
                                                   atinn = lv_atinn1.
    "ZOUT_X外寄标识
    zout_x = ls_mcha-atwrt.
    CLEAR ls_charg.
*    ls_charg-atnam = 'ZVFDAT'.
*    ls_charg-atwrt = '20200202'.
*    APPEND ls_charg TO lt_charg.
*    ls_item-lines_of_character = lt_charg.

    CLEAR ls_mcha.
    READ TABLE lt_mcha INTO ls_mcha WITH KEY matnr = ls_note-matnr
                                             charg = ls_note-charg
                                             werks = ls_note-werks
                                             atinn = lv_atinn2.
    "ZOUT_X外寄标识
    zst_code = ls_mcha-atwrt.
    CLEAR ls_mcha.
    "XCONSIGN寄售库存标记
    READ TABLE lt_mkol INTO DATA(ls_mkol) WITH KEY matnr = ls_note-matnr
                                                   charg = ls_note-charg
                                                   werks = ls_note-werks.
    xconsign = ls_mkol-sobkz.
    CLEAR ls_mkol.

    READ TABLE lt_zmmwms_ext_st INTO DATA(ls_zmmwms_ext_st) WITH KEY zout_x = zout_x
                                                               zst_code = zst_code
                                                               xconsign = xconsign.
    ls_item-wms_ext_st = ls_zmmwms_ext_st-wms_ext_st.
    CLEAR ls_zmmwms_ext_st.
    ls_item-shkzg = 'H'.
    "通知行
    ls_item-sapnote_line = ls_note-zsapnote_line.
    "物料号
    ls_item-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_note-matnr i_out = 'X' ).
    "批号
    ls_item-charg = ls_note-charg.
    "需求日期、需求时间等字段
    ls_item-bdter = ls_note-datum.
    ls_item-bdztp = ls_note-uzeit.
    ls_item-menge = ls_note-menge.
    ls_item-meins = ls_note-meins.
    ls_item-mseht = ls_note-mseht.
    ls_item-lifnr = ls_note-lifnr.
    ls_item-kdauf = ls_note-kdauf.
    ls_item-kdpos = ls_note-kdpos.
    ls_item-stock_st = ls_note-stock_st.
    ls_item-dstock_st = ls_note-dstock_st.
    ls_item-lgort   = ls_note-lgort.
    ls_item-dwerks  = ls_note-werks.
    ls_item-dlgort  = ls_note-lgort.
*    ls_item-stock_st = 'B'.
*    ls_item-dstock_st = 'B'.
*    ls_item-wms_ext_st = '123'.
    ls_item-sgtxt = '无'.
*    LS_ITEM-KOSTL   = LS_NOTE
*    LS_ITEM-AUFNR   = LS_NOTE-
*    LS_ITEM-ABLAD   = LS_NOTE
*    LS_ITEM-SGTXT   = LS_NOTE-
*    CLEAR ls_charg.
*    ls_charg-atnam = 'ZVFDAT'.
*    ls_charg-atwrt = '20200202'.
*    APPEND ls_charg TO lt_charg.
*    ls_item-lines_of_character = lt_charg.
    CLEAR ls_mcha.
    LOOP AT lt_mcha INTO ls_mcha WHERE matnr = ls_note-matnr AND charg = ls_note-charg AND werks = ls_note-werks.
      IF ls_mcha-atinn = zhsdat OR ls_mcha-atinn = z_batch_scrq OR ls_mcha-atinn = zvfdat OR ls_mcha-atinn = z_batch_scrq OR ls_mcha-atinn = zlwedt.
        ls_charg-atnam = ls_mcha-atinn.
        CALL FUNCTION 'CTCV_CONVERT_FLOAT_TO_DATE'
          EXPORTING
            float = ls_mcha-atflv
          IMPORTING
            date  = ls_charg-atwrt.
      ELSE.
        ls_charg-atnam = ls_mcha-atinn.
        ls_charg-atwrt = ls_mcha-atwrt.
      ENDIF.
      APPEND ls_charg TO lt_charg.
      CLEAR ls_charg.
    ENDLOOP.
    ls_item-lines_of_character = lt_charg.


    APPEND ls_item TO lt_item.
    CLEAR:ls_note,ls_item,lt_charg.
  ENDLOOP.

  ls_output-mt_sap2wms_other-list_of_others = lt_item[].

*----------------------------------------------------------------------
*   获取接口Message ID
*----------------------------------------------------------------------
  lv_message_id = zcl_bc_public=>get_guid( ).

*----------------------------------------------------------------------
*   日志记录
*----------------------------------------------------------------------
  zcl_bc_public=>write_log( iv_logid = CONV #( lv_message_id )
  iv_intid = lv_intf_id
  iv_ptype = 'I'
  is_data = ls_output ). "记录输入参数

  TRY.
      CREATE OBJECT lr_service.

      CALL METHOD lr_service->si_sap2wms_other_out
        EXPORTING
          output = ls_output
        IMPORTING
          input  = ls_input.

    CATCH cx_ai_system_fault INTO lr_system_fault.
      output-message = lr_system_fault->if_message~get_text( ).
      message_exit 'E' output-message ''.
  ENDTRY.

  IF ls_input-mt_sap2wsm_other_ret-returnflag = '1' OR ls_input-mt_sap2wsm_other_ret-returnflag = 'S'.
    message_exit 'S' '发送成功' ''.
  ELSE.
    message_exit 'E' ls_input-mt_sap2wsm_other_ret-returndesc ''.
  ENDIF.

  zcl_bc_public=>write_log( iv_logid = CONV #( lv_message_id )
  iv_intid = lv_intf_id
  iv_ptype = 'E'
  is_data = ls_input ).



ENDFUNCTION.