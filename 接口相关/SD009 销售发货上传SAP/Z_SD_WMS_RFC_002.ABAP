FUNCTION z_sd_wms_rfc_002.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_INPUT) TYPE  ZSDS0002
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  ZSDS0002_OUT
*"----------------------------------------------------------------------
  DATA: ls_header_data        TYPE bapiobdlvhdrcon,
        ls_header_control     TYPE bapiobdlvhdrctrlcon,
        ls_header_data_chg    TYPE bapiobdlvhdrchg,
        ls_header_control_chg TYPE bapiobdlvhdrctrlchg,
        lt_header_lines       TYPE TABLE OF bapidlvdeadln,
        l_vbeln               TYPE bapiobdlvhdrcon-deliv_numb,
        l_wadat               TYPE sy-datum,
        lt_return             TYPE TABLE OF bapiret2,
        ls_exlikp             TYPE zex_likp,
        ls_exlikpx            TYPE zex_likp_flg.
  l_vbeln = zcl_bc_public=>conv_by_ddic( i_input = i_input-vbeln i_abap = l_vbeln ).

  e_output-vbeln = i_input-vbeln.
  MOVE-CORRESPONDING i_input TO ls_exlikp.
  ls_exlikp-vbeln  = l_vbeln.

  zcl_bc_public=>movex( EXPORTING is_in = ls_exlikp IMPORTING es_out = ls_exlikpx ).

  CALL FUNCTION 'Z_SD_DN_BAPIEX_SET'
    EXPORTING
      is_likp  = ls_exlikp
      is_likpx = ls_exlikpx.

  IF i_input-wadat_ist IS NOT INITIAL.
    ls_header_data-deliv_numb       = l_vbeln.
    ls_header_control-deliv_numb    = l_vbeln.
    ls_header_control-post_gi_flg   = 'X'.
*    ls_header_control-gdsi_date_flg = 'X'.

    CONVERT DATE i_input-wadat_ist INTO TIME STAMP DATA(l_stamp) TIME ZONE 'UTC'.
    lt_header_lines = VALUE #( ( deliv_numb = l_vbeln timetype = 'WSHDRWADTI' timestamp_utc = l_stamp ) )."实际过账日期
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CONFIRM_DEC'
      EXPORTING
        header_data      = ls_header_data
        header_control   = ls_header_control
        delivery         = l_vbeln
      TABLES
        header_deadlines = lt_header_lines
        return           = lt_return.

  ELSE.
    ls_header_data_chg-deliv_numb        = l_vbeln.
    ls_header_control_chg-deliv_numb     = l_vbeln.
*    ls_header_control_chg-deliv_date_flg = 'X'.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = ls_header_data_chg
        header_control = ls_header_control_chg
        delivery       = l_vbeln
      TABLES
        return         = lt_return.

  ENDIF.

  LOOP AT lt_return INTO DATA(ls_return) WHERE type CA 'AEX'.
    CONCATENATE e_output-message ls_return-message ';' INTO e_output-message.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    e_output-type = 'E'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    e_output-type = 'S'.
  ENDIF.

********hk 添加返回物料凭证字段
  SELECT SINGLE
  mkpf~mblnr,
  mkpf~mjahr
  INTO @DATA(ls_temp)
        FROM vbfa
        INNER JOIN mkpf
        ON mkpf~mblnr = vbfa~vbeln
        AND vbfa~erdat = mkpf~cpudt
        WHERE vbfa~vbtyp_n = 'R'
        AND vbfa~bwart = '601'
        AND vbfa~vbelv = @l_vbeln.
  e_output-sap_docno = ls_temp-mblnr && ls_temp-mjahr.
************************END*********
ENDFUNCTION.