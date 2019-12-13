METHOD if_ex_le_shp_delivery_proc~save_document_prepare.

    DATA l_bapix(1).
    CALL FUNCTION 'Z_SD_DN_BAPIEX_GET'
      IMPORTING
        ev_bapix = l_bapix. "BAPI修改标识
  
  *1.交货单保存时，检查客户的账期是否逾期，逾期后给出提示，并更新相应的账期状态。
  *  DO 1 TIMES.
  *    CHECK if_tcode EQ 'VL01N' OR if_tcode EQ 'VL02N'.
  *    CHECK ct_xlikp[] IS NOT INITIAL.
  *    READ TABLE ct_xlikp ASSIGNING FIELD-SYMBOL(<fs_xlikp>) INDEX 1.
  *    CHECK <fs_xlikp>-vbtyp EQ 'J' AND <fs_xlikp>-updkz NE 'D' AND <fs_xlikp>-zzqzt NE 'D' AND <fs_xlikp>-zzqzt NE 'E'.
  *
  *    SELECT SINGLE bukrs INTO @DATA(l_bukrs) FROM tvko WHERE vkorg EQ @<fs_xlikp>-vkorg.
  *
  *    SELECT bukrs,gjahr,belnr,buzei,shkzg,dmbtr INTO TABLE @DATA(lt_bsid) FROM bsid
  *      WHERE bukrs EQ @l_bukrs
  *        AND kunnr EQ @<fs_xlikp>-kunag
  *        AND umskz IN (' ', 'A').
  *
  *    IF lt_bsid IS NOT INITIAL.
  *      SELECT bukrs,gjahr,belnr,buzei,shkzg,dmbtr INTO TABLE @DATA(lt_bseg) FROM bseg
  *        FOR ALL ENTRIES IN @lt_bsid
  *        WHERE bukrs EQ @lt_bsid-bukrs
  *          AND gjahr EQ @lt_bsid-gjahr
  *          AND belnr EQ @lt_bsid-belnr
  *          AND buzei EQ @lt_bsid-buzei
  *          AND netdt GT @sy-datum.
  *    ENDIF.
  *
  *    DATA: l_dmbtr1 TYPE bseg-dmbtr, "未清项总计金额
  *          l_dmbtr2 TYPE bseg-dmbtr. "未到期金额
  *
  *    LOOP AT lt_bsid INTO DATA(ls_bsid).
  *      IF ls_bsid-shkzg EQ 'S'.
  *        l_dmbtr1 = l_dmbtr1 + ls_bsid-dmbtr.
  *      ELSE.
  *        l_dmbtr1 = l_dmbtr1 - ls_bsid-dmbtr.
  *      ENDIF.
  *    ENDLOOP.
  *
  *    LOOP AT lt_bseg INTO DATA(ls_bseg).
  *      IF ls_bseg-shkzg EQ 'S'.
  *        l_dmbtr2 = l_dmbtr2 + ls_bseg-dmbtr.
  *      ELSE.
  *        l_dmbtr2 = l_dmbtr2 - ls_bseg-dmbtr.
  *      ENDIF.
  *    ENDLOOP.
  *
  *    IF l_dmbtr1 > l_dmbtr2.
  *      IF l_bapix IS INITIAL.
  *        APPEND VALUE #( vbeln = <fs_xlikp>-vbeln msgty = 'I' msgid = 'ZSD' msgno = '000' msgv1 = |{ '该客户账期已逾期' }{ l_dmbtr1 - l_dmbtr2 }{ '，请处理！' }| ) TO ct_log.
  *      ELSE.
  *        APPEND VALUE #( vbeln = <fs_xlikp>-vbeln msgty = 'W' msgid = 'ZSD' msgno = '000' msgv1 = |{ '该客户账期已逾期' }{ l_dmbtr1 - l_dmbtr2 }{ '，请处理！' }| ) TO ct_log.
  *      ENDIF.
  *      <fs_xlikp>-lifsk = 'Z1'.
  *      <fs_xlikp>-zzqzt = 'C'.
  *    ENDIF.
  *  ENDDO.
  
  *2.保存交货单时，检查WMS状态为A时，要给出系统提示。
    DO 1 TIMES.
      CHECK if_tcode EQ 'VL01N' OR if_tcode EQ 'VL02N'.
      CHECK ct_xlikp[] IS NOT INITIAL.
      READ TABLE ct_xlikp ASSIGNING FIELD-SYMBOL(<fs_xlikp>) INDEX 1.
      CHECK <fs_xlikp>-vbtyp EQ 'J' AND <fs_xlikp>-updkz NE 'D'.
  
      DATA: l_msg       TYPE string,
            l_answer(1).
  
      CHECK l_bapix IS INITIAL."BAPI修改的不触发该增强
  
      IF <fs_xlikp>-zwmszt CA 'AB'.
        IF <fs_xlikp>-zwmszt EQ 'A'.
          l_msg = 'WMS已执行发货，请与仓库人员联系，确认改单'.
        ELSE.
          l_msg = '该交货单此前已发送到WMS系统，请注意再次下发WMS系统'.
  
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question         = l_msg
              text_button_1         = '是'
              icon_button_1         = 'ICON_OKAY'
              text_button_2         = '否'
              icon_button_2         = 'ICON_CANCEL'
              display_cancel_button = ''
            IMPORTING
              answer                = l_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
        ENDIF.
  
        IF l_answer NE '1'.
          APPEND VALUE #( vbeln = <fs_xlikp>-vbeln msgty = 'E' msgid = 'ZSD' msgno = '000' msgv1 = l_msg ) TO ct_log.
        ENDIF.
      ENDIF.
    ENDDO.
  *3.件数自动计算逻辑
    DO 1 TIMES.
      CHECK if_tcode EQ 'VL01N' OR if_tcode EQ 'VL02N'.
  
      DATA: l_menge TYPE ekpo-menge.
      LOOP AT ct_xlips ASSIGNING FIELD-SYMBOL(<fs_xlips>) WHERE updkz NE 'D'.
        IF <fs_xlips>-lfimg IS INITIAL.
          <fs_xlips>-zjians = 0.
          <fs_xlips>-zlxsl  = 0.
        ELSE.
          CHECK <fs_xlips>-vrkme NE 'ST'.
          CLEAR l_menge.
          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              i_matnr              = <fs_xlips>-matnr
              i_in_me              = 'ST' "PC
              i_out_me             = <fs_xlips>-vrkme
              i_menge              = '1.000'
            IMPORTING
              e_menge              = l_menge
            EXCEPTIONS
              error_in_application = 1
              error                = 2
              OTHERS               = 3.
  
          IF l_menge GT 0.
            <fs_xlips>-zjians = <fs_xlips>-lfimg / l_menge DIV 1.
            <fs_xlips>-zlxsl  = <fs_xlips>-lfimg MOD l_menge.
          ENDIF.
        ENDIF.
  
        IF <fs_xlips>-zbzgg IS INITIAL.
          SELECT SINGLE zbzgg INTO <fs_xlips>-zbzgg FROM vbap WHERE vbeln EQ <fs_xlips>-vgbel AND posnr EQ <fs_xlips>-vgpos.
        ENDIF.
      ENDLOOP.
    ENDDO.
  
  ENDMETHOD.