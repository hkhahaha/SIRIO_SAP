METHOD if_ex_le_shp_delivery_proc~change_delivery_item.

    DO 1 TIMES."交货单bapi创建/修改LIPS增强字段赋值
      DATA: lt_exlips  TYPE zex_lips_t,
            lt_exlipsx TYPE zex_lips_flg_t.
  
      CALL FUNCTION 'Z_SD_DN_BAPIEX_GET'
        IMPORTING
          et_lips  = lt_exlips
          et_lipsx = lt_exlipsx.
  
      READ TABLE lt_exlips INTO DATA(ls_exlips) WITH KEY vbeln = cs_lips-vbeln posnr = cs_lips-posnr.
      READ TABLE lt_exlipsx INTO DATA(ls_exlipsx) WITH KEY vbeln = cs_lips-vbeln posnr = cs_lips-posnr.
      CHECK sy-subrc EQ 0.
  
      zcl_bc_public=>movex_to_struc( EXPORTING is_in = ls_exlips is_inx = ls_exlipsx IMPORTING es_out = cs_lips ).
    ENDDO.
  
    DO 1 TIMES."件数自动计算逻辑
      DATA: l_menge TYPE ekpo-menge.
  
      FIELD-SYMBOLS <ft_xlips> TYPE shp_lips_t.
  
      CHECK if_tcode EQ 'VL01N' OR if_tcode EQ 'VL02N'.
      IF cs_lips-lfimg IS INITIAL.
        cs_lips-zjians = 0.
        cs_lips-zlxsl  = 0.
      ELSE.
        CHECK cs_lips-vrkme NE 'ST'.
        CLEAR l_menge.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = cs_lips-matnr
            i_in_me              = 'ST' "PC
            i_out_me             = cs_lips-vrkme
            i_menge              = '1.000'
          IMPORTING
            e_menge              = l_menge
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
  
        IF l_menge GT 0.
          cs_lips-zjians = cs_lips-lfimg / l_menge DIV 1.
          cs_lips-zlxsl  = cs_lips-lfimg MOD l_menge.
        ELSE.
          APPEND VALUE #( vbeln = cs_lips-vbeln posnr = cs_lips-posnr msgty = 'W' msgid = 'ZSD' msgno = '000' msgv1 = '未找到单位转换关系！' ) TO ct_log.
        ENDIF.
      ENDIF.
  
      IF is_v50agl-chargensplitbild EQ 'X'. "针对批次拆分过来的项目只能修改数量和单位限制
        TRY.
            ASSIGN ('(SAPFV50P)XLIPS[]') TO <ft_xlips>.
            IF <ft_xlips> IS ASSIGNED.
              READ TABLE <ft_xlips> ASSIGNING FIELD-SYMBOL(<fs_xlips>) WITH KEY vbeln = cs_lips-vbeln posnr = cs_lips-posnr.
              IF sy-subrc EQ 0.
                <fs_xlips>-zjians = cs_lips-zjians.
                <fs_xlips>-zlxsl = cs_lips-zlxsl.
              ENDIF.
            ENDIF.
          CATCH cx_sy_assign_error.
            APPEND VALUE #( vbeln = cs_lips-vbeln posnr = cs_lips-posnr msgty = 'E' msgid = 'ZSD' msgno = '000' msgv1 = '批次拆分件数计算异常！' ) TO ct_log.
            EXIT.
        ENDTRY.
      ENDIF.
  
      IF cs_lips-zbzgg IS INITIAL.
        SELECT SINGLE zbzgg INTO cs_lips-zbzgg FROM vbap WHERE vbeln EQ cs_lips-vgbel AND posnr EQ cs_lips-vgpos.
      ENDIF.
  
    ENDDO.
  
  ENDMETHOD.