METHOD if_ex_mb_document_badi~mb_document_update.
    DATA: ls_mseg     TYPE mseg,
          lt_zqmt     TYPE STANDARD TABLE OF zqmt001,
          ls_zqmt     TYPE zqmt001,
          lv_art      TYPE qpart,
          lv_prueflos TYPE qplos,
          lv_objek    TYPE cuobn,
          lv_clint    TYPE clint,
          lv_class    TYPE klasse_d,
          lv_zqjts    TYPE i.
    DATA: lv_objectkey TYPE bapi1003_key-object,
          lv_classnum  TYPE bapi1003_key-classnum,
          lt_snum      TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
          lt_schar     TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
          ls_schar     LIKE LINE OF lt_schar,
          lt_scurr     TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
          lt_return    TYPE STANDARD TABLE OF bapiret2.

    BREAK abap02.

    LOOP AT xmseg INTO ls_mseg.
      IF ls_mseg-bwart = '101'.
        " 订单收货保存时，判断检验类型为01或04
        CLEAR: lv_art.
        SELECT SINGLE art FROM qmat INTO lv_art
          WHERE matnr = ls_mseg-matnr
            AND werks = ls_mseg-werks
            AND aktiv = 'X'
            AND ( art = '01' OR art = '04' OR art = 'Z04' ).
        IF sy-subrc = 0.
          CLEAR: lt_zqmt, ls_zqmt.
          SELECT SINGLE * FROM zqmt001 INTO ls_zqmt
            WHERE matnr = ls_mseg-matnr
              AND werks = ls_mseg-werks
              AND loekz = ''.
          IF sy-subrc <> 0.
            ls_zqmt-mandt = sy-mandt.
            ls_zqmt-werks = ls_mseg-werks.
            ls_zqmt-matnr = ls_mseg-matnr.
            CLEAR: lv_objek, lv_clint.
            lv_objek = ls_mseg-matnr.
            " 根据物料号取对应的001分类的分类编号
            SELECT SINGLE clint FROM kssk INTO lv_clint
              WHERE objek = lv_objek AND klart = '001'.
            IF sy-subrc = 0.
              CLEAR: lv_class.
              SELECT SINGLE class FROM klah INTO lv_class
                WHERE clint = lv_clint AND klart = '001'.
              IF sy-subrc = 0.
                CLEAR: lt_snum, lt_schar, lt_scurr, lt_return, lv_objectkey, lv_classnum.
                lv_objectkey = ls_mseg-matnr.
                lv_classnum = lv_class.
                " 取批次特性值全检天数
                CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
                  EXPORTING
                    objectkey       = lv_objectkey
                    objecttable     = 'MARA'
                    classnum        = lv_classnum
                    classtype       = '001'
                  TABLES
                    allocvaluesnum  = lt_snum
                    allocvalueschar = lt_schar
                    allocvaluescurr = lt_scurr
                    return          = lt_return.
                IF sy-subrc = 0.
                  CLEAR: ls_schar, lv_zqjts.
                  READ TABLE lt_schar INTO ls_schar WITH KEY charact = 'ZQJTS'.
                  IF sy-subrc = 0.
                    lv_zqjts = ls_schar-value_char_long.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
***********************HK 外围添加判断***********************
          IF lv_zqjts > 0.
            ls_zqmt-qndat = lv_zqjts + sy-datum.
            ls_zqmt-loekz = ''.
            ls_zqmt-ersteller = sy-uname.
            ls_zqmt-enstehdat = sy-datum.
            ls_zqmt-entstezeit = sy-uzeit.
            APPEND ls_zqmt TO lt_zqmt.
            CLEAR: ls_zqmt.
            MODIFY zqmt001 FROM TABLE lt_zqmt.
          ENDIF.
**********************END**********************************
*          ELSE.
*            CLEAR: LV_OBJEK, LV_CLINT.
*            LV_OBJEK = LS_MSEG-MATNR.
*          " 根据物料号取对应的001分类的分类编号
*            SELECT SINGLE CLINT FROM KSSK INTO LV_CLINT
*              WHERE OBJEK = LV_OBJEK AND KLART = '001'.
*            IF SY-SUBRC = 0.
*              CLEAR: LV_CLASS.
*              SELECT SINGLE CLASS FROM KLAH INTO LV_CLASS
*                WHERE CLINT = LV_CLINT AND KLART = '001'.
*              IF SY-SUBRC = 0.
*                CLEAR: LT_SNUM, LT_SCHAR, LT_SCURR, LT_RETURN, LV_OBJECTKEY, LV_CLASSNUM.
*                LV_OBJECTKEY = LS_MSEG-MATNR.
*                LV_CLASSNUM = LV_CLASS.
*              " 取批次特性值全检天数
*                CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*                  EXPORTING
*                    OBJECTKEY       = LV_OBJECTKEY
*                    OBJECTTABLE     = 'MARA'
*                    CLASSNUM        = LV_CLASSNUM
*                    CLASSTYPE       = '001'
*                  TABLES
*                    ALLOCVALUESNUM  = LT_SNUM
*                    ALLOCVALUESCHAR = LT_SCHAR
*                    ALLOCVALUESCURR = LT_SCURR
*                    RETURN          = LT_RETURN.
*                IF SY-SUBRC = 0.
*                  CLEAR: LS_SCHAR, LV_ZQJTS.
*                  READ TABLE LT_SCHAR INTO LS_SCHAR WITH KEY CHARACT = 'ZQJTS'.
*                  IF SY-SUBRC = 0.
*                    LV_ZQJTS = LS_SCHAR-VALUE_CHAR_LONG.
*                  ENDIF.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*            LS_ZQMT-QNDAT = LV_ZQJTS + SY-DATUM.
*            LS_ZQMT-LOEKZ = ''.
*            LS_ZQMT-AENDERER = SY-UNAME.
*            LS_ZQMT-AENDERDAT = SY-DATUM.
*            LS_ZQMT-AENDERZEIT = SY-UZEIT.
*            APPEND LS_ZQMT TO LT_ZQMT.
*            CLEAR: LS_ZQMT.
          ENDIF.
*          MODIFY ZQMT001 FROM TABLE LT_ZQMT.
        ENDIF.
      ENDIF.
    ENDLOOP.



* 退货102 跟新ZMATDOC 表
    DATA: lt_zmatdoc TYPE TABLE OF zmatdoc.

    IF sy-tcode = 'MIGO'.
      LOOP AT xmseg INTO ls_mseg WHERE bwart = '102'.
        CHECK ls_mseg-mblnr <> ls_mseg-lfbnr."当前凭证与参考凭证不一样才处理
        SELECT SINGLE *
        FROM zmatdoc
        WHERE mblnr = @ls_mseg-lfbnr
          AND mjahr = @ls_mseg-mjahr
          AND zeile = @ls_mseg-lfpos
          INTO @DATA(ls_zmatdoc).
        IF sy-subrc = 0.
          ls_zmatdoc-mblnr = ls_mseg-mblnr.
          ls_zmatdoc-mjahr = ls_mseg-mjahr.
          ls_zmatdoc-zeile = ls_mseg-zeile.
          ls_zmatdoc-lfbnr = ls_mseg-lfbnr.
          ls_zmatdoc-lfpos = ls_mseg-lfpos.
          ls_zmatdoc-shkzg = 'H'.
          APPEND ls_zmatdoc TO lt_zmatdoc.
        ENDIF.
      ENDLOOP.

      IF lt_zmatdoc IS NOT INITIAL.
        MODIFY zmatdoc FROM TABLE lt_zmatdoc.
      ENDIF.
    ENDIF.


* 报废接口处理
    READ TABLE xmseg TRANSPORTING NO FIELDS WITH KEY bwart+0(2) = '55'.
    IF sy-subrc = 0.
      CALL FUNCTION 'Z_MM_SAP2WMS_SCRAP' STARTING NEW TASK 'TSK_SCRAP'
        EXPORTING
          i_mkpf = xmkpf
          i_mseg = xmseg.
    ENDIF.


  ENDMETHOD.