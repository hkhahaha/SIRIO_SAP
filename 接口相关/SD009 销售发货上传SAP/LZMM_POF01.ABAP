*----------------------------------------------------------------------*
***INCLUDE LZMM_POF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CREATE_BATCH_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_RECV
*&---------------------------------------------------------------------*
FORM create_batch_data  USING   ut_batch TYPE   ty_tab_batch
                        CHANGING ut_return TYPE bapiret2.
  DATA:ls_batch  TYPE ty_batch,
       lt_return TYPE TABLE OF bapiret2,
       ls_return TYPE bapiret2.

  DATA:lt_num  TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
       ls_num  TYPE bapi1003_alloc_values_num,
       lt_char TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
       ls_char TYPE bapi1003_alloc_values_char,
       lt_curr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
       ls_curr TYPE bapi1003_alloc_values_curr.

  DATA: l_atfor TYPE  cabn-atfor,
        l_objek TYPE  cuobn,
        l_obtab TYPE  tabelle,
        l_klart TYPE  klassenart,
        l_class TYPE  klasse_d.

  DATA:ls_mcha TYPE mcha.

  DATA: l_smp_qyt TYPE qamr-original_input.

*  CALL FUNCTION 'BAPI_BATCH_CREATE'
*    EXPORTING
*      material = us_recv-matnr
*      batch    = us_recv-charg
*      plant    = us_recv-werks
*    TABLES
*      return   = lt_return.
*
*  IF sy-subrc EQ 0.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*  ENDIF.
**************************************************************
*  READ TABLE us_recv-lines_of_character INTO ls_batch WITH KEY atnam = '包装量的分类特性'.
*  IF sy-subrc = 0.
*    CALL FUNCTION 'Z_QM_OBSMP_CMP'
*      EXPORTING
*        matnr_in    = us_recv-matnr
*        menge_in    = us_recv-menge
*        pack_qyt    = ls_batch-atwrt
*      IMPORTING
*        smp_qyt_out = l_smp_qyt.
*    CLEAR ls_batch.
*    IF sy-subrc = 0.
*      ls_char-charact    = '包装量的分类特性' .
*      ls_char-value_char = l_smp_qyt.
*      APPEND ls_char TO lt_char.
*    ENDIF.
*  ENDIF.
  DATA l_xchpf TYPE marc-xchpf.
  SORT ut_batch BY matnr werks charg.
  DELETE ADJACENT DUPLICATES FROM ut_batch COMPARING matnr werks charg atnam.
*************************************************************
  LOOP AT ut_batch INTO ls_batch.
    SELECT SINGLE atfor INTO l_atfor
      FROM cabn
      WHERE atnam = ls_batch-atnam.

    CASE l_atfor .
      WHEN 'CHAR'.
        ls_char-charact    = ls_batch-atnam.
**********************HK修复长度30位的问题***************
        ls_char-value_neutral_long = ls_batch-atwrt.
        ls_char-value_char_long = ls_batch-atwrt.
*************************END*****************
        APPEND ls_char TO lt_char.
      WHEN 'NUM'.
        ls_num-charact     = ls_batch-atnam.
        ls_num-value_from  = ls_batch-atwrt.
        APPEND ls_num TO lt_num.
      WHEN 'CURR'.
        ls_curr-charact    = ls_batch-atnam.
        ls_curr-currency_from = ls_batch-atwrt.
        APPEND ls_curr TO lt_curr.
      WHEN OTHERS.
    ENDCASE.
    AT END OF charg .


      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = ls_batch-matnr
        IMPORTING
          output = ls_batch-matnr.
      SELECT SINGLE xchpf INTO l_xchpf
       FROM marc
       WHERE matnr = ls_batch-matnr
        AND  werks = ls_batch-werks.
      CHECK l_xchpf = 'X'.
      CLEAR ls_mcha.
      SELECT SINGLE * INTO ls_mcha FROM mcha
           WHERE matnr = ls_batch-matnr
           AND   werks = ls_batch-werks
           AND   charg = ls_batch-charg.
      IF ls_mcha IS  INITIAL.
        ls_mcha-matnr = ls_batch-matnr.
        ls_mcha-werks = ls_batch-werks.
        ls_mcha-charg = ls_batch-charg.

*        BREAK-POINT.
        CALL FUNCTION 'VB_CREATE_BATCH'
          EXPORTING
            ymcha = ls_mcha
*           NEW_LGORT                          =
*           BYPASS_LOCK                        = ' '
*           MESSAGE_WHEN_AUTO                  = ' '
*           X_BNCOM                            =
*           QM_ZUSTD                           =
*           REF_MATNR                          =
*           REF_CHARG                          =
*           REF_WERKS                          =
*           KZCLA =
*           XKCFC =
*           CLASS =
*           NO_CHECK_OF_QM_CHAR                = ' '
*           BUFFER_REFRESH                     = ' '
*           NO_CHANGE_DOCUMENT                 = ' '
*           CHECK_EXTERNAL                     = X
*           CHECK_CUSTOMER                     = X
*           NO_CFC_CALLS                       = ' '
*           NO_CHANGE_OF_BWTAR                 = ' '
*           NO_CLF_INIT                        = ' '
*           LS_GOITEM                          =
*           IREF_PCBT_BATCH                    =
* IMPORTING
*           YMCHA =
* TABLES
*           CHAR_OF_BATCH                      =
*           NEW_BATCH                          =
*           NEW_BATCH_STOLOC                   =
*           RETURN                             =
* EXCEPTIONS
*           NO_MATERIAL                        = 1
*           NO_BATCH                           = 2
*           NO_PLANT                           = 3
*           MATERIAL_NOT_FOUND                 = 4
*           PLANT_NOT_FOUND                    = 5
*           STOLOC_NOT_FOUND                   = 6
*           LOCK_ON_MATERIAL                   = 7
*           LOCK_ON_PLANT                      = 8
*           LOCK_ON_BATCH                      = 9
*           LOCK_SYSTEM_ERROR                  = 10
*           NO_AUTHORITY                       = 11
*           BATCH_EXIST                        = 12
*           STOLOC_EXIST                       = 13
*           ILLEGAL_BATCH_NUMBER               = 14
*           NO_BATCH_HANDLING                  = 15
*           NO_VALUATION_AREA                  = 16
*           VALUATION_TYPE_NOT_FOUND           = 17
*           NO_VALUATION_FOUND                 = 18
*           ERROR_AUTOMATIC_BATCH_NUMBER       = 19
*           CANCELLED                          = 20
*           WRONG_STATUS                       = 21
*           INTERVAL_NOT_FOUND                 = 22
*           NUMBER_RANGE_NOT_EXTERN            = 23
*           OBJECT_NOT_FOUND                   = 24
*           ERROR_CHECK_BATCH_NUMBER           = 25
*           NO_EXTERNAL_NUMBER                 = 26
*           NO_CUSTOMER_NUMBER                 = 27
*           NO_CLASS                           = 28
*           ERROR_IN_CLASSIFICATION            = 29
*           INCONSISTENCY_IN_KEY               = 30
*           REGION_OF_ORIGIN_NOT_FOUND         = 31
*           COUNTRY_OF_ORIGIN_NOT_FOUND        = 32
*           VENDOR_ERROR                       = 33
*           OTHERS                             = 34
          .


        IF sy-subrc = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ELSE.
          ut_return = ls_return.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
        ENDIF.
      ENDIF.
      CHECK sy-subrc = 0.
      CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
        EXPORTING
          i_matnr = ls_batch-matnr
          i_charg = ls_batch-charg
          i_werks = ls_batch-werks
        IMPORTING
          e_objek = l_objek
          e_obtab = l_obtab
          e_klart = l_klart
          e_class = l_class.

      BREAK abap05.

      CALL FUNCTION 'BAPI_OBJCL_EXISTENCECHECK'
        EXPORTING
          objectkey_long = l_objek
          objecttable    = 'MCHA'
          classnum       = 'ZBATCH_SIRIO'
          classtype      = '022'
        TABLES
          return         = lt_return.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S'.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_OBJCL_CHANGE'
          EXPORTING
            objecttable        = 'MCHA'
            classnum           = 'ZBATCH_SIRIO'
            classtype          = '022'
            objectkey_long     = l_objek
          TABLES
            allocvaluesnumnew  = lt_num
            allocvaluescharnew = lt_char
            allocvaluescurrnew = lt_curr
            return             = lt_return.
      ELSE.
        CALL FUNCTION 'BAPI_OBJCL_CREATE'
          EXPORTING
            objecttablenew    = 'MCHA'
            classnumnew       = 'ZBATCH_SIRIO'
            classtypenew      = '022'
            objectkeynew_long = l_objek
          TABLES
            allocvaluesnum    = lt_num
            allocvalueschar   = lt_char
            allocvaluescurr   = lt_curr
            return            = lt_return.
      ENDIF.

*      CALL FUNCTION 'BAPI_OBJCL_CREATE'
*        EXPORTING
**         OBJECTKEYNEW      =
*          objecttablenew    = 'MCHA'
*          classnumnew       = 'ZBATCH_SIRIO'
*          classtypenew      = '022'
**          status            = '1'
**          standardclass     = 'X'
**         CHANGENUMBER      =
**          keydate           = sy-datum
**         NO_DEFAULT_VALUES = ' '
*          objectkeynew_long = l_objek
**       IMPORTING
**         CLASSIF_STATUS    =
*        TABLES
*          allocvaluesnum    = lt_num
*          allocvalueschar   = lt_char
*          allocvaluescurr   = lt_curr
*          return            = lt_return.

      READ TABLE lt_return INTO ls_return  WITH KEY type = 'E'.
      IF sy-subrc <> 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        ut_return = ls_return.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      ENDIF.
      CLEAR:lt_num,lt_char,lt_curr,
             l_objek,l_obtab,l_klart,l_class.
    ENDAT.
    CLEAR ls_batch.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA_021
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_INPUT
*&---------------------------------------------------------------------*
FORM check_data_021  USING  ps_input TYPE  zdt_wms2sap_cgsh.
  DATA:ls_recv TYPE zdt_wms2sap_recv.
  IF ps_input-dlv_planno IS INITIAL
  OR ps_input-wms_docno IS INITIAL
  OR ps_input-wms_custid IS INITIAL
  OR ps_input-budat IS INITIAL
  OR ps_input-operator IS INITIAL.
*    抬头必输字段未输入

  ELSE.
    LOOP AT ps_input-list_of_recv INTO ls_recv.
      IF ls_recv-wms_itemno IS INITIAL
      OR ls_recv-dlv_itemno IS INITIAL
      OR ls_recv-ebeln      IS INITIAL
      OR ls_recv-ebelp      IS INITIAL
      OR ls_recv-matnr      IS INITIAL
      OR ls_recv-charg      IS INITIAL
      OR ls_recv-chargl    IS INITIAL
      OR ls_recv-menge      IS INITIAL
      OR ls_recv-meins      IS INITIAL
      OR ls_recv-werks      IS INITIAL .
      ENDIF.
*      *    行项目必输字段未输入
    ENDLOOP.
  ENDIF.

*  SELECT SINGLE werks INTO @DATA(l_werks)
*    FROM t001w
*    WHERE werks = ls_recv-werks.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA_021
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_INPUT
*&---------------------------------------------------------------------*
FORM check_data_025  USING  ps_input TYPE  zdt_wms2sap_poret
                                      CHANGING l_flag TYPE c.
  DATA:ls_recv TYPE zdt_wms2sap_poret_sub.
  IF ps_input-dlv_planno IS INITIAL
  OR ps_input-wms_docno IS INITIAL
  OR ps_input-wms_custid IS INITIAL
  OR ps_input-poret_no IS INITIAL
  OR ps_input-ebeln IS INITIAL.
*    抬头必输字段未输入

  ELSE.
    LOOP AT ps_input-list_of_poret INTO ls_recv.
      IF ls_recv-wms_itemno IS INITIAL
      OR ls_recv-ebelp      IS INITIAL
      OR ls_recv-matnr      IS INITIAL
      OR ls_recv-budat      IS INITIAL
      OR ls_recv-menge      IS INITIAL
      OR ls_recv-meins      IS INITIAL
      OR ls_recv-werks      IS INITIAL .
      ENDIF.
*      *    行项目必输字段未输入
    ENDLOOP.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_CKPZ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_MATDOC
*&---------------------------------------------------------------------*
FORM frm_get_ckpz  CHANGING c_matdoc TYPE zmatdoc.


* 根据122 找出对应的101

  SELECT SINGLE
         lfbnr,
         lfpos
    FROM mseg
    WHERE mblnr = @c_matdoc-mblnr
      AND zeile = @c_matdoc-zeile
      AND mjahr = @c_matdoc-mjahr
      AND bwart = '122'
    INTO @DATA(ls_mseg).
  IF sy-subrc  = 0.
    c_matdoc-lfbnr = ls_mseg-lfbnr.
    c_matdoc-lfpos = ls_mseg-lfpos.
  ENDIF.

ENDFORM.