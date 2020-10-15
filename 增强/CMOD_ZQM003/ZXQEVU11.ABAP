*&---------------------------------------------------------------------*
*& 包含               ZXQEVU11
*&---------------------------------------------------------------------*

***批次属性中实际样本量的值并进行扣减。

DATA: lv_objek TYPE  cuobn,
      lv_obtab TYPE  tabelle,
      lv_klart TYPE  klassenart,
      lv_class TYPE  klasse_d.

DATA: ls_key             TYPE bapi1003_key,
      lv_status          LIKE bapi1003_key-status,
      lt_allocvaluesnum  LIKE TABLE OF bapi1003_alloc_values_num,
      ls_allocvaluesnum  LIKE bapi1003_alloc_values_num,
      lt_allocvalueschar LIKE TABLE OF bapi1003_alloc_values_char,
      ls_allocvalueschar TYPE bapi1003_alloc_values_char,
      lt_allocvaluescurr LIKE TABLE OF bapi1003_alloc_values_curr,
      lt_return          LIKE TABLE OF bapiret2.

e_exit_activ = 'X'.
*add by ljm 2020-04-08  begin--
CHECK i_qals-charg IS NOT INITIAL.
*add by ljm 2020-04-08  end--
*****读取批次中的实际样本量

CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
  EXPORTING
    i_matnr = i_qals-matnr
    i_charg = i_qals-charg
    i_werks = i_qals-werk
  IMPORTING
    e_objek = lv_objek
    e_obtab = lv_obtab
    e_klart = lv_klart
    e_class = lv_class.

ls_key-object = lv_objek.
ls_key-objecttable = lv_obtab.
ls_key-classnum = lv_class.
ls_key-classtype = lv_klart.
ls_key-object_long = lv_objek.

CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
  EXPORTING
    objectkey       = ls_key-object
    objecttable     = ls_key-objecttable
    classnum        = ls_key-classnum
    classtype       = ls_key-classtype
    objectkey_long  = ls_key-object_long
  TABLES
    allocvaluesnum  = lt_allocvaluesnum
    allocvalueschar = lt_allocvalueschar
    allocvaluescurr = lt_allocvaluescurr
    return          = lt_return.

READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'S'.
CHECK sy-subrc = 0.

READ TABLE  lt_allocvaluesnum INTO ls_allocvaluesnum
                                   WITH KEY charact = 'ZS_VOLUME'.

IF sy-subrc = 0.
  e_sample_quantity = ls_allocvaluesnum-value_from.
ENDIF.