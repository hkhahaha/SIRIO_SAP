*&---------------------------------------------------------------------*
*& 包含               ZQMR0045C01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_change_screen .
  LOOP AT SCREEN .
    CASE screen-group1.
      WHEN 'M1'.
        IF r_01 = 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN 'M2'.
        IF r_02 = 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

  DATA: lv_name TYPE vrm_id.
  lv_name = 'p_bwart'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = gt_list.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_init .
  PERFORM frm_set_list_value.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_LIST_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_list_value .
  SELECT bwart AS key
  btext AS text
  INTO CORRESPONDING FIELDS OF TABLE gt_list
  FROM t156t
  WHERE spras = sy-langu
  AND sobkz = ''
  AND bwart IN ('325','344').
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_value .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_HEAD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_head .
  gs_head-bwart = p_bwart.
  t001w-werks = p_werks.
  gs_head-note = '物料近效期调状态/库存地点'.
  "获取抬头的货主ID字段
  SELECT SINGLE
  wms_custid
  INTO wms_custid
  FROM zmmwms_sys
  WHERE werks = p_werks.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_item.
  TYPES:BEGIN OF ty_all,
          z_table(5),
          matnr      TYPE mchb-matnr,
          werks      TYPE mchb-werks,
          lgort      TYPE mchb-lgort,
          charg      TYPE mchb-charg,
          meins      TYPE mara-meins,
          cspem      TYPE mchb-cspem,
          clabs      TYPE mchb-clabs,
          kaspe      TYPE mska-kaspe,
          kalab      TYPE mska-kalab,
          sspem      TYPE mkol-sspem,
          slabs      TYPE mkol-slabs,
          sobkz(1),
        END OF ty_all.
  DATA:lt_all TYPE TABLE OF ty_all,
       ls_all LIKE LINE OF lt_all.
  "MCHB批量库存表
  SELECT
    'MCHB' AS z_table,
    mchb~matnr,
    werks,
    lgort,
    charg,
    meins,
    cspem,
    clabs
  INTO CORRESPONDING FIELDS OF TABLE @lt_all
  FROM mchb
  INNER JOIN mara
  ON mchb~matnr = mara~matnr
  WHERE mchb~werks = @p_werks
    AND mchb~matnr IN @s_matnr
    AND charg IN @s_charg
    AND mara~mtart IN @s_mtart
    AND mchb~lgort IN @s_lgort
    AND mchb~lgort IN ( SELECT lgort FROM zmm_lgort WHERE wms_sys <> '' )
    AND mara~mtart IN ('Z010','Z020','Z050').
  "MSKA销售订单库存表
  SELECT
    'MSKA' AS z_table,
    mska~matnr,
    werks,
    lgort,
    charg,
    meins,
    kaspe,
    kalab,
    sobkz
  APPENDING CORRESPONDING FIELDS OF TABLE @lt_all
  FROM mska
  INNER JOIN mara
  ON mara~matnr = mska~matnr
  WHERE mska~werks = @p_werks
    AND mska~matnr IN @s_matnr
    AND charg IN @s_charg
    AND mara~mtart IN @s_mtart
    AND mska~lgort IN @s_lgort
    AND mska~lgort IN ( SELECT lgort FROM zmm_lgort WHERE wms_sys <> '' )
    AND mara~mtart IN ('Z010','Z020','Z050').
  "MSLB表里面的数据
  SELECT
    'MKOL' AS z_table,
    mkol~matnr,
    mkol~werks,
    lgort,
    charg,
    meins,
    sspem,
    slabs,
    sobkz
  APPENDING CORRESPONDING FIELDS OF TABLE @lt_all
  FROM mkol
  INNER JOIN mara
  ON mara~matnr = mkol~matnr
  WHERE mkol~werks = @p_werks
    AND mkol~matnr IN @s_matnr
    AND charg IN @s_charg
    AND mara~mtart IN @s_mtart
    AND mkol~lgort IN @s_lgort
    AND mkol~lgort IN ( SELECT lgort FROM zmm_lgort WHERE wms_sys <> '' )
    AND mara~mtart IN ('Z010','Z020','Z050').







ENDFORM.