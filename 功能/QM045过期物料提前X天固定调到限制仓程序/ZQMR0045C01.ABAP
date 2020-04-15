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
          lifnr      TYPE mkol-lifnr,
          vbeln      TYPE mska-vbeln,
          posnr      TYPE mska-posnr,
          mseh3      TYPE t006a-mseh3,
          mseh6      TYPE t006a-mseh6,
          mseht      TYPE t006a-mseht,
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
    sobkz,
    vbeln,
    posnr
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
    sobkz,
    lifnr
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

  IF lt_all IS NOT INITIAL.
    "MAKT表获取物料描述
    SELECT
      matnr,
      maktx
    INTO TABLE @DATA(lt_makt)
    FROM makt
    FOR ALL ENTRIES IN @lt_all
    WHERE makt~matnr = @lt_all-matnr
      AND makt~spras = '1'.
    "T006A基本计量单位表
    SELECT
      mseht,
      msehi,
      mseh3,
      mseh6
    INTO TABLE @DATA(lt_t006a)
    FROM t006a
    FOR ALL ENTRIES IN @lt_all
    WHERE t006a~spras = '1'
      AND t006a~msehi = @lt_all-meins.
    "MCHA表获取货架寿命到期日
    SELECT
      matnr,
      werks,
      charg,
      vfdat
    INTO TABLE @DATA(lt_mcha)
    FROM mcha
    FOR ALL ENTRIES IN @lt_all
    WHERE mcha~matnr = @lt_all-matnr
      AND mcha~werks  = @lt_all-werks
      AND mcha~charg = @lt_all-charg.
  ENDIF.

  CLEAR gt_item.
  LOOP AT lt_all INTO ls_all.
    CLEAR gs_item.
    MOVE-CORRESPONDING ls_all TO gs_item.
    CLEAR gs_item-menge.
    "判断移动类型，如果是325的话，排除限制仓
    "8100:1180    8200:3180
    IF p_bwart = '325'.
      IF ls_all-werks = '8100' AND ls_all-lgort = '1180'.
        CLEAR:ls_all,gs_item.
        CONTINUE.
      ELSEIF ls_all-werks = '8100' AND ls_all-lgort = '2180'.
        CLEAR:ls_all,gs_item.
        CONTINUE.
      ELSEIF ls_all-werks = '8200' AND ls_all-lgort = '3180'.
        CLEAR:ls_all,gs_item.
        CONTINUE.
      ENDIF.
      READ TABLE lt_mcha INTO DATA(ls_mcha) WITH KEY matnr = ls_all-matnr
                                                     werks = ls_all-werks
                                                     charg = ls_all-charg.
      IF sy-subrc = 0.
        "货架寿命到期日
        gs_item-vfdat = ls_mcha-vfdat.
        DATA(date) = gs_item-vfdat - sy-datum.
        IF date > p_day.
          CLEAR:gs_item,ls_all.
          CONTINUE.
        ENDIF.
      ENDIF.
      gs_item-menge = ls_all-cspem + ls_all-kaspe + ls_all-sspem.
      CLEAR ls_mcha.
    ENDIF.
    IF p_bwart = '344'.
      READ TABLE lt_mcha INTO ls_mcha WITH KEY matnr = ls_all-matnr
                                               werks = ls_all-werks
                                               charg = ls_all-charg.
      IF sy-subrc = 0.
        "货架寿命到期日
        gs_item-vfdat = ls_mcha-vfdat.
        CLEAR date.
        date = gs_item-vfdat - sy-datum.
        IF date > p_day.
          CLEAR:gs_item,ls_all.
          CONTINUE.
        ENDIF.
      ENDIF.
      gs_item-menge = ls_all-clabs + ls_all-kalab + ls_all-slabs.
      CLEAR ls_mcha.
    ENDIF.
    IF gs_item-menge IS INITIAL.
      CLEAR:gs_item,ls_all.
      CONTINUE.
    ENDIF.

    "变更之后的库存地点
    IF p_bwart = 325.
      DATA(lv_flag) = gs_item-lgort+0(1).
      IF gs_item-werks = 8100 AND lv_flag = '1'.
        gs_item-dlgort = 1180.
      ELSEIF gs_item-werks = 8100 AND lv_flag = '2'.
        gs_item-dlgort = 2180.
      ENDIF.
      IF gs_item-werks = 8200.
        gs_item-dlgort = 3180.
      ENDIF.
      CLEAR lv_flag.
      gs_item-stock_st = 'B'.
      gs_item-dstock_st = 'B'.
    ELSEIF p_bwart = 344.
      gs_item-dlgort = gs_item-lgort.
      gs_item-dstock_st = 'B'.
    ENDIF.

    "计量单位文本
    READ TABLE lt_t006a INTO DATA(ls_t006a) WITH KEY msehi = ls_all-meins.
    IF sy-subrc = 0.
      gs_item-mseht = ls_t006a-mseht.
      gs_item-mseh3 = ls_t006a-mseh3.
      gs_item-mseh6 = ls_t006a-mseh6.
      gs_item-meins = ls_all-meins.
    ENDIF.
    CLEAR ls_t006a.
    "判断特殊标识
    IF ls_all-z_table = 'MSKA'.
      gs_item-sobkz = 'E'.
    ELSEIF ls_all-z_table = 'MKOL'.
      gs_item-sobkz = 'K'.
    ELSEIF ls_all-z_table = 'MCHB'.
      gs_item-sobkz = ' '.
    ENDIF.
    "物料描述
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_all-matnr.
    IF sy-subrc = 0.
      gs_item-maktx = ls_makt-maktx.
    ENDIF.
    CLEAR ls_makt.
    "销售订单号和行号
    IF gs_item-kdauf IS INITIAL.
      gs_item-kdauf = ls_all-vbeln.
      gs_item-kdpos = ls_all-posnr.
    ENDIF.


    APPEND gs_item TO gt_item.
    CLEAR:ls_all,gs_item.
  ENDLOOP.









ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_item .
  DATA lv_flag1(1) VALUE ''.
  DATA lv_flag2(1) VALUE ''.
  IF gt_item IS NOT INITIAL.
    READ TABLE gt_item INTO gs_item INDEX 1.
    lv_flag1 = gs_item-lgort+0(1).
    LOOP AT gt_item INTO gs_item.
      lv_flag2 = gs_item-lgort+0(1).
      IF lv_flag1 <> lv_flag2.
        CLEAR:gs_item.
        MESSAGE '存在不同工厂的库位地点' TYPE 'S' DISPLAY LIKE 'E'.
        CALL SCREEN 100.
      ENDIF.
      CLEAR:gs_item,lv_flag2,lv_flag1.
      READ TABLE gt_item INTO DATA(gs_item2) INDEX 1.
      lv_flag1 = gs_item2-lgort+0(1).
    ENDLOOP.
  ENDIF.

ENDFORM.