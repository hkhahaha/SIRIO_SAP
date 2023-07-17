*&---------------------------------------------------------------------*
*& Module      :  PP
*& Author      :  05025
*& Date        :  2022.11.19
*& Description :  APS查询SAP流程订单投料信息
*&---------------------------------------------------------------------*
*& Modified Recorder :
*& Date         C#NO          Author   Content
*& ----------   ----------    -----    ---------------
*& 2022.12.15   S4DK906621    04815    PP：PP084APS查询SAP流程订单投料信息接口bug修复-20221215
*&                                     1、修复物料描述取不到数据问题
*&---------------------------------------------------------------------*
FUNCTION z_pp_pp084_selecttouliao.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_APS2SAP_SELECT_TOU_LIAO
*"     REFERENCE(MESSAGE_ID) TYPE  SXMSGUID
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_APS2SAP_SELECT_TOU_LIAO_RE
*"----------------------------------------------------------------------

  DATA:lt_detail TYPE zdt_aps2sap_select_tou_lia_tab,
       ls_detail TYPE zdt_aps2sap_select_tou_liao_re.
  DATA:lv_date_30 LIKE sy-datum.

*S-S4DK906621***********************************************************************
  "定义物料结构
  DATA: BEGIN OF ls_makt,
          matnr TYPE makt-matnr,
          maktx TYPE makt-maktx,
        END OF ls_makt,
        lt_makt LIKE TABLE OF ls_makt.

*E-S4DK906621***********************************************************************

  lv_date_30 = sy-datum - 30.
  "首先先判断数据的完整性
  IF input-mt_aps2sap_select_tou_liao-input-i_werks IS INITIAL.
    output-mt_aps2sap_select_tou_liao_ret-type = 'E'.
    output-mt_aps2sap_select_tou_liao_ret-message = '工厂为空'.
    EXIT.
  ENDIF.

  "如果工厂不为空的话，获取对应工厂下的所有流程订单
  "关联AFKO以及AFPO获取订单数据
  SELECT
    rsnum,
    afko~aufnr,
    werks,
    matnr,
    afpo~kdauf,
    afpo~kdpos,
    afpo~charg,
    afko~gstrp,
    zlock,
    zaufnr_s,
    zchanx,
    gsuzs,
    aufld,
    stlal,
    idat2,
    psmng,
    wemng,
    zmes,
    verid,
    objnr,
    zkdauf_s,
    zkdpos_s
    FROM afko
    INNER JOIN afpo
    ON afko~aufnr = afpo~aufnr
    INNER JOIN aufk
    ON afko~aufnr = aufk~aufnr
    WHERE aufk~werks = @input-mt_aps2sap_select_tou_liao-input-i_werks
      AND zmes = 'X'
    INTO TABLE @DATA(lt_all).

  "获取S物料
  IF lt_all IS NOT INITIAL.
    SELECT DISTINCT aufnr,matnr_s INTO TABLE @DATA(lt_matnr_s)
          FROM zppt0001_subs
      FOR ALL ENTRIES IN @lt_all
      WHERE aufnr  = @lt_all-aufnr.
    "获取物料描述
    SELECT
      "*                                        "S4DK906621
      matnr, maktx                                          "S4DK906621
    FROM makt
    FOR ALL ENTRIES IN @lt_all
    WHERE matnr  = @lt_all-matnr
      AND spras = @sy-langu                                 "S4DK906621
      "into table @data(lt_makt).                           "S4DK906621
      INTO CORRESPONDING FIELDS OF TABLE @lt_makt.          "S4DK906621
    SORT lt_makt BY matnr.                                  "S4DK906621


    "获取基本开始时间
    SELECT
      aufnr,
      gsuzp
    FROM caufv
    FOR ALL ENTRIES IN @lt_all
    WHERE aufnr = @lt_all-aufnr
    INTO TABLE @DATA(lt_caufv).


    "获取快速交付标识

    SELECT
      vbeln,
      posnr,
      zquick
    FROM vbap
    FOR ALL ENTRIES IN @lt_all
    WHERE vbeln = @lt_all-kdauf
      AND posnr = @lt_all-kdpos
    INTO TABLE @DATA(lt_vbap).

    SELECT
      vbeln,
      posnr,
      zquick
    FROM vbap
    FOR ALL ENTRIES IN @lt_all
    WHERE vbeln = @lt_all-zkdauf_s
      AND posnr = @lt_all-zkdpos_s
    APPENDING TABLE @lt_vbap.

    "获取物料类型
    SELECT
      matnr,
      mtart
    FROM mara
    FOR ALL ENTRIES IN @lt_all
    WHERE mara~matnr = @lt_all-matnr
      AND mtart = 'Z030'
    INTO TABLE @DATA(lt_mara).

    "对象状态
    SELECT
      *
    FROM jest
    FOR ALL ENTRIES IN @lt_all
    WHERE objnr = @lt_all-objnr
      AND stat IN ( 'I0002', 'I0045' )
      AND inact <> 'X'
    INTO TABLE @DATA(lt_jest).
  ENDIF.

  LOOP AT lt_all INTO DATA(ls_all).
    "筛选Z030
    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_all-matnr.
    IF sy-subrc <> 0.
      CLEAR:ls_mara,ls_all.
      CONTINUE.
    ENDIF.
    "筛选对象
    READ TABLE lt_jest INTO DATA(ls_jest) WITH KEY objnr = ls_all-objnr
                                                   stat = 'I0002'.
    IF sy-subrc <> 0.
      IF ls_all-idat2 >= lv_date_30.
        READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_all-objnr
                                                           stat = 'I0045'.
        IF sy-subrc <> 0.
          CLEAR:ls_mara,ls_all,ls_jest.
          CONTINUE.
        ENDIF.
      ELSE.
        CLEAR:ls_mara,ls_all,ls_jest.
        CONTINUE.
      ENDIF.

    ENDIF.



    MOVE-CORRESPONDING ls_all TO ls_detail.

*S-S4DK906620***********************************************************************
    "赋值物料描述
    CLEAR ls_makt.
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_detail-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_detail-maktx = ls_makt-maktx.
    ENDIF.
*E-S4DK906620***********************************************************************
    "S物料
    READ TABLE lt_matnr_s WITH KEY aufnr = ls_all-aufnr INTO DATA(ls_matnr_s).
    IF sy-subrc = 0.
      ls_detail-zmatnr = ls_matnr_s-matnr_s. "更新C、G的上级S物料
    ENDIF.
    "锁定解锁标识
    "ZLOCK字段为“锁定/解锁标记”，读取AUFK- ZLOCK字段,当为空时为”UL”，AUFK- ZLOCK 不为空时传值：L
    IF ls_all-zlock IS INITIAL OR ls_all-zlock = ''.
      ls_all-zlock = 'UL'.
    ELSEIF ls_all-zlock IS NOT INITIAL.
      ls_all-zlock = 'L'.
    ENDIF.

    "基本开始时间
    READ TABLE lt_caufv INTO DATA(ls_caufv) WITH KEY aufnr = ls_all-aufnr.
    IF sy-subrc = 0.
      ls_detail-gsuzs = ls_caufv-gsuzp.
    ENDIF.
    CLEAR ls_caufv.
    "关闭状态
    IF ls_all-idat2 IS INITIAL.
      ls_detail-zrsv01 = 'CRTD'.
    ELSE.
      ls_detail-zrsv01 = 'TECO'.
    ENDIF.

    "快速交付标识
    READ TABLE lt_vbap INTO DATA(ls_vbap) WITH KEY vbeln = ls_all-kdauf
                                                   posnr = ls_all-kdpos.
    IF sy-subrc = 0.
      ls_detail-zrsv04 = ls_vbap-zquick.
    ELSE.
      READ TABLE lt_vbap INTO ls_vbap WITH KEY vbeln = ls_all-zkdauf_s
                                                     posnr = ls_all-zkdpos_s.
      IF sy-subrc = 0.
        ls_detail-zrsv04 = ls_vbap-zquick.
      ENDIF.
    ENDIF.
    CLEAR ls_vbap.

    "计划生产数量
    ls_detail-zrsv02 = ls_all-psmng.
    "已收货数量
    ls_detail-zrsv03 = ls_all-wemng.
    "传MES标识
    ls_detail-zrsv05 = ls_all-zmes.
    "生产版本
    ls_detail-zrsv06 = ls_all-verid.

    APPEND ls_detail TO lt_detail.

    CLEAR:ls_all,ls_detail,ls_matnr_s,ls_vbap.
  ENDLOOP.

  output-mt_aps2sap_select_tou_liao_ret-detail = lt_detail.




ENDFUNCTION.