*&---------------------------------------------------------------------*
*& Report ZMMR0024
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr0024.

INCLUDE zsdr0001_alv.

TABLES: ekko, ekpo, ekbe, mkpf, ausp, zmmtreord, dd07t, zmm_lgort.

TYPES BEGIN OF ty_alv.
INCLUDE STRUCTURE zmmtreord.
TYPES: name1       TYPE name1_gp,
       maktx       TYPE maktx,
       fkimg       TYPE vbrp-fkimg, "开票数量
       menge_lk    TYPE mseg-menge, "冻结库存
       cspem       TYPE mchb-cspem, "可退货数量
       bstar_t(20),"库存类型
       zstat_l(20),"下传WMS状态
       style       TYPE lvc_t_styl,
       drop1       TYPE int4,
       sel(1),
       type(1),
       END OF ty_alv.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_ekorg FOR ekko-ekorg OBLIGATORY,
                s_werks FOR ekpo-werks OBLIGATORY,
                s_ekgrp FOR ekko-ekgrp OBLIGATORY,
                s_parea FOR zmm_lgort-plant_area,
                s_lifnr FOR ekko-lifnr,
                s_ebeln FOR ekko-ebeln,
                s_ernam FOR ekko-ernam,
                s_zreno FOR zmmtreord-zrenotice,
                s_dlvno FOR zmmtreord-dlv_planno,
                s_budat FOR sy-datum,
                "s_mblnr FOR mkpf-mblnr,
                s_charg FOR ausp-atwrt,
                s_matnr FOR ekpo-matnr.
PARAMETERS: p_closed AS CHECKBOX DEFAULT 'X',
            p_ret    AS CHECKBOX DEFAULT 'X'.
*            p_scwt   AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

CONSTANTS: gc_enable  TYPE raw4 VALUE cl_gui_alv_grid=>mc_style_enabled,
           gc_disable TYPE raw4 VALUE cl_gui_alv_grid=>mc_style_disabled.

DATA: gt_alv TYPE TABLE OF ty_alv,
      go_alv TYPE REF TO cl_gui_alv_grid.

DATA: gt_list        TYPE lvc_t_drop, "单元格下拉列表
      gt_enable      TYPE lvc_t_styl,
      gt_disable     TYPE lvc_t_styl,
      g_fcode        TYPE fcode VALUE 'SHOW',
      g_input        TYPE i VALUE 0,
      gr_renotice_st TYPE REF TO zcl_domain_helper,
      gr_bstar       TYPE REF TO zcl_domain_helper.

INITIALIZATION.
  PERFORM frm_init.

START-OF-SELECTION.
  PERFORM frm_atuh_check.
  PERFORM frm_get_data.
  PERFORM frm_show_data.
*&---------------------------------------------------------------------*
*& Form FRM_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_init .
*初始化可编辑单元格
  gt_enable = VALUE #( style = gc_enable ( fieldname = 'ZREQU' ) ( fieldname = 'ZREDA' ) ( fieldname = 'NOTE' ) ( fieldname = 'LGORT' ) ).
  gt_disable = VALUE #( style = gc_disable ( fieldname = 'ZREQU' ) ( fieldname = 'ZREDA' ) ( fieldname = 'NOTE' ) ( fieldname = 'LGORT' ) ).

*通知单状态文本描述
  gr_renotice_st = NEW zcl_domain_helper( 'ZD_RENOTICE_ST' ).
  gt_list = gr_renotice_st->get_list( 1 ).
*库存状态
  gr_bstar = NEW zcl_domain_helper( 'ZD_BSTAR' ).

*获取默认工厂采购组
  GET PARAMETER ID 'WRK' FIELD DATA(l_werks).
  IF l_werks IS NOT INITIAL.
    s_werks[] = VALUE #( sign = 'I' option = 'EQ' ( low = l_werks ) ).
  ENDIF.

  GET PARAMETER ID 'EKG' FIELD DATA(l_ekorg).
  IF l_ekorg IS NOT INITIAL.
    s_ekorg[] = VALUE #( sign = 'I' option = 'EQ' ( low = l_ekorg ) ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_ATUH_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_atuh_check .

  g_input = 1.

  LOOP AT s_werks INTO DATA(ls_werks).
    AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
     ID 'ACTVT' FIELD '01'
     ID 'WERKS' FIELD ls_werks-low.
    IF sy-subrc NE 0.
      MESSAGE '没有工厂权限' && ls_werks-low TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDLOOP.

  LOOP AT s_ekorg INTO DATA(ls_ekorg).
    AUTHORITY-CHECK OBJECT 'M_BEST_EKO'
     ID 'ACTVT' FIELD '01'
     ID 'EKORG' FIELD ls_ekorg-low.
    IF sy-subrc NE 0.
      MESSAGE '没有采购组织权限' && ls_ekorg-low TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDLOOP.

  LOOP AT s_ekgrp INTO DATA(ls_ekgrp).
    AUTHORITY-CHECK OBJECT 'M_BEST_EKG'
     ID 'ACTVT' FIELD '01'
     ID 'EKGRP' FIELD ls_ekgrp-low.
    IF sy-subrc NE 0.
      MESSAGE '没有采购组权限' && ls_ekgrp-low TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_get_data .
  DATA: lt_data   TYPE TABLE OF zmmtreord,
        l_key(90),
        l_sql     TYPE string VALUE 'XCLOSED EQ SPACE',
        r_bsart   TYPE RANGE OF ekko-bsart,
        r_bwart   TYPE RANGE OF mseg-bwart,
        r_lgort   TYPE RANGE OF mseg-lgort.

  IF p_closed EQ 'X'.
    l_sql = 'XCLOSED IN (``,`X`)'.
  ENDIF.
  r_bsart = VALUE #( sign = 'I' option = 'EQ' ( low = 'NB' ) ( low = 'ZNB' ) ( low = 'ZNB9' ) ( low = 'ZJS' ) ).
  IF p_ret EQ 'X'.
    APPEND  VALUE #( sign = 'I' option = 'EQ' low = 'ZRT' ) TO r_bsart.
  ENDIF.

  SELECT * INTO TABLE @DATA(lt_lgort) FROM zmm_lgort WHERE werks IN @s_werks AND plant_area IN @s_parea AND xreturn EQ 'X'.

  CLEAR: gt_alv[].
  IF g_fcode EQ 'SHOW' OR g_fcode EQ 'WMS'.
    SELECT * INTO TABLE lt_data FROM zmmtreord
      WHERE ekorg IN s_ekorg
        AND werks IN s_werks
        AND ekgrp IN s_ekgrp
        AND lifnr IN s_lifnr
        AND ebeln IN s_ebeln
        AND ernam IN s_ernam
        AND dlv_planno IN s_dlvno
        AND zrenotice IN s_zreno
        AND budat IN s_budat
        "AND mblnr IN s_mblnr
        AND charg_l IN s_charg
        AND matnr IN s_matnr
        AND zstat IN ('A','B','C')
        AND bsart IN r_bsart
        AND (l_sql).
  ELSEIF g_fcode EQ 'NEW'.
    SELECT a~ekorg a~ernam a~ekgrp a~lifnr b~ebeln b~ebelp b~werks b~matnr b~menge b~meins a~bsart
      INTO CORRESPONDING FIELDS OF TABLE lt_data
      FROM ekko AS a
      INNER JOIN ekpo AS b ON b~ebeln EQ a~ebeln
      WHERE a~ebeln IN s_ebeln
        AND a~ekorg IN s_ekorg
        AND a~ekgrp IN s_ekgrp
        AND a~lifnr IN s_lifnr
        AND a~ernam IN s_ernam
        AND b~werks IN s_werks
        AND b~matnr IN s_matnr
        AND a~bsart IN r_bsart.
    "AND a~frgke EQ 'F'.
    CHECK lt_data[] IS NOT INITIAL.
*采购收货
    SELECT a~ebeln,a~ebelp,a~bwart,a~budat,b~werks,b~lgort,b~mblnr,b~zeile,b~charg,b~sobkz,b~matnr,b~menge AS zenge,b~lfbnr,b~lfpos,a~bewtp AS bstar
      INTO TABLE @DATA(lt_charg) FROM ekbe AS a
      INNER JOIN mseg AS b ON b~mblnr EQ a~belnr AND b~zeile EQ a~buzei AND b~gjahr EQ a~gjahr
      FOR ALL ENTRIES IN @lt_data
      WHERE a~ebeln EQ @lt_data-ebeln
        AND a~ebelp EQ @lt_data-ebelp
        AND a~bwart EQ '101'.

    IF p_ret EQ 'X'.
      SELECT a~ebeln a~ebelp b~werks b~lgort a~charg b~sobkz b~matnr a~menge AS zenge
        APPENDING CORRESPONDING FIELDS OF TABLE lt_charg FROM eket AS a
        INNER JOIN ekpo AS b ON b~ebeln = a~ebeln AND b~ebelp = a~ebelp
        FOR ALL ENTRIES IN lt_data
        WHERE a~ebeln EQ lt_data-ebeln
          AND a~ebelp EQ lt_data-ebelp
          AND a~charg NE space
          AND a~menge GT 0.
    ENDIF.
*送货通知
    CHECK lt_charg[] IS NOT INITIAL.
    SELECT mblnr,zeile,dlv_planno,dlv_itemno,chargl INTO TABLE @DATA(lt_mtdoc) FROM zmatdoc
      FOR ALL ENTRIES IN @lt_charg
      WHERE mblnr EQ @lt_charg-mblnr
        AND zeile EQ @lt_charg-zeile.
*退货库
    DATA: ls_mseg LIKE LINE OF lt_charg,
          lt_mseg LIKE TABLE OF ls_mseg.

    SORT lt_charg BY ebeln ebelp.
    LOOP AT lt_charg INTO DATA(ls_charg).
      IF ls_charg-bwart IS INITIAL.
        ls_charg-bstar = 'B'.
        "ls_charg-mblnr = ls_charg-ebeln.
        "ls_charg-zeile = ls_charg-ebelp.
        APPEND ls_charg TO lt_mseg.
      ELSE.
        LOOP AT lt_lgort INTO DATA(ls_lgort).
          CLEAR ls_charg-bstar."非限制
          ls_charg-lgort = ls_lgort-lgort.
          APPEND ls_charg TO lt_mseg.
          ls_charg-bstar = 'B'."冻结
          APPEND ls_charg TO lt_mseg.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
*已退货数量
    SELECT * INTO TABLE @DATA(lt_reord) FROM zmmtreord
      FOR ALL ENTRIES IN @lt_mseg
      WHERE ebeln EQ @lt_mseg-ebeln
        AND ebelp EQ @lt_mseg-ebelp
        AND mblnr EQ @lt_mseg-mblnr
        AND charg EQ @lt_mseg-charg
        AND xclosed NE 'X'.
*冻结库存
    SELECT matnr,werks,lgort,charg,clabs,cinsm,cspem INTO TABLE @DATA(lt_mchb) FROM mchb
      FOR ALL ENTRIES IN @lt_mseg
      WHERE matnr EQ @lt_mseg-matnr
        AND werks EQ @lt_mseg-werks
        AND lgort EQ @lt_mseg-lgort
        AND charg EQ @lt_mseg-charg.
*寄售库存
    SELECT matnr werks lgort charg slabs AS clabs sspem AS cspem
      APPENDING CORRESPONDING FIELDS OF TABLE lt_mchb FROM mkol
      FOR ALL ENTRIES IN lt_mseg
      WHERE matnr EQ lt_mseg-matnr
        AND werks EQ lt_mseg-werks
        AND lgort EQ lt_mseg-lgort
        AND charg EQ lt_mseg-charg.

    SORT lt_mtdoc BY mblnr zeile.
  ENDIF.

  IF lt_data[] IS NOT INITIAL.
    SELECT matnr,maktx INTO TABLE @DATA(lt_makt) FROM makt
      FOR ALL ENTRIES IN @lt_data
      WHERE matnr EQ @lt_data-matnr
        AND spras EQ @sy-langu.

    SELECT lifnr,name1 INTO TABLE @DATA(lt_lfa1) FROM lfa1
      FOR ALL ENTRIES IN @lt_data
      WHERE lifnr EQ @lt_data-lifnr.
*开票记录/过账记录
    SELECT belnr,buzei,bewtp,bwart,lfbnr,lfpos,shkzg,menge INTO TABLE @DATA(lt_ekbe) FROM ekbe
      FOR ALL ENTRIES IN @lt_data
      WHERE ebeln EQ @lt_data-ebeln
        AND ebelp EQ @lt_data-ebelp
        AND bewtp IN ('E','T','Q')
        AND bwart IN ('102','122','123','').

    SORT lt_makt BY matnr.
    SORT lt_lfa1 BY lifnr.
    SORT lt_ekbe BY belnr buzei.
  ENDIF.

  LOOP AT lt_data INTO DATA(ls_data).
    IF g_fcode EQ 'NEW'.
      LOOP AT lt_mseg INTO ls_mseg WHERE ebeln EQ ls_data-ebeln AND ebelp EQ ls_data-ebelp.
        READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WITH KEY ebeln = ls_mseg-ebeln ebelp = ls_mseg-ebelp bstar = ls_mseg-bstar
                                                                    charg = ls_mseg-charg lgort = ls_mseg-lgort mblnr = ls_mseg-mblnr zeile = ls_mseg-zeile.
        IF sy-subrc NE 0.
          APPEND VALUE #( drop1 = 1 style = gt_enable ) TO gt_alv ASSIGNING <fs_alv>.
          MOVE-CORRESPONDING ls_data TO <fs_alv>.
          MOVE-CORRESPONDING ls_mseg TO <fs_alv>.
        ENDIF.
        <fs_alv>-menge_dr = <fs_alv>-menge_dr + ls_mseg-zenge.
        IF <fs_alv>-bsart EQ 'ZRT'.
          <fs_alv>-zrequ = <fs_alv>-cspem = <fs_alv>-menge.
        ELSE.
          "已收货数量
          LOOP AT lt_ekbe INTO DATA(ls_ekbe) WHERE lfbnr EQ <fs_alv>-mblnr AND lfpos EQ <fs_alv>-zeile AND bewtp EQ 'E'.
            CASE ls_ekbe-bwart.
              WHEN '123'.
                <fs_alv>-menge_dr = <fs_alv>-menge_dr + ls_ekbe-menge.
              WHEN '102' OR '122'.
                <fs_alv>-menge_dr = <fs_alv>-menge_dr - ls_ekbe-menge.
            ENDCASE.
          ENDLOOP.
          "已开票数量
          LOOP AT lt_ekbe INTO ls_ekbe WHERE lfbnr EQ <fs_alv>-mblnr AND lfpos EQ <fs_alv>-zeile AND ( bewtp EQ 'T' OR bewtp EQ 'Q').
            IF ls_ekbe-shkzg EQ 'S'.
              <fs_alv>-fkimg = <fs_alv>-fkimg + ls_ekbe-menge.
            ELSE.
              <fs_alv>-fkimg = <fs_alv>-fkimg - ls_ekbe-menge.
            ENDIF.
          ENDLOOP.
          "已退货通知数量
          LOOP AT lt_reord INTO DATA(ls_reord) WHERE ebeln EQ <fs_alv>-ebeln AND ebelp EQ <fs_alv>-ebelp "AND mblnr EQ <fs_alv>-mblnr AND zeile EQ <fs_alv>-zeile
                                                 AND charg EQ <fs_alv>-charg AND lgort EQ <fs_alv>-lgort.
            <fs_alv>-menge_cr = <fs_alv>-menge_cr + ( ls_reord-zrequ - ls_reord-menge_cr )."退货通知单数量-已过帐数量
          ENDLOOP.
          "冻结库存
          LOOP AT lt_mchb INTO DATA(ls_mchb) WHERE matnr EQ <fs_alv>-matnr AND werks EQ <fs_alv>-werks
                                               AND lgort EQ <fs_alv>-lgort AND charg EQ <fs_alv>-charg.
            IF <fs_alv>-bstar EQ 'B'.
              <fs_alv>-menge_lk = <fs_alv>-menge_lk + ls_mchb-cspem. "冻结
            ELSE.
              <fs_alv>-menge_lk = <fs_alv>-menge_lk + ls_mchb-clabs. "非限制
            ENDIF.
          ENDLOOP.

          <fs_alv>-cspem = <fs_alv>-menge_lk - <fs_alv>-menge_cr.
          <fs_alv>-zrequ = <fs_alv>-menge_dr - <fs_alv>-fkimg - <fs_alv>-menge_cr.
          IF <fs_alv>-fkimg GE <fs_alv>-menge_dr. CLEAR <fs_alv>-menge_dr. ENDIF.
        ENDIF.

        READ TABLE lt_mtdoc INTO DATA(ls_mtdoc) WITH KEY mblnr = <fs_alv>-mblnr zeile = <fs_alv>-zeile BINARY SEARCH.
        IF sy-subrc EQ 0.
          <fs_alv>-charg_l    = ls_mtdoc-chargl.
          <fs_alv>-dlv_planno = ls_mtdoc-dlv_planno.
          <fs_alv>-dlv_itemno = ls_mtdoc-dlv_itemno.
        ENDIF.

        READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = <fs_alv>-matnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          <fs_alv>-maktx = ls_makt-maktx.
        ENDIF.

        READ TABLE lt_lfa1 INTO DATA(ls_lfa1) WITH KEY lifnr = <fs_alv>-lifnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          <fs_alv>-name1 = ls_lfa1-name1.
        ENDIF.

        <fs_alv>-bstar_t = gr_bstar->get_value( <fs_alv>-bstar ).
        <fs_alv>-zstat_l = gr_renotice_st->get_value_l( <fs_alv>-zstat ).
      ENDLOOP.
    ELSE.
      APPEND VALUE #( drop1 = 1 style = gt_disable ) TO gt_alv ASSIGNING <fs_alv>.
      MOVE-CORRESPONDING ls_data TO <fs_alv>.
      READ TABLE lt_makt INTO ls_makt WITH KEY matnr = <fs_alv>-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_alv>-maktx = ls_makt-maktx.
      ENDIF.

      READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = <fs_alv>-lifnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_alv>-name1 = ls_lfa1-name1.
      ENDIF.

      <fs_alv>-bstar_t = gr_bstar->get_value( <fs_alv>-bstar ).
      <fs_alv>-zstat_l = gr_renotice_st->get_value_l( <fs_alv>-zstat ).
*已开票数量
      CHECK <fs_alv>-mblnr IS NOT INITIAL.
      LOOP AT lt_ekbe INTO ls_ekbe WHERE lfbnr EQ <fs_alv>-mblnr AND lfpos EQ <fs_alv>-zeile AND ( bewtp EQ 'T' OR bewtp EQ 'Q').
        IF ls_ekbe-shkzg EQ 'S'.
          <fs_alv>-fkimg = <fs_alv>-fkimg + ls_ekbe-menge.
        ELSE.
          <fs_alv>-fkimg = <fs_alv>-fkimg - ls_ekbe-menge.
        ENDIF.
      ENDLOOP.
*已过账数量
      CHECK <fs_alv>-menge_cr IS INITIAL.
      LOOP AT lt_ekbe INTO ls_ekbe WHERE lfbnr EQ <fs_alv>-mblnr AND lfpos EQ <fs_alv>-zeile AND bewtp EQ 'E'.
        CASE ls_ekbe-bwart.
          WHEN '123'.
            <fs_alv>-menge_cr = <fs_alv>-menge_cr - ls_ekbe-menge.
          WHEN '122'.
            <fs_alv>-menge_cr = <fs_alv>-menge_cr + ls_ekbe-menge.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  DELETE gt_alv WHERE charg_l NOT IN s_charg.
  DELETE gt_alv WHERE dlv_planno NOT IN s_dlvno.

  IF g_fcode EQ 'NEW'.
    DELETE gt_alv WHERE menge_dr LE 0 OR cspem LE 0.
    SORT gt_alv BY ebeln DESCENDING ebelp bstar.
  ELSE.
    SORT gt_alv BY zrenotice DESCENDING zrelines bstar.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SAVE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_save_data .
  DATA: l_notice TYPE zmm_renotice,
        l_line   TYPE zmm_relines,
        lt_msg   TYPE TABLE OF bapiret2,
        lt_data  TYPE TABLE OF zmmtreord,
        lt_key   TYPE TABLE OF char20,
        l_key    TYPE char20.
*check
  CLEAR lt_msg[].
  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE sel EQ 'X' AND zstat EQ space.
    PERFORM frm_check_data USING <fs_alv> lt_msg.
    l_key = <fs_alv>-ebeln && <fs_alv>-zreda.
    COLLECT l_key INTO lt_key.
  ENDLOOP.
  IF lt_msg[] IS NOT INITIAL.
    PERFORM frm_show_error USING lt_msg.
    EXIT.
  ENDIF.
*save
  CLEAR: l_notice, l_line, lt_data[].
  LOOP AT lt_key INTO l_key.
    PERFORM frm_get_next CHANGING l_notice. CLEAR l_line.
    LOOP AT gt_alv ASSIGNING <fs_alv> WHERE sel EQ 'X' AND zstat EQ space AND ebeln EQ l_key(10) AND zreda EQ l_key+10(8).
      l_line = l_line + 10.
      <fs_alv>-zrenotice = l_notice.
      <fs_alv>-zrelines = l_line.
      <fs_alv>-zstat = 'A'.
      <fs_alv>-zstat_l = gr_renotice_st->get_value_l( <fs_alv>-zstat ).
      <fs_alv>-style = gt_disable.
      APPEND INITIAL LINE TO lt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      MOVE-CORRESPONDING <fs_alv> TO <fs_data>.
      CLEAR <fs_data>-menge_cr."实际退货数量
    ENDLOOP.
  ENDLOOP.
  IF sy-subrc EQ 0.
    IF lt_data IS NOT INITIAL.
      MODIFY zmmtreord FROM TABLE lt_data.
      COMMIT WORK AND WAIT.
      IF sy-subrc EQ 0.
        MESSAGE '退货通知单创建成功！' TYPE 'S'.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE '未选中数据！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_NEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- ZSD_TSNMR
*&---------------------------------------------------------------------*
FORM frm_get_next CHANGING p_notice TYPE zmm_renotice.

*  IF p_notice IS INITIAL.
*    SELECT MAX( zrenotice ) INTO p_notice FROM zmmtreord.
*  ENDIF.
*
*  p_notice = p_notice + 1.
*  p_notice = zcl_bc_public=>conv_by_ddic( p_notice ).

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '24'
      object                  = 'ZMM030'
      quantity                = '1'
    IMPORTING
      number                  = p_notice
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      OTHERS                  = 7.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SEND_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_send_data USING no_popup.
  DATA: lt_msg     TYPE TABLE OF bapiret2,
        ls_input   TYPE zmt_sap2wms_poret,
        ls_output  TYPE zmt_sap2wms_poret_ret,
        lr_proxy   TYPE REF TO zco_si_sap2wms_poret_out,
        lt_notice  TYPE TABLE OF zmm_renotice,
        l_error(1),
        l_key(90),
        l_notes    TYPE string,
        l_intid    TYPE ze_intf_id VALUE 'MM024'.

  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE sel EQ 'X'.
    COLLECT <fs_alv>-zrenotice INTO lt_notice.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE '未选中数据！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF NOT zcl_bc_public=>int_actived( l_intid ).
    MESSAGE l_intid && '接口未启用！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CREATE OBJECT lr_proxy.
  LOOP AT lt_notice INTO DATA(l_notice).
    CLEAR: ls_input, ls_output, l_error, l_notes.
    READ TABLE gt_alv INTO DATA(ls_alv) WITH KEY zrenotice = l_notice.
    ASSIGN ls_input-mt_sap2wms_poret TO FIELD-SYMBOL(<fs_head>).
    MOVE-CORRESPONDING ls_alv TO <fs_head>.
    <fs_head>-bus_type = 'B04'.
    <fs_head>-name1_gp = ls_alv-name1.
    <fs_head>-poret_no = ls_alv-zrenotice.
    <fs_head>-wms_custid = ls_alv-werks.
    <fs_head>-ebeln = zcl_bc_public=>conv_by_ddic( i_input = ls_alv-ebeln i_out = 'X' ).
    <fs_head>-lifnr = zcl_bc_public=>conv_by_ddic( i_input = ls_alv-lifnr i_out = 'X' ).
    LOOP AT gt_alv ASSIGNING <fs_alv> WHERE zrenotice EQ l_notice AND xclosed NE 'X'.
*      IF <fs_alv>-zstat NE 'A'.
*        l_error = 'X'.
*        APPEND VALUE #( type = 'E' message = |{ '退货通知单:' } { <fs_alv>-zrenotice } { <fs_alv>-zrelines } { '不能重复下传！' }| ) TO lt_msg.
*        CONTINUE.
*      ENDIF.
*      <fs_alv>-zstat = 'B'.
*      <fs_alv>-zstat_l = gr_renotice_st->get_value_l( <fs_alv>-zstat ).

      APPEND INITIAL LINE TO ls_input-mt_sap2wms_poret-list_of_po_ret ASSIGNING FIELD-SYMBOL(<fs_item>).
      MOVE-CORRESPONDING <fs_alv> TO <fs_item>.
      <fs_item>-poret_item = <fs_alv>-zrelines.
      <fs_item>-stock_st = <fs_alv>-bstar.
      "<fs_item>-xcanceled = <fs_alv>-xclosed.
      <fs_item>-matnr = zcl_bc_public=>conv_by_ddic( i_input = <fs_alv>-matnr i_out = 'X' ).
      <fs_item>-menge = <fs_alv>-zrequ.
      <fs_item>-eeind = <fs_alv>-zreda.
      IF l_notes IS INITIAL.
        l_notes = <fs_alv>-note.
      ELSE.
        CONCATENATE l_notes <fs_alv>-note INTO l_notes SEPARATED BY ','.
      ENDIF.

      IF <fs_alv>-charg IS NOT INITIAL.
        DATA: zst_code(2),
              zout_x(1).
        l_key(40) = <fs_alv>-matnr.
        l_key+40(4) = <fs_alv>-werks.
        l_key+44(10) = <fs_alv>-charg.
        DATA(lt_value) = zcl_bc_public=>get_class_values( key = l_key classnum = 'ZBATCH_SIRIO' classtype = '022' objtable = 'MCHA' ).
        LOOP AT lt_value INTO DATA(ls_value).
          APPEND VALUE #( atnam = ls_value-charact atwrt = ls_value-value_char ) TO <fs_item>-linesof_character.
          zst_code = COND #( WHEN ls_value-charact EQ 'ZST_CODE' THEN ls_value-value_char ELSE zst_code ).
          zout_x = COND #( WHEN ls_value-charact EQ 'ZOUT_X' THEN ls_value-value_char ELSE zout_x ).
        ENDLOOP.
        SELECT SINGLE wms_ext_st INTO <fs_item>-wms_ext_st FROM zmmwms_ext_st WHERE zout_x EQ zout_x AND zst_code EQ zst_code AND xconsign EQ <fs_alv>-sobkz.
      ENDIF.

    ENDLOOP.
    IF sy-subrc EQ 0.
      <fs_head>-note = l_notes.
    ELSE.
      <fs_head>-xclosed_h  = 'X'.
    ENDIF.
    CHECK l_error IS INITIAL.

    TRY.
        DATA(l_guid) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
        DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'I' is_data = ls_input ). "记录输入参数

        lr_proxy->si_sap2wms_poret_out( EXPORTING output = ls_input
                                        IMPORTING input = ls_output )."调用pi接口

        zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'E' is_data = ls_output ). "记录输出参数

        ASSIGN ls_output-mt_sap2wms_poret_ret TO FIELD-SYMBOL(<fs_out>).
        IF <fs_out>-ret_flag EQ '1'.
          UPDATE zmmtreord SET zstat = 'C' WHERE zrenotice EQ l_notice.
          COMMIT WORK AND WAIT.
          IF sy-subrc EQ 0.
            ls_alv-type = 'S'.
            ls_alv-zstat = 'C'.
            ls_alv-zstat_l = gr_renotice_st->get_value_l( ls_alv-zstat ).
            MODIFY gt_alv FROM ls_alv TRANSPORTING type zstat zstat_l WHERE zrenotice EQ l_notice.
          ENDIF.
        ELSE.
          ls_alv-type = 'E'.
          MODIFY gt_alv FROM ls_alv TRANSPORTING type WHERE zrenotice EQ l_notice.
          APPEND VALUE #( type = 'E' message = |{ '退货通知单:' } { l_notice } { '下传失败！' }{ <fs_out>-ret_msg }| ) TO lt_msg.
        ENDIF.

      CATCH cx_root INTO DATA(lr_root).
        MESSAGE lr_root->get_text(  ) TYPE 'I'.
        RETURN.
    ENDTRY.
  ENDLOOP.

  CHECK no_popup NE 'X'.

  IF lt_msg[] IS INITIAL.
    MESSAGE '退货通知单下传成功！' TYPE 'S'.
  ELSE.
    PERFORM frm_show_error USING lt_msg.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CLOSE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_close_data .
  DATA: lt_msg      TYPE TABLE OF bapiret2,
        r_renotice  TYPE RANGE OF zmm_renotice,
        l_answer(1).

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = '确定关闭已选择的退货通知单吗？'
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

  CHECK l_answer EQ '1'.
*没有下发的可以直接关闭
  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE sel EQ 'X' AND zstat EQ 'A'.
    <fs_alv>-xclosed = 'X'.
    UPDATE zmmtreord SET xclosed = 'X' WHERE zrenotice EQ <fs_alv>-zrenotice AND zrelines EQ <fs_alv>-zrelines.
  ENDLOOP.
*已经下发的需要WMS返回成功才能关闭
  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE sel EQ 'X' AND zstat EQ 'C'.
    CLEAR <fs_alv>-type.
    <fs_alv>-xclosed = 'X'.
  ENDLOOP.

  PERFORM frm_send_data USING 'X'.
  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE sel EQ 'X' AND zstat EQ 'C' AND xclosed EQ 'X'.
    IF <fs_alv>-type EQ 'S'.
      UPDATE zmmtreord SET xclosed = 'X' WHERE zrenotice EQ <fs_alv>-zrenotice AND zrelines EQ <fs_alv>-zrelines.
    ELSE.
      CLEAR <fs_alv>-xclosed.
    ENDIF.
  ENDLOOP.

  IF lt_msg IS INITIAL.
    MESSAGE '数据已经修改！' TYPE 'S'.
  ELSE.
    PERFORM frm_show_error USING lt_msg.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_data USING is_data TYPE ty_alv it_msg TYPE bapiret2_t.
  DATA l_msg TYPE string.

  IF is_data-zreda IS INITIAL.
    l_msg = '计划退货日期不能为空！'.
  ELSEIF is_data-zreda LT sy-datum.
    l_msg = '计划退货日期不能小于当前日期！'.
  ENDIF.

  IF is_data-zrequ IS INITIAL.
    l_msg = l_msg && '退货数量不能为空！'.
  ELSE.
    IF is_data-zrequ GT ( is_data-menge_dr - is_data-fkimg ).
      l_msg = l_msg && '退货数量不能大于已收货数量'.
    ENDIF.
    DATA l_qty TYPE mseg-menge.
    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE charg EQ is_data-charg AND bstar EQ is_data-bstar.
      l_qty = l_qty + <fs_alv>-zrequ.
    ENDLOOP.
    IF l_qty GT is_data-cspem.
      l_msg = l_msg && |{ '退货总数' }{ l_qty } { '不能大于已冻结库存' }{ is_data-cspem }|.
    ENDIF.
  ENDIF.

  IF is_data-lgort IS INITIAL.
    l_msg = l_msg && '退货库存地不能为空！'.
  ELSE.
    SELECT SINGLE COUNT(*) FROM zmm_lgort
    WHERE werks IN @s_werks AND plant_area IN @s_parea AND xreturn EQ 'X' AND lgort EQ @is_data-lgort.
    IF sy-subrc NE 0.
      l_msg = l_msg && '不是退货库存地！'.
    ENDIF.
  ENDIF.

  IF l_msg IS NOT INITIAL.
    APPEND VALUE #( type = 'E' message = l_msg ) TO it_msg.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_data .
  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_fieldcat TYPE lvc_s_fcat,
        lt_sort     TYPE lvc_t_sort,
        lt_events   TYPE slis_t_event,
        ls_layout   TYPE lvc_s_layo,
        ls_variant  TYPE disvariant,
        l_pos       TYPE i VALUE 0.

  DEFINE setfieldcat.
    l_pos = l_pos + 1.
    wa_fieldcat-col_pos = l_pos.
    wa_fieldcat-fieldname = &1.
    wa_fieldcat-coltext   = &2.
    wa_fieldcat-scrtext_l = &2.
    wa_fieldcat-scrtext_m = &2.
    wa_fieldcat-scrtext_s = &2.
    IF &3 NE space.
      SPLIT &3 AT '-' INTO wa_fieldcat-ref_table wa_fieldcat-ref_field.
    ENDIF.
    wa_fieldcat-outputlen = &4.
    CASE &1.
      WHEN 'ZREQU' OR 'ZREDA' OR 'NOTE'.
        wa_fieldcat-edit = 'X'.
      WHEN 'ZSTAT_L'.
        wa_fieldcat-drdn_field = 'DROP1'.
    ENDCASE.
    APPEND wa_fieldcat TO lt_fieldcat. CLEAR: wa_fieldcat.
  END-OF-DEFINITION.

  ls_layout-zebra      = 'X'.
  ls_layout-sel_mode   = 'A'.
*  ls_layout-cwidth_opt = 'X'."自动列宽，自定义列宽时需注释掉
  ls_layout-box_fname  = 'SEL'.
  ls_layout-stylefname = 'STYLE'.
  "ls_layout-totals_bef = 'X'.
  ls_variant-report = sy-repid.

  setfieldcat 'ZRENOTICE' '退货通知单号' '' '12'.
  setfieldcat 'ZRELINES' '行项目' '' '6'.
  setfieldcat 'BSART' '采购凭证类型' '' ''.
  setfieldcat 'EKORG' '采购组织' '' '6'.
  setfieldcat 'WERKS' '工厂' '' '6'.
  setfieldcat 'LIFNR' '供应商' 'LFA1-LIFNR' '12'.
  setfieldcat 'NAME1' '供应商名称' '' '20'.
  setfieldcat 'ERNAM' '订单创建者' '' ''.
  setfieldcat 'EKGRP' '采购组' '' '6'.
  setfieldcat 'EBELN ' '采购订单' 'EKKO-EBELN' ''.
  setfieldcat 'EBELP' '订单行项目' '' ''.
  setfieldcat 'MATNR' '物料号' 'MARA-MATNR' '12'.
  setfieldcat 'MAKTX' '物料名称' '' '20'.
  setfieldcat 'MENGE' '采购数量' 'MSEG-MENGE' '13'.
  setfieldcat 'MENGE_DR' '已收货数量' 'MSEG-MENGE' '13'.
  setfieldcat 'FKIMG' '已开票数量' 'MSEG-MENGE' '13'.
  IF sy-uname EQ 'MM'.
    setfieldcat 'MENGE_CR' '已退货数量' 'MSEG-MENGE' '13'.
  ENDIF.
*  setfieldcat 'MENGE_LK' '冻结数量' 'MSEG-MENGE' '13'.
*  setfieldcat 'CSPEM' '可退货数量' 'MSEG-MENGE' '13'.
  setfieldcat 'MEINS' '单位' '' ''.
  setfieldcat 'BSTAR_T' '库存类型' '' ''.
  setfieldcat 'ZREQU' '退货数量' 'MSEG-MENGE' '13'.
  setfieldcat 'ZREDA' '计划退货日期' 'MKPF-BUDAT' ''.
  setfieldcat 'LGORT' '库存地点' '' ''.
  setfieldcat 'CHARG' '批次' '' ''.
  setfieldcat 'CHARG_L' '长批号' '' ''.
  setfieldcat 'MBLNR' '物料凭证' 'MKPF-MBLNR' ''.
  setfieldcat 'ZEILE' '凭证行' '' ''.
  setfieldcat 'BUDAT' '过账日期' 'MKPF-BUDAT' ''.
  setfieldcat 'DLV_PLANNO' '送货计划号' '' ''.
  setfieldcat 'DLV_ITEMNO' '送货计划行' '' ''.
  setfieldcat 'XCLOSED' '已关闭' '' '6'.
  setfieldcat 'ZSTAT_L' '状态' '' '15'.
  setfieldcat 'NOTE' '备注' '' '50'.

  APPEND VALUE #( name = 'CALLER_EXIT' form = 'FRM_CALLER_EXIT' ) TO lt_events.
*  APPEND VALUE #( spos = 1 fieldname = 'MATNR' up = 'X' ) TO lt_sort.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      it_fieldcat_lvc          = lt_fieldcat[]
      it_events                = lt_events
*     it_sort_lvc              = lt_sort
      is_layout_lvc            = ls_layout
      is_variant               = ls_variant
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_alv[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CALLER_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LS_DATA   text
*----------------------------------------------------------------------*
FORM frm_caller_exit USING ls_data TYPE slis_data_caller_exit.
  DATA: lr_event TYPE REF TO lcl_event_handler.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = go_alv.

  CHECK go_alv IS NOT INITIAL.

  CREATE OBJECT lr_event.
  SET HANDLER lr_event->data_changed_finished FOR go_alv.

  CALL METHOD go_alv->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD go_alv->set_ready_for_input
    EXPORTING
      i_ready_for_input = g_input.

  CALL METHOD go_alv->set_drop_down_table
    EXPORTING
      it_drop_down = gt_list.

ENDFORM.                    "frm_caller_exit
*&---------------------------------------------------------------------*
*&      Form  frm_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM frm_pf_status USING rt_extab TYPE slis_t_extab.
  DATA l_mode TYPE string.

  CASE g_fcode.
    WHEN 'NEW'.
      l_mode = '创建'.
      rt_extab = VALUE #( ( fcode = g_fcode ) ( fcode = 'WMS' ) ( fcode = 'SEND' ) ( fcode = 'CLOSE' ) ).
    WHEN 'SHOW'.
      l_mode = '显示'.
      rt_extab = VALUE #( ( fcode = g_fcode ) ( fcode = 'SAVE' ) ( fcode = 'SEND' ) ( fcode = 'CLOSE' ) ).
    WHEN 'WMS'.
      l_mode = '下传WMS'.
      rt_extab = VALUE #( ( fcode = g_fcode ) ( fcode = 'SAVE' ) ).
  ENDCASE.

  SET PF-STATUS 'S1000' EXCLUDING rt_extab.
  SET TITLEBAR 'T1000' WITH l_mode.

ENDFORM.                    "frm_pf_status
*&---------------------------------------------------------------------*
*&      Form  frm_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM frm_user_command USING r_ucomm LIKE sy-ucomm rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN 'NEW'.
      g_fcode = r_ucomm.
      PERFORM frm_get_data.
    WHEN 'SHOW'.
      g_fcode = r_ucomm.
      PERFORM frm_get_data.
    WHEN 'WMS'.
      g_fcode = r_ucomm.
      PERFORM frm_get_data.
    WHEN 'SAVE'.
      go_alv->check_changed_data( ).
      PERFORM frm_save_data.
    WHEN 'SEND'.
      PERFORM frm_send_data USING ''.
    WHEN 'CLOSE'.
      PERFORM frm_close_data.
  ENDCASE.

  rs_selfield-refresh = 'X'.

ENDFORM.                    " FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  FRM_ALV_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_alv_data_changed_finished USING e_modified et_good_cells TYPE lvc_t_modi.
  DATA: l_modified(1),
        lt_msg  TYPE TABLE OF bapiret2.

  CHECK e_modified EQ 'X'.
  LOOP AT et_good_cells INTO DATA(ls_cells).
    READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) INDEX ls_cells-row_id.
    IF sy-subrc EQ 0.
      CASE ls_cells-fieldname.
        WHEN 'ZREDA'.
          PERFORM frm_check_zreda USING <fs_alv>.
        WHEN 'ZREQU'.
          PERFORM frm_check_zrequ USING <fs_alv>.
        WHEN 'LGORT'.
          PERFORM frm_check_lgort USING <fs_alv>.
      ENDCASE.
      l_modified = 'X'.
    ENDIF.
  ENDLOOP.

  IF l_modified EQ 'X'.
    go_alv->refresh_table_display( is_stable = VALUE #( col = 'X' ) i_soft_refresh = 'X' ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_show_error USING it_msg TYPE bapiret2_t.

  CHECK it_msg IS NOT INITIAL.
  IF lines( it_msg ) GT 1.
    CALL FUNCTION 'SUSR_DISPLAY_LOG'
      EXPORTING
        display_in_popup = 'X'
      TABLES
        it_log_bapiret2  = it_msg
      EXCEPTIONS
        parameter_error  = 1
        OTHERS           = 2.
  ELSE.
    READ TABLE it_msg INTO DATA(ls_msg) INDEX 1.
    MESSAGE ls_msg-message TYPE 'S' DISPLAY LIKE ls_msg-type.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_ZREDA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_zreda USING is_data TYPE ty_alv.

  CHECK is_data-zreda IS NOT INITIAL.
  IF is_data-zreda LT sy-datum.
    MESSAGE '计划退货日期不能小于当前日期！' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_ZREQU
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_zrequ USING is_data TYPE ty_alv.

  CHECK is_data-zrequ IS NOT INITIAL.
  IF is_data-zrequ GT ( is_data-menge_dr - is_data-fkimg ).
    MESSAGE '退货数量不能大于已收货数量' TYPE 'E'.
  ENDIF.

  DATA l_qty TYPE mseg-menge.
  LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE charg EQ is_data-charg AND bstar EQ is_data-bstar.
    l_qty = l_qty + <fs_alv>-zrequ.
  ENDLOOP.
  IF l_qty GT is_data-cspem.
    DATA(l_str) = |{ '退货总数' }{ l_qty } { '不能大于已冻结库存' }{ is_data-cspem }|.
    MESSAGE l_str TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_LGORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_check_lgort USING is_data TYPE ty_alv.

  CHECK is_data-lgort IS NOT INITIAL.
  SELECT SINGLE COUNT(*) FROM zmm_lgort
    WHERE werks IN @s_werks AND plant_area IN @s_parea AND xreturn EQ 'X' AND lgort EQ @is_data-lgort.
  IF sy-subrc NE 0.
    MESSAGE is_data-lgort && '不是退货库存地！' TYPE 'E'.
  ENDIF.

ENDFORM.