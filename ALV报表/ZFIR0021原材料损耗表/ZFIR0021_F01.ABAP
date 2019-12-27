*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
TABLES:afko.
FORM frm_get_data .
  DEFINE range_aufnr.
    CLEAR: gs_aufnr.
    gs_aufnr-sign = 'I'.
    gs_aufnr-option = 'EQ'.
    gs_aufnr-low = &1.
    APPEND gs_aufnr TO gr_aufnr.
  END-OF-DEFINITION.

  DATA: lv_date   TYPE aufidat2,
        lv_objnr  TYPE j_objnr,
        lv_ausss  TYPE char10,
        lv_menge  TYPE bdmng,
        lv_zlcl   TYPE c LENGTH 10,
        lt_status TYPE TABLE OF jstat,
        lt_stb    TYPE STANDARD TABLE OF stpox,
        lt_out    TYPE STANDARD TABLE OF gty_out,
        ls_out    TYPE gty_out.

  " 设置期间
  CLEAR: gr_date, gs_date, lv_date.
  lv_date+0(4) = p_gjahr.
  lv_date+4(2) = s_month-low+1(2).
  lv_date+6(2) = '01'.
  gs_date-sign = 'I'.
  gs_date-option = 'BT'.
  gs_date-low = lv_date.
  IF s_month-high IS INITIAL.
    gs_date-high = '99991231'.
  ELSE.
    CLEAR: lv_date.
    lv_date+0(4) = p_gjahr.
    lv_date+4(2) = s_month-high+1(2).
    lv_date+6(2) = '28'.
    lv_date = lv_date + 5.
    lv_date+6(2) = '01'.
    lv_date = lv_date - 1.
    gs_date-high = lv_date.
  ENDIF.
  APPEND gs_date TO gr_date.

  DATA: lv_aufnr TYPE aufnr.
  " 取流程订单
  SELECT aufk~aufnr, werks, auart, idat2
    FROM aufk
    INNER JOIN afko
    ON afko~aufnr = aufk~aufnr
    INTO TABLE @DATA(lt_aufk)
    WHERE werks = @p_werks
      AND idat2 IN @gr_date
      AND auart IN @s_auart
      AND afko~plnnr <> ''.
*      AND AUFNR = @LV_AUFNR.
  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH '无数据'.
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT aufnr, plnbez, stlnr, stlan, stlal, aufld
      FROM afko
      INTO TABLE @DATA(lt_afko)
      FOR ALL ENTRIES IN @lt_aufk
      WHERE aufnr = @lt_aufk-aufnr
        AND plnbez IN @s_matnr
        AND afko~plnnr <> ''.
    " 筛选订单状态为TECO, CLOS, DLV
    CLEAR: lt_out.
    LOOP AT lt_aufk INTO DATA(ls_aufk).
      CLEAR: lv_objnr, lt_status, ls_out.
      lv_objnr = 'OR' && ls_aufk-aufnr.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
          objnr            = lv_objnr
          only_active      = 'X'
        TABLES
          status           = lt_status
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc = 0.
        READ TABLE lt_status TRANSPORTING NO FIELDS WITH KEY stat = 'I0045'.
        IF sy-subrc = 0.
          ls_out-txt30 = 'TECO'.
        ELSE.
          READ TABLE lt_status TRANSPORTING NO FIELDS WITH KEY stat = 'I0012'.
          IF sy-subrc = 0.
            ls_out-txt30 = 'DLV'.
          ELSE.
            READ TABLE lt_status TRANSPORTING NO FIELDS WITH KEY stat = 'I0135'.
            IF sy-subrc = 0.
              ls_out-txt30 = 'CLOS'.
            ENDIF.
          ENDIF.
        ENDIF.
        IF ls_out-txt30 IS NOT INITIAL.
          READ TABLE lt_afko INTO DATA(ls_afko) WITH KEY aufnr = ls_aufk-aufnr.
          IF sy-subrc = 0.
            ls_out-plnbez = ls_afko-plnbez.
            ls_out-aufnr = ls_aufk-aufnr.
            ls_out-auart = ls_aufk-auart.
            ls_out-werks = ls_aufk-werks.
            ls_out-idat2 = ls_aufk-idat2.
            APPEND ls_out TO lt_out.
            range_aufnr: ls_aufk-aufnr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF lt_out IS INITIAL.
    MESSAGE e001(00) WITH '未找到符合状态的订单'.
    LEAVE LIST-PROCESSING.
  ELSE.
    SORT lt_out BY aufnr.
    DELETE ADJACENT DUPLICATES FROM lt_out COMPARING aufnr.
    " 取装配报废(%)
    SELECT matnr, werks, ausss
      FROM marc
      INTO TABLE @DATA(lt_marc)
      FOR ALL ENTRIES IN @lt_out
      WHERE matnr = @lt_out-plnbez
        AND werks = @lt_out-werks.

    " 取产品名称
    SELECT matnr, spras, maktx
      FROM makt
      INTO TABLE @DATA(lt_makt)
      FOR ALL ENTRIES IN @lt_out
      WHERE matnr = @lt_out-plnbez
        AND spras = 1.

    " 取数量/单位（移动类型101/102）
    SELECT aufnr, charg, meins,
      SUM( CASE bwart WHEN '101' THEN menge
                      WHEN '102' THEN - menge
                      WHEN 'Y01' THEN menge
                      WHEN 'Y02' THEN - menge END ) AS menge
      FROM matdoc
      INTO TABLE @DATA(lt_matdoc)
      WHERE aufnr IN @gr_aufnr
        AND ( bwart = '101' OR bwart = '102' )
      GROUP BY aufnr, charg, meins.

    " 取实际领用量及单位（移动类型261/262、531/532）
    SELECT aufnr, matnr, meins, charg_sid,
      SUM( CASE bwart WHEN '261' THEN menge
                      WHEN '262' THEN - menge
                      WHEN '531' THEN menge
                      WHEN '532' THEN - menge END ) AS menge
      FROM matdoc
      INTO TABLE @DATA(lt_matdoc_sj)
      WHERE aufnr IN @gr_aufnr
        AND ( bwart = '261' OR bwart = '262' OR bwart = '531' OR bwart = '532' )
      GROUP BY aufnr, matnr, meins, charg_sid.
    IF sy-subrc = 0.
      " 取产品名称(实际)
      SELECT matnr, spras, maktx
        FROM makt
        INTO TABLE @DATA(lt_makt_sj)
        FOR ALL ENTRIES IN @lt_matdoc_sj
        WHERE matnr = @lt_matdoc_sj-matnr
          AND spras = 1.
    ENDIF.

    " 标准使用量及单位
    CLEAR: lt_afko.
    SELECT aufnr plnbez stlnr stlan stlal aufld
      FROM afko
      INTO TABLE lt_afko
      WHERE aufnr IN gr_aufnr.
    IF sy-subrc = 0.
*      SELECT A~STLTY, A~STLNR, A~STLAL, A~STKOZ, A~BMENG, A~BMEIN,
*             B~STLKN, B~STPOZ, B~IDNRK, B~MENGE, B~FMENG, B~AUSCH
*        FROM STKO AS A
*        INNER JOIN STPO AS B
*        ON B~STLTY = A~STLTY AND B~STLNR = A~STLNR
*        INTO TABLE @DATA(LT_STPO)
*        FOR ALL ENTRIES IN @LT_AFKO
*        WHERE A~STLNR = @LT_AFKO-STLNR
*          AND B~FMENG = 'X'.

      SELECT matnr, spras, maktx
        FROM makt
        INTO TABLE @DATA(lt_makt_bom)
        FOR ALL ENTRIES IN @lt_afko
        WHERE matnr = @lt_afko-plnbez
          AND spras = 1.
    ENDIF.
    CLEAR: gs_out, gt_out.
    LOOP AT lt_out INTO ls_out.
      " 装配报废(%)
      READ TABLE lt_marc INTO DATA(ls_marc) WITH KEY matnr = ls_out-plnbez werks = ls_out-werks.
      IF sy-subrc = 0.
        CLEAR: lv_ausss.
        lv_ausss = ls_marc-ausss.
        CONCATENATE lv_ausss '%' INTO ls_out-sausss.
      ENDIF.
      " 物料描述
      READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_out-plnbez.
      IF sy-subrc = 0.
        ls_out-maktx = ls_makt-maktx.
      ENDIF.
      " 数量/单位（移动类型101/102、Y01/Y02）
      READ TABLE lt_matdoc INTO DATA(ls_matdoc) WITH KEY aufnr = ls_out-aufnr.
      IF sy-subrc = 0.
        ls_out-charg = ls_matdoc-charg.
        ls_out-menge = ls_matdoc-menge.
        ls_out-meins = ls_matdoc-meins.
      ENDIF.
      " 实际领用量及单位（移动类型261/262、531/532）
      LOOP AT lt_matdoc_sj INTO DATA(ls_matdoc_sj) WHERE aufnr = ls_out-aufnr.
        MOVE-CORRESPONDING ls_out TO gs_out.
        gs_out-smatnr = ls_matdoc_sj-matnr.
        gs_out-scharg = ls_matdoc_sj-charg_sid.
        READ TABLE lt_makt_sj INTO DATA(ls_makt_sj) WITH KEY matnr = ls_matdoc_sj-matnr.
        IF sy-subrc = 0.
          gs_out-smaktx = ls_makt_sj-maktx.
        ENDIF.
        gs_out-smenge = ls_matdoc_sj-menge.
        gs_out-smeins = ls_matdoc_sj-meins.
        CLEAR: lv_menge.
*        LOOP AT LT_RESB INTO DATA(LS_RESB) WHERE AUFNR = LS_MATDOC_SJ-AUFNR AND MATNR = LS_MATDOC_SJ-MATNR.
*          LV_MENGE = LV_MENGE + LS_RESB-BDMNG.
*        ENDLOOP.
*        READ TABLE LT_RESB INTO LS_RESB WITH KEY AUFNR = LS_MATDOC_SJ-AUFNR MATNR = LS_MATDOC_SJ-MATNR.
*        IF SY-SUBRC = 0.
*          GS_OUT-BMENGE = LV_MENGE.
*          GS_OUT-BMEINS = LS_RESB-MEINS.
*          IF LS_RESB-FMENG IS NOT INITIAL.
*            GS_OUT-HMENGE = GS_OUT-BMENGE.
*          ENDIF.
*          GS_OUT-AUSCH = LS_RESB-AUSCH.
*        ENDIF.
        APPEND gs_out TO gt_out.
        CLEAR: gs_out.
      ENDLOOP.
      " 标准使用量及单位
      IF ls_out-menge IS NOT INITIAL.
        CLEAR: ls_afko.
        READ TABLE lt_afko INTO ls_afko WITH KEY aufnr = ls_out-aufnr.
        IF sy-subrc = 0.
          CLEAR: lt_stb.
          CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
            EXPORTING
              capid                 = 'PI01'              "BOM APPLICATION
              datuv                 = ls_afko-aufld
              ehndl                 = '1'
              emeng                 = ls_out-menge
              mtnrv                 = ls_afko-plnbez      "MATERIAL
              stlan                 = ls_afko-stlan
              stlal                 = ls_afko-stlal
              mktls                 = 'X'
              mehrs                 = '' "   LV_CC        "是否多层次展开，X代表多层
              werks                 = ls_out-werks
            TABLES
              stb                   = lt_stb
            EXCEPTIONS
              alt_not_found         = 1
              call_invalid          = 2
              material_not_found    = 3
              missing_authorization = 4
              no_bom_found          = 5
              no_plant_data         = 6
              no_suitable_bom_found = 7
              OTHERS                = 8.
          LOOP AT lt_stb INTO DATA(ls_stb).
            READ TABLE gt_out ASSIGNING FIELD-SYMBOL(<fs_out>) WITH KEY aufnr = ls_out-aufnr smatnr = ls_stb-idnrk.
            IF sy-subrc = 0.
              CLEAR: lv_ausss.
              IF ls_stb-fmeng IS NOT INITIAL.
                <fs_out>-hmenge = ls_stb-mnglg.
              ELSE.
                <fs_out>-bmenge = ls_stb-mnglg.
                <fs_out>-ausch = ls_stb-ausch.
                lv_ausss = ls_stb-ausch.
                CONCATENATE lv_ausss '%' INTO <fs_out>-sausch.
              ENDIF.
*              READ TABLE LT_STPO INTO DATA(LS_STPO) WITH KEY STLNR = LS_AFKO-STLNR IDNRK = LS_STB-IDNRK.
*              IF SY-SUBRC = 0.
*                <FS_OUT>-BMENGE = LS_STB-MNGLG + LS_STPO-MENGE.
*                <FS_OUT>-HMENGE = LS_STPO-MENGE.
*              ELSE.
*                <FS_OUT>-BMENGE = LS_STB-MNGLG.
*              ENDIF.
*              CLEAR: LV_AUSSS.
              <fs_out>-bmeins = ls_stb-meins.
*              <FS_OUT>-AUSCH = LS_STB-AUSCH.
*              LV_AUSSS = LS_STB-AUSCH.
*              CONCATENATE LV_AUSSS '%' INTO <FS_OUT>-SAUSCH.
            ELSE.
              MOVE-CORRESPONDING ls_out TO gs_out.
              gs_out-smatnr = ls_stb-idnrk.
              READ TABLE lt_makt_bom INTO DATA(ls_makt_bom) WITH KEY matnr = ls_afko-plnbez.
              IF sy-subrc = 0.
                gs_out-smaktx = ls_makt_bom-maktx.
              ENDIF.
              CLEAR: lv_ausss.
              IF ls_stb-fmeng IS NOT INITIAL.
                gs_out-hmenge = ls_stb-mnglg.
              ELSE.
                gs_out-bmenge = ls_stb-mnglg.
                gs_out-ausch = ls_stb-ausch.
                lv_ausss = ls_stb-ausch.
                CONCATENATE lv_ausss '%' INTO gs_out-sausch.
                APPEND gs_out TO gt_out.
                CLEAR: gs_out.
              ENDIF.
*              READ TABLE LT_STPO INTO LS_STPO WITH KEY STLNR = LS_AFKO-STLNR IDNRK = LS_STB-IDNRK.
*              IF SY-SUBRC = 0.
*                GS_OUT-BMENGE = LS_STB-MNGLG + LS_STPO-MENGE.
*                GS_OUT-HMENGE = LS_STPO-MENGE.
*              ELSE.
*                GS_OUT-BMENGE = LS_STB-MNGLG.
*              ENDIF.
*              CLEAR: LV_AUSSS.
              gs_out-bmeins = ls_stb-meins.
*              GS_OUT-AUSCH = LS_STB-AUSCH.
*              LV_AUSSS = LS_STB-AUSCH.
*              CONCATENATE LV_AUSSS '%' INTO GS_OUT-SAUSCH.
*              APPEND GS_OUT TO GT_OUT.
*              CLEAR: GS_OUT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
*      LOOP AT LT_RESB INTO LS_RESB WHERE AUFNR = LS_OUT-AUFNR.
*        MOVE-CORRESPONDING LS_OUT TO GS_OUT.
*        READ TABLE LT_MATDOC_SJ TRANSPORTING NO FIELDS WITH KEY AUFNR = LS_RESB-AUFNR MATNR = LS_RESB-MATNR.
*        IF SY-SUBRC <> 0.
*          GS_OUT-SMATNR = LS_RESB-MATNR.
*          READ TABLE LT_MAKT_BOM INTO DATA(LS_MAKT_BOM) WITH KEY MATNR = LS_RESB-MATNR.
*          IF SY-SUBRC = 0.
*            GS_OUT-SMAKTX = LS_MAKT_BOM-MAKTX.
*          ENDIF.
*          GS_OUT-BMENGE = LS_RESB-BDMNG.
*          GS_OUT-BMEINS = LS_RESB-MEINS.
*          IF LS_RESB-FMENG IS NOT INITIAL.
*            GS_OUT-HMENGE = GS_OUT-BMENGE.
*          ENDIF.
*          GS_OUT-AUSCH = LS_RESB-AUSCH.
*          APPEND GS_OUT TO GT_OUT.
*          CLEAR: GS_OUT.
*        ENDIF.
*      ENDLOOP.
    ENDLOOP.
  ENDIF.
  IF gt_out IS NOT INITIAL.
    LOOP AT gt_out ASSIGNING <fs_out>.
*      <FS_OUT>-BMENGE = ( <FS_OUT>-BMENGE - <FS_OUT>-HMENGE ) / ( ( 1 + <FS_OUT>-AUSSS ) * ( 1 + <FS_OUT>-AUSCH ) ).
      <fs_out>-zlc = <fs_out>-smenge - <fs_out>-bmenge.
      IF <fs_out>-bmenge IS NOT INITIAL.
        CLEAR: lv_zlcl.
        <fs_out>-zlcl = <fs_out>-zlc * 100 / <fs_out>-bmenge.
        lv_zlcl = <fs_out>-zlcl.
        lv_zlcl = abs( lv_zlcl ).
        CONDENSE lv_zlcl NO-GAPS.
        IF <fs_out>-zlcl >= 0.
          CONCATENATE lv_zlcl '%' INTO <fs_out>-zslcl.
        ELSE.
          CONCATENATE '-' lv_zlcl '%' INTO <fs_out>-zslcl.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DISPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_display_alv .
  PERFORM frm_set_field CHANGING gt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET = 'FRM_PF_STATUS'
*     I_CALLBACK_USER_COMMAND  = 'FRM_USER_COMMAND'
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = gt_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_field  CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv.
  gs_layout-zebra = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  PERFORM frm_set_fieldcat USING:'AUFNR' '订单' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'AUART' '订单类型' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'WERKS' '工厂' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'IDAT2' '日期' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'PLNBEZ' '物料号' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'TXT30' '订单状态' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SAUSSS' '装配报废(%)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'MAKTX' '产品名称' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'CHARG' ' 批次' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'MENGE' '数量' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'MEINS' '单位' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SMATNR' '物料号' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SMAKTX' '产品名称' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SCHARG' '产品批次' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SMENGE' '数量(实际)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SMEINS' '单位(实际)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'BMENGE' '数量(标准)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'BMEINS' '单位(标准)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'HMENGE' '固定损耗量' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SAUSCH' '组件报废百分比' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'ZLC' '量差' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'ZSLCL' '量差率' CHANGING ct_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      <-- CT_FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat  USING    u_fieldname TYPE slis_fieldcat_alv-fieldname
                                u_seltext TYPE slis_fieldcat_alv-seltext_l
                       CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv.

  APPEND INITIAL LINE TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

  <fs_fieldcat>-fieldname = u_fieldname.
  <fs_fieldcat>-seltext_s = <fs_fieldcat>-seltext_m = <fs_fieldcat>-seltext_l = u_seltext.
  IF u_fieldname = 'PLNBEZ'
    OR u_fieldname = 'SMATNR'
    OR u_fieldname = 'AUFNR'.
    <fs_fieldcat>-no_zero = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_authority_check .
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
   ID 'BUKRS' FIELD p_werks
   ID 'ACTVT' DUMMY .
  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH '无权限'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.