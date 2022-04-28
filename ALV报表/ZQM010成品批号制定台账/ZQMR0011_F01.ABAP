*&---------------------------------------------------------------------*
*& 包含               ZQMR0011_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_authority_check .
  AUTHORITY-CHECK OBJECT 'Q_INSPLOT'
         ID 'WERKS' FIELD p_werks.
  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH '没有权限'.
    LEAVE LIST-PROCESSING.
  ENDIF.
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
  DEFINE cunit_input.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input                = &1
        language             = sy-langu
      IMPORTING
        output               = &2
      EXCEPTIONS
        unit_not_found       = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  END-OF-DEFINITION.
  DATA: lv_zchej TYPE zchej,
        lt_afpo  TYPE STANDARD TABLE OF afpo,
        lt_mcha  TYPE STANDARD TABLE OF mcha,
        lt_mseg  TYPE STANDARD TABLE OF mseg,
        lt_mseg1 TYPE STANDARD TABLE OF mseg,
        lt_mseg2 TYPE STANDARD TABLE OF mseg,
        lt_mara  TYPE STANDARD TABLE OF mara,
        lt_makt  TYPE STANDARD TABLE OF makt,
        lt_out   TYPE STANDARD TABLE OF gty_out.
  DATA: lv_objek       TYPE cuobn,
        lv_obtab       TYPE tabelle,
        lv_klart       TYPE klassenart,
        lv_class       TYPE klasse_d,
        lv_objectkey   TYPE bapi1003_key-object,
        lv_objecttable TYPE bapi1003_key-objecttable,
        lv_classnum    TYPE bapi1003_key-classnum,
        lv_classtype   TYPE bapi1003_key-classtype,
        lv_long        TYPE bapi1003_key-object_long,
        lv_meins       TYPE meins,
        lv_meins1      TYPE meins,
        lv_psmng       TYPE rke_menge,
        lv_quantity    TYPE rke_menge,
        lt_snum        TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
        lt_schar       TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
        lt_scurr       TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
        lt_return      TYPE STANDARD TABLE OF bapiret2.
  " get AFPO data
  CLEAR: lt_out.
  SELECT a~aufnr, a~posnr, a~kdauf, a~kdpos,
         a~psmng, a~matnr, a~pwerk, a~charg,
         a~meins, b~bu_sort1
    FROM afpo AS a
    INNER JOIN vbak AS c
    ON c~vbeln = a~kdauf
   LEFT JOIN but000 AS b   " INNER JOIN  改为left join 20191223 waymon
    ON b~partner = c~kunnr
    INTO CORRESPONDING FIELDS OF TABLE @lt_out
    WHERE pwerk = @p_werks
      AND kdauf IN @s_kdauf
      AND matnr IN @s_matnr
      AND b~bu_sort1 IN @s_sort1.
  IF sy-subrc <> 0.
    MESSAGE s000(zfi) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    " get MCHA data
    CLEAR: lt_mcha.
    SELECT matnr werks charg ersda
      FROM mcha
      INTO CORRESPONDING FIELDS OF TABLE lt_mcha
      FOR ALL ENTRIES IN lt_out
      WHERE charg = lt_out-charg
        AND matnr = lt_out-matnr
        AND werks = lt_out-pwerk.
    IF sy-subrc = 0.
      LOOP AT lt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
        IF <fs_out>-charg IS NOT INITIAL.
          READ TABLE lt_mcha INTO DATA(ls_mcha)
            WITH KEY charg = <fs_out>-charg matnr = <fs_out>-matnr werks = <fs_out>-pwerk.
          IF sy-subrc = 0.
            IF ls_mcha-ersda IS INITIAL.
              <fs_out>-ersda = ls_mcha-ersda.
            ELSE.
              IF ls_mcha-ersda IN s_ersda.
                <fs_out>-ersda = ls_mcha-ersda.
              ELSE.
                <fs_out>-aufnr = ''.
              ENDIF.
            ENDIF.
          ELSE.
            <fs_out>-ersda = ls_mcha-ersda.
          ENDIF.
        ENDIF.
      ENDLOOP.
      DELETE lt_out WHERE aufnr IS INITIAL.
    ENDIF.
    " get RESB data
    SELECT rsnum, rspos, rsart, aufnr,
           matnr, werks, charg, bdmng
      FROM resb
      INTO TABLE @DATA(lt_resb)
      FOR ALL ENTRIES IN @lt_out
      WHERE matnr IN @s_smatn
        AND charg IN @s_charg
        AND aufnr = @lt_out-aufnr
        AND bwart = '261'
        AND matnr LIKE '0000000003%'.
    IF sy-subrc = 0.
      SORT lt_resb BY aufnr matnr werks charg bdmng.
      DELETE ADJACENT DUPLICATES FROM lt_resb COMPARING aufnr matnr werks charg bdmng.
      LOOP AT lt_out ASSIGNING <fs_out>.
        LOOP AT lt_resb INTO DATA(ls_resb) WHERE aufnr = <fs_out>-aufnr.
          <fs_out>-smatnr = ls_resb-matnr.
          <fs_out>-swerks = ls_resb-werks.
          <fs_out>-scharg = ls_resb-charg.
          <fs_out>-menge = ls_resb-bdmng.
          APPEND <fs_out> TO gt_out.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
*    " get MSEG data
*    CLEAR: LT_MSEG1, LT_MSEG2, LT_MSEG.
*    SELECT MBLNR MJAHR ZEILE MATNR
*           WERKS CHARG MENGE AUFNR
*      FROM MSEG
*      INTO CORRESPONDING FIELDS OF TABLE LT_MSEG1
*      FOR ALL ENTRIES IN LT_OUT
*      WHERE MATNR IN S_SMATN
*        AND CHARG IN S_CHARG
*        AND AUFNR = LT_OUT-AUFNR
*        AND BWART = '261'
*        AND MATNR LIKE '0000000003%'.
*    IF SY-SUBRC = 0.
*      SELECT MBLNR MJAHR ZEILE
*        FROM MSEG
*        INTO CORRESPONDING FIELDS OF TABLE LT_MSEG2
*        FOR ALL ENTRIES IN LT_MSEG1
*        WHERE SMBLN = LT_MSEG1-MBLNR.
*      IF SY-SUBRC = 0.
*        LOOP AT LT_MSEG1 INTO DATA(LS_MSEG1).
*          READ TABLE LT_MSEG2 TRANSPORTING NO FIELDS WITH KEY SMBLN = LS_MSEG1-MBLNR.
*          IF SY-SUBRC <> 0.
*            APPEND LS_MSEG1 TO LT_MSEG.
*          ENDIF.
*        ENDLOOP.
*      ELSE.
*        LT_MSEG = LT_MSEG1.
*      ENDIF.
*      LOOP AT LT_OUT ASSIGNING <FS_OUT>.
*        LOOP AT LT_MSEG INTO DATA(LS_MSEG) WHERE AUFNR = <FS_OUT>-AUFNR.
*          <FS_OUT>-SMATNR = LS_MSEG-MATNR.
*          <FS_OUT>-SWERKS = LS_MSEG-WERKS.
*          <FS_OUT>-SCHARG = LS_MSEG-CHARG.
*          <FS_OUT>-MENGE = LS_MSEG-MENGE.
*          APPEND <FS_OUT> TO GT_OUT.
*        ENDLOOP.
*      ENDLOOP.
*    ELSE.
*      MESSAGE S000(ZFI) DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
  ENDIF.
  CHECK gt_out IS NOT INITIAL.
  " get MARA data
  CLEAR: lt_mara, lt_makt.
  SELECT matnr groes mhdhb
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FOR ALL ENTRIES IN gt_out
    WHERE matnr = gt_out-matnr.
  IF sy-subrc = 0.
    DATA: lt_output TYPE STANDARD TABLE OF zmaterial_tx_value,
          ls_output TYPE zmaterial_tx_value.
    LOOP AT gt_out ASSIGNING <fs_out>.
      READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = <fs_out>-matnr.
      IF sy-subrc = 0.
*        <FS_OUT>-GROES = LS_MARA-GROES.
        CLEAR: lt_output.
        CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
          EXPORTING
            matnr  = ls_mara-matnr
            atnam  = 'ZPSPE'
          TABLES
            output = lt_output.
        READ TABLE lt_output INTO ls_output INDEX 1.
        IF sy-subrc = 0.
          <fs_out>-groes = ls_output-atwrt.
        ENDIF.
        <fs_out>-mhdhb = ls_mara-mhdhb.
      ENDIF.

*增加标签版本 20191223 top 20191223 waymon
      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
        EXPORTING
          matnr  = ls_mara-matnr
          atnam  = 'ZVERNUM'
        TABLES
          output = lt_output.
      READ TABLE lt_output INTO ls_output INDEX 1.
      IF sy-subrc = 0.
        <fs_out>-zbqbb = ls_output-atwrt.
      ENDIF.

*增加标签版本 20191223 end 20191223 waymon

    ENDLOOP.
  ENDIF.
  CLEAR: lt_mara.
  SELECT matnr groes mhdhb
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FOR ALL ENTRIES IN gt_out
    WHERE matnr = gt_out-smatnr.
  IF sy-subrc = 0.
    LOOP AT gt_out ASSIGNING <fs_out>.
      CLEAR: ls_mara.
      READ TABLE lt_mara INTO ls_mara WITH KEY matnr = <fs_out>-smatnr.
      IF sy-subrc = 0.
        <fs_out>-sgroes = ls_mara-groes.
      ENDIF.
    ENDLOOP.
  ENDIF.
  " get MAKT data
  SELECT matnr maktx
    FROM makt
    INTO CORRESPONDING FIELDS OF TABLE lt_makt
    FOR ALL ENTRIES IN gt_out
    WHERE spras = 1
      AND ( matnr = gt_out-matnr OR matnr = gt_out-smatnr ).
  IF sy-subrc = 0.
    LOOP AT gt_out ASSIGNING <fs_out>.
      READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = <fs_out>-matnr.
      IF sy-subrc = 0.
        <fs_out>-maktx = ls_makt-maktx.
      ENDIF.
      CLEAR ls_makt.
      READ TABLE lt_makt INTO ls_makt WITH KEY matnr = <fs_out>-smatnr.
      IF sy-subrc = 0.
        <fs_out>-smaktx = ls_makt-maktx.
      ENDIF.
    ENDLOOP.
  ENDIF.
  LOOP AT gt_out ASSIGNING <fs_out>.
    CLEAR: lv_meins, lv_psmng.
    lv_psmng = <fs_out>-psmng.
    IF <fs_out>-groes CS '瓶'.
      lv_meins = '瓶'.
    ELSEIF <fs_out>-groes CS '盒'.
      lv_meins = '盒'.
    ENDIF.
    IF lv_psmng IS NOT INITIAL AND lv_meins IS NOT INITIAL.
      CLEAR: lv_quantity, lv_meins1.
      cunit_input: <fs_out>-meins <fs_out>-meins.
      cunit_input: lv_meins lv_meins.
      CALL FUNCTION 'KEPSL_MATERIAL_UNIT_CONVERSION'
        EXPORTING
          i_matnr          = <fs_out>-matnr
          i_quantity       = lv_psmng
          i_uom            = <fs_out>-meins
          i_target_uom     = lv_meins
        IMPORTING
          e_quantity       = lv_quantity
          e_uom            = lv_meins1
        EXCEPTIONS
          conversion_error = 1
          OTHERS           = 2.
      IF sy-subrc = 0.
        <fs_out>-zpack_sta = lv_quantity.
      ENDIF.
    ENDIF.
    CLEAR: lv_zchej.
*    SELECT SINGLE zchej FROM zppt0001 INTO lv_zchej
*      WHERE aufnr = <fs_out>-aufnr.
*    IF sy-subrc = 0.
*      <fs_out>-zchej = lv_zchej.
*    ENDIF.
***************2021.2.19 Seashell 修改车间字段取值*********
    SELECT SINGLE
      zchej,
      zchanx
    FROM aufk
    INTO @DATA(ls_aufk)
    WHERE aufk~aufnr = @<fs_out>-aufnr.
    IF ls_aufk-zchej IS NOT INITIAL.
      lv_zchej = ls_aufk-zchej && '-' && ls_aufk-zchanx.
      <fs_out>-zchej = lv_zchej.
      CLEAR ls_aufk.
    ENDIF.
*******************END******************
    IF <fs_out>-charg IS NOT INITIAL.
      CLEAR: lv_objek, lv_obtab, lv_klart, lv_class.
      CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
        EXPORTING
          i_matnr = <fs_out>-matnr
          i_charg = <fs_out>-charg
          i_werks = <fs_out>-pwerk
        IMPORTING
          e_objek = lv_objek
          e_obtab = lv_obtab
          e_klart = lv_klart
          e_class = lv_class.
      CLEAR: lv_objectkey, lv_long, lv_objecttable, lv_classnum, lv_classtype.
*      LV_OBJECTKEY =
      lv_long = lv_objek.
      lv_objecttable = lv_obtab.
      lv_classnum = lv_class.
      lv_classtype = lv_klart.
      CLEAR: lt_snum, lt_schar, lt_scurr, lt_return.
      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
        EXPORTING
          objectkey       = lv_objectkey
          objecttable     = lv_objecttable
          classnum        = lv_classnum
          classtype       = lv_classtype
          objectkey_long  = lv_long
        TABLES
          allocvaluesnum  = lt_snum
          allocvalueschar = lt_schar
          allocvaluescurr = lt_scurr
          return          = lt_return.
      LOOP AT lt_return INTO DATA(ls_return) WHERE type = 'E'.
      ENDLOOP.
      IF sy-subrc <> 0.
        LOOP AT lt_snum INTO DATA(ls_snum).
          IF ls_snum-charact = 'Z_BATCH_SCRQ'.
            PERFORM frm_format_date USING ls_snum-value_from CHANGING <fs_out>-zscrq.
          ELSEIF ls_snum-charact = 'Z_BATCH_YXQZ'.
            PERFORM frm_format_date USING ls_snum-value_from CHANGING <fs_out>-zyxqz.
          ENDIF.
        ENDLOOP.
        LOOP AT lt_schar INTO DATA(ls_schar).
          IF ls_schar-charact = 'Z_BATCH_DYPC'.
            <fs_out>-zdypc = ls_schar-value_char_long.
          ELSEIF ls_schar-charact = 'Z_BATCH_SCRQA'.
            <fs_out>-scrqa = ls_schar-value_char_long.
          ELSEIF ls_schar-charact = 'Z_BATCH_SCRQB'.
            <fs_out>-scrqb = ls_schar-value_char_long.
          ELSEIF ls_schar-charact = 'Z_BATCH_SCRQC'.
            <fs_out>-scrqc = ls_schar-value_char_long.
          ELSEIF ls_schar-charact = 'Z_BATCH_SCRQD'.
            <fs_out>-scrqd = ls_schar-value_char_long.
          ELSEIF ls_schar-charact = 'Z_BATCH_YXQZA'.
            <fs_out>-yxqza = ls_schar-value_char_long.
          ELSEIF ls_schar-charact = 'Z_BATCH_YXQZB'.
            <fs_out>-yxqzb = ls_schar-value_char_long.
          ELSEIF ls_schar-charact = 'Z_BATCH_YXQZC'.
            <fs_out>-yxqzc = ls_schar-value_char_long.
          ELSEIF ls_schar-charact = 'Z_BATCH_YXQZD'.
            <fs_out>-yxqzd = ls_schar-value_char_long.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
*    REPLACE ALL OCCURRENCES OF REGEX
  ENDLOOP.

  "2021.2.19 Seashell 添加包装批号的筛选条件
  IF s_zcpdy IS NOT INITIAL.
    SELECT
      *
    FROM @gt_out AS a
    WHERE a~zdypc IN @s_zcpdy
    INTO TABLE @DATA(gt_out2).
    CLEAR gt_out.
    MOVE gt_out2 TO gt_out.
  ENDIF.
**************END******************88
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
  PERFORM frm_set_fieldcat USING:'ERSDA' '日期' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'KDAUF' '销售订单号' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'BU_SORT1' '客户' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'MATNR' '产品代码' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'MAKTX' '产品名称' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'CHARG' '批号' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'AUFNR' '生产订单' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SGROES' '规格' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'GROES' '规格型号' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'ZDYPC' ' 包装批号（打印）' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'MENGE' '生产批量' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'PSMNG' '包装数量' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'ZPACK_STA' '包装数量(瓶盒)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'BRGEW' '毛重' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'NTGEW' '净重' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'ZSCRQ' '生产日期' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'MHDHB' '保质期（天）' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'ZYXQZ' '有效期至' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'ZBQBB' '标签版本' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'ZCHEJ' '车间' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SCRQA' '生产日期（PVC/瓶）(打印)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SCRQB' '生产日期（袋）(打印)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SCRQC' '生产日期（盒）(打印)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SCRQD' '生产日期（礼盒）(打印)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'YXQZA' '有效期至（PVC/瓶）(打印)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'YXQZB' '有效期至（袋）(打印)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'YXQZC' '有效期至（盒）(打印)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'YXQZD' '有效期至（礼盒）(打印)' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'YTGBQ' '已提供标签' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SCHARG' '工艺批号' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SMATNR' 'S产品代码' CHANGING ct_fieldcat.
  PERFORM frm_set_fieldcat USING:'SMAKTX' 'S产品名称' CHANGING ct_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&      <-- CT_FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat  USING    u_fieldname TYPE slis_fieldcat_alv-fieldname
                                u_seltext TYPE slis_fieldcat_alv-seltext_l
*                                U_CHECK TYPE CHAR01
                       CHANGING ct_fieldcat TYPE slis_t_fieldcat_alv.

  APPEND INITIAL LINE TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

*  IF U_CHECK IS NOT INITIAL.
  <fs_fieldcat>-fieldname = u_fieldname.
  <fs_fieldcat>-seltext_s = <fs_fieldcat>-seltext_m = <fs_fieldcat>-seltext_l = u_seltext.
  IF u_fieldname = 'MATNR'
    OR u_fieldname = 'SMATNR'.
    <fs_fieldcat>-no_zero = 'X'.
  ENDIF.
*  ELSE.
*    <FS_FIELDCAT>-FIELDNAME = U_FIELDNAME.
*    <FS_FIELDCAT>-NO_OUT = 'X'.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_FORMAT_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_SNUM_VALUE_FROM
*&      <-- <FS_OUT>_ZSCRQ
*&---------------------------------------------------------------------*
FORM frm_format_date  USING i_date CHANGING c_date.
  DATA: lv_date TYPE qsollwertc.

  CLEAR: lv_date.
  CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
    EXPORTING
      i_number_of_digits       = '0'
      i_fltp_value             = i_date
      i_value_not_initial_flag = 'X'
      i_screen_fieldlength     = 16
    IMPORTING
      e_char_field             = lv_date.
  IF lv_date IS NOT INITIAL.
    CONDENSE lv_date NO-GAPS.
    c_date = lv_date.
  ENDIF.
ENDFORM.