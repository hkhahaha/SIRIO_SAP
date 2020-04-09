*&---------------------------------------------------------------------*
*& Report ZSDR0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0001.

TABLES: vbak, vbap, likp, lips.

TYPES BEGIN OF ty_alv_hd.
INCLUDE TYPE zsds0001_head.
TYPES: sel(1),
       light        TYPE icon-id,
       message(100),
       zwmszt       TYPE likp-zwmszt,
       zwmszt_l(20),
       END OF ty_alv_hd.

TYPES BEGIN OF ty_alv_it.
INCLUDE TYPE zsds0001_item.
TYPES: vbeln TYPE likp-vbeln,
       END OF ty_alv_it.

DATA: gv_fcode  LIKE sy-ucomm,
      gt_alv_hd TYPE TABLE OF ty_alv_hd,
      gt_alv_it TYPE TABLE OF ty_alv_it,
      gt_item   TYPE TABLE OF ty_alv_it,
      gt_item2  TYPE TABLE OF ty_alv_it,
      gr_zwmszt TYPE REF TO zcl_domain_helper.

CONSTANTS: gc_green  LIKE dv70a-statusicon VALUE '@08@',
           gc_yellow LIKE dv70a-statusicon VALUE '@09@',
           gc_red    LIKE dv70a-statusicon VALUE '@0A@'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: p_vkorg FOR vbak-vkorg NO INTERVALS NO-EXTENSION,
                s_werks FOR vbap-werks OBLIGATORY,
                s_vbeln FOR likp-vbeln,
                s_lfart FOR likp-lfart,
                s_kunag FOR likp-kunag,
                s_erdat FOR likp-erdat,
                s_zfhrq FOR likp-zfhrq,
                s_ernam FOR likp-ernam.
PARAMETERS p_ref AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM frm_auth_check.
  PERFORM frm_get_data.
  PERFORM frm_show_data USING 'GT_ALV_HD'.
*&---------------------------------------------------------------------*
*& Form FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_auth_check .

  LOOP AT s_werks INTO DATA(l_werks).
    AUTHORITY-CHECK OBJECT 'ZSD001'
     ID 'WERKS' FIELD l_werks-low.
    IF sy-subrc NE 0.
      MESSAGE '没有工厂' && l_werks-low && '权限！' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDLOOP.

  IF s_lfart[] IS INITIAL.
    SELECT lfart INTO TABLE @DATA(lt_lfart) FROM tvlk WHERE lfart LIKE 'Z%' OR lfart = 'NLCC'.
    s_lfart[] = VALUE #( FOR <fs> IN lt_lfart ( sign = 'I' option = 'EQ' low = <fs> ) ).
  ENDIF.

  LOOP AT s_lfart INTO DATA(l_lfart).
    AUTHORITY-CHECK OBJECT 'ZSD002'
     ID 'ZFART' FIELD l_lfart-low.
    IF sy-subrc NE 0.
      MESSAGE '没有交货类型' && l_lfart-low && '权限！' TYPE 'S' DISPLAY LIKE 'E'.
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
  DATA: lt_head      TYPE TABLE OF ty_alv_hd,
        r_zwmszt     TYPE RANGE OF likp-zwmszt,
        lv_chang(15).
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'ZCUMCODE'
    IMPORTING
      output = lv_chang.


  CLEAR: lt_head[], gt_alv_hd[], gt_item[].
  IF p_ref EQ 'X'.
    r_zwmszt = VALUE #( sign = 'I' option = 'EQ' ( low = 'B' ) ( low = 'C' ) ).
  ELSE.
    r_zwmszt = VALUE #( sign = 'I' option = 'EQ' ( low = space ) ).
  ENDIF.
*抬头数据
  SELECT likp~*, sdabw AS zysfs, kunnr AS kunag
    INTO CORRESPONDING FIELDS OF TABLE @lt_head FROM likp
    WHERE vkorg IN @p_vkorg
      AND vbeln IN @s_vbeln
      AND lfart IN @s_lfart
      AND kunnr IN @s_kunag
      AND erdat IN @s_erdat
      AND zfhrq IN @s_zfhrq
      AND ernam IN @s_ernam
      AND zwmszt IN @r_zwmszt
      AND zzqzt IN ('', 'D')
      AND wbstk NE 'C'
      AND cmgst IN ('', 'A', 'D').
  IF sy-subrc NE 0.
    MESSAGE '未查询到数据！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*行项目数据
  SELECT lips~*, meins AS zmeins
    INTO CORRESPONDING FIELDS OF TABLE @gt_item FROM lips
    FOR ALL ENTRIES IN @lt_head
    WHERE vbeln EQ @lt_head-vbeln
      AND werks IN @s_werks.
  IF sy-subrc NE 0.
    MESSAGE '未查询到数据！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*业务伙伴的搜索词 1
  SELECT partner,bu_sort1 INTO TABLE @DATA(lt_but) FROM but000
    FOR ALL ENTRIES IN @lt_head
    WHERE partner EQ @lt_head-kunag.
*物料描述
  SELECT matnr,maktx INTO TABLE @DATA(lt_makt) FROM makt
    FOR ALL ENTRIES IN @gt_item
    WHERE matnr EQ @gt_item-matnr.
*WMS状态
  gr_zwmszt = NEW zcl_domain_helper( 'ZD_WMSZT' ).
*收货地址
  SELECT a~vbeln,
         b~street,
         b~str_suppl1,
         b~str_suppl2,
         b~str_suppl3
    INTO TABLE @DATA(lt_adrc) FROM vbpa AS a
    LEFT JOIN adrc AS b ON b~addrnumber EQ a~adrnr
    FOR ALL ENTRIES IN @lt_head
    WHERE a~vbeln EQ @lt_head-vbeln AND parvw EQ 'Z2'.
*联系人
  SELECT a~vbeln,
         b~remark
    INTO TABLE @DATA(lt_adrct) FROM vbpa AS a
    LEFT JOIN adrct AS b ON b~addrnumber EQ a~adrnr AND b~langu EQ @sy-langu
    FOR ALL ENTRIES IN @lt_head
    WHERE a~vbeln EQ @lt_head-vbeln AND parvw EQ 'Z2'.
*联系电话
  SELECT a~vbeln,
         b~tel_number,
         b~telnr_call
    INTO TABLE @DATA(lt_adr2) FROM vbpa AS a
    LEFT JOIN adr2 AS b ON b~addrnumber EQ a~adrnr
    FOR ALL ENTRIES IN @lt_head
    WHERE a~vbeln EQ @lt_head-vbeln AND parvw EQ 'Z2'.

  SORT lt_but   BY partner.
  SORT lt_makt  BY matnr.
  SORT lt_adrc  BY vbeln.
  SORT lt_adrct BY vbeln.
  SORT lt_adr2  BY vbeln.
  SORT gt_item  BY vgpos posnr.

  LOOP AT lt_head INTO DATA(ls_head).
    LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<fs_item>) WHERE vbeln = ls_head-vbeln.
      READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = <fs_item>-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs_item>-zbzmc = ls_makt-maktx.
      ENDIF.
************************hk添加客户物料编码、单位转描述***************
      IF <fs_item>-matnr IS NOT INITIAL.
        DATA objek TYPE ausp-objek.
        CLEAR objek.
        objek = <fs_item>-matnr.
        SELECT SINGLE
        atwrt
        FROM ausp
        INTO <fs_item>-zcumcode
        WHERE ausp~objek = objek
          AND ausp~atinn = lv_chang.
        IF <fs_item>-zcumcode IS INITIAL.
          <fs_item>-zcumcode = ''.
        ENDIF.
      ENDIF.
      IF <fs_item>-vrkme IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = <fs_item>-vrkme
            language       = '1'
          IMPORTING
            long_text      = <fs_item>-vrkme
*           OUTPUT         =
*           SHORT_TEXT     =
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

      ENDIF.
*******************************END*****************
    ENDLOOP.
    SORT gt_item BY posnr zfhsx.
    CHECK sy-subrc EQ 0.

    READ TABLE lt_but INTO DATA(ls_but) WITH KEY partner = ls_head-kunag BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_head-bu_sort1 = ls_but-bu_sort1.
    ENDIF.

    ls_head-zwmszt_l = gr_zwmszt->get_value_l( ls_head-zwmszt ).

    READ TABLE lt_adrc INTO DATA(ls_adrc) WITH KEY vbeln = ls_head-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_head-address = ls_adrc-street && ls_adrc-str_suppl1 && ls_adrc-str_suppl2 && ls_adrc-str_suppl3.
    ENDIF.

    READ TABLE lt_adrct INTO DATA(ls_adrct) WITH KEY vbeln = ls_head-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_head-zlxr = ls_adrct-remark.
    ENDIF.

    LOOP AT lt_adr2 INTO DATA(ls_adr2) WHERE vbeln EQ ls_head-vbeln.
      IF ls_head-phone IS INITIAL.
        ls_head-phone = ls_adr2-tel_number.
      ELSE.
        CONCATENATE ls_head-phone ls_adr2-tel_number INTO ls_head-phone SEPARATED BY '/'.
      ENDIF.
    ENDLOOP.

    PERFORM frm_read_text USING ls_head-vbeln CHANGING ls_head-zfhwb.

    APPEND ls_head TO gt_alv_hd.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SHOW_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_show_data USING p_name.
  DATA: lt_fieldcat TYPE lvc_t_fcat,
        wa_fieldcat TYPE lvc_s_fcat,
        lt_sort     TYPE lvc_t_sort,
        lt_events   TYPE slis_t_event,
        ls_layout   TYPE lvc_s_layo,
        ls_variant  TYPE disvariant,
        l_pos       TYPE i VALUE 0.

  FIELD-SYMBOLS <fs_tab> TYPE table.

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
    IF &1 EQ 'VBELN'.
      wa_fieldcat-hotspot = 'X'.
    ENDIF.
    APPEND wa_fieldcat TO lt_fieldcat. CLEAR: wa_fieldcat.
  END-OF-DEFINITION.

  CLEAR: ls_layout, ls_variant, lt_events[], lt_sort[],  lt_fieldcat[], l_pos.
  ls_layout-zebra      = 'X'.
  ls_layout-sel_mode   = 'A'.
  ls_layout-cwidth_opt = 'X'.
*  ls_layout-box_fname  = 'SEL'.
*  ls_layout-stylefname = 'STYLE'.
  "ls_layout-totals_bef = 'X'.

  gv_fcode = p_name.
  ls_variant-report = sy-repid.

  CASE p_name.
    WHEN 'GT_ALV_HD'.
      ls_layout-box_fname  = 'SEL'.

      setfieldcat 'LFART' '交货类型' '' ''.
      setfieldcat 'VBELN' '交货单号' 'LIKP-VBELN' ''.
      setfieldcat 'ZFHRQ' '发货日期' '' ''.
      setfieldcat 'ERDAT' '创建日期' '' ''.
      setfieldcat 'KUNAG' '客户编码' 'KNA1-KUNNR' ''.
      setfieldcat 'BU_SORT1' '客户简称' '' ''.
      setfieldcat 'ADDRESS' '收货地址' '' ''.
      setfieldcat 'ZLXR' '联系人' '' ''.
      setfieldcat 'PHONE' '电话' '' ''.
      setfieldcat 'ZYSFS' '运输方式' '' ''.
      setfieldcat 'ZFHQD' '是否特殊发货清单' '' ''.
      setfieldcat 'ZSHCOA' '是否需要随货COA' '' ''.
      setfieldcat 'ZCPH' '车牌号' '' ''.
      setfieldcat 'ZFQH' '封签号' '' ''.
      setfieldcat 'ZHGH' '货柜号' '' ''.
      setfieldcat 'ZWLGS' '物流公司' '' ''.
      setfieldcat 'ERNAM' '制单人' '' ''.
      setfieldcat 'ZFHWB' '发货文本' '' ''.
      setfieldcat 'ADDITIONAL1' '备用字段1' '' ''.
      setfieldcat 'ADDITIONAL2' '备用字段2' '' ''.
      setfieldcat 'ADDITIONAL3' '备用字段3' '' ''.
      setfieldcat 'ADDITIONAL4' '备用字段4' '' ''.
      setfieldcat 'ADDITIONAL5' '备用字段5' '' ''.
      setfieldcat 'ZWMSZT_L' 'WMS状态' '' ''.
      setfieldcat 'LIGHT' '状态' '' ''.
      setfieldcat 'MESSAGE' '处理消息' '' ''.
    WHEN 'GT_ALV_IT'.
      setfieldcat 'POSNR' '行项目号' '' ''.
      setfieldcat 'ZFHSX' 'WMS发货顺序' '' ''.
      setfieldcat 'MATNR' '产品编码' 'MARA-MATNR' ''.
      setfieldcat 'ZBZMC' '标准名称' '' ''.
      setfieldcat 'ZBZGG' '包装规格' '' ''.
      setfieldcat 'WERKS' '工厂' '' ''.
      setfieldcat 'LGORT' '库存地点' '' ''.
      setfieldcat 'LGMNG' '数量' '' ''.
      setfieldcat 'ZMEINS' '单位（基本计量单位）' '' ''.
      setfieldcat 'CHARG' '批次' '' ''.
      setfieldcat 'ZJIANS' '件数' '' ''.
      setfieldcat 'ZLXSL' '零箱数量（销售单位）' '' ''.
      setfieldcat 'LFIMG' '发货数量' '' ''.
      setfieldcat 'ZCUMCODE' '客户物料编码' '' ''.
      setfieldcat 'BWART' '移动类型' '' ''.
      setfieldcat 'VRKME' '销售单位描述' '' ''.
      setfieldcat 'ADDITIONAL1' '备用字段1' '' ''.
      setfieldcat 'ADDITIONAL2' '备用字段2' '' ''.
      setfieldcat 'ADDITIONAL3' '备用字段3' '' ''.
      setfieldcat 'ADDITIONAL4' '备用字段4' '' ''.
      setfieldcat 'ADDITIONAL5' '备用字段5' '' ''.
      setfieldcat 'VGBEL' '销售订单号' 'VBAK-VBELN' ''.
  ENDCASE.

*  APPEND VALUE #( name = 'CALLER_EXIT' form = 'FRM_CALLER_EXIT' ) TO lt_events.
*  APPEND VALUE #( spos = 1 fieldname = 'MATNR' up = 'X' ) TO lt_sort.

  ASSIGN (p_name) TO <fs_tab>.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      it_fieldcat_lvc          = lt_fieldcat[]
*     it_events                = lt_events
*     it_sort_lvc              = lt_sort
      is_layout_lvc            = ls_layout
      is_variant               = ls_variant
      i_save                   = 'A'
    TABLES
      t_outtab                 = <fs_tab>.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  frm_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM frm_pf_status USING rt_extab TYPE slis_t_extab.
  IF p_ref EQ 'X'.
    APPEND VALUE #( fcode = 'NOSEND' ) TO rt_extab.
  ELSE.
    APPEND VALUE #( fcode = 'CANCEL' ) TO rt_extab.
  ENDIF.
  SET PF-STATUS 'S1000' EXCLUDING rt_extab.
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
    WHEN 'SEND'.
      PERFORM frm_send_data.
      rs_selfield-refresh = 'X'.
    WHEN 'NOSEND'.
      PERFORM frm_change_data.
      rs_selfield-refresh = 'X'.
    WHEN 'CANCEL'.
      PERFORM frm_send_cancel.
      rs_selfield-refresh = 'X'.
    WHEN '&IC1'.
      IF rs_selfield-fieldname EQ 'VBELN'.
        CLEAR gt_alv_it[].
        READ TABLE gt_alv_hd INTO DATA(ls_alv) INDEX rs_selfield-tabindex.
        LOOP AT gt_item INTO DATA(ls_item) WHERE vbeln EQ ls_alv-vbeln.
          APPEND ls_item TO gt_alv_it.
        ENDLOOP.
        SORT gt_alv_it BY vgpos posnr.
        PERFORM frm_show_data USING 'GT_ALV_IT'.
      ENDIF.
  ENDCASE.

ENDFORM.                    " FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*& Form FRM_SEND_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_send_data .
  DATA: ls_input  TYPE zsds0001,
        ls_output TYPE zsds0001_out,
        lt_zero   TYPE TABLE OF ty_alv_it,
        l_key(90).
  DATA:lt_lips TYPE TABLE OF lips,
       ls_lips TYPE lips,
       gs_item LIKE LINE OF gt_item.

******************排序，优先按照发货顺序进行排序*********************
  IF gt_item IS NOT INITIAL.
    CLEAR gt_item2.
    MOVE gt_item TO gt_item2.
    CLEAR gt_item.
    SORT gt_item2 BY zfhsx.
    LOOP AT gt_item2 INTO gs_item WHERE zfhsx <> ''.
      APPEND gs_item TO gt_item.
      CLEAR gs_item.
    ENDLOOP.
    DELETE gt_item2 WHERE zfhsx <> ''.
    SORT gt_item2 BY posnr.
    LOOP AT gt_item2 INTO gs_item.
      APPEND gs_item TO gt_item.
      CLEAR gs_item.
    ENDLOOP.
  ENDIF.
*************************END***********************************
  LOOP AT gt_alv_hd ASSIGNING FIELD-SYMBOL(<fs_head>) WHERE sel EQ 'X'.
    CLEAR ls_input.
    MOVE-CORRESPONDING <fs_head> TO ls_input-header.
    LOOP AT gt_item INTO DATA(ls_item) WHERE vbeln EQ <fs_head>-vbeln.
      IF ls_item-lgmng LE 0."不发送数量0行
        APPEND ls_item TO lt_zero.
        CONTINUE.
      ENDIF.
***************Seashell Huang 20200115 判断是否有批次，如果存在批次为空的情况，整单报错并且不下传WMS。
*      CLEAR lt_lips.
*      SELECT
*        *
*      INTO CORRESPONDING FIELDS OF TABLE lt_lips
*      FROM lips
*      WHERE lips~xchpf = 'X'
*        AND lips~vbeln = <fs_head>-vbeln
*        AND lips~posnr = ls_item-posnr
*        AND lips~charg = ''.
*      IF lt_lips IS NOT INITIAL.
*        CLEAR ls_input.
*        EXIT.
*      ELSE.

      CLEAR lt_lips.
      SELECT
      *
     INTO CORRESPONDING FIELDS OF TABLE lt_lips
      FROM lips
      WHERE lips~xchpf = 'X'
        AND lips~vbeln = <fs_head>-vbeln
        AND lips~posnr = ls_item-posnr.
      LOOP AT lt_lips INTO ls_lips.
        IF ls_lips-lfimg > 0.
          IF ls_lips-charg = '' OR ls_lips-charg IS INITIAL.
            MESSAGE '批次为空，不允许下传,行号：' && ls_item-posnr TYPE 'E' DISPLAY LIKE 'S'.
            EXIT.
          ENDIF.
        ENDIF.

        IF ls_lips-charg IS NOT INITIAL .
          IF ls_lips-lfimg IS INITIAL OR ls_lips-lfimg = 0.
            MESSAGE '批次不为空数量大于0时，不允许下传，行号' && ls_item-posnr TYPE 'E' DISPLAY LIKE 'S'.
            EXIT.
          ENDIF.
        ENDIF.



      ENDLOOP.


*********************END*********************************
      APPEND INITIAL LINE TO ls_input-items ASSIGNING FIELD-SYMBOL(<fs_item>).
      MOVE-CORRESPONDING ls_item TO <fs_item>.
      ls_input-header-wms_custid = <fs_item>-werks."分发wms系统标识
      READ TABLE lt_zero INTO ls_item WITH KEY vgbel = ls_item-vgbel vgpos = ls_item-vgpos.
      IF sy-subrc EQ 0.
        <fs_item>-zbzgg = ls_item-zbzgg.
        "<fs_item>-zfhsx = ls_item-zfhsx.
      ENDIF.

      IF <fs_item>-charg IS NOT INITIAL.
        l_key(40) = <fs_item>-matnr.
        l_key+40(4) = <fs_item>-werks.
        l_key+44(10) = <fs_item>-charg.
        DATA(lt_value) = zcl_bc_public=>get_class_values( key = l_key classnum = 'ZBATCH_SIRIO' classtype = '022' objtable = 'MCHA' ).
        IF lt_value[] IS INITIAL.
          lt_value = zcl_bc_public=>get_class_values( key = l_key classnum = 'BATCH_CP' classtype = '022' objtable = 'MCHA' ).
        ENDIF.
        LOOP AT lt_value INTO DATA(ls_value) WHERE value_char NE space.
          CASE ls_value-charact.
            WHEN 'ZLICHA' OR 'Z_BATCH_DYPC'.
              <fs_item>-dypc = ls_value-value_char.
            WHEN 'ZHSDAT' OR 'Z_BATCH_SCRQ'.
              <fs_item>-scrq = ls_value-value_char.
            WHEN 'ZVFDAT' OR 'Z_BATCH_YXQZ'.
              <fs_item>-yxqz = ls_value-value_char.
          ENDCASE.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'Z_SD_WMS_RFC_001'
      EXPORTING
        i_input  = ls_input
      IMPORTING
        e_output = ls_output.

    <fs_head>-message = ls_output-message.
    IF ls_output-type EQ 'S'.
      UPDATE likp SET zwmszt = 'B' WHERE vbeln EQ <fs_head>-vbeln.
      COMMIT WORK AND WAIT.
      IF sy-subrc EQ 0.
        <fs_head>-light = gc_green.
        <fs_head>-zwmszt = 'B'.
        <fs_head>-zwmszt_l = gr_zwmszt->get_value_l( <fs_head>-zwmszt ).
      ENDIF.
    ELSE.
      <fs_head>-light = gc_red.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE '未选中有效数据！' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE '数据已发送！' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHANGE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_change_data .
  DATA: r_vbeln     TYPE RANGE OF likp-vbeln,
        l_answer(1).

  LOOP AT gt_alv_hd ASSIGNING FIELD-SYMBOL(<fs_head>) WHERE sel EQ 'X'.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_head>-vbeln ) TO r_vbeln.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE '未选中有效数据！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = '确定不传WMS吗？'
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

  DELETE gt_alv_hd WHERE vbeln IN r_vbeln.
  UPDATE likp SET zwmszt = 'F' WHERE vbeln IN r_vbeln.
  COMMIT WORK AND WAIT.
  IF sy-subrc EQ 0.
    MESSAGE '数据已更新！' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_read_text USING p_vbeln CHANGING c_text.
  DATA: ls_header TYPE thead,
        lt_lines  TYPE tline_tab,
        l_name    TYPE thead-tdname.

  CLEAR: c_text, ls_header, lt_lines[].
  l_name = p_vbeln.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      language = sy-langu
      id       = 'Z004'
      name     = l_name
      object   = 'VBBK'
    IMPORTING
      header   = ls_header
    TABLES
      lines    = lt_lines
    EXCEPTIONS
      id       = 1
      language = 2
      OTHERS   = 3.

  CHECK sy-subrc EQ 0.
  LOOP AT lt_lines INTO DATA(ls_lines).
    c_text = c_text && ls_lines-tdline.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_send_cancel
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_send_cancel .
  DATA: l_answer(1).

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = '确定取消下发WMS吗？'
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

  LOOP AT gt_alv_hd ASSIGNING FIELD-SYMBOL(<fs_head>) WHERE sel EQ 'X' AND zwmszt EQ 'B'.
    PERFORM frm_send_cancel_pi USING <fs_head>.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE '未选中有效数据！' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE '数据已发送！' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_send_cancel_pi
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_send_cancel_pi USING is_data TYPE ty_alv_hd.
  DATA: l_intid   TYPE ze_intf_id VALUE 'SD013',
        lr_proxy  TYPE REF TO zco_si_sap2wms_dn_cal_out, "代理类
        ls_output TYPE zmt_sap2wms_dn_cal,
        ls_input  TYPE zmt_sap2wms_dn_cal_ret.

  ls_output-mt_sap2wms_dn_cal-lfart = is_data-lfart.
  ls_output-mt_sap2wms_dn_cal-vbeln = zcl_bc_public=>conv_by_ddic( i_input = is_data-vbeln i_out = 'X' ).
*----------------------------------------------------------------------
*   日志记录-记录输入参数
*----------------------------------------------------------------------
  DATA(l_guid) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
  DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'I' is_data = ls_output ). "记录输入参数
  IF l_flag EQ 'A'."接口是否启用判断
    MESSAGE '接口未启用！' TYPE 'E'.
    EXIT.
  ENDIF.
*----------------------------------------------------------------------
*   业务逻辑处理
*----------------------------------------------------------------------
  TRY.
      CREATE OBJECT lr_proxy.

      lr_proxy->si_sap2wms_dn_cal_out( EXPORTING output = ls_output
                                       IMPORTING input = ls_input )."调用pi接口

      IF ls_input-mt_sap2wms_dn_cal_ret-ret-ret_flag EQ '1'.
        UPDATE likp SET zwmszt = 'C' WHERE vbeln EQ is_data-vbeln.
        COMMIT WORK AND WAIT.
        IF sy-subrc EQ 0.
          is_data-light = gc_green.
          is_data-zwmszt = 'C'.
          is_data-zwmszt_l = gr_zwmszt->get_value_l( is_data-zwmszt ).
        ENDIF.
      ELSE.
        is_data-light = gc_red.
        is_data-message = ls_input-mt_sap2wms_dn_cal_ret-ret-ret_desc.
      ENDIF.
    CATCH cx_root INTO DATA(lr_root).
      MESSAGE lr_root->get_text(  ) TYPE 'I'.
      EXIT.
  ENDTRY.
*----------------------------------------------------------------------
*   日志记录-记录返回参数
*----------------------------------------------------------------------
  zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'E' is_data = ls_input ). "记录输出参数

ENDFORM.