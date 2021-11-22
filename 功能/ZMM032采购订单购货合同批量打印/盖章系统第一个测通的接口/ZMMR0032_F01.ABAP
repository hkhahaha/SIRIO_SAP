*&---------------------------------------------------------------------*
*&  包含               ZMMR0032_F01
*&---------------------------------------------------------------------*

*  PERFORM FRM_SHOW_DATE.

*  INCLUDE ZMMR0032_FRM.
*
*  INCLUDE ZMMR0032_PRT.

*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
DATA flag(1).


FORM frm_get_data.
  CASE 'X'.
    WHEN r_01.


      SELECT
  e~ebeln "凭证编号
  e~lifnr "供应商
  l~name1 "供应商名称
  e~ekgrp "采购组
  e~bedat "凭证日期
  e~ernam "制单人
  a~ebelp "行项目
  a~matnr
  e~ekorg
  INTO CORRESPONDING FIELDS OF TABLE lt_ebeln
  FROM ekko AS e
  LEFT JOIN lfa1 AS l ON e~lifnr = l~lifnr
  INNER JOIN ekpo AS a
  ON a~ebeln = e~ebeln
  "LEFT JOIN EKPO as t2 on ( t1~EBELN = t2~EBELN )
  WHERE e~ebeln IN s_ebeln
    AND e~lifnr IN s_lifnr
    AND a~werks = p_werks
    AND e~ekgrp IN s_ekgrp
    AND e~bsart IN s_bsart
    AND e~bedat IN s_bedat
    AND e~ernam IN s_ernam
  AND a~loekz = ''
    AND ( e~frgke = ' ' OR e~frgke = 'R' OR e~frgke = 'Z').


      IF lt_ebeln[] IS INITIAL .
        MESSAGE '没有符合输入条件的数据，请检查！'  TYPE 'E' .
        STOP.
      ELSE.
        SELECT
          *
        FROM makt
        INTO TABLE @DATA(lt_makt)
        FOR ALL ENTRIES IN @lt_ebeln
        WHERE matnr = @lt_ebeln-matnr
          AND spras = '1'.
        LOOP AT lt_ebeln INTO DATA(ls_ebeln).
          READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_ebeln-matnr.
          IF sy-subrc = 0.
            ls_ebeln-maktx = ls_makt-maktx.
          ENDIF.
          MODIFY lt_ebeln FROM ls_ebeln.
          CLEAR ls_ebeln.
        ENDLOOP.
      ENDIF.


    WHEN r_02.
      SELECT
      e~ebeln "凭证编号
      e~lifnr "供应商
      l~name1 "供应商名称
      e~ekgrp "采购组
      e~bedat "凭证日期
      e~ernam "制单人
      a~ebelp "行项目
      a~matnr
      e~ekorg
      INTO CORRESPONDING FIELDS OF TABLE lt_ebeln2
      FROM ekko AS e
      LEFT JOIN lfa1 AS l ON e~lifnr = l~lifnr
      INNER JOIN ekpo AS a
      ON a~ebeln = e~ebeln
      "LEFT JOIN EKPO as t2 on ( t1~EBELN = t2~EBELN )
      WHERE e~ebeln IN s_ebeln
        AND e~lifnr IN s_lifnr
        AND a~werks = p_werks
        AND e~ekgrp IN s_ekgrp
        AND e~bsart IN s_bsart
        AND e~bedat IN s_bedat
        AND e~ernam IN s_ernam
        AND a~loekz = ''
        AND ( e~frgke = ' ' OR e~frgke = 'R' OR e~frgke = 'Z').


      IF lt_ebeln2[] IS INITIAL .
        MESSAGE '没有符合输入条件的数据，请检查！'  TYPE 'E' .
        STOP.
      ELSE.
        SELECT
          *
        FROM makt
        INTO TABLE @lt_makt
        FOR ALL ENTRIES IN @lt_ebeln2
        WHERE matnr = @lt_ebeln2-matnr
          AND spras = '1'.
        LOOP AT lt_ebeln2 INTO ls_ebeln2.
          READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_ebeln2-matnr.
          IF sy-subrc = 0.
            ls_ebeln2-maktx = ls_makt-maktx.
          ENDIF.
          ls_ebeln2-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_ebeln2-matnr i_out = 'X' ).
          ls_ebeln2-lifnr = zcl_bc_public=>conv_by_ddic( i_input = ls_ebeln2-lifnr i_out = 'X' ).
          MODIFY lt_ebeln2 FROM ls_ebeln2.
          CLEAR ls_ebeln2.
        ENDLOOP.
      ENDIF.



    WHEN r_03.
      SELECT
        *
      FROM zmmt0032
      INTO TABLE lt_zmmt0032
      ORDER BY aedat last_changed_time DESCENDING.

    WHEN OTHERS.
  ENDCASE.


ENDFORM .

*&---------------------------------------------------------------------*
*&      2.Form  layout_build 设置alv显示格式和属性
*&---------------------------------------------------------------------*
FORM layout_build .
  it_layout-info_fieldname           = 'COLOR'.  "带有简单行彩色代码的字段名称
ENDFORM.                    "layout_build

*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_DATE
*&---------------------------------------------------------------------*
*       text alv 展示列表
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_display_data .
  PERFORM frm_get_field.
  PERFORM frm_show_alv.
ENDFORM. " frm_display_data


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


FORM frm_get_field .




  CASE 'X'.
    WHEN r_01.
      DEFINE get_field.

        ADD 1 TO wa_alv_field-col_pos.
        wa_alv_field-tabname = &1.
        wa_alv_field-fieldname = &2.
        wa_alv_field-seltext_m = &3.

        CASE &2.
          WHEN 'MEINS'.
            wa_alv_field-edit_mask = '==CUNIT'."单位转换

          WHEN 'SEL'.
            wa_alv_field-checkbox = 'X'.
            wa_alv_field-edit = 'X'.

          WHEN OTHERS.
        ENDCASE.
        APPEND wa_alv_field TO i_fieldcat_alv.
        CLEAR wa_alv_field.
      END-OF-DEFINITION.

      get_field 'lt_ebeln' 'SEL' '选择'.
      get_field 'lt_ebeln' 'EBELN' '采购凭证'.
      get_field 'lt_ebeln' 'EBELP' '采购订单行项目'.
      get_field 'lt_ebeln' 'MATNR' '物料号'.
      get_field 'lt_ebeln' 'MAKTX' '物料描述'.
      get_field 'lt_ebeln' 'LIFNR' '供应商'.
      get_field 'lt_ebeln' 'NAME1' '供应商名称'.
      get_field 'lt_ebeln' 'EKGRP' '采购组'.
      get_field 'lt_ebeln' 'BEDAT' '凭证日期'.
      get_field 'lt_ebeln' 'ERNAM' '制单人'.


    WHEN r_02.
      DEFINE get_field.

        ADD 1 TO wa_alv_field-col_pos.
        wa_alv_field-tabname = &1.
        wa_alv_field-fieldname = &2.
        wa_alv_field-seltext_m = &3.
        APPEND wa_alv_field TO i_fieldcat_alv.
        CLEAR wa_alv_field.
      END-OF-DEFINITION.

*      get_field 'lt_ebeln' 'SEL' '选择'.
      get_field 'lt_ebeln2' 'LIGHT' '指示灯'.
      get_field 'lt_ebeln2' 'MSG' '消息提示'.
      get_field 'lt_ebeln2' 'EBELN' '采购凭证'.
      get_field 'lt_ebeln2' 'EBELP' '采购订单行项目'.
      get_field 'lt_ebeln2' 'MATNR' '物料号'.
      get_field 'lt_ebeln2' 'MAKTX' '物料描述'.
      get_field 'lt_ebeln2' 'LIFNR' '供应商'.
      get_field 'lt_ebeln2' 'NAME1' '供应商名称'.
      get_field 'lt_ebeln2' 'EKGRP' '采购组'.
      get_field 'lt_ebeln2' 'BEDAT' '凭证日期'.
      get_field 'lt_ebeln2' 'ERNAM' '制单人'.
      get_field 'lt_ebeln2' 'VERSION' '版本记录号'.
      get_field 'lt_ebeln2' 'SIGN' '合同状态'.
      it_layout-zebra = 'X'.
      it_layout-box_fieldname = 'SEL'.
    WHEN r_03.
      DEFINE get_field.

        ADD 1 TO wa_alv_field-col_pos.
        wa_alv_field-tabname = &1.
        wa_alv_field-fieldname = &2.
        wa_alv_field-seltext_m = &3.
        APPEND wa_alv_field TO i_fieldcat_alv.
        CLEAR wa_alv_field.
      END-OF-DEFINITION.

*      get_field 'lt_ebeln' 'SEL' '选择'.
      get_field 'lt_zmmt0032' 'VERSION' '版本记录号'.
      get_field 'lt_zmmt0032' 'BUKRS' '公司代码'.
      get_field 'lt_zmmt0032' 'EBELN' '采购凭证'.
      get_field 'lt_zmmt0032' 'USER_NO' '操作人员工号'.
      get_field 'lt_zmmt0032' 'ERNAM' '创建人'.
      get_field 'lt_zmmt0032' 'AEDAT' '创建日期'.
      get_field 'lt_zmmt0032' 'LAST_CHANGED_TIME' '最后更改时间'.
      get_field 'lt_zmmt0032' 'LIFNR' '供应商'.
      get_field 'lt_zmmt0032' 'FLAG' '合同唯一标识'.
      get_field 'lt_zmmt0032' 'FLAG_DOCU' '合同文档唯一标识'.
      get_field 'lt_zmmt0032' 'SIGN' '合同状态'.
      get_field 'lt_zmmt0032' 'NAME' '合同名称'.
      get_field 'lt_zmmt0032' 'APPROVE_ID' '签署人ID'.
      get_field 'lt_zmmt0032' 'APPROVE_USER_ID' '签署人用户id'.
      get_field 'lt_zmmt0032' 'APPROVE_NAME' '合同签署人姓名'.
      get_field 'lt_zmmt0032' 'APPROVE_SIGN' '合同签署人签署状态'.
      get_field 'lt_zmmt0032' 'APPROVE_LAST_TIME' '最后签署时间'.
      get_field 'lt_zmmt0032' 'BIN_FILE' 'BASE 64码值'.

      it_layout-zebra = 'X'.


    WHEN OTHERS.
  ENDCASE.
ENDFORM. " FRM_GET_FIELD

*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM frm_show_alv .

  CASE 'X'.
    WHEN r_01.
      DATA l_repid TYPE sy-repid.
      l_repid = sy-repid.
      it_layout-colwidth_optimize = 'X'. "列宽度自动根据内容优化
      "it_layout-box_fieldname = 'SEL'. "选择行

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program       = l_repid
          i_callback_pf_status_set = 'SET_PF_STATUS'
          i_save                   = 'U'
          i_callback_user_command  = 'FRM_UCOMM'
          is_layout                = it_layout
          it_fieldcat              = i_fieldcat_alv
          i_default                = 'X'
        TABLES
          t_outtab                 = lt_ebeln.
      IF sy-subrc <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  *         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ENDIF.
    WHEN r_02.
      l_repid = sy-repid.
      it_layout-colwidth_optimize = 'X'. "列宽度自动根据内容优化
      "it_layout-box_fieldname = 'SEL'. "选择行

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program       = l_repid
          i_callback_pf_status_set = 'SET_PF_STATUS'
          i_save                   = 'U'
          i_callback_user_command  = 'FRM_UCOMM'
          is_layout                = it_layout
          it_fieldcat              = i_fieldcat_alv
          i_default                = 'X'
        TABLES
          t_outtab                 = lt_ebeln2.
      IF sy-subrc <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  *         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ENDIF.
    WHEN r_03.
      l_repid = sy-repid.
      it_layout-colwidth_optimize = 'X'. "列宽度自动根据内容优化
      "it_layout-box_fieldname = 'SEL'. "选择行

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program       = l_repid
          i_callback_pf_status_set = 'SET_PF_STATUS'
          i_save                   = 'U'
*         i_callback_user_command  = 'FRM_UCOMM'
          is_layout                = it_layout
          it_fieldcat              = i_fieldcat_alv
          i_default                = 'X'
        TABLES
          t_outtab                 = lt_zmmt0032.
      IF sy-subrc <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  *         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.


ENDFORM. " FRM_SHOW_ALV



*&--------------------------------------------------------------------*
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  DATA:wa_extab TYPE LINE OF slis_t_extab.
  CLEAR:rt_extab,rt_extab[],wa_extab.

*  IF p_dyfs8 IS INITIAL.
*    wa_extab-fcode = 'ZPRINT2'.
*    APPEND wa_extab TO rt_extab.
*  ENDIF.


  CASE 'X'.
    WHEN r_01.
      SET PF-STATUS 'ZPPR0032' EXCLUDING rt_extab.
    WHEN r_02.
      SET PF-STATUS 'ZMMR0032B' EXCLUDING rt_extab.
    WHEN r_03.
      SET PF-STATUS 'ZMMR0032C' EXCLUDING rt_extab.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "set_pf_status
*&---------------------------------------------------------------------*
*&      Form  frm_ucomm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM frm_ucomm USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  CASE 'X'.
    WHEN r_01.
      DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
      DATA: l_valid TYPE c.
*  4此处代码将用户输入读入内表（这一步不可少）
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lr_grid.
      CALL METHOD lr_grid->check_changed_data.

      rs_selfield-refresh = 'X'.
      CLEAR:zucomm.
      zucomm = sy-ucomm.

      CASE sy-ucomm.
        WHEN 'SAL_ALL'.
          LOOP AT lt_ebeln ASSIGNING FIELD-SYMBOL(<fs_ebeln>).
            <fs_ebeln>-sel = 'X'.
          ENDLOOP.
        WHEN 'DES_ALL'.
          LOOP AT lt_ebeln ASSIGNING <fs_ebeln>.
            <fs_ebeln>-sel = ''.
          ENDLOOP.
        WHEN 'PRINT'.
          PERFORM frm_print USING 'A'.
      ENDCASE.
    WHEN r_02.
      IF sy-ucomm = 'GZ'.
        MESSAGE 'HHHHH' TYPE 'S'.
        PERFORM send_message .
      ENDIF.
    WHEN OTHERS.
  ENDCASE.



ENDFORM.



FORM frm_print_data TABLES gt_detail STRUCTURE zmms_zmmr0017_detail
        USING u_gs_head LIKE zmms_zmmr0017_head
        u_form_name TYPE tdsfname
        s_ebeln .
*        p_znum TYPE i.
  DATA: fm_name TYPE rs38l_fnam.
  DATA ls_output_info TYPE ssfcrescl.
  DATA ls_output_option TYPE ssfcompop.
  DATA ls_control TYPE ssfctrlop.
*  DATA p_znum TYPE i.


  "获取SMARTFOMRS函数
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = u_form_name "Smart Form名称
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

*  CALL FUNCTION 'SSF_OPEN'
*    EXCEPTIONS
*      formatting_error = 1
*      internal_error   = 2
*      send_error       = 3
*      user_canceled    = 4
*      OTHERS           = 5.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*  ls_control-no_open = 'X'.
*  ls_control-no_close = 'X'.
*  ls_output_option-tdnoprint = 'X'.
*  CALL FUNCTION fm_name
*    EXPORTING
**     ARCHIVE_INDEX      =
**     ARCHIVE_INDEX_TAB  =
**     ARCHIVE_PARAMETERS =
*      control_parameters = ls_control
**     MAIL_APPL_OBJ      =
**     MAIL_RECIPIENT     =
**     MAIL_SENDER        =
*      output_options     = ls_output_option
*      user_settings      = 'X'
*      gs_head            = u_gs_head
*      gs_line            = gs_line
**   IMPORTING
**     DOCUMENT_OUTPUT_INFO       =
*      job_output_info    = ls_output_info
**     JOB_OUTPUT_OPTIONS =
*    TABLES
*      gt_detail          = t_gt_detail
*    EXCEPTIONS
*      formatting_error   = 1
*      internal_error     = 2
*      send_error         = 3
*      user_canceled      = 4
*      OTHERS             = 5.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  MESSAGE '打印次数' && p_znum TYPE 'S'.
*
*  CALL FUNCTION 'SSF_CLOSE'
*    IMPORTING
*      job_output_info  = lw_ssfcrescl
*    EXCEPTIONS
*      formatting_error = 1
*      internal_error   = 2
*      send_error       = 3
*      OTHERS           = 4.
*
*  CHECK lw_ssfcrescl-outputdone = 'X'.
*
*  SELECT ztype, aufnr, znum
*  INTO TABLE @DATA(lt_aufnr)
*  FROM zppt0004
*  WHERE aufnr = @gs_head-aufnr.
*
*  IF sy-subrc <>  0.
*    APPEND  VALUE #( ztype ='PROD' aufnr = gs_head-aufnr znum = 0 ) TO lt_aufnr.
*    INSERT zppt0004 FROM TABLE lt_aufnr.
*
*  ENDIF.
*
*  IF lw_ssfcrescl-outputdone = 'X'.
*    lt_aufnr[ 1 ]-znum =   lt_aufnr[ 1 ]-znum + 1.
*
*    UPDATE zppt0004 FROM TABLE lt_aufnr.
*  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PRINT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_print USING flag.
  DATA: fm_name  TYPE rs38l_fnam.
  DATA: it_ebeln  TYPE STANDARD TABLE OF ls_ebeln,
        iw_ebeln  LIKE LINE OF it_ebeln,
        lt_t007v  TYPE STANDARD TABLE OF t007v,
        ls_t007v  TYPE t007v,
        lt_ekpo   TYPE STANDARD TABLE OF ekpo,
        ls_ekpo   TYPE ekpo,
        lv_amount TYPE brtwr,
        lv_ltext  TYPE tdline.

  CLEAR: it_ebeln, iw_ebeln.
  IF lt_ebeln IS NOT INITIAL.
    LOOP AT lt_ebeln INTO DATA(lw_ebeln) WHERE sel = 'X'.
      iw_ebeln = lw_ebeln.
      APPEND iw_ebeln TO it_ebeln.
      CLEAR: iw_ebeln.
    ENDLOOP.
  ENDIF.
  IF lt_ebeln2 IS NOT INITIAL.
    LOOP AT lt_ebeln2 INTO DATA(iw_ebeln2) WHERE sel = 'X'.
      MOVE-CORRESPONDING iw_ebeln2 TO iw_ebeln.
      APPEND iw_ebeln TO it_ebeln.
      CLEAR: iw_ebeln.
    ENDLOOP.
  ENDIF.

  IF sy-subrc = 0.
    " 订单和供应商信息
    SELECT a~ebeln, a~frgke, a~bedat, a~bukrs, a~ekorg, a~waers, a~zterm,a~bsart,
           b~lifnr, b~name1, b~telfx, b~stras, b~adrnr
      ,a~knumv "ZZD 20200225
      FROM ekko AS a
      INNER JOIN lfa1 AS b
      ON b~lifnr = a~lifnr
      INTO TABLE @DATA(lt_ekko)
      FOR ALL ENTRIES IN @it_ebeln
      WHERE a~ebeln = @it_ebeln-ebeln.
    IF sy-subrc = 0.
      " 电话
      SELECT addrnumber, persnumber, date_from, consnumber, tel_number
        FROM adr2
        INTO TABLE @DATA(lt_adr2)
        FOR ALL ENTRIES IN @lt_ekko
        WHERE addrnumber = @lt_ekko-adrnr
          AND consnumber = 1.

      " 供应商 联系人
      SELECT lifnr, ekorg, verkf, zterm
        FROM lfm1
        INTO TABLE @DATA(lt_lfm1)
        FOR ALL ENTRIES IN @lt_ekko
        WHERE lifnr = @lt_ekko-lifnr
          AND ekorg = @lt_ekko-ekorg.
      IF sy-subrc = 0.
        " 结算方式
        SELECT spras, zterm, ztagg, text1
          FROM t052u
          INTO TABLE @DATA(lt_t052u)
          FOR ALL ENTRIES IN @lt_lfm1
          WHERE zterm = @lt_lfm1-zterm
            AND spras = 1.
      ENDIF.

      " 币别
      SELECT spras, waers, ltext, ktext
        FROM tcurt
        INTO TABLE @DATA(lt_tcurt)
        FOR ALL ENTRIES IN @lt_ekko
        WHERE waers = @lt_ekko-waers
          AND spras = 1.

      " 买方公司名称
      SELECT bukrs, butxt, adrnr
        FROM t001
        INTO TABLE @DATA(lt_t001)
        FOR ALL ENTRIES IN @lt_ekko
        WHERE bukrs = @lt_ekko-bukrs.
      IF sy-subrc = 0.
        " 公司地址&传真&邮编
        SELECT addrnumber, date_from, nation, street, fax_number, post_code1
          FROM adrc
          INTO TABLE @DATA(lt_adrc)
          FOR ALL ENTRIES IN @lt_t001
          WHERE addrnumber = @lt_t001-adrnr.
      ENDIF.

      " 卖方银行 基本户 银行账号
      SELECT f~acc_id, f~revision, f~bukrs, f~acc_num, b~banka
        FROM fclm_bam_amd AS f
        INNER JOIN bnka AS b
        ON b~bankl = f~bankl
        INTO TABLE @DATA(lt_fclm)
        FOR ALL ENTRIES IN @lt_ekko
        WHERE f~bukrs =  @lt_ekko-bukrs
          AND f~acc_type_id ='01'
          AND f~status ='02'.

      " 卖方 税号
      SELECT *
        FROM adrcts2
        INTO TABLE @DATA(lt_adrcts2).

      " 文件编号
      SELECT *
        FROM zqmt007
        INTO TABLE @DATA(lt_zqmt007)
        FOR ALL ENTRIES IN @lt_ekko
        WHERE werks = @lt_ekko-bukrs
          AND zbddh ='ZMM017'.

      " 供应商开户银行，银行账号
      SELECT l~lifnr, l~banks, l~bankl, l~bankn,
             b~banka
        FROM lfbk AS l
        INNER JOIN bnka AS b
        ON b~bankl = l~bankl
        INTO TABLE @DATA(lt_lfbk)
        FOR ALL ENTRIES IN @lt_ekko
        WHERE l~lifnr = @lt_ekko-lifnr
          AND l~bvtyp = '0001'.

      " 供应商税号
      SELECT *
        FROM dfkkbptaxnum
        INTO TABLE @DATA(lt_dfkkbptaxnum)
        FOR ALL ENTRIES IN @lt_ekko
        WHERE partner = @lt_ekko-lifnr
          AND taxtype = 'CN0'.

      " 付款条件
      SELECT *
        FROM t052u
        INTO TABLE @DATA(lt_t052)
        FOR ALL ENTRIES IN @lt_ekko
        WHERE zterm = @lt_ekko-zterm
          AND spras = 1.

      CLEAR: gt_head, gs_head.
      LOOP AT lt_ekko INTO DATA(ls_ekko).
        gs_head-ebeln = ls_ekko-ebeln.
        gs_head-lifnr_n = ls_ekko-name1.
        gs_head-bukrs = ls_ekko-bukrs.
        gs_head-bedat = ls_ekko-bedat.
        gs_head-frgke = ls_ekko-frgke.
        gs_head-stras = ls_ekko-stras.
        gs_head-telfx = ls_ekko-telfx.
        gs_head-bsart = ls_ekko-bsart."modify by wwj
        CASE gs_head-bukrs.
          WHEN '8100'.
            gs_head-adrnr_t ='一厂：汕头市泰山路83号；二厂：汕头市黄山路珠业南街11号'.
          WHEN '8200'.
            gs_head-adrnr_t ='马鞍山地址：安徽省马鞍山经济技术开发区红旗南路1980号'.
        ENDCASE.
        READ TABLE lt_t052 INTO DATA(ls_t052) WITH KEY zterm = ls_ekko-zterm.
        IF sy-subrc = 0.
          gs_head-text1 = ls_t052-text1.
        ENDIF.
        READ TABLE lt_fclm INTO DATA(ls_fclm) WITH KEY bukrs = ls_ekko-bukrs.
        IF sy-subrc = 0.
          gs_head-bankn = ls_fclm-acc_num.
          gs_head-banka = ls_fclm-banka.
        ENDIF.
        READ TABLE lt_adrcts2 INTO DATA(ls_adrcts2) WITH KEY objkey+3(4) = ls_ekko-bukrs.
        IF sy-subrc = 0.
          gs_head-taxnum = ls_adrcts2-remark.
        ENDIF.
        READ TABLE lt_zqmt007 INTO DATA(ls_zqmt007) WITH KEY werks = ls_ekko-bukrs.
        IF sy-subrc = 0.
          gs_head-ztxbh = ls_zqmt007-ztxbh.
        ENDIF.
        READ TABLE lt_lfbk INTO DATA(ls_lkbk) WITH KEY lifnr = ls_ekko-lifnr.
        IF sy-subrc = 0.
          gs_head-banka_n = ls_lkbk-banka.
          gs_head-bankn_n = ls_lkbk-bankn.
        ENDIF.
        READ TABLE lt_dfkkbptaxnum INTO DATA(ls_dfkkbptaxnum) WITH KEY partner = ls_ekko-lifnr.
        IF sy-subrc = 0.
          gs_head-taxnum_n = ls_dfkkbptaxnum-taxnum.
        ENDIF.
        READ TABLE lt_adr2 INTO DATA(ls_adr2) WITH KEY addrnumber = ls_ekko-adrnr.
        IF sy-subrc = 0.
          gs_head-telf1 = ls_adr2-tel_number.
        ENDIF.
        READ TABLE lt_lfm1 INTO DATA(ls_lfm1) WITH KEY lifnr = ls_ekko-lifnr ekorg = ls_ekko-ekorg.
        IF sy-subrc = 0.
          gs_head-verkf = ls_lfm1-verkf.
          READ TABLE lt_t052u INTO DATA(ls_t052u) WITH KEY zterm = ls_lfm1-zterm.
          IF sy-subrc = 0.
            gs_head-zterm_t = ls_t052u-text1.
          ENDIF.
        ENDIF.
        READ TABLE lt_tcurt INTO DATA(ls_tcurt) WITH KEY waers = ls_ekko-waers.
        IF sy-subrc = 0.
          gs_head-waers_k = ls_tcurt-ktext.
          CASE ls_tcurt-ktext.
            WHEN '人民币'.
              gs_head-zcurr ='￥'.
            WHEN '美元'.
              gs_head-zcurr = '$'.
          ENDCASE.
        ENDIF.
        READ TABLE lt_t001 INTO DATA(ls_t001) WITH KEY bukrs = ls_ekko-bukrs.
        IF sy-subrc = 0.
          gs_head-butxt = ls_t001-butxt.
          READ TABLE lt_adrc INTO DATA(ls_adrc) WITH KEY addrnumber = ls_t001-adrnr.
          IF sy-subrc = 0.
            gs_head-street = ls_adrc-street.
            gs_head-fax_number = ls_adrc-fax_number.
            gs_head-post_code1 = ls_adrc-post_code1.
*            IF GS_HEAD-BUKRS = '8100'.
*              CONCATENATE '一厂：' GS_HEAD-STREET INTO GS_HEAD-STREET.
*            ENDIF.
          ENDIF.
        ENDIF.
        APPEND gs_head TO gt_head.
        CLEAR: gs_head.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF gt_head IS NOT INITIAL.
    CLEAR: lt_ekpo, ls_ekpo.
    SELECT *
      INTO TABLE @lt_ekpo
      FROM ekpo
      FOR ALL ENTRIES IN @gt_head
      WHERE ebeln =  @gt_head-ebeln
      AND loekz = ''. "ZZD 20200224
    IF sy-subrc = 0.
      DATA: lr_matnr TYPE RANGE OF matnr,
            lw_matnr LIKE LINE OF lr_matnr.
      CLEAR: lr_matnr, lw_matnr.
      LOOP AT lt_ekpo INTO ls_ekpo.
        lw_matnr-sign = 'I'.
        lw_matnr-option = 'EQ'.
        lw_matnr-low = ls_ekpo-matnr.
        APPEND lw_matnr TO lr_matnr.
        CLEAR: lw_matnr.
        lw_matnr-sign = 'I'.
        lw_matnr-option = 'EQ'.
        lw_matnr-low = ls_ekpo-ematn.
        APPEND lw_matnr TO lr_matnr.
        CLEAR: lw_matnr.
      ENDLOOP.
      SORT lr_matnr BY low.
      DELETE ADJACENT DUPLICATES FROM lr_matnr COMPARING low.
      SELECT m~matnr, m~maktx, a~matkl, a~groes
        INTO TABLE @DATA(lt_matnr)
        FROM makt AS m
        LEFT JOIN mara AS a
        ON m~matnr = a~matnr
        WHERE m~matnr IN @lr_matnr
          AND m~spras = 1.

      SELECT t~ebeln, t~ebelp, t~etenr, t~eindt
        INTO TABLE @DATA(lt_eket)
        FROM eket AS t
        INNER JOIN ekpo AS o
        ON o~ebeln = t~ebeln AND o~ebelp = t~ebelp
        FOR ALL ENTRIES IN @lt_ekpo
        WHERE t~ebeln = @lt_ekpo-ebeln
        AND  o~loekz = ''  "20200221 ZZD
          AND t~ebelp = @lt_ekpo-ebelp.

*      SELECT K~KNUMH, K~KOPOS, K~MWSK1, K~KBETR,
*             O~EBELN, O~EBELP, O~MWSKZ
*        INTO TABLE @DATA(LT_KONP)
*        FROM KONP AS K
*        INNER JOIN EKPO AS O
*        ON O~MWSKZ = K~MWSK1
*        FOR ALL ENTRIES IN @LT_EKPO
*        WHERE O~EBELN = @LT_EKPO-EBELN
*          AND O~EBELP = @LT_EKPO-EBELP
*          AND K~KAPPL = 'TX'.
      CLEAR: lt_t007v.
      SELECT *
        FROM t007v
        INTO TABLE lt_t007v
        WHERE aland = 'CN'
          AND datam <> '00000000'.
      IF sy-subrc = 0.
        SORT lt_t007v BY datum uzeit DESCENDING.
      ENDIF.

      SELECT e~infnr, e~ekorg, e~esokz, e~werks,
             e~netpr, e~peinh, e~mwskz, a~matnr
        INTO TABLE @DATA(lt_eine)
        FROM eine AS e
        INNER JOIN eina AS a
        ON e~infnr = a~infnr
        FOR ALL ENTRIES IN @lt_ekpo
        WHERE e~esokz = '2'
          AND e~infnr = @lt_ekpo-infnr .

      SELECT a~kappl, a~kschl, a~lifnr, a~matnr, a~ekorg,
             a~werks, a~esokz, a~datbi, a~datab,
             b~knumh, b~kopos, b~kbetr, b~kpein
        FROM a017 AS a
        INNER JOIN konp AS b
        ON b~knumh = a~knumh
        INTO TABLE @DATA(lt_a017)
        FOR ALL ENTRIES IN @lt_ekpo
        WHERE a~kappl = 'M'
          AND a~esokz = '2'
          AND a~matnr = @lt_ekpo-matnr
          AND a~werks = @lt_ekpo-werks.

      " 生产厂家（制造商），Seashell 2021-10-25新增ZMFRNR生厂商字段，如果取不到的话就取自定义字段的生产商
      SELECT e~ebeln, e~ebelp, e~matnr, e~ematn,
             b~partner, b~name_org1,e~zmfrnr
        FROM ekpo AS e
        INNER JOIN mara AS m
        ON e~matnr = m~matnr OR e~ematn = m~matnr
        INNER JOIN but000 AS b
        ON b~partner = m~mfrnr "OR B~PARTNER = E~ZMFRNR  "waymon 2020.9.22 添加OR b~partner = e~zmfrnr.
        INTO TABLE @DATA(lt_but000)
        FOR ALL ENTRIES IN @lt_ekpo
        WHERE e~ebeln =  @lt_ekpo-ebeln
        AND  e~loekz = '' "20200221 ZZD
          AND e~ebelp = @lt_ekpo-ebelp .

      SELECT
        partner,
        name_org1
      FROM but000
      FOR ALL ENTRIES IN @lt_ekpo
      WHERE partner = @lt_ekpo-zmfrnr
      INTO TABLE @DATA(lt_but000b).

***********************************************************ZZD 获取附加费 2020022

*将小计1的值赋回订单值
      LOOP AT lt_ekpo INTO ls_ekpo .

        ls_ekpo-brtwr = ls_ekpo-kzwi1 .
        MODIFY lt_ekpo FROM ls_ekpo .
        CLEAR ls_ekpo.
      ENDLOOP.

*获取批次内码
*ZGROES  规格型号/产品规格
*ZMATDET 材质明细


      TYPES: BEGIN OF ty_tx,"存入物料特性
               matnr TYPE mara-matnr, "物料
               objek TYPE ausp-objek, "对象
               atinn TYPE ausp-atinn, "内码
               zgg   TYPE ausp-atwrt, "规格
             END OF ty_tx.

      DATA: gt_cabn TYPE TABLE OF cabn,
            gs_cabn TYPE cabn,
            gt_wltx TYPE TABLE OF ty_tx,
            gs_wltx TYPE ty_tx,
            gt_ausp TYPE TABLE OF ausp,
            gs_ausp TYPE ausp,
            gt_mara TYPE TABLE OF mara,
            gs_mara TYPE mara.

*取特性内码
      SELECT
        atinn
        atnam
        INTO CORRESPONDING FIELDS OF TABLE gt_cabn
        FROM cabn
        WHERE atnam IN ('ZGROES','ZMATDET').
*取物料类型
      SELECT
        matnr
        mtart
        INTO CORRESPONDING FIELDS OF TABLE gt_mara
        FROM mara
        FOR ALL ENTRIES IN lt_ekpo
        WHERE matnr = lt_ekpo-matnr.

      LOOP AT lt_ekpo INTO ls_ekpo.
        gs_wltx-matnr = ls_ekpo-matnr.
        gs_wltx-objek = ls_ekpo-matnr.
        APPEND gs_wltx TO gt_wltx.
        CLEAR gs_wltx.
      ENDLOOP.

      LOOP AT gt_wltx INTO gs_wltx.
        READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_wltx-matnr.
        IF gs_mara-mtart = 'Z010'.
          READ TABLE gt_cabn INTO gs_cabn WITH KEY atnam = 'ZGROES'.
          gs_wltx-atinn = gs_cabn-atinn.
        ELSEIF gs_mara-mtart = 'Z020'.
          READ TABLE gt_cabn INTO gs_cabn WITH KEY atnam = 'ZMATDET'.
          gs_wltx-atinn = gs_cabn-atinn.
        ENDIF.


        CLEAR gs_cabn.
        MODIFY gt_wltx FROM gs_wltx.
        CLEAR gs_wltx.
      ENDLOOP.

      SELECT
        objek
        atinn
        atwrt
        INTO CORRESPONDING FIELDS OF TABLE gt_ausp
        FROM ausp
        FOR ALL ENTRIES IN gt_wltx
        WHERE objek = gt_wltx-objek
        AND atinn = gt_wltx-atinn.

      LOOP AT gt_wltx INTO gs_wltx.
        READ TABLE gt_ausp INTO gs_ausp WITH KEY objek = gs_wltx-objek
        atinn = gs_wltx-atinn.
        IF sy-subrc = 0.
          gs_wltx-zgg = gs_ausp-atwrt.
        ENDIF.
        MODIFY gt_wltx FROM gs_wltx.
        CLEAR gs_wltx.
      ENDLOOP.


      DATA ls_matnr LIKE LINE OF lt_matnr.
      LOOP AT lt_matnr INTO ls_matnr.
        READ TABLE gt_wltx INTO gs_wltx WITH KEY matnr = ls_matnr-matnr.
        IF sy-subrc = 0.
          ls_matnr-groes = gs_wltx-zgg.
          MODIFY lt_matnr FROM ls_matnr.
          CLEAR ls_matnr.
        ENDIF.
      ENDLOOP.
***********************************************************ZZD 获取附加费 20200225



      LOOP AT lt_ekpo INTO ls_ekpo.
        " EKPO- PSTYP EQ 2 寄售
        " 含税单价 不含税单价  税率
        IF ls_ekpo-pstyp EQ  '2' .
          READ TABLE lt_eine INTO DATA(ls_eine) WITH KEY infnr = ls_ekpo-infnr.
          IF sy-subrc = 0.
            ls_ekpo-netpr = ls_eine-netpr.
            ls_ekpo-peinh = ls_eine-peinh.
          ENDIF.
        ENDIF.
        MOVE-CORRESPONDING ls_ekpo TO gs_detail.
        READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = ls_ekpo-matnr.
        IF sy-subrc = 0.
          IF ls_matnr-matkl+0(1) = '1'.
            gs_detail-matnr = ls_ekpo-ematn.
            "Seashell Huang 2020.6.4 添加制造商的获取条件partner = ls_ekpo-mfrnr
            "Seashell Huang 2021-10-25 添加制造商为空的时候获取自定义字段的判断
            IF ls_ekpo-mfrnr IS INITIAL.
              ls_ekpo-mfrnr = ls_ekpo-zmfrnr.
            ENDIF.
            "END
            READ TABLE lt_but000 INTO DATA(ls_but000) WITH KEY ebeln = ls_ekpo-ebeln ebelp = ls_ekpo-ebelp ematn = ls_ekpo-ematn partner = ls_ekpo-mfrnr.
            IF sy-subrc = 0.
              gs_detail-name_org1 = ls_but000-name_org1.
              "waymon 2020.9.22 添加制造商的获取条件partner = ls_ekpo-Zmfrnr TOP.
            ELSE.
              READ TABLE lt_but000 INTO ls_but000 WITH KEY ebeln = ls_ekpo-ebeln ebelp = ls_ekpo-ebelp ematn = ls_ekpo-ematn. "PARTNER = LS_EKPO-ZMFRNR.
              IF sy-subrc = 0.
                gs_detail-name_org1 = ls_but000-name_org1.
              ENDIF.
              "waymon 2020.9.22 添加制造商的获取条件partner = ls_ekpo-Zmfrnr END.
            ENDIF.
            "Seashell 如果生厂商（制造商）没哟找到的话取自定义字段ZMFRNR  2021.10.26
            IF gs_detail-name_org1 IS INITIAL.
              READ TABLE lt_but000b INTO DATA(ls_but000b) WITH KEY partner = ls_ekpo-zmfrnr.
              IF sy-subrc = 0.
                gs_detail-name_org1 = ls_but000b-name_org1.
              ENDIF.
            ENDIF.
            "end

            CLEAR: ls_matnr.
*****************ZZD 20200227
*           READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = ls_ekpo-ematn.
            READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = ls_ekpo-matnr.
*****************ZZD 20200227
            IF sy-subrc = 0.
              gs_detail-maktx = ls_matnr-maktx.
              gs_detail-groes = ls_matnr-groes.
            ENDIF.
          ELSE.
            gs_detail-matnr = ls_ekpo-matnr.
            READ TABLE lt_but000 INTO ls_but000 WITH KEY ebeln = ls_ekpo-ebeln ebelp = ls_ekpo-ebelp matnr = ls_ekpo-matnr.
            IF sy-subrc = 0.
              gs_detail-name_org1 = ls_but000-name_org1.
            ENDIF.
            gs_detail-maktx = ls_matnr-maktx.
            gs_detail-groes = ls_matnr-groes.
          ENDIF.
        ENDIF.
        READ TABLE lt_eket INTO DATA(ls_eket) WITH KEY ebeln = ls_ekpo-ebeln ebelp = ls_ekpo-ebelp.
        IF sy-subrc = 0.
          gs_detail-eindt = ls_eket-eindt.
        ENDIF.
*        READ TABLE LT_KONP INTO DATA(LS_KONP) WITH KEY EBELN = LS_EKPO-EBELN EBELP = LS_EKPO-EBELP.
*        IF SY-SUBRC = 0.
*          GS_DETAIL-KBETR = LS_KONP-KBETR.
*        ENDIF.
        "寄售 含税金额  不含税金额
        IF ls_ekpo-pstyp EQ  '2'.
          CLEAR: ls_ekko.
          READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = ls_ekpo-ebeln.
          IF sy-subrc = 0.
            CLEAR: ls_eine.
            READ TABLE lt_eine INTO ls_eine
              WITH KEY infnr = ls_ekpo-infnr matnr = ls_ekpo-matnr werks = ls_ekpo-werks ekorg = ls_ekko-ekorg.
            IF sy-subrc = 0.
              CLEAR: ls_t007v.
              READ TABLE lt_t007v INTO ls_t007v WITH KEY mwskz = ls_eine-mwskz.
              IF sy-subrc = 0.
                gs_detail-kbetr = ls_t007v-kbetr.
              ENDIF.
            ENDIF.
            LOOP AT lt_a017 INTO DATA(ls_a017)
              WHERE matnr = ls_ekpo-matnr AND werks = ls_ekpo-werks AND ekorg = ls_ekko-ekorg
                AND lifnr = ls_ekko-lifnr AND datab <= ls_ekko-bedat AND datbi >= ls_ekko-bedat.
              " 含税单价
              gs_detail-bsprs = ls_a017-kbetr / ls_a017-kpein.
              " 不含税单价

              gs_detail-netpr = gs_detail-bsprs / ( 1 + gs_detail-kbetr / 1000 ) .
              gs_detail-netwr = gs_detail-netpr * ls_ekpo-menge.
              gs_detail-brtwr = gs_detail-bsprs * ls_ekpo-menge. "ZZD 20200309.
              EXIT.
            ENDLOOP.
          ENDIF.
        ELSE.
          CLEAR: ls_t007v.
          READ TABLE lt_t007v INTO ls_t007v WITH KEY mwskz = ls_ekpo-mwskz.
          IF sy-subrc = 0.
            gs_detail-kbetr = ls_t007v-kbetr.
          ENDIF.
*          " 含税单价
          gs_detail-bsprs = ls_ekpo-brtwr  / ls_ekpo-menge.
*          " 不含税单价
          gs_detail-netpr = gs_detail-bsprs / ( 1 + gs_detail-kbetr / 1000 ).




        ENDIF.
        " 送货地址
        CASE ls_ekpo-lgort+0(1).
          WHEN '1'.
            gs_detail-adrnr = '一厂'.
          WHEN '2'.
            gs_detail-adrnr = '二厂'.
        ENDCASE.
        " 采购订单行项目文本
        CLEAR: lv_ltext.
        PERFORM frm_read_txtl USING ls_ekpo-ebeln ls_ekpo-ebelp CHANGING lv_ltext.
        gs_detail-ltxts = lv_ltext.

        "UP BY LANJINGCUN 20201230 无物料号的处理-S857
*        READ TABLE GT_HEAD INTO GS_HEAD WITH KEY EBELN = LS_EKPO-EBELN.
*        IF SY-SUBRC = 0.
*          IF GS_HEAD-BSART = 'ZNF'.  "非库存物资订单类型，无物料编码 modify by wwj _20201030
*            GS_DETAIL-MAKTX = LS_EKPO-TXZ01 .
*          ENDIF.
*        ENDIF.
        IF gs_detail-maktx = '' .
          gs_detail-maktx = ls_ekpo-txz01 .
        ENDIF .
        "UP BY LANJINGCUN 20201230 无物料号的处理-E857

        APPEND gs_detail TO gt_detail.
        lv_amount = gs_detail-brtwr + lv_amount.
        CLEAR: gs_detail.
        AT END OF ebeln.
*          SUM.
          READ TABLE gt_head ASSIGNING FIELD-SYMBOL(<fs_head>) WITH KEY ebeln = ls_ekpo-ebeln.
          IF sy-subrc = 0.
            <fs_head>-amount = lv_amount.
          ENDIF.
          CLEAR: lv_amount.
        ENDAT.
      ENDLOOP.
      SORT gt_detail BY ebelp.
    ENDIF.
  ENDIF.
  DATA: lt_detail  TYPE STANDARD TABLE OF zmms_zmmr0017_detail,
        ls_detail  TYPE zmms_zmmr0017_detail,
        lw_output  TYPE ssfcompop,
        lw_control TYPE ssfctrlop,
        lv_lines   TYPE i,
        lv_div     TYPE i.
  IF gt_head IS NOT INITIAL OR gt_detail IS NOT INITIAL.
    " 获取SMARTFOMRS函数
    "ADD BY LANJINGCUN 添加维乐维单据类型-S888
    CLEAR gv_tdsfname .
    CASE p_werks .
      WHEN '8100' .
        gv_tdsfname = 'ZMM017_PC' .
      WHEN '8200' .
        gv_tdsfname = 'ZMM017_PC' .
      WHEN  OTHERS .
        gv_tdsfname = 'ZMM017_WL' .
    ENDCASE .
    "ADD BY LANJINGCUN 添加维乐维单据类型-E888

    CLEAR: fm_name, lw_output, lw_control.
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'ZMM017_PC'
      IMPORTING
        fm_name            = fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

*    LW_OUTPUT-TDNOPREV  = ' '.                      " 是否打印预览，X是不预览。
*    LW_OUTPUT-TDNOPRINT = ' '.                      " 没有打印预览的打印输出，设置X。
*    LW_OUTPUT-TDIMMED   = ' '.                      " 立即打印标志。
*    LW_OUTPUT-TDDEST    = 'LP01'.                   " 假脱机，输出设备。
*    LW_OUTPUT-TDNEWID = 'X'.
*    LW_OUTPUT-TDIMMED = 'X'.
*    LW_OUTPUT-TDDELETE = 'X'.
*    LW_OUTPUT-TDFINAL = 'X'.
*    LW_OUTPUT-TDIEXIT = 'X'.

    lw_control-preview       = 'X'.                 " 打印预览
    lw_control-no_open       = 'X'.                 " 新的假脱机请求
    lw_control-no_close      = 'X'.
    lw_control-no_dialog     = 'X'.                 " 必须设置
    IF flag = 'A'.
      CALL FUNCTION 'SSF_OPEN'
        EXPORTING
          output_options     = lw_output
          control_parameters = lw_control
          user_settings      = ' '
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

      LOOP AT gt_head INTO gs_head.
        CLEAR: lt_detail, ls_detail.
        LOOP AT gt_detail INTO gs_detail WHERE ebeln = gs_head-ebeln.
          MOVE-CORRESPONDING gs_detail TO ls_detail.
          APPEND ls_detail TO lt_detail.
          CLEAR: ls_detail.
        ENDLOOP.

        CLEAR: gs_line, lv_div, lv_lines.
        DESCRIBE TABLE lt_detail LINES gs_line.
        IF gs_line > 11.
          lv_div = gs_line DIV 12.
          lv_lines = ( lv_div + 1 ) * 12 - 1.
          DO lv_lines - gs_line TIMES.
            APPEND ls_detail TO lt_detail.
          ENDDO.
        ELSE.
          DO 11 - gs_line TIMES.
            APPEND ls_detail TO lt_detail.
          ENDDO.
        ENDIF.

        CALL FUNCTION fm_name
          EXPORTING
            gs_head            = gs_head
            gs_line            = gs_line
            control_parameters = lw_control
            output_options     = lw_output
          TABLES
            gt_detail          = lt_detail
          EXCEPTIONS
            formatting_error   = 1
            internal_error     = 2
            send_error         = 3
            user_canceled      = 4
            OTHERS             = 5.
      ENDLOOP.
      CALL FUNCTION 'SSF_CLOSE'
        EXCEPTIONS
          formatting_error = 1
          internal_error   = 2
          send_error       = 3
          OTHERS           = 4.

      CLEAR: gt_head, gs_head, gt_detail, gs_detail.
    ELSEIF flag = 'B'.
      DATA: gwa_result     TYPE ssfcrescl,
            gwa_output     TYPE ssfcompop,
            g_filename     TYPE string,
            g_bin_filesize TYPE i,
            git_lines      TYPE STANDARD TABLE OF tline,
            gwa_control    TYPE ssfctrlop,
            lv_pdf_xstring TYPE xstring.
      DATA: l_json   TYPE string,
            l_xstr   TYPE xstring,
            l_encode TYPE string.

*      gwa_control-no_open = 'X'.
      gwa_control-getotf    = abap_true. "取得OTF数据
      gwa_control-no_dialog = 'X'. "不显示对话框
      gwa_output-tddest   = 'LP01'.      "指定打印机
      LOOP AT gt_head INTO gs_head.
        CLEAR: lt_detail, ls_detail.
        LOOP AT gt_detail INTO gs_detail WHERE ebeln = gs_head-ebeln.
          MOVE-CORRESPONDING gs_detail TO ls_detail.
          APPEND ls_detail TO lt_detail.
          CLEAR: ls_detail.
        ENDLOOP.

        CLEAR: gs_line, lv_div, lv_lines.
        DESCRIBE TABLE lt_detail LINES gs_line.
        IF gs_line > 11.
          lv_div = gs_line DIV 12.
          lv_lines = ( lv_div + 1 ) * 12 - 1.
          DO lv_lines - gs_line TIMES.
            APPEND ls_detail TO lt_detail.
          ENDDO.
        ELSE.
          DO 11 - gs_line TIMES.
            APPEND ls_detail TO lt_detail.
          ENDDO.
        ENDIF.


        CALL FUNCTION fm_name
          EXPORTING
            gs_head            = gs_head
            gs_line            = gs_line
            control_parameters = gwa_control
            output_options     = gwa_output
          IMPORTING
*           DOCUMENT_OUTPUT_INFO       =
            job_output_info    = gwa_result
*           JOB_OUTPUT_OPTIONS =
          TABLES
            gt_detail          = lt_detail
          EXCEPTIONS
            formatting_error   = 1
            internal_error     = 2
            send_error         = 3
            user_canceled      = 4
            OTHERS             = 5.

        CALL FUNCTION 'CONVERT_OTF'
          EXPORTING
            format                = 'PDF'
          IMPORTING
            bin_filesize          = g_bin_filesize
            bin_file              = lv_pdf_xstring
          TABLES
            otf                   = gwa_result-otfdata[]
            lines                 = git_lines
          EXCEPTIONS
            err_max_linewidth     = 1
            err_format            = 2
            err_conv_not_possible = 3
            OTHERS                = 4.

        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = lv_pdf_xstring
          IMPORTING
            output = l_encode.
        "接下来先对接口除了content的字段赋值，然后转BASE64，获取对应的接口字段的签名，保持一致性

        DATA:ls_output1         TYPE zmt_sap2sign_send_file,
             ls_output1_content TYPE zdt_sap2sign_send_file_content,
             ls_input1          TYPE zmt_sap2sign_send_file_ret,
             ls_input1_data     TYPE zdt_sap2sign_send_file_ret_dat,
             lr_proxy       TYPE REF TO zco_si_sap2sign_send_new_out.

        DATA: ls_zmmt0032_detail TYPE zmmt0032_detail,
              lt_zmmt0032_detail LIKE TABLE OF ls_zmmt0032_detail.
        ls_output1_content-code = gs_head-ebeln.
        ls_output1_content-base64 = l_encode.
        ls_output1_content-name = gs_head-ebeln.
        ls_output1_content-sign_count = '2'.
        ls_output1_content-doc_name = gs_head-ebeln .
        ls_output1_content-creator = 'L77cz04tvotuh6'."sy-uname.
        ls_output1_content-enterprise_id = 'L7702u'."gs_head-bukrs.
        ls_output1_content-sync_url = 'http://baidu.com'.
        "根据上面的字段，获取对应的签名
        DATA: json TYPE string,
              data TYPE xstring,
              key  TYPE xstring.
        DATA: lv_str  TYPE string,
              lv_xstr TYPE xstring.
        key = cl_abap_hmac=>string_to_xstring( if_input = '7a8b8d6bf68c482f9a5854f371bedb9f' && 'contract003' ).
        json = /ui2/cl_json=>serialize( data = ls_output1_content compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-camel_case numc_as_string = abap_true ).
*        json = '{"base64":"","code":"4600000023","creator":"05025","docName":"4600000023","enterpriseId":"8200","name":"4600000023","signCount":"2","syncUrl":"4600000023"}'.
*        json = '{"base64":"","code":"4600000023","creator":"05025","docName":"4600000023","enterpriseId":"8200","name":"4600000023","signCount":"2","syncUrl":"http://baidu.com"}'.
        data = cl_abap_hmac=>string_to_xstring( if_input = json ).






        TRY.
            CALL METHOD cl_abap_hmac=>calculate_hmac_for_raw
              EXPORTING
*               if_algorithm     = 'SHA1'
                if_key           = key
                if_data          = data
*               if_length        = 0
              IMPORTING
*               ef_hmacstring    =
                ef_hmacxstring   = lv_xstr
                ef_hmacb64string = lv_str.
          CATCH cx_abap_message_digest .
        ENDTRY.
        MOVE-CORRESPONDING ls_output1_content TO ls_output1-mt_sap2sign_send_file.
        CONCATENATE 'HMAC-SHA1' lv_str INTO ls_output1-mt_sap2sign_send_file-content SEPARATED BY space.


        DATA: ls_output TYPE zmt_sap2sign_send_new,
              ls_input  TYPE zmt_sap2sign_send_ret_new.

        ls_output-mt_sap2sign_send_new-info = json.
        ls_output-mt_sap2sign_send_new-action = 'createByFile'.
        CONCATENATE 'HMAC-SHA1' lv_str INTO ls_output-mt_sap2sign_send_new-sign SEPARATED BY space.

        DATA: lo_server_context TYPE REF TO if_ws_server_context.
        DATA: g_message_protocol TYPE REF TO if_wsprotocol_message_id.
        DATA: g_message_id TYPE sxmsguid.
        DATA: g_lo_sys_exception TYPE REF TO cx_ai_system_fault.
        DATA: lv_intf_id(6) TYPE c,
              lv_err_info   TYPE bapi_msg.

        "逐步开始调用接口
        "第一步接口，SAP下发采购订单合同给印章系统
        "创建日志关联主键，类似于表头主键
        TRY .
            lo_server_context   = cl_proxy_access=>get_server_context( ).
            g_message_protocol ?= lo_server_context->get_protocol( if_wsprotocol=>message_id ).
            "获取Message ID
            g_message_id = g_message_protocol->get_message_id( ).

          CATCH cx_ai_system_fault INTO g_lo_sys_exception.
            EXIT.
        ENDTRY.
        "记录接口出参日志
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                  iv_intid = 'MM070A' iv_ptype = 'X'
                                  is_data = ls_output1 ).
        TRY.
*            CREATE OBJECT lr_proxy.
*            "*****TEST******************8
*            DATA:dynamic_t  TYPE sxms_msghdr30_dynamic_t.
*            DATA:dynamic_s  TYPE sxms_msghdr30_dynamic.
*            dynamic_s-param_name = 'HK'.
*            dynamic_s-param_namespace = 'S:HK'.
*            dynamic_s-param_value = 'HK'.
*            APPEND dynamic_s TO dynamic_t.
*
*            CALL FUNCTION 'SXMB_SET_DYN_HDR_OUT'
*              EXPORTING
*                dynamic_t           = dynamic_t
*              EXCEPTIONS
*                parameter_not_valid = 1
*                OTHERS              = 2.
*            IF sy-subrc <> 0.
** Implement suitable error handling here
*            ENDIF.
*
*            "***********END***************
*            lr_proxy->si_sap2sign_send_file_out( EXPORTING output = ls_output1
*                                             IMPORTING input = ls_input1 )."调用pi接口

            CREATE OBJECT lr_proxy.
            lr_proxy->si_sap2sign_send_new_out( EXPORTING output = ls_output
              IMPORTING input = ls_input ).

          CATCH cx_root INTO DATA(lr_root).
            MESSAGE lr_root->get_text(  ) TYPE 'I'.
            EXIT.
        ENDTRY.

        "记录接口入参日志
        zcl_bc_public=>write_log( iv_logid = CONV #( g_message_id )
                                      iv_intid = 'MM070A' iv_ptype = 'Y'
                                      is_data = ls_input1 ).
        CLEAR:ls_zmmt0032_detail,lt_zmmt0032_detail.
        "写入日志到过程记录表中
        zcl_bc_zmm032=>write_log( iv_logid = CONV #( g_message_id )
                                  iv_intid = gs_head-ebeln
                                  is_data = ls_output1
                                  is_data2 = ls_input1
                                  zbs = '1'
                                    ).
        IF ls_input1-mt_sap2sign_send_file_ret-status = '1'.
          "上面第一步处理好了，接下来处理第二部接口

        ELSEIF ls_input1-mt_sap2sign_send_file_ret-status <> '1'.

          "存在错误，需要反写数据到ALV中，并中断后续的操作

        ENDIF.

        CLEAR gs_head.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_READ_TXTL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_EKPO_EBELN
*&      --> LS_EKPO_EBELP
*&      <-- LV_LTEXT
*&---------------------------------------------------------------------*
FORM frm_read_txtl  USING uv_ebeln uv_ebelp
                 CHANGING cv_ltext .
  DATA: tdlines  TYPE STANDARD TABLE OF tline,
        p_tdname LIKE stxl-tdname.

  CLEAR: p_tdname, tdlines.
  CONCATENATE uv_ebeln uv_ebelp INTO p_tdname.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'F01'
      language                = sy-langu
      name                    = CONV tdobname( p_tdname )
      object                  = 'EKPO'
    TABLES
      lines                   = tdlines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc = 0.
    LOOP AT tdlines INTO DATA(lw_lines).
      IF cv_ltext IS INITIAL.
        cv_ltext = lw_lines-tdline.
      ELSE.
        CONCATENATE cv_ltext lw_lines-tdline INTO cv_ltext.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_AUTH_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_auth_check .
  AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
         ID 'WERKS' FIELD p_werks.
  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH '没有权限'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
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
      WHEN 'M3'.
        IF r_03 = 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN 'M4'.
        IF r_03 <> 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN 'M5'.
        AUTHORITY-CHECK OBJECT 'Q_INSPLOT'
               ID 'WERKS' FIELD p_werks.
        IF sy-subrc <> 0.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEND_MESSAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM send_message .

  IF sy-ucomm = 'GZ'.
    PERFORM frm_print USING 'B'.
  ENDIF.
ENDFORM.