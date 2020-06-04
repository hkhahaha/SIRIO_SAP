*&---------------------------------------------------------------------*
*& 包含               ZMMR0032_F01
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

FORM frm_get_data.

    SELECT
      e~ebeln "凭证编号
      e~lifnr "供应商
      l~name1 "供应商名称
      e~ekgrp "采购组
      e~bedat "凭证日期
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
      AND a~loekz = ''
        AND ( e~frgke = ' ' OR e~frgke = 'R' ).
  
    IF lt_ebeln[] IS INITIAL .
      MESSAGE '没有符合输入条件的数据，请检查！'  TYPE 'E' .
      STOP.
    ENDIF.
  
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
    get_field 'lt_ebeln' 'LIFNR' '供应商'.
    get_field 'lt_ebeln' 'NAME1' '供应商名称'.
    get_field 'lt_ebeln' 'EKGRP' '采购组'.
    get_field 'lt_ebeln' 'BEDAT' '凭证日期'.
  
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
  
    SET PF-STATUS 'ZPPR0032' EXCLUDING rt_extab.
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
        PERFORM frm_print.
    ENDCASE.
  *取该行数据
    "READ TABLE lt_ebeln INTO wa_tab INDEX rs_selfield-tabindex.
  
  *  CASE sy-ucomm.
  *    WHEN 'PRINT'.
  *      PERFORM prt USING zucomm.
  *    WHEN 'ZPRINT2'.
  *      PERFORM prt USING zucomm.
  *    WHEN 'SEL_ALL'.
  *      READ TABLE lt_ebeln INTO wa_tab INDEX 1.
  *      wa_tab-sel = 'X'.
  *      MODIFY lt_ebeln FROM wa_tab TRANSPORTING sel WHERE sel EQ ''.
  *
  *    WHEN 'CAN_ALL'.
  *      READ TABLE lt_ebeln INTO wa_tab INDEX 1.
  *      wa_tab-sel = ''.
  *      MODIFY lt_ebeln FROM wa_tab TRANSPORTING sel WHERE sel EQ 'X'.
  *  ENDCASE.
  
  
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
  FORM frm_print .
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
    LOOP AT lt_ebeln INTO DATA(lw_ebeln) WHERE sel = 'X'.
      iw_ebeln = lw_ebeln.
      APPEND iw_ebeln TO it_ebeln.
      CLEAR: iw_ebeln.
    ENDLOOP.
    IF sy-subrc = 0.
      " 订单和供应商信息
      SELECT a~ebeln, a~frgke, a~bedat, a~bukrs, a~ekorg, a~waers, a~zterm,
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
  
        " 生产厂家（制造商）
        SELECT e~ebeln, e~ebelp, e~matnr, e~ematn,
               b~partner, b~name_org1
          FROM ekpo AS e
          INNER JOIN mara AS m
          ON e~matnr = m~matnr OR e~ematn = m~matnr
          INNER JOIN but000 AS b
          ON b~partner = m~mfrnr
          INTO TABLE @DATA(lt_but000)
          FOR ALL ENTRIES IN @lt_ekpo
          WHERE e~ebeln =  @lt_ekpo-ebeln
          AND  e~loekz = '' "20200221 ZZD
            AND e~ebelp = @lt_ekpo-ebelp .
  
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
              READ TABLE lt_but000 INTO DATA(ls_but000) WITH KEY ebeln = ls_ekpo-ebeln ebelp = ls_ekpo-ebelp ematn = ls_ekpo-ematn partner = ls_ekpo-mfrnr.
              IF sy-subrc = 0.
                gs_detail-name_org1 = ls_but000-name_org1.
              ENDIF.
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