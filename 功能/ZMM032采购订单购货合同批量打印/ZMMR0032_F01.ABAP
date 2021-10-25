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

FORM FRM_GET_DATA.

  SELECT
    E~EBELN "凭证编号
    E~LIFNR "供应商
    L~NAME1 "供应商名称
    E~EKGRP "采购组
    E~BEDAT "凭证日期
    INTO CORRESPONDING FIELDS OF TABLE LT_EBELN
    FROM EKKO AS E
    LEFT JOIN LFA1 AS L ON E~LIFNR = L~LIFNR
    INNER JOIN EKPO AS A
    ON A~EBELN = E~EBELN
    "LEFT JOIN EKPO as t2 on ( t1~EBELN = t2~EBELN )
    WHERE E~EBELN IN S_EBELN
      AND E~LIFNR IN S_LIFNR
      AND A~WERKS = P_WERKS
      AND E~EKGRP IN S_EKGRP
      AND E~BSART IN S_BSART
      AND E~BEDAT IN S_BEDAT
    AND A~LOEKZ = ''
      AND ( E~FRGKE = ' ' OR E~FRGKE = 'R' OR E~FRGKE = 'Z').

  IF LT_EBELN[] IS INITIAL .
    MESSAGE '没有符合输入条件的数据，请检查！'  TYPE 'E' .
    STOP.
  ENDIF.

ENDFORM .

*&---------------------------------------------------------------------*
*&      2.Form  layout_build 设置alv显示格式和属性
*&---------------------------------------------------------------------*
FORM LAYOUT_BUILD .
  IT_LAYOUT-INFO_FIELDNAME           = 'COLOR'.  "带有简单行彩色代码的字段名称
ENDFORM.                    "layout_build

*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_DATE
*&---------------------------------------------------------------------*
*       text alv 展示列表
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FRM_DISPLAY_DATA .
  PERFORM FRM_GET_FIELD.
  PERFORM FRM_SHOW_ALV.
ENDFORM. " frm_display_data


*&---------------------------------------------------------------------*
*&      Form  FRM_GET_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


FORM FRM_GET_FIELD .
  DEFINE GET_FIELD.

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

  GET_FIELD 'lt_ebeln' 'SEL' '选择'.
  GET_FIELD 'lt_ebeln' 'EBELN' '采购凭证'.
  GET_FIELD 'lt_ebeln' 'LIFNR' '供应商'.
  GET_FIELD 'lt_ebeln' 'NAME1' '供应商名称'.
  GET_FIELD 'lt_ebeln' 'EKGRP' '采购组'.
  GET_FIELD 'lt_ebeln' 'BEDAT' '凭证日期'.

ENDFORM. " FRM_GET_FIELD

*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM FRM_SHOW_ALV .
  DATA L_REPID TYPE SY-REPID.
  L_REPID = SY-REPID.
  IT_LAYOUT-COLWIDTH_OPTIMIZE = 'X'. "列宽度自动根据内容优化
  "it_layout-box_fieldname = 'SEL'. "选择行

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = L_REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_SAVE                   = 'U'
      I_CALLBACK_USER_COMMAND  = 'FRM_UCOMM'
      IS_LAYOUT                = IT_LAYOUT
      IT_FIELDCAT              = I_FIELDCAT_ALV
      I_DEFAULT                = 'X'
    TABLES
      T_OUTTAB                 = LT_EBELN.
  IF SY-SUBRC <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*  *         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

ENDFORM. " FRM_SHOW_ALV



*&--------------------------------------------------------------------*
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  DATA:WA_EXTAB TYPE LINE OF SLIS_T_EXTAB.
  CLEAR:RT_EXTAB,RT_EXTAB[],WA_EXTAB.

*  IF p_dyfs8 IS INITIAL.
*    wa_extab-fcode = 'ZPRINT2'.
*    APPEND wa_extab TO rt_extab.
*  ENDIF.

  SET PF-STATUS 'ZPPR0032' EXCLUDING RT_EXTAB.
ENDFORM.                    "set_pf_status
*&---------------------------------------------------------------------*
*&      Form  frm_ucomm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM FRM_UCOMM USING R_UCOMM LIKE SY-UCOMM
      RS_SELFIELD TYPE SLIS_SELFIELD.

  DATA: LR_GRID TYPE REF TO CL_GUI_ALV_GRID.
  DATA: L_VALID TYPE C.
*  4此处代码将用户输入读入内表（这一步不可少）
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = LR_GRID.
  CALL METHOD LR_GRID->CHECK_CHANGED_DATA.

  RS_SELFIELD-REFRESH = 'X'.
  CLEAR:ZUCOMM.
  ZUCOMM = SY-UCOMM.

  CASE SY-UCOMM.
    WHEN 'SAL_ALL'.
      LOOP AT LT_EBELN ASSIGNING FIELD-SYMBOL(<FS_EBELN>).
        <FS_EBELN>-SEL = 'X'.
      ENDLOOP.
    WHEN 'DES_ALL'.
      LOOP AT LT_EBELN ASSIGNING <FS_EBELN>.
        <FS_EBELN>-SEL = ''.
      ENDLOOP.
    WHEN 'PRINT'.
      PERFORM FRM_PRINT.
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



FORM FRM_PRINT_DATA TABLES GT_DETAIL STRUCTURE ZMMS_ZMMR0017_DETAIL
        USING U_GS_HEAD LIKE ZMMS_ZMMR0017_HEAD
        U_FORM_NAME TYPE TDSFNAME
        S_EBELN .
*        p_znum TYPE i.
  DATA: FM_NAME TYPE RS38L_FNAM.
  DATA LS_OUTPUT_INFO TYPE SSFCRESCL.
  DATA LS_OUTPUT_OPTION TYPE SSFCOMPOP.
  DATA LS_CONTROL TYPE SSFCTRLOP.
*  DATA p_znum TYPE i.


  "获取SMARTFOMRS函数
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = U_FORM_NAME "Smart Form名称
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
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
FORM FRM_PRINT .
  DATA: FM_NAME  TYPE RS38L_FNAM.
  DATA: IT_EBELN  TYPE STANDARD TABLE OF LS_EBELN,
        IW_EBELN  LIKE LINE OF IT_EBELN,
        LT_T007V  TYPE STANDARD TABLE OF T007V,
        LS_T007V  TYPE T007V,
        LT_EKPO   TYPE STANDARD TABLE OF EKPO,
        LS_EKPO   TYPE EKPO,
        LV_AMOUNT TYPE BRTWR,
        LV_LTEXT  TYPE TDLINE.

  CLEAR: IT_EBELN, IW_EBELN.
  LOOP AT LT_EBELN INTO DATA(LW_EBELN) WHERE SEL = 'X'.
    IW_EBELN = LW_EBELN.
    APPEND IW_EBELN TO IT_EBELN.
    CLEAR: IW_EBELN.
  ENDLOOP.
  IF SY-SUBRC = 0.
    " 订单和供应商信息
    SELECT A~EBELN, A~FRGKE, A~BEDAT, A~BUKRS, A~EKORG, A~WAERS, A~ZTERM,A~BSART,
           B~LIFNR, B~NAME1, B~TELFX, B~STRAS, B~ADRNR
      ,A~KNUMV "ZZD 20200225
      FROM EKKO AS A
      INNER JOIN LFA1 AS B
      ON B~LIFNR = A~LIFNR
      INTO TABLE @DATA(LT_EKKO)
      FOR ALL ENTRIES IN @IT_EBELN
      WHERE A~EBELN = @IT_EBELN-EBELN.
    IF SY-SUBRC = 0.
      " 电话
      SELECT ADDRNUMBER, PERSNUMBER, DATE_FROM, CONSNUMBER, TEL_NUMBER
        FROM ADR2
        INTO TABLE @DATA(LT_ADR2)
        FOR ALL ENTRIES IN @LT_EKKO
        WHERE ADDRNUMBER = @LT_EKKO-ADRNR
          AND CONSNUMBER = 1.

      " 供应商 联系人
      SELECT LIFNR, EKORG, VERKF, ZTERM
        FROM LFM1
        INTO TABLE @DATA(LT_LFM1)
        FOR ALL ENTRIES IN @LT_EKKO
        WHERE LIFNR = @LT_EKKO-LIFNR
          AND EKORG = @LT_EKKO-EKORG.
      IF SY-SUBRC = 0.
        " 结算方式
        SELECT SPRAS, ZTERM, ZTAGG, TEXT1
          FROM T052U
          INTO TABLE @DATA(LT_T052U)
          FOR ALL ENTRIES IN @LT_LFM1
          WHERE ZTERM = @LT_LFM1-ZTERM
            AND SPRAS = 1.
      ENDIF.

      " 币别
      SELECT SPRAS, WAERS, LTEXT, KTEXT
        FROM TCURT
        INTO TABLE @DATA(LT_TCURT)
        FOR ALL ENTRIES IN @LT_EKKO
        WHERE WAERS = @LT_EKKO-WAERS
          AND SPRAS = 1.

      " 买方公司名称
      SELECT BUKRS, BUTXT, ADRNR
        FROM T001
        INTO TABLE @DATA(LT_T001)
        FOR ALL ENTRIES IN @LT_EKKO
        WHERE BUKRS = @LT_EKKO-BUKRS.
      IF SY-SUBRC = 0.
        " 公司地址&传真&邮编
        SELECT ADDRNUMBER, DATE_FROM, NATION, STREET, FAX_NUMBER, POST_CODE1
          FROM ADRC
          INTO TABLE @DATA(LT_ADRC)
          FOR ALL ENTRIES IN @LT_T001
          WHERE ADDRNUMBER = @LT_T001-ADRNR.
      ENDIF.

      " 卖方银行 基本户 银行账号
      SELECT F~ACC_ID, F~REVISION, F~BUKRS, F~ACC_NUM, B~BANKA
        FROM FCLM_BAM_AMD AS F
        INNER JOIN BNKA AS B
        ON B~BANKL = F~BANKL
        INTO TABLE @DATA(LT_FCLM)
        FOR ALL ENTRIES IN @LT_EKKO
        WHERE F~BUKRS =  @LT_EKKO-BUKRS
          AND F~ACC_TYPE_ID ='01'
          AND F~STATUS ='02'.

      " 卖方 税号
      SELECT *
        FROM ADRCTS2
        INTO TABLE @DATA(LT_ADRCTS2).

      " 文件编号
      SELECT *
        FROM ZQMT007
        INTO TABLE @DATA(LT_ZQMT007)
        FOR ALL ENTRIES IN @LT_EKKO
        WHERE WERKS = @LT_EKKO-BUKRS
          AND ZBDDH ='ZMM017'.

      " 供应商开户银行，银行账号
      SELECT L~LIFNR, L~BANKS, L~BANKL, L~BANKN,
             B~BANKA
        FROM LFBK AS L
        INNER JOIN BNKA AS B
        ON B~BANKL = L~BANKL
        INTO TABLE @DATA(LT_LFBK)
        FOR ALL ENTRIES IN @LT_EKKO
        WHERE L~LIFNR = @LT_EKKO-LIFNR
          AND L~BVTYP = '0001'.

      " 供应商税号
      SELECT *
        FROM DFKKBPTAXNUM
        INTO TABLE @DATA(LT_DFKKBPTAXNUM)
        FOR ALL ENTRIES IN @LT_EKKO
        WHERE PARTNER = @LT_EKKO-LIFNR
          AND TAXTYPE = 'CN0'.

      " 付款条件
      SELECT *
        FROM T052U
        INTO TABLE @DATA(LT_T052)
        FOR ALL ENTRIES IN @LT_EKKO
        WHERE ZTERM = @LT_EKKO-ZTERM
          AND SPRAS = 1.

      CLEAR: GT_HEAD, GS_HEAD.
      LOOP AT LT_EKKO INTO DATA(LS_EKKO).
        GS_HEAD-EBELN = LS_EKKO-EBELN.
        GS_HEAD-LIFNR_N = LS_EKKO-NAME1.
        GS_HEAD-BUKRS = LS_EKKO-BUKRS.
        GS_HEAD-BEDAT = LS_EKKO-BEDAT.
        GS_HEAD-FRGKE = LS_EKKO-FRGKE.
        GS_HEAD-STRAS = LS_EKKO-STRAS.
        GS_HEAD-TELFX = LS_EKKO-TELFX.
        GS_HEAD-BSART = LS_EKKO-BSART."modify by wwj
        CASE GS_HEAD-BUKRS.
          WHEN '8100'.
            GS_HEAD-ADRNR_T ='一厂：汕头市泰山路83号；二厂：汕头市黄山路珠业南街11号'.
          WHEN '8200'.
            GS_HEAD-ADRNR_T ='马鞍山地址：安徽省马鞍山经济技术开发区红旗南路1980号'.
        ENDCASE.
        READ TABLE LT_T052 INTO DATA(LS_T052) WITH KEY ZTERM = LS_EKKO-ZTERM.
        IF SY-SUBRC = 0.
          GS_HEAD-TEXT1 = LS_T052-TEXT1.
        ENDIF.
        READ TABLE LT_FCLM INTO DATA(LS_FCLM) WITH KEY BUKRS = LS_EKKO-BUKRS.
        IF SY-SUBRC = 0.
          GS_HEAD-BANKN = LS_FCLM-ACC_NUM.
          GS_HEAD-BANKA = LS_FCLM-BANKA.
        ENDIF.
        READ TABLE LT_ADRCTS2 INTO DATA(LS_ADRCTS2) WITH KEY OBJKEY+3(4) = LS_EKKO-BUKRS.
        IF SY-SUBRC = 0.
          GS_HEAD-TAXNUM = LS_ADRCTS2-REMARK.
        ENDIF.
        READ TABLE LT_ZQMT007 INTO DATA(LS_ZQMT007) WITH KEY WERKS = LS_EKKO-BUKRS.
        IF SY-SUBRC = 0.
          GS_HEAD-ZTXBH = LS_ZQMT007-ZTXBH.
        ENDIF.
        READ TABLE LT_LFBK INTO DATA(LS_LKBK) WITH KEY LIFNR = LS_EKKO-LIFNR.
        IF SY-SUBRC = 0.
          GS_HEAD-BANKA_N = LS_LKBK-BANKA.
          GS_HEAD-BANKN_N = LS_LKBK-BANKN.
        ENDIF.
        READ TABLE LT_DFKKBPTAXNUM INTO DATA(LS_DFKKBPTAXNUM) WITH KEY PARTNER = LS_EKKO-LIFNR.
        IF SY-SUBRC = 0.
          GS_HEAD-TAXNUM_N = LS_DFKKBPTAXNUM-TAXNUM.
        ENDIF.
        READ TABLE LT_ADR2 INTO DATA(LS_ADR2) WITH KEY ADDRNUMBER = LS_EKKO-ADRNR.
        IF SY-SUBRC = 0.
          GS_HEAD-TELF1 = LS_ADR2-TEL_NUMBER.
        ENDIF.
        READ TABLE LT_LFM1 INTO DATA(LS_LFM1) WITH KEY LIFNR = LS_EKKO-LIFNR EKORG = LS_EKKO-EKORG.
        IF SY-SUBRC = 0.
          GS_HEAD-VERKF = LS_LFM1-VERKF.
          READ TABLE LT_T052U INTO DATA(LS_T052U) WITH KEY ZTERM = LS_LFM1-ZTERM.
          IF SY-SUBRC = 0.
            GS_HEAD-ZTERM_T = LS_T052U-TEXT1.
          ENDIF.
        ENDIF.
        READ TABLE LT_TCURT INTO DATA(LS_TCURT) WITH KEY WAERS = LS_EKKO-WAERS.
        IF SY-SUBRC = 0.
          GS_HEAD-WAERS_K = LS_TCURT-KTEXT.
          CASE LS_TCURT-KTEXT.
            WHEN '人民币'.
              GS_HEAD-ZCURR ='￥'.
            WHEN '美元'.
              GS_HEAD-ZCURR = '$'.
          ENDCASE.
        ENDIF.
        READ TABLE LT_T001 INTO DATA(LS_T001) WITH KEY BUKRS = LS_EKKO-BUKRS.
        IF SY-SUBRC = 0.
          GS_HEAD-BUTXT = LS_T001-BUTXT.
          READ TABLE LT_ADRC INTO DATA(LS_ADRC) WITH KEY ADDRNUMBER = LS_T001-ADRNR.
          IF SY-SUBRC = 0.
            GS_HEAD-STREET = LS_ADRC-STREET.
            GS_HEAD-FAX_NUMBER = LS_ADRC-FAX_NUMBER.
            GS_HEAD-POST_CODE1 = LS_ADRC-POST_CODE1.
*            IF GS_HEAD-BUKRS = '8100'.
*              CONCATENATE '一厂：' GS_HEAD-STREET INTO GS_HEAD-STREET.
*            ENDIF.
          ENDIF.
        ENDIF.
        APPEND GS_HEAD TO GT_HEAD.
        CLEAR: GS_HEAD.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF GT_HEAD IS NOT INITIAL.
    CLEAR: LT_EKPO, LS_EKPO.
    SELECT *
      INTO TABLE @LT_EKPO
      FROM EKPO
      FOR ALL ENTRIES IN @GT_HEAD
      WHERE EBELN =  @GT_HEAD-EBELN
      AND LOEKZ = ''. "ZZD 20200224
    IF SY-SUBRC = 0.
      DATA: LR_MATNR TYPE RANGE OF MATNR,
            LW_MATNR LIKE LINE OF LR_MATNR.
      CLEAR: LR_MATNR, LW_MATNR.
      LOOP AT LT_EKPO INTO LS_EKPO.
        LW_MATNR-SIGN = 'I'.
        LW_MATNR-OPTION = 'EQ'.
        LW_MATNR-LOW = LS_EKPO-MATNR.
        APPEND LW_MATNR TO LR_MATNR.
        CLEAR: LW_MATNR.
        LW_MATNR-SIGN = 'I'.
        LW_MATNR-OPTION = 'EQ'.
        LW_MATNR-LOW = LS_EKPO-EMATN.
        APPEND LW_MATNR TO LR_MATNR.
        CLEAR: LW_MATNR.
      ENDLOOP.
      SORT LR_MATNR BY LOW.
      DELETE ADJACENT DUPLICATES FROM LR_MATNR COMPARING LOW.
      SELECT M~MATNR, M~MAKTX, A~MATKL, A~GROES
        INTO TABLE @DATA(LT_MATNR)
        FROM MAKT AS M
        LEFT JOIN MARA AS A
        ON M~MATNR = A~MATNR
        WHERE M~MATNR IN @LR_MATNR
          AND M~SPRAS = 1.

      SELECT T~EBELN, T~EBELP, T~ETENR, T~EINDT
        INTO TABLE @DATA(LT_EKET)
        FROM EKET AS T
        INNER JOIN EKPO AS O
        ON O~EBELN = T~EBELN AND O~EBELP = T~EBELP
        FOR ALL ENTRIES IN @LT_EKPO
        WHERE T~EBELN = @LT_EKPO-EBELN
        AND  O~LOEKZ = ''  "20200221 ZZD
          AND T~EBELP = @LT_EKPO-EBELP.

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
      CLEAR: LT_T007V.
      SELECT *
        FROM T007V
        INTO TABLE LT_T007V
        WHERE ALAND = 'CN'
          AND DATAM <> '00000000'.
      IF SY-SUBRC = 0.
        SORT LT_T007V BY DATUM UZEIT DESCENDING.
      ENDIF.

      SELECT E~INFNR, E~EKORG, E~ESOKZ, E~WERKS,
             E~NETPR, E~PEINH, E~MWSKZ, A~MATNR
        INTO TABLE @DATA(LT_EINE)
        FROM EINE AS E
        INNER JOIN EINA AS A
        ON E~INFNR = A~INFNR
        FOR ALL ENTRIES IN @LT_EKPO
        WHERE E~ESOKZ = '2'
          AND E~INFNR = @LT_EKPO-INFNR .

      SELECT A~KAPPL, A~KSCHL, A~LIFNR, A~MATNR, A~EKORG,
             A~WERKS, A~ESOKZ, A~DATBI, A~DATAB,
             B~KNUMH, B~KOPOS, B~KBETR, B~KPEIN
        FROM A017 AS A
        INNER JOIN KONP AS B
        ON B~KNUMH = A~KNUMH
        INTO TABLE @DATA(LT_A017)
        FOR ALL ENTRIES IN @LT_EKPO
        WHERE A~KAPPL = 'M'
          AND A~ESOKZ = '2'
          AND A~MATNR = @LT_EKPO-MATNR
          AND A~WERKS = @LT_EKPO-WERKS.

      " 生产厂家（制造商）
      SELECT E~EBELN, E~EBELP, E~MATNR, E~EMATN,
             B~PARTNER, B~NAME_ORG1
        FROM EKPO AS E
        INNER JOIN MARA AS M
        ON E~MATNR = M~MATNR OR E~EMATN = M~MATNR
        INNER JOIN BUT000 AS B
        ON B~PARTNER = M~MFRNR "OR B~PARTNER = E~ZMFRNR  "waymon 2020.9.22 添加OR b~partner = e~zmfrnr.
        INTO TABLE @DATA(LT_BUT000)
        FOR ALL ENTRIES IN @LT_EKPO
        WHERE E~EBELN =  @LT_EKPO-EBELN
        AND  E~LOEKZ = '' "20200221 ZZD
          AND E~EBELP = @LT_EKPO-EBELP .

***********************************************************ZZD 获取附加费 2020022

*将小计1的值赋回订单值
      LOOP AT LT_EKPO INTO LS_EKPO .

        LS_EKPO-BRTWR = LS_EKPO-KZWI1 .
        MODIFY LT_EKPO FROM LS_EKPO .
        CLEAR LS_EKPO.
      ENDLOOP.

*获取批次内码
*ZGROES  规格型号/产品规格
*ZMATDET 材质明细


      TYPES: BEGIN OF TY_TX,"存入物料特性
               MATNR TYPE MARA-MATNR, "物料
               OBJEK TYPE AUSP-OBJEK, "对象
               ATINN TYPE AUSP-ATINN, "内码
               ZGG   TYPE AUSP-ATWRT, "规格
             END OF TY_TX.

      DATA: GT_CABN TYPE TABLE OF CABN,
            GS_CABN TYPE CABN,
            GT_WLTX TYPE TABLE OF TY_TX,
            GS_WLTX TYPE TY_TX,
            GT_AUSP TYPE TABLE OF AUSP,
            GS_AUSP TYPE AUSP,
            GT_MARA TYPE TABLE OF MARA,
            GS_MARA TYPE MARA.

*取特性内码
      SELECT
        ATINN
        ATNAM
        INTO CORRESPONDING FIELDS OF TABLE GT_CABN
        FROM CABN
        WHERE ATNAM IN ('ZGROES','ZMATDET').
*取物料类型
      SELECT
        MATNR
        MTART
        INTO CORRESPONDING FIELDS OF TABLE GT_MARA
        FROM MARA
        FOR ALL ENTRIES IN LT_EKPO
        WHERE MATNR = LT_EKPO-MATNR.

      LOOP AT LT_EKPO INTO LS_EKPO.
        GS_WLTX-MATNR = LS_EKPO-MATNR.
        GS_WLTX-OBJEK = LS_EKPO-MATNR.
        APPEND GS_WLTX TO GT_WLTX.
        CLEAR GS_WLTX.
      ENDLOOP.

      LOOP AT GT_WLTX INTO GS_WLTX.
        READ TABLE GT_MARA INTO GS_MARA WITH KEY MATNR = GS_WLTX-MATNR.
        IF GS_MARA-MTART = 'Z010'.
          READ TABLE GT_CABN INTO GS_CABN WITH KEY ATNAM = 'ZGROES'.
          GS_WLTX-ATINN = GS_CABN-ATINN.
        ELSEIF GS_MARA-MTART = 'Z020'.
          READ TABLE GT_CABN INTO GS_CABN WITH KEY ATNAM = 'ZMATDET'.
          GS_WLTX-ATINN = GS_CABN-ATINN.
        ENDIF.


        CLEAR GS_CABN.
        MODIFY GT_WLTX FROM GS_WLTX.
        CLEAR GS_WLTX.
      ENDLOOP.

      SELECT
        OBJEK
        ATINN
        ATWRT
        INTO CORRESPONDING FIELDS OF TABLE GT_AUSP
        FROM AUSP
        FOR ALL ENTRIES IN GT_WLTX
        WHERE OBJEK = GT_WLTX-OBJEK
        AND ATINN = GT_WLTX-ATINN.

      LOOP AT GT_WLTX INTO GS_WLTX.
        READ TABLE GT_AUSP INTO GS_AUSP WITH KEY OBJEK = GS_WLTX-OBJEK
        ATINN = GS_WLTX-ATINN.
        IF SY-SUBRC = 0.
          GS_WLTX-ZGG = GS_AUSP-ATWRT.
        ENDIF.
        MODIFY GT_WLTX FROM GS_WLTX.
        CLEAR GS_WLTX.
      ENDLOOP.


      DATA LS_MATNR LIKE LINE OF LT_MATNR.
      LOOP AT LT_MATNR INTO LS_MATNR.
        READ TABLE GT_WLTX INTO GS_WLTX WITH KEY MATNR = LS_MATNR-MATNR.
        IF SY-SUBRC = 0.
          LS_MATNR-GROES = GS_WLTX-ZGG.
          MODIFY LT_MATNR FROM LS_MATNR.
          CLEAR LS_MATNR.
        ENDIF.
      ENDLOOP.
***********************************************************ZZD 获取附加费 20200225



      LOOP AT LT_EKPO INTO LS_EKPO.
        " EKPO- PSTYP EQ 2 寄售
        " 含税单价 不含税单价  税率
        IF LS_EKPO-PSTYP EQ  '2' .
          READ TABLE LT_EINE INTO DATA(LS_EINE) WITH KEY INFNR = LS_EKPO-INFNR.
          IF SY-SUBRC = 0.
            LS_EKPO-NETPR = LS_EINE-NETPR.
            LS_EKPO-PEINH = LS_EINE-PEINH.
          ENDIF.
        ENDIF.
        MOVE-CORRESPONDING LS_EKPO TO GS_DETAIL.
        READ TABLE LT_MATNR INTO LS_MATNR WITH KEY MATNR = LS_EKPO-MATNR.
        IF SY-SUBRC = 0.
          IF LS_MATNR-MATKL+0(1) = '1'.
            GS_DETAIL-MATNR = LS_EKPO-EMATN.
            "Seashell Huang 2020.6.4 添加制造商的获取条件partner = ls_ekpo-mfrnr
            READ TABLE LT_BUT000 INTO DATA(LS_BUT000) WITH KEY EBELN = LS_EKPO-EBELN EBELP = LS_EKPO-EBELP EMATN = LS_EKPO-EMATN PARTNER = LS_EKPO-MFRNR.
            IF SY-SUBRC = 0.
              GS_DETAIL-NAME_ORG1 = LS_BUT000-NAME_ORG1.
              "waymon 2020.9.22 添加制造商的获取条件partner = ls_ekpo-Zmfrnr TOP.
            ELSE.
              READ TABLE LT_BUT000 INTO LS_BUT000 WITH KEY EBELN = LS_EKPO-EBELN EBELP = LS_EKPO-EBELP EMATN = LS_EKPO-EMATN. "PARTNER = LS_EKPO-ZMFRNR.
              IF SY-SUBRC = 0.
                GS_DETAIL-NAME_ORG1 = LS_BUT000-NAME_ORG1.
              ENDIF.
              "waymon 2020.9.22 添加制造商的获取条件partner = ls_ekpo-Zmfrnr END.
            ENDIF.
            CLEAR: LS_MATNR.
*****************ZZD 20200227
*           READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = ls_ekpo-ematn.
            READ TABLE LT_MATNR INTO LS_MATNR WITH KEY MATNR = LS_EKPO-MATNR.
*****************ZZD 20200227
            IF SY-SUBRC = 0.
              GS_DETAIL-MAKTX = LS_MATNR-MAKTX.
              GS_DETAIL-GROES = LS_MATNR-GROES.
            ENDIF.
          ELSE.
            GS_DETAIL-MATNR = LS_EKPO-MATNR.
            READ TABLE LT_BUT000 INTO LS_BUT000 WITH KEY EBELN = LS_EKPO-EBELN EBELP = LS_EKPO-EBELP MATNR = LS_EKPO-MATNR.
            IF SY-SUBRC = 0.
              GS_DETAIL-NAME_ORG1 = LS_BUT000-NAME_ORG1.
            ENDIF.
            GS_DETAIL-MAKTX = LS_MATNR-MAKTX.
            GS_DETAIL-GROES = LS_MATNR-GROES.
          ENDIF.
        ENDIF.
        READ TABLE LT_EKET INTO DATA(LS_EKET) WITH KEY EBELN = LS_EKPO-EBELN EBELP = LS_EKPO-EBELP.
        IF SY-SUBRC = 0.
          GS_DETAIL-EINDT = LS_EKET-EINDT.
        ENDIF.
*        READ TABLE LT_KONP INTO DATA(LS_KONP) WITH KEY EBELN = LS_EKPO-EBELN EBELP = LS_EKPO-EBELP.
*        IF SY-SUBRC = 0.
*          GS_DETAIL-KBETR = LS_KONP-KBETR.
*        ENDIF.
        "寄售 含税金额  不含税金额
        IF LS_EKPO-PSTYP EQ  '2'.
          CLEAR: LS_EKKO.
          READ TABLE LT_EKKO INTO LS_EKKO WITH KEY EBELN = LS_EKPO-EBELN.
          IF SY-SUBRC = 0.
            CLEAR: LS_EINE.
            READ TABLE LT_EINE INTO LS_EINE
              WITH KEY INFNR = LS_EKPO-INFNR MATNR = LS_EKPO-MATNR WERKS = LS_EKPO-WERKS EKORG = LS_EKKO-EKORG.
            IF SY-SUBRC = 0.
              CLEAR: LS_T007V.
              READ TABLE LT_T007V INTO LS_T007V WITH KEY MWSKZ = LS_EINE-MWSKZ.
              IF SY-SUBRC = 0.
                GS_DETAIL-KBETR = LS_T007V-KBETR.
              ENDIF.
            ENDIF.
            LOOP AT LT_A017 INTO DATA(LS_A017)
              WHERE MATNR = LS_EKPO-MATNR AND WERKS = LS_EKPO-WERKS AND EKORG = LS_EKKO-EKORG
                AND LIFNR = LS_EKKO-LIFNR AND DATAB <= LS_EKKO-BEDAT AND DATBI >= LS_EKKO-BEDAT.
              " 含税单价
              GS_DETAIL-BSPRS = LS_A017-KBETR / LS_A017-KPEIN.
              " 不含税单价

              GS_DETAIL-NETPR = GS_DETAIL-BSPRS / ( 1 + GS_DETAIL-KBETR / 1000 ) .
              GS_DETAIL-NETWR = GS_DETAIL-NETPR * LS_EKPO-MENGE.
              GS_DETAIL-BRTWR = GS_DETAIL-BSPRS * LS_EKPO-MENGE. "ZZD 20200309.
              EXIT.
            ENDLOOP.
          ENDIF.
        ELSE.
          CLEAR: LS_T007V.
          READ TABLE LT_T007V INTO LS_T007V WITH KEY MWSKZ = LS_EKPO-MWSKZ.
          IF SY-SUBRC = 0.
            GS_DETAIL-KBETR = LS_T007V-KBETR.
          ENDIF.
*          " 含税单价
          GS_DETAIL-BSPRS = LS_EKPO-BRTWR  / LS_EKPO-MENGE.
*          " 不含税单价
          GS_DETAIL-NETPR = GS_DETAIL-BSPRS / ( 1 + GS_DETAIL-KBETR / 1000 ).




        ENDIF.
        " 送货地址
        CASE LS_EKPO-LGORT+0(1).
          WHEN '1'.
            GS_DETAIL-ADRNR = '一厂'.
          WHEN '2'.
            GS_DETAIL-ADRNR = '二厂'.
        ENDCASE.
        " 采购订单行项目文本
        CLEAR: LV_LTEXT.
        PERFORM FRM_READ_TXTL USING LS_EKPO-EBELN LS_EKPO-EBELP CHANGING LV_LTEXT.
        GS_DETAIL-LTXTS = LV_LTEXT.

        "UP BY LANJINGCUN 20201230 无物料号的处理-S857
*        READ TABLE GT_HEAD INTO GS_HEAD WITH KEY EBELN = LS_EKPO-EBELN.
*        IF SY-SUBRC = 0.
*          IF GS_HEAD-BSART = 'ZNF'.  "非库存物资订单类型，无物料编码 modify by wwj _20201030
*            GS_DETAIL-MAKTX = LS_EKPO-TXZ01 .
*          ENDIF.
*        ENDIF.
        IF GS_DETAIL-MAKTX = '' .
          GS_DETAIL-MAKTX = LS_EKPO-TXZ01 .
        ENDIF .
        "UP BY LANJINGCUN 20201230 无物料号的处理-E857

        APPEND GS_DETAIL TO GT_DETAIL.
        LV_AMOUNT = GS_DETAIL-BRTWR + LV_AMOUNT.
        CLEAR: GS_DETAIL.
        AT END OF EBELN.
*          SUM.
          READ TABLE GT_HEAD ASSIGNING FIELD-SYMBOL(<FS_HEAD>) WITH KEY EBELN = LS_EKPO-EBELN.
          IF SY-SUBRC = 0.
            <FS_HEAD>-AMOUNT = LV_AMOUNT.
          ENDIF.
          CLEAR: LV_AMOUNT.
        ENDAT.
      ENDLOOP.
      SORT GT_DETAIL BY EBELP.
    ENDIF.
  ENDIF.
  DATA: LT_DETAIL  TYPE STANDARD TABLE OF ZMMS_ZMMR0017_DETAIL,
        LS_DETAIL  TYPE ZMMS_ZMMR0017_DETAIL,
        LW_OUTPUT  TYPE SSFCOMPOP,
        LW_CONTROL TYPE SSFCTRLOP,
        LV_LINES   TYPE I,
        LV_DIV     TYPE I.
  IF GT_HEAD IS NOT INITIAL OR GT_DETAIL IS NOT INITIAL.
    " 获取SMARTFOMRS函数
    "ADD BY LANJINGCUN 添加维乐维单据类型-S888
    CLEAR GV_TDSFNAME .
    CASE P_WERKS .
      WHEN '8100' .
        GV_TDSFNAME = 'ZMM017_PC' .
      WHEN '8200' .
        GV_TDSFNAME = 'ZMM017_PC' .
      WHEN  OTHERS .
        GV_TDSFNAME = 'ZMM017_WL' .
    ENDCASE .
    "ADD BY LANJINGCUN 添加维乐维单据类型-E888

    CLEAR: FM_NAME, LW_OUTPUT, LW_CONTROL.
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = 'ZMM017_PC'
      IMPORTING
        FM_NAME            = FM_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
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

    LW_CONTROL-PREVIEW       = 'X'.                 " 打印预览
    LW_CONTROL-NO_OPEN       = 'X'.                 " 新的假脱机请求
    LW_CONTROL-NO_CLOSE      = 'X'.
    LW_CONTROL-NO_DIALOG     = 'X'.                 " 必须设置

    CALL FUNCTION 'SSF_OPEN'
      EXPORTING
        OUTPUT_OPTIONS     = LW_OUTPUT
        CONTROL_PARAMETERS = LW_CONTROL
        USER_SETTINGS      = ' '
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.

    LOOP AT GT_HEAD INTO GS_HEAD.
      CLEAR: LT_DETAIL, LS_DETAIL.
      LOOP AT GT_DETAIL INTO GS_DETAIL WHERE EBELN = GS_HEAD-EBELN.
        MOVE-CORRESPONDING GS_DETAIL TO LS_DETAIL.
        APPEND LS_DETAIL TO LT_DETAIL.
        CLEAR: LS_DETAIL.
      ENDLOOP.

      CLEAR: GS_LINE, LV_DIV, LV_LINES.
      DESCRIBE TABLE LT_DETAIL LINES GS_LINE.
      IF GS_LINE > 11.
        LV_DIV = GS_LINE DIV 12.
        LV_LINES = ( LV_DIV + 1 ) * 12 - 1.
        DO LV_LINES - GS_LINE TIMES.
          APPEND LS_DETAIL TO LT_DETAIL.
        ENDDO.
      ELSE.
        DO 11 - GS_LINE TIMES.
          APPEND LS_DETAIL TO LT_DETAIL.
        ENDDO.
      ENDIF.

      CALL FUNCTION FM_NAME
        EXPORTING
          GS_HEAD            = GS_HEAD
          GS_LINE            = GS_LINE
          CONTROL_PARAMETERS = LW_CONTROL
          OUTPUT_OPTIONS     = LW_OUTPUT
        TABLES
          GT_DETAIL          = LT_DETAIL
        EXCEPTIONS
          FORMATTING_ERROR   = 1
          INTERNAL_ERROR     = 2
          SEND_ERROR         = 3
          USER_CANCELED      = 4
          OTHERS             = 5.
    ENDLOOP.
    CALL FUNCTION 'SSF_CLOSE'
      EXCEPTIONS
        FORMATTING_ERROR = 1
        INTERNAL_ERROR   = 2
        SEND_ERROR       = 3
        OTHERS           = 4.

    CLEAR: GT_HEAD, GS_HEAD, GT_DETAIL, GS_DETAIL.
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
FORM FRM_READ_TXTL  USING UV_EBELN UV_EBELP
                 CHANGING CV_LTEXT .
  DATA: TDLINES  TYPE STANDARD TABLE OF TLINE,
        P_TDNAME LIKE STXL-TDNAME.

  CLEAR: P_TDNAME, TDLINES.
  CONCATENATE UV_EBELN UV_EBELP INTO P_TDNAME.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = 'F01'
      LANGUAGE                = SY-LANGU
      NAME                    = CONV TDOBNAME( P_TDNAME )
      OBJECT                  = 'EKPO'
    TABLES
      LINES                   = TDLINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.
  IF SY-SUBRC = 0.
    LOOP AT TDLINES INTO DATA(LW_LINES).
      IF CV_LTEXT IS INITIAL.
        CV_LTEXT = LW_LINES-TDLINE.
      ELSE.
        CONCATENATE CV_LTEXT LW_LINES-TDLINE INTO CV_LTEXT.
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
FORM FRM_AUTH_CHECK .
  AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
         ID 'WERKS' FIELD P_WERKS.
  IF SY-SUBRC <> 0.
    MESSAGE E001(00) WITH '没有权限'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.