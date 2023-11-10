*----------------------------------------------------------------------*
***INCLUDE LZMM006F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_INPUT
*&      <-- E_OUTPUT
*&---------------------------------------------------------------------*
FORM frm_check_input  USING    u_input TYPE zdt_oa2sap_bp
                      CHANGING c_output TYPE zdt_oa2sap_bp_ret.

  DATA: l_partner   TYPE but000-partner,
        l_type      TYPE but0id-type,
        lw_bankinfo TYPE zdt_oa2sap_bp_bankinfo.

  DATA: ls_but0bk   TYPE but0bk,
        l_bank_acct TYPE cvis_bp_bank_details-bank_acct,
        l_bank_ref  TYPE cvis_bp_bank_details-bank_ref.

  PERFORM frm_get_g_partner USING u_input-bpartner .
  PERFORM frm_check_bp_exist USING    u_input-bpartner g_mcflag
                             CHANGING c_output-type c_output-message.
  IF c_output-type = 'E'.
    RETURN.
  ENDIF.
  PERFORM frm_check_bp_group USING    u_input-grouping g_mcflag
                             CHANGING c_output-type c_output-message.
  IF c_output-type = 'E'.
    RETURN.
  ENDIF.

  IF g_mcflag = cns_mcflag_modify.
    LOOP AT u_input-bankinfo INTO lw_bankinfo.
      CLEAR: l_bank_acct, l_bank_ref.
      PERFORM frm_split_bankn USING lw_bankinfo-bankn
                           CHANGING l_bank_acct
                                    l_bank_ref.

      PERFORM frm_check_bankinfo USING u_input-bpartner
                                       lw_bankinfo-bkvid
                                       lw_bankinfo-banks
                                       lw_bankinfo-bankl
                                       l_bank_acct
                                       l_bank_ref
                                CHANGING c_output-type
                                         c_output-message.
      IF c_output-type = 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF c_output-type = 'E'.
      RETURN.
    ENDIF.
  ENDIF.

  PERFORM frm_check_taxnumber USING g_partner u_input-taxtype u_input-taxnumxl
                              CHANGING c_output-type c_output-message.
  IF c_output-type = 'E'.
    RETURN.
  ENDIF.

  "技术ID不能重复，标准功能
  LOOP AT u_input-oa_but0id INTO DATA(lw_but0id).
**    IF lw_but0id-TYPE+0(3) <> 'ZQM'.
**      DELETE TABLE u_input-oa_but0id[] FROM lw_but0id.
**      CLEAR lw_but0id.
**      CONTINUE.
**    ENDIF.

    SELECT SINGLE partner type INTO (l_partner, l_type) FROM but0id
      WHERE idnumber = lw_but0id-idnumber
        AND type = lw_but0id-type
        AND partner NE g_partner.
    IF sy-subrc = 0.
      c_output-type = 'E'.
      MESSAGE ID 'R1' TYPE 'E' NUMBER 477 INTO c_output-message
        WITH l_type lw_but0id-idnumber l_partner.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF c_output-type = 'E'.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_INSERT_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_INPUT_OAID
*&      --> I_INPUT_MCFLAG
*&      --> I_INPUT_BPARTNER
*&      <-- E_OUTPUT_TYPE
*&      <-- E_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_insert_log  USING    u_oaid
                              u_bptype
                              u_mcflag
                     CHANGING c_bpartner
                              c_type
                              c_message.
  "先锁定，创建/修改完成之后再解锁，避免短时间内的重复调用，第一次调用BP还没创建/修改完成
  "第二次调用已经开始执行了

  DATA: lw_zbct060 TYPE zbct060.

  CLEAR: g_insertlog.
  IF u_oaid = ''.
    c_type = 'E'.
    c_message = '输入错误，请输入OAID'.
    RETURN.
  ENDIF.
  PERFORM frm_lock_zbct060 USING    u_oaid u_bptype
                            CHANGING c_type c_message.
**  SELECT SINGLE * INTO lw_zbct060 FROM zbct060 WHERE oaid = u_oaid AND bptype = u_bptype.
**  IF sy-subrc = 0 AND lw_zbct060-type = 'S'.
**    c_type = 'E'.
**    c_message = 'OAID重复，对应BP编号为：' && lw_zbct060-partner.
**    PERFORM frm_unlock_zbct060 USING    u_oaid u_bptype.
**    RETURN.
**  ELSEIF sy-subrc = 0 AND lw_zbct060-type = 'W' AND lw_zbct060-mcflag = cns_mcflag_create.
**    IF c_bpartner IS INITIAL AND lw_zbct060-partner IS NOT INITIAL.
**      c_bpartner = lw_zbct060-partner.
**      g_partner = lw_zbct060-partner.
**    ENDIF.
**    IF c_bpartner IS NOT INITIAL .
**      g_mcflag = cns_mcflag_modify. "第一次调用创建成功，部分插入失败，直接改为修改
**    ELSE.
**      c_type = 'E'.
**      c_message = 'OAID重复，对应BP编号为：' && lw_zbct060-partner.
**      PERFORM frm_unlock_zbct060 USING    u_oaid u_bptype.
**      RETURN.
**    ENDIF.
**  ELSEIF sy-subrc NE 0.
**    CLEAR: lw_zbct060.
**    lw_zbct060-oaid = u_oaid.
**    lw_zbct060-bptype = u_bptype.
**    lw_zbct060-mcflag = u_mcflag.
**    lw_zbct060-partner = c_bpartner.
**    lw_zbct060-erdat = sy-datum.
**    lw_zbct060-ezeit = sy-uzeit.
**    lw_zbct060-ernam = sy-uname.
**    INSERT zbct060 FROM lw_zbct060.
**    IF sy-subrc = 0.
**      COMMIT WORK AND WAIT .
**      g_insertlog = 'X'.
**    ELSE.
**      PERFORM frm_unlock_zbct060 USING    u_oaid u_bptype.
**      c_type = 'E'.
**      c_message = 'OAID重复，请重新执行，OAID: ' && u_oaid.
**      RETURN.
**    ENDIF.
**  ELSE.
**    "已存在调用错误日志则不插入日志
**  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATE_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_INPUT_OAID
*&      --> I_INPUT_MCFLAG
*&      --> G_PARTNER
*&      --> E_OUTPUT_TYPE
*&      --> E_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_update_log  USING   u_oaid
                              u_bptype
                              u_mcflag
                              u_partner
                              u_type
                              u_message.

  DATA: lw_zbct060 TYPE zbct060.

  CLEAR: lw_zbct060.
  lw_zbct060-oaid = u_oaid.
  SELECT SINGLE * INTO lw_zbct060 FROM zbct060 WHERE oaid = u_oaid AND bptype = u_bptype.
  lw_zbct060-partner = u_partner.
  lw_zbct060-mcflag  = u_mcflag.
  IF lw_zbct060-type NE 'W' AND u_mcflag = cns_mcflag_create.  "W状态不更新为E状态，以避免同一OAID能再次创建新的BP
    lw_zbct060-type = u_type.
  ENDIF.
  lw_zbct060-type = u_type.
  lw_zbct060-message = u_message.
  IF g_insertlog IS INITIAL.  "第一次插入时不更新修改相关字段
    lw_zbct060-aedat = sy-datum.
    lw_zbct060-aezet = sy-uzeit.
    lw_zbct060-aenam = sy-uname.
  ENDIF.
  MODIFY zbct060 FROM lw_zbct060.
  COMMIT WORK AND WAIT.

  PERFORM frm_unlock_zbct060 USING    u_oaid u_bptype.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_LOCK_ZBCT060
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_OAID
*&      <-- C_TYPE
*&      <-- C_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_lock_zbct060  USING    u_oaid
                                 u_bptype
                        CHANGING c_type
                                 c_message.

  DATA: l_oaid   TYPE zbct060-oaid,
        l_bptype TYPE zbct060-bptype.

  l_oaid = u_oaid.
  l_bptype = u_bptype.

  CALL FUNCTION 'ENQUEUE_EZBCT060'
    EXPORTING
      mode_zbct060   = 'E'
      mandt          = sy-mandt
      oaid           = l_oaid
      bptype         = l_bptype
*     X_OAID         = ' '
*     _SCOPE         = '2'
*     _WAIT          = ' '
*     _COLLECT       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    c_type = 'E'.
    c_message = '新建/修改BP失败，' && u_oaid && '锁定失败'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UNLOCK_ZBCT060
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_OAID
*&---------------------------------------------------------------------*
FORM frm_unlock_zbct060  USING    u_oaid
                                   u_bptype.
  DATA: l_oaid   TYPE zbct060-oaid,
        l_bptype TYPE zbct060-bptype.

  l_oaid = u_oaid.
  l_bptype = u_bptype.

  CALL FUNCTION 'DEQUEUE_EZBCT060'
    EXPORTING
      mode_zbct060 = 'E'
      mandt        = sy-mandt
      oaid         = l_oaid
      bptype       = l_bptype
*     X_OAID       = ' '
*     _SCOPE       = '3'
*     _SYNCHRON    = ' '
*     _COLLECT     = ' '
    .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_IMPORT_DATA_FROM_EXCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- I_INPUT
*&---------------------------------------------------------------------*
FORM frm_import_data_from_excel  CHANGING c_input TYPE zdt_oa2sap_bp.

*  DATA: BEGIN OF lw_excel,
*          oaid            TYPE string,
*          mcflag          TYPE string,
*          grouping        TYPE string,
*          bpartner        TYPE string,
*          name1           TYPE string,
*          searchterm1     TYPE string,
*          searchterm2     TYPE string,
*          street          TYPE string,
*          country         TYPE string,
*          langu           TYPE string,
*          telephone       TYPE string,
*          extension       TYPE string,
*          e_mail          TYPE string,
*          region          TYPE string,
*          regiogroup      TYPE string,
*          remark          TYPE string,
*          bpkind          TYPE string,
*          taxtype         TYPE string,
*          taxnumxl        TYPE string,
*          bkvid           TYPE string,
*          banks           TYPE string,
*          bankl           TYPE string,
*          banka           TYPE string,
*          bankn           TYPE string,
*          type            TYPE string,
*          idnumber        TYPE string,
*          valid_date_from TYPE string,
*          valid_date_to   TYPE string,
*          ekorg           TYPE string,
*          waers           TYPE string,
*          verkf           TYPE string,
*          m1_zterm        TYPE string,
*          ekgrp           TYPE string,
*          sales           TYPE string,
*          sperm           TYPE string,
*          bukrs           TYPE string,
*          akont           TYPE string,
*          zterm           TYPE string,
*          zwels           TYPE string,
*          loevm           TYPE string,
*        END OF lw_excel,
*        lt_excel LIKE TABLE OF lw_excel.
*  DATA: lw_bankinfo TYPE zdt_oa2sap_bp_bankinfo,
*        lw_but0id   TYPE zdt_oa2sap_bp_oa_but0id,
*        lw_lfm1     TYPE zdt_oa2sap_bp_oa_lfm1,
*        lw_lfb1     TYPE zdt_oa2sap_bp_oa_lfb1,
*        l_oaid      TYPE string.
*
*  REFRESH lt_excel.
*  CALL FUNCTION 'ZCOMM_EXCEL_UPLOAD'
*    EXPORTING
**     i_path  =
*      i_row_f = '5'
*      i_row_t = '60000'
*      i_col_f = '1'
*      i_col_t = '40'
*    TABLES
*      pt_up   = lt_excel.
*
*  DELETE lt_excel WHERE oaid = ''.
*  IF lt_excel[] IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  SORT lt_excel BY oaid.
*  READ TABLE lt_excel INTO lw_excel INDEX 1.
*  l_oaid = lw_excel-oaid.
*
*  LOOP AT lt_excel INTO lw_excel WHERE oaid = l_oaid.
*    IF c_input IS INITIAL.
*      MOVE-CORRESPONDING lw_excel TO c_input.
*    ENDIF.
*    CLEAR: lw_bankinfo, lw_but0id, lw_lfm1, lw_lfb1.
*    MOVE-CORRESPONDING lw_excel TO lw_bankinfo.
*    IF lw_bankinfo IS NOT INITIAL.
*      APPEND lw_bankinfo TO c_input-bankinfo.
*    ENDIF.
*
*    MOVE-CORRESPONDING lw_excel TO lw_but0id.
*    IF lw_but0id IS NOT INITIAL.
*      APPEND lw_but0id TO c_input-oa_but0id.
*    ENDIF.
*
*    MOVE-CORRESPONDING lw_excel TO lw_lfm1.
*    IF lw_lfm1 IS NOT INITIAL.
*      TRANSLATE lw_lfm1-sperm TO UPPER CASE.
*      APPEND lw_lfm1 TO c_input-oa_lfm1.
*    ENDIF.
*
*    MOVE-CORRESPONDING lw_excel TO lw_lfb1.
*    IF lw_lfb1 IS NOT INITIAL.
*      TRANSLATE lw_lfb1-loevm TO UPPER CASE.
*      APPEND lw_lfb1 TO c_input-oa_lfb1.
*    ENDIF.
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_IMPORT_CUSTOMER_FROM_EXCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- I_INPUT
*&---------------------------------------------------------------------*
FORM frm_import_customer_from_excel  CHANGING c_input TYPE zdt_oa2other_custom.

*  DATA: BEGIN OF lw_excel,
*          oaid            TYPE string,
*          mcflag          TYPE string,
*          grouping        TYPE string,
*          bpartner        TYPE string,
*          name1            TYPE string,
*          searchterm1      TYPE string,
*          searchterm2      TYPE string,
*          title_let        TYPE string,
*          street          TYPE string,
*          city1            TYPE string,
*          country          TYPE string,
*          langu            TYPE string,
*          region          TYPE string,
*          remark          TYPE string,
*          telf1            TYPE string,
*          extension        TYPE string,
*          telf2            TYPE string,
*          street_pi        TYPE string,
*          city1_pi        TYPE string,
*          country_pi      TYPE string,
*          langu_pi        TYPE string,
*          region_pi        TYPE string,
*          telf1_pi        TYPE string,
*          taxtype          TYPE string,
*          taxnumxl        TYPE string,
*          kukla            TYPE string,
*          aufsd            TYPE string,
*          taxkd            TYPE string,
*          bktyp            TYPE string,
*          banks            TYPE string,
*          bankl            TYPE string,
*          bankn            TYPE string,
*          type            TYPE string,
*          idnumber        TYPE string,
*          institute        TYPE string,
*          valid_date_from  TYPE string,
*          valid_date_to    TYPE string,
*          vkorg            TYPE string,
*          vtweg            TYPE string,
*          spart            TYPE string,
*          bzirk            TYPE string,
*          vkbur            TYPE string,
*          waers            TYPE string,
*          kalks            TYPE string,
*          vsbed            TYPE string,
*          incov            TYPE string,
*          inco1            TYPE string,
*          inco2_l          TYPE string,
*          inco3_l          TYPE string,
*          zterm            TYPE string,
*          kkber            TYPE string,
*          ktgrd            TYPE string,
*          sp_pernr        TYPE string,
*          bp_pernr        TYPE string,
*          py_pernr        TYPE string,
*          sh_pernr        TYPE string,
*          z1_pernr        TYPE string,
*          z2_pernr        TYPE string,
*          pernr            TYPE string,
*          kvgr1            TYPE string,
*          kvgr2            TYPE string,
*          bukrs            TYPE string,
*          akont            TYPE string,
*          limit_rule      TYPE string,
*          risk_class      TYPE string,
*          check_rule      TYPE string,
*          credit_sgmnt    TYPE string,
*          credit_limit    TYPE string,
*        END OF lw_excel,
*        lt_excel LIKE TABLE OF lw_excel.
*  DATA: lw_bankinfo      TYPE zdt_bankinfo,
*        lw_but0id        TYPE zdt_oa_but0id,
*        lw_knvv          TYPE zdt_knvv,
*        lw_knb1          TYPE zdt_oa_knb1,
*        lw_ukmbp_cms_sgm TYPE zdt_oa_ukmbp_cms_sgm,
*        l_oaid           TYPE string.
*
*  REFRESH lt_excel.
*  CALL FUNCTION 'ZCOMM_EXCEL_UPLOAD'
*    EXPORTING
**     i_path  =
*      i_row_f = '5'
*      i_row_t = '60000'
*      i_col_f = '1'
*      i_col_t = '70'
*    TABLES
*      pt_up   = lt_excel.
*
*  DELETE lt_excel WHERE oaid = ''.
*  IF lt_excel[] IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  SORT lt_excel BY oaid.
*  READ TABLE lt_excel INTO lw_excel INDEX 1.
*  l_oaid = lw_excel-oaid.
*
*  LOOP AT lt_excel INTO lw_excel WHERE oaid = l_oaid.
*    IF c_input IS INITIAL.
*      MOVE-CORRESPONDING lw_excel TO c_input.
*    ENDIF.
*    CLEAR: lw_bankinfo, lw_but0id, lw_knvv, lw_knb1, lw_ukmbp_cms_sgm.
*    MOVE-CORRESPONDING lw_excel TO lw_bankinfo.
*    IF lw_bankinfo IS NOT INITIAL.
*      APPEND lw_bankinfo TO c_input-bankinfo.
*    ENDIF.
*
*    MOVE-CORRESPONDING lw_excel TO lw_but0id.
*    IF lw_but0id IS NOT INITIAL.
*      APPEND lw_but0id TO c_input-oa_but0id.
*    ENDIF.
*
*    MOVE-CORRESPONDING lw_excel TO lw_knvv.
*    IF lw_knvv IS NOT INITIAL.
**      TRANSLATE lw_lfm1-sperm TO UPPER CASE.
*      APPEND lw_knvv TO c_input-oa_knvv.
*    ENDIF.
*
*    MOVE-CORRESPONDING lw_excel TO lw_knb1.
*    IF lw_knb1 IS NOT INITIAL.
**      TRANSLATE lw_lfb1-loevm TO UPPER CASE.
*      APPEND lw_knb1 TO c_input-oa_knb1.
*    ENDIF.
*    MOVE-CORRESPONDING lw_excel TO lw_ukmbp_cms_sgm.
*    IF lw_ukmbp_cms_sgm IS NOT INITIAL.
*      APPEND lw_ukmbp_cms_sgm TO c_input-oa_ukmbp_cms_sgm.
*    ENDIF.
*
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_BP_EXIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_INPUT_BPARTNER
*&      --> U_INPUT_MCFLAG
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_check_bp_exist  USING    u_bpartner
                                  u_mcflag
                         CHANGING c_type
                                  c_message.
  DATA: l_partner   TYPE but000-partner.

  SELECT SINGLE partner INTO l_partner FROM but000 WHERE partner = g_partner.
  IF u_mcflag = cns_mcflag_create AND sy-subrc = 0 OR
     u_mcflag = cns_mcflag_modify AND sy-subrc NE 0.
    c_type = 'E'.
    IF u_mcflag = cns_mcflag_create.
      c_message = l_partner && '已存在'.
    ELSEIF g_partner IS INITIAL.
      c_message = '请输入BP编号'.
    ELSE.
      c_message = u_bpartner && '不存在'.
    ENDIF.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_BP_GROUP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_INPUT_GROUPING
*&      --> U_INPUT_MCFLAG
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_check_bp_group  USING    u_grouping
                                  u_mcflag
                         CHANGING c_type
                                  c_message.
  DATA: l_externind     TYPE nriv-externind.

  IF u_mcflag = cns_mcflag_create.
    IF u_grouping = ''.
      c_type = 'E'.
      c_message = '请输入业务伙伴分组'.
      RETURN.
    ENDIF.
    SELECT SINGLE b~externind INTO l_externind
      FROM tb001 AS a
      INNER JOIN nriv AS b ON a~nrrng = b~nrrangenr
      WHERE a~bu_group = u_grouping
        AND b~object = 'BU_PARTNER'.
    IF sy-subrc NE 0.
      c_type = 'E'.
      CONCATENATE '业务伙伴分组：' u_grouping '不存在' INTO c_message.
      RETURN.
    ENDIF.
    IF l_externind = 'X' AND g_partner IS INITIAL.
      c_type = 'E'.
      c_message = '外部编号，请输入BP编号'.
      RETURN.
    ELSEIF l_externind = '' AND g_partner IS NOT INITIAL.
      c_type = 'E'.
      c_message = '内部编号，请不要输入BP编号'.
      RETURN.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_G_PARTNER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_INPUT_BPARTNER
*&---------------------------------------------------------------------*
FORM frm_get_g_partner  USING   u_bpartner.
  CLEAR: g_partner.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = u_bpartner
    IMPORTING
      output = g_partner.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CONVERT_STAMP_TO_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_BUT021_FS_VALID_FROM
*&      <-- L_VALID_FROM
*&---------------------------------------------------------------------*
FORM frm_convert_stamp_to_date  USING    u_stamp TYPE tzntstmps
                                CHANGING c_date TYPE datum.
  DATA: lv_tzone       TYPE adrc-time_zone.
  lv_tzone = 'UTC+8'.
  CONVERT TIME STAMP u_stamp
         TIME ZONE  lv_tzone
         INTO DATE  c_date.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_TAXNUMBER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> G_PARTNER
*&      --> U_INPUT_TAXTYPE
*&      --> U_INPUT_TAXNUMXL
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_check_taxnumber  USING    u_partner
                                   u_taxtype
                                   u_taxnumxl
                          CHANGING c_type
                                   c_message.
  DATA: l_partner TYPE but000-partner.

  "税码不能重复
  SELECT SINGLE partner INTO l_partner
    FROM dfkkbptaxnum
    WHERE partner NE u_partner
      AND taxtype = u_taxtype
      AND taxnum = u_taxnumxl."2021.4.22 Seashell 税号变更只会更新短的字段，长字段不进行更新，查询的时候实际上为空
*      AND ( taxnumxl = u_taxnumxl
*      OR taxnum = u_taxnumxl ).
  IF sy-subrc = 0.
    c_type = 'E'.
    c_message = '导入失败，已经存在BP：' && l_partner && '，税号：' && u_taxnumxl.
    RETURN.
  ENDIF.
ENDFORM.