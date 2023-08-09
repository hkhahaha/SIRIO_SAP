*----------------------------------------------------------------------*
***INCLUDE LZMM006F03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_BANKINFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BP_BANK_DETAILS
*&      --> LW_BP_BANKINFO
*&---------------------------------------------------------------------*
FORM frm_append_bankinfo  TABLES   et_bank_details STRUCTURE cvis_bp_bank_details
                          USING    ut_bankinfo TYPE zdt_oa2sap_bp_bankinfo_tab
                                   u_bpartner
                          CHANGING c_type
                                   c_message.

  DATA: lw_bankinfo        TYPE zdt_oa2sap_bp_bankinfo,
        lw_bp_bank_details TYPE  cvis_bp_bank_details.
  DATA: l_bpartner TYPE but000-partner.

  l_bpartner = u_bpartner.

  LOOP AT ut_bankinfo INTO lw_bankinfo.

    PERFORM frm_maintain_bank_md USING    lw_bankinfo-banka
                                          lw_bankinfo-banks
                                          lw_bankinfo-bankl
                                 CHANGING c_type
                                          c_message.
    IF c_type = 'E'.
      EXIT.
    ENDIF.

    PERFORM frm_set_bankinfo USING lw_bankinfo-bkvid
                                   lw_bankinfo-banks
                                   lw_bankinfo-bankl
                                   lw_bankinfo-bankn
                                   l_bpartner
                           CHANGING lw_bp_bank_details.
    IF lw_bp_bank_details IS NOT INITIAL .
      APPEND lw_bp_bank_details TO et_bank_details.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_BANKINFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_BANKINFO_BKVID
*&      --> LW_BANKINFO_BANKS
*&      --> LW_BANKINFO_BANKL
*&      --> LW_BANKINFO_BANKN
*&      --> U_BPARTNER
*&      --> U_MCFLAG
*&      <-- LW_BP_BANK_DETAILS
*&---------------------------------------------------------------------*
FORM frm_set_bankinfo  USING   u_bkvid
                                u_banks
                                u_bankl
                                u_bankn
                                u_bpartner TYPE but000-partner
                       CHANGING c_bp_bank_details TYPE cvis_bp_bank_details.

  DATA: ls_but0bk   TYPE but0bk,
        l_bank_acct TYPE cvis_bp_bank_details-bank_acct,
        l_bank_ref  TYPE cvis_bp_bank_details-bank_ref.

  PERFORM frm_split_bankn USING u_bankn
                       CHANGING l_bank_acct
                                l_bank_ref.

  IF u_bkvid IS NOT INITIAL.
    "“更新的时候，如果银行国家/账户/账号/参考账号都一样（只有序列号不一样），检查时报错
*    IF u_mcflag = 'M'.
*      SELECT SINGLE ('*') INTO ls_but0bk FROM but0bk
*        WHERE partner = u_bpartner
*          AND banks = u_banks
*          AND bankl = u_bankl
*          AND bankn = c_bp_bank_details-bank_acct
*          AND bkref = c_bp_bank_details-bank_ref.
*      IF sy-subrc NE 0.
*        c_bp_bank_details-data_key  = u_bkvid.
*        c_bp_bank_details-bank_ctry = u_banks.
*        c_bp_bank_details-bank_key = u_bankl.
*        c_bp_bank_details-bank_acct = l_bank_acct.
*        c_bp_bank_details-bank_ref = l_bank_ref.
*      ENDIF.
*    ELSE.
    c_bp_bank_details-data_key  = u_bkvid.
    c_bp_bank_details-bank_ctry = u_banks.
    c_bp_bank_details-bank_key = u_bankl.
    c_bp_bank_details-bank_acct = l_bank_acct.
    c_bp_bank_details-bank_ref = l_bank_ref.
*    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SPLIT_BANKN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_BANKN
*&      <-- L_BANK_ACCT
*&      <-- L_BANK_REF
*&---------------------------------------------------------------------*
FORM frm_split_bankn  USING    u_bankn
                      CHANGING c_bank_acct
                               c_bank_ref.
  DATA: l_len       TYPE i.

  l_len = strlen( u_bankn ).
  IF l_len GT 18.
    c_bank_acct = u_bankn(18).
    l_len = l_len - 18.
    c_bank_ref  = u_bankn+18(l_len).
  ELSE.
    c_bank_acct = u_bankn.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CHECK_BANKINFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_INPUT_BPARTNER
*&      --> LW_BANKINFO_BKVID
*&      --> LW_BANKINFO_BANKS
*&      --> LW_BANKINFO_BANKL
*&      --> L_BANK_ACCT
*&      --> L_BANK_REF
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_check_bankinfo  USING    u_bpartner
                                  u_bkvid
                                  u_banks
                                  u_bankl
                                  u_bank_acct
                                  u_bank_ref
                         CHANGING c_type
                                  c_message.
  DATA: ls_but0bk TYPE but0bk.

  SELECT SINGLE ('*') INTO ls_but0bk FROM but0bk
        WHERE partner = u_bpartner
          AND banks = u_banks
          AND bankl = u_bankl
          AND bankn = u_bank_acct
          AND bkref = u_bank_ref
          AND bkvid NE u_bkvid.
  IF sy-subrc = 0.
    c_type = 'E'.
    c_message = u_bank_acct && u_bank_ref.
    CONCATENATE '银行信息错误：' u_bkvid u_banks u_bankl c_message
      INTO c_message SEPARATED BY space.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_BP_NAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_BP_NAME1
*&      <-- LW_BP_GENERAL_NAME1
*&      <-- LW_BP_GENERAL_NAME2
*&      <-- LW_BP_GENERAL_NAME3
*&      <-- LW_BP_GENERAL_NAME4
*&---------------------------------------------------------------------*
FORM frm_set_bp_name  USING    u_name1
                      CHANGING c_name1 TYPE cvis_bp_general-name1
                               c_name2 TYPE cvis_bp_general-name2
                               c_name3 TYPE cvis_bp_general-name3
                               c_name4 TYPE cvis_bp_general-name4.

  DATA: l_len TYPE i.

  l_len = strlen( u_name1 ).
  IF l_len LE 40.
    c_name1 = u_name1.
  ELSEIF l_len GT 40 AND l_len LE 80.
    c_name1 = u_name1(40).
    l_len = l_len - 40.
    c_name2 = u_name1+40(l_len).
  ELSEIF l_len GT 80 AND l_len LE 120.
    c_name1 = u_name1(40).
    c_name2 = u_name1+40(40).
    l_len = l_len - 80.
    c_name3 = u_name1+80(l_len).
  ELSEIF l_len GT 120.
    c_name1 = u_name1(40).
    c_name2 = u_name1+40(40).
    c_name3 = u_name1+80(40).
    l_len = l_len - 120.
    c_name2 = u_name1+120(l_len).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_GET_MESSAGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CT_RETURN
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_get_message  TABLES   t_return STRUCTURE bapiret2
                      CHANGING c_message
                               c_type.
  DATA: lw_return LIKE LINE OF t_return,
        l_line    TYPE n.

  LOOP AT t_return INTO lw_return WHERE type = 'E' OR type = 'A'.
    c_type = 'E'.
    l_line = l_line + 1.
    IF c_message IS INITIAL.
      CONCATENATE l_line lw_return-message INTO c_message SEPARATED BY space.
    ELSEIF l_line LE 9.
      CONCATENATE c_message l_line lw_return-message INTO c_message SEPARATED BY space.
    ELSE.
      CONCATENATE c_message lw_return-message INTO c_message SEPARATED BY space.
    ENDIF.
  ENDLOOP.
  IF sy-subrc = 0.
    CONCATENATE c_message '。' INTO c_message.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_REMOVE_IDENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> G_PARTNER
*&      --> LW_BUT0ID_OLD_TYPE
*&      --> LW_BUT0ID_OLD_IDNUMBER
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_remove_ident  USING    u_partner
                                u_type
                                u_idnumber
                       CHANGING c_type
                                c_message.
  DATA: lt_return  TYPE STANDARD TABLE OF bapiret2,
        lw_return  TYPE bapiret2,
        l_partner  TYPE but000-partner,
        l_type     TYPE but0id-type,
        l_idnumber TYPE but0id-idnumber.

  IF  u_partner IS NOT INITIAL AND u_type IS NOT INITIAL AND
        u_idnumber IS NOT INITIAL.
    l_partner = u_partner.
    l_type = u_type.
    l_idnumber = u_idnumber.
  ELSE.
    c_type = 'E'.
    CONCATENATE c_message '移除技术ID错误' INTO c_message SEPARATED BY space.
    RETURN.
  ENDIF.
  CALL FUNCTION 'BAPI_IDENTIFICATION_REMOVE'
    EXPORTING
      businesspartner        = l_partner
      identificationcategory = l_type
      identificationnumber   = l_idnumber
    TABLES
      return                 = lt_return.

  LOOP AT  lt_return INTO lw_return  WHERE type CA 'EA'.
    c_type = 'E'.
    EXIT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_MAINTAIN_BANK_MD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_BANKINFO_BANKA
*&      --> LW_BANKINFO_BANKS
*&      --> LW_BANKINFO_BANKL
*&      <-- C_TYPE
*&      <-- C_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_maintain_bank_md  USING    u_banka
                                    u_banks
                                    u_bankl
                           CHANGING c_type
                                    c_message.
  DATA: lw_return    TYPE bapiret2,
        lw_bnka      TYPE bnka,
        lw_bank_addr TYPE bapi1011_address,
        l_banks      TYPE bapi1011_key-bank_ctry,
        l_bankl      TYPE bapi1011_key-bank_key,
        l_banka      TYPE bapi1011_address-bank_name.


  CLEAR: lw_return.
  l_banks = u_banks.
  l_bankl = u_bankl.
  l_banka = u_banka.

  SELECT SINGLE * INTO lw_bnka
      FROM bnka
     WHERE banks = l_banks
       AND bankl = l_bankl
      .
  IF sy-subrc NE 0.
    " 银行主记录不存在，创建
    lw_bank_addr-bank_name = l_banka.
    CALL FUNCTION 'BAPI_BANK_CREATE'
      EXPORTING
        bank_ctry    = l_banks
        bank_key     = l_bankl
        bank_address = lw_bank_addr
        i_xupdate    = ''
      IMPORTING
        return       = lw_return.
    IF lw_return-type = 'E'.
      c_message = lw_return-message.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
        .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      DO.
        SELECT SINGLE * INTO lw_bnka
          FROM bnka
          WHERE banks = l_banks
            AND bankl = l_bankl
            .
        IF sy-subrc IS INITIAL.
          EXIT.
        ELSE.
          WAIT UP TO '0.2' SECONDS.
        ENDIF.
      ENDDO.
    ENDIF.
  ELSE.

    IF lw_bnka-banka NE l_banka.
      "没有地址等信息，不更新银行主数据

    ENDIF.
  ENDIF.

ENDFORM.