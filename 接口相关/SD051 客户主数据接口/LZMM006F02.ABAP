*----------------------------------------------------------------------*
***INCLUDE LZMM006F02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_MAINTAIN_SUPPLY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_INPUT
*&      <-- E_OUTPUT
*&---------------------------------------------------------------------*
FORM frm_maintain_supply  USING    u_input  TYPE zdt_oa2sap_bp
                           CHANGING c_output TYPE  zdt_oa2sap_bp_ret.

  DATA:l_flag TYPE c.
  DATA:lt_bp_general            TYPE TABLE OF cvis_bp_general,
       lw_bp_general            TYPE  cvis_bp_general,
       lw_bp_role               TYPE  cvis_bp_role,
       lt_bp_role               TYPE TABLE OF cvis_bp_role,
       lt_bp_address            TYPE TABLE OF cvis_bp_address,
       lw_bp_address            TYPE  cvis_bp_address,
       lt_bp_address_teleno     TYPE TABLE OF cvis_bp_address_teleno,
       lw_bp_address_teleno     TYPE cvis_bp_address_teleno,
       lw_cvis_bp_address_email TYPE cvis_bp_address_email,
       lt_cvis_bp_address_email TYPE TABLE OF cvis_bp_address_email,
       lt_bp_bank_details       TYPE TABLE OF cvis_bp_bank_details,
       lt_sup_company           TYPE TABLE OF cvis_supplier_company,
       lt_cust_company          TYPE TABLE OF cvis_customer_company,
       ct_return                TYPE TABLE OF cvis_bp_return,
       ct_return1               TYPE TABLE OF cvis_bp_return,
       lt_return                TYPE TABLE OF bapiret2,
       lw_return                TYPE bapiret2,
       cw_return                TYPE cvis_bp_return.
  DATA:lt_cust_general TYPE TABLE OF cvis_customer_general,
       lw_cust_general TYPE cvis_customer_general.
  DATA:lw_sup_general TYPE cvis_supplier_general,
       lt_sup_general TYPE TABLE OF cvis_supplier_general.
  DATA:lt_cvis_bp_tax_number TYPE TABLE OF cvis_bp_tax_number,
       lw_cvis_bp_tax_number TYPE cvis_bp_tax_number.
  DATA: l_msg TYPE bapi_msg.
*  DATA:gw_msg TYPE bdcmsgcoll,
*       gt_msg TYPE TABLE OF bdcmsgcoll.
*  DATA l_mode.
*  DATA:p_object TYPE REF TO cl_md_bp_maintain,
*       e_return TYPE mdg_bs_bp_msgmap_t.
*  DATA:p_msgcl TYPE REF TO cx_ai_system_fault.
*  DATA:p_msg TYPE string.
*  DATA:bus_ei_instance TYPE bus_ei_instance,
*       bus_ei_header   TYPE bus_ei_header,
*       bus_ei_extern   TYPE bus_ei_extern.
*  DATA:bus_ei_bupa_roles   TYPE bus_ei_bupa_roles,
*       zbus_ei_bupa_roles  TYPE TABLE OF bus_ei_bupa_roles,
*       bus_ei_roles        TYPE bus_ei_roles,
*       bus_ei_central_data TYPE bus_ei_central_data.
*  DATA:cmds_ei_instance TYPE cmds_ei_instance,
*       cmds_ei_header   TYPE cmds_ei_header.
  DATA:l_partner TYPE but000-partner.


  DATA: lw_bp TYPE zdt_oa2sap_bp.

  DATA: lt_remarks   TYPE TABLE OF cvis_address_telno_remarks,
        lw_remarks   TYPE cvis_address_telno_remarks,
        lt_sup_ekorg TYPE TABLE OF cvis_supplier_purchasing,
        lw_sup_ekorg TYPE cvis_supplier_purchasing,
        lt_sup_text  TYPE TABLE OF cvis_supplier_porg_texts,
        lt_sup_text1 TYPE TABLE OF cvis_supplier_porg_texts,
        lw_sup_text  TYPE cvis_supplier_porg_texts,
        lt_bp_ident  TYPE TABLE OF cvis_bp_ident_numbers,
        lw_bp_ident  TYPE cvis_bp_ident_numbers.
  DATA: lt_ekorg TYPE tt_ekorg,
        lt_bukrs TYPE tt_bukrs.

  REFRESH: lt_ekorg, lt_bukrs.
  CLEAR: lw_bp.
  MOVE-CORRESPONDING u_input TO lw_bp.

  IF lw_bp-bankinfo[] IS NOT INITIAL.
    PERFORM frm_append_bankinfo TABLES lt_bp_bank_details
                                USING  lw_bp-bankinfo
                                       g_partner
                                CHANGING c_output-type
                                         c_output-message.
    IF c_output-type = 'E'.
      RETURN.
    ENDIF.
  ENDIF.


  IF lw_bp-oa_lfm1 IS NOT INITIAL.
    PERFORM frm_append_ekorg TABLES lt_sup_ekorg
                                    lt_sup_text
                                    lt_sup_text1
                                    lt_ekorg
                             USING  lw_bp-oa_lfm1.

  ENDIF.

  IF lw_bp-oa_lfb1 IS NOT INITIAL.
    PERFORM frm_append_bukrs TABLES lt_sup_company
                                    lt_bukrs
                             USING  lw_bp-oa_lfb1.

  ENDIF.


  IF lw_bp-oa_but0id IS NOT INITIAL.
    PERFORM frm_append_but0id TABLES lt_bp_ident
                              USING  lw_bp-oa_but0id.
  ENDIF.

  PERFORM frm_append_general TABLES lt_bp_general
                             USING  lw_bp.
  PERFORM frm_append_address TABLES lt_bp_address
                             USING  lw_bp.

  PERFORM frm_append_teleno TABLES lt_bp_address_teleno
                            USING lw_bp.

  IF lw_bp-e_mail IS NOT INITIAL AND g_mcflag = cns_mcflag_create.

*          lw_cvis_bp_address_email-E_MAIL = lw_bp-SMTP_ADDR.
*          APPEND lw_cvis_bp_address_email TO lt_cvis_bp_address_email.
*          CLEAR lw_cvis_bp_address_email.
    PERFORM frm_append_email TABLES lt_cvis_bp_address_email  "输入参数
                             USING  g_mcflag
                                    lw_bp-e_mail
                                    lw_bp-bpartner.

  ENDIF.

  PERFORM frm_append_tax_number TABLES lt_cvis_bp_tax_number
                                USING lw_bp-taxtype lw_bp-taxnumxl.

*  BUPA_TAX_ADD "税码需单独添加，CN0会变成CN5
  CASE g_mcflag .
    WHEN cns_mcflag_create.
      IF lt_sup_ekorg IS NOT INITIAL.
        lw_bp_role-data_key = cns_bp_role_po.
        APPEND lw_bp_role TO lt_bp_role .
        lw_sup_general-object_task = 'I'.

*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              INPUT  = lw_bp-VBUND
*            IMPORTING
*              OUTPUT = lw_bp-VBUND.

*        lw_sup_general-vbund = lw_bp-vbund.
*        lw_sup_general-stenr = lw_bp-stenr.
        APPEND lw_sup_general TO lt_sup_general.
        CLEAR lw_sup_general.
      ENDIF.
      IF lt_sup_company IS NOT INITIAL.
        lw_bp_role-data_key = cns_bp_role_comp.
        APPEND lw_bp_role TO lt_bp_role .
        lw_sup_general-object_task = 'I'.


        APPEND lw_sup_general TO lt_sup_general.
        CLEAR lw_sup_general.
      ENDIF.


      CALL FUNCTION 'RFC_CVI_EI_INBOUND_MAIN'
        EXPORTING
          iv_docommit             = 'X'
          iv_create_applog        = 'X'
*         IV_SUPPRESS_TAXJUR_CHECK     =
        TABLES
          it_bp_general           = lt_bp_general
          it_bp_role              = lt_bp_role
*         IT_BP_INDUSTRY          =
*         it_bp_ident_numbers     = lt_bp_ident
          it_bp_bank_details      = lt_bp_bank_details
*         it_bp_tax_number        = lt_cvis_bp_tax_number "调用其他函数修改，LBUPA_INTERFACE_SERVICEF13中130行起会自动把CN0变成CN5.
*         IT_BP_TAX_NUMBER_COMMON =
          it_bp_address           = lt_bp_address
*         IT_BP_ADDRESS_USAGE     =
          it_bp_address_teleno    = lt_bp_address_teleno
*         it_bp_address_teleno_remarks = lt_remarks
*         IT_BP_ADDRESS_FAXNO     =
          it_bp_address_email     = lt_cvis_bp_address_email
*         IT_BP_ADDRESS_EMAIL_REMARKS  =
*         IT_BP_ADDRESS_URI       =
*         IT_BP_RELATIONS         =
*         IT_BP_FINSERV           =
          it_cust_general         = lt_cust_general
*         IT_CUST_GENERAL_CREDITCARD   =
*         IT_CUST_GENERAL_TEXTS   =
*         IT_CUST_GENERAL_LOADING =
*         IT_CUST_GENERAL_EXPORT  =
*         IT_CUST_GENERAL_ALT_PAYEE    =
          it_cust_company         = lt_cust_company
*         IT_CUST_COMPANY_WTAX    =
*         IT_CUST_COMPANY_TEXTS   =
*         IT_CUST_COMPANY_ALT_PAYEE    =
*         IT_CUST_SALES           =
*         IT_CUST_SALES_TEXTS     =
*         IT_CUST_COMP_DUNNING    =
*         IT_CUST_SALES_FUNCTIONS =
*         IT_CUST_TAX_INDICATOR   =
*         IT_CUST_CONTACTS        =
*         IT_CUST_CONT_ADDRESS1   =
*         IT_CUST_CONT_ADDRESS2   =
*         IT_CUST_CONT_ADDRESS3   =
*         IT_CUST_CONT_TELENO1    =
*         IT_CUST_CONT_TELENO2    =
*         IT_CUST_CONT_TELENO3    =
*         IT_CUST_CONT_FAX1       =
*         IT_CUST_CONT_FAX2       =
*         IT_CUST_CONT_FAX3       =
*         IT_CUST_CONT_EMAIL1     =
*         IT_CUST_CONT_EMAIL2     =
*         IT_CUST_CONT_EMAIL3     =
*         IT_CUST_CONT_TEXTS      =
*         IT_CUST_GENERAL_VAT     =
*         IT_SUP_GENERAL_ALT_PAYEE     =
          it_sup_general          = lt_sup_general
*         IT_SUP_GENERAL_TEXTS    =
          it_sup_company          = lt_sup_company
*         IT_SUP_COMPANY_TEXTS    =
*         IT_SUP_COMPANY_WTAX     =
*         IT_SUP_COMPANY_ALT_PAYEE     =
          it_sup_purchasing_texts = lt_sup_text
          it_sup_purchasing       = lt_sup_ekorg
*         IT_SUP_PURCHASING2      =
*         IT_SUP_PURCH_FUNCTIONS  =
*         IT_SUPPLIER_CONTACTS    =
*         IT_SUP_CONT_ADDRESS1    =
*         IT_SUP_CONT_ADDRESS2    =
*         IT_SUP_CONT_ADDRESS3    =
*         IT_SUP_CONT_TELENO1     =
*         IT_SUP_CONT_TELENO2     =
*         IT_SUP_CONT_TELENO3     =
*         IT_SUP_CONT_FAX1        =
*         IT_SUP_CONT_FAX2        =
*         IT_SUP_CONT_FAX3        =
*         IT_SUP_CONT_EMAIL1      =
*         IT_SUP_CONT_EMAIL2      =
*         IT_SUP_CONT_EMAIL3      =
*         IT_SUP_GENERAL_VAT      =
          ct_return               = ct_return.


      READ TABLE ct_return INTO cw_return WITH KEY type = 'S'.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        IF cw_return-object_key IS NOT INITIAL.
          g_partner = cw_return-object_key.
          CONCATENATE   cw_return-object_key '创建成功' INTO l_msg.
          c_output-message = l_msg.
          c_output-partner = cw_return-object_key.
          c_output-type = 'S'.

          CLEAR: lw_bp_address.
          READ TABLE lt_bp_address INTO lw_bp_address INDEX 1.
          PERFORM frm_update_remark USING g_partner lw_bp-remark lw_bp_address-langu
                              CHANGING c_output-type c_output-message.
          PERFORM frm_update_bp_after_maintain TABLES   lt_bp_ident lt_cvis_bp_tax_number lt_sup_text1
                                                        lt_ekorg lt_bukrs
                                               USING    g_partner g_mcflag lw_bp-remark
                                               CHANGING c_output-type c_output-message.
*          partner = cw_return-object_key.
*          IF  lw_bp-comp_head  IS NOT INITIAL. "公司注册号
*            UPDATE bp001 SET comp_head = lw_bp-comp_head WHERE partner = partner.
*          ELSEIF lw_bp-comp_head  IS  INITIAL AND lw_bp-comp_headc IS NOT INITIAL. "公司注册号
*            UPDATE bp001 SET comp_head = lw_bp-comp_headc WHERE partner = partner.
*          ENDIF.
*          IF   lw_bp-vbund  IS NOT INITIAL. "公司注册号
**                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
**                EXPORTING
**                  INPUT  = lw_bp-VBUND
**                IMPORTING
**                  OUTPUT = lw_bp-VBUND.
*            UPDATE bp001 SET vbund = lw_bp-vbund WHERE partner = partner.
*          ELSEIF lw_bp-vbund  IS  INITIAL AND lw_bp-vbundc  IS NOT INITIAL. "公司注册号
**                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
**                EXPORTING
**                  INPUT  = lw_bp-VBUNDC
**                IMPORTING
**                  OUTPUT = lw_bp-VBUNDC.
*            UPDATE bp001 SET vbund = lw_bp-vbundc WHERE partner = partner.
*          ENDIF.
*          IF lw_bp-vbund  IS NOT INITIAL.
*            UPDATE lfa1 SET  vbund = lw_bp-vbund WHERE lifnr = lw_bp-bpartner.
*          ENDIF.
*          IF  lw_bp-vbundc  IS NOT INITIAL.
*            UPDATE kna1 SET  vbund = lw_bp-vbundc WHERE kunnr = lw_bp-bpartner.
*          ENDIF.

        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        REFRESH lt_return.
        LOOP AT ct_return INTO cw_return.
          CLEAR: lw_return.
          MOVE-CORRESPONDING cw_return TO lw_return.
          APPEND lw_return TO lt_return.
        ENDLOOP.
        PERFORM frm_get_message TABLES   lt_return
                                CHANGING c_output-message
                                         c_output-type.
      ENDIF.
*      ENDIF.

      WAIT UP TO 1 SECONDS.

    WHEN cns_mcflag_modify.

      " IT_CUST_GENERAL不传值则会报客户已存在错误
*        IF lt_sup_company IS NOT INITIAL.

      lw_sup_general-lifnr = g_partner.
      lw_sup_general-object_task = 'U'.
*                        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*                EXPORTING
*                  INPUT  = lw_bp-VBUND
*                IMPORTING
*                  OUTPUT = lw_bp-VBUND.
*        lw_sup_general-vbund = lw_bp-vbund.
*        lw_sup_general-stenr = lw_bp-stenr.
      APPEND lw_sup_general TO lt_sup_general.


      IF lt_sup_ekorg IS NOT INITIAL.
        lw_bp_role-data_key = cns_bp_role_po.
        APPEND lw_bp_role TO lt_bp_role .

        lw_sup_general-lifnr = g_partner.
        lw_sup_general-object_task = 'U'.

        APPEND lw_sup_general TO lt_sup_general.
        CLEAR lw_sup_general.
      ENDIF.
      IF lt_sup_company IS NOT INITIAL.
        lw_bp_role-data_key = cns_bp_role_comp.
        APPEND lw_bp_role TO lt_bp_role .
        lw_sup_general-object_task = 'U'.
        lw_sup_general-lifnr = g_partner.
        APPEND lw_sup_general TO lt_sup_general.
        CLEAR lw_sup_general.
      ENDIF.

      "直接改表，不报错
      IF lw_bp-region IS INITIAL.
        PERFORM frm_update_region USING g_partner lw_bp-country.
      ENDIF.
      PERFORM frm_update_bankinfo TABLES   lt_bp_bank_details
                                  USING    g_partner
                                  CHANGING c_output-type c_output-message.
      IF c_output-type = 'E'.
        RETURN.
      ENDIF.

      CALL FUNCTION 'RFC_CVI_EI_INBOUND_MAIN'
        EXPORTING
          iv_docommit        = 'X'
*         IV_CREATE_APPLOG   = '-'
*         IV_SUPPRESS_TAXJUR_CHECK     =
        TABLES
          it_bp_general      = lt_bp_general
          it_bp_role         = lt_bp_role
*         IT_BP_INDUSTRY     =
*         it_bp_ident_numbers          = lt_bp_ident
          it_bp_bank_details = lt_bp_bank_details
*         it_bp_tax_number   = lt_cvis_bp_tax_number
*         IT_BP_TAX_NUMBER_COMMON      =
*         it_bp_address      = lt_bp_address
*         IT_BP_ADDRESS_USAGE          =
*         it_bp_address_teleno         = lt_bp_address_teleno
*         it_bp_address_teleno_remarks = lt_remarks
*         IT_BP_ADDRESS_FAXNO          =
*         it_bp_address_email          = lt_cvis_bp_address_email
*         IT_BP_ADDRESS_EMAIL_REMARKS  =
*         IT_BP_ADDRESS_URI  =
*         IT_BP_RELATIONS    =
*         IT_BP_FINSERV      =
          it_cust_general    = lt_cust_general
*         IT_CUST_GENERAL_CREDITCARD   =
*         IT_CUST_GENERAL_TEXTS        =
*         IT_CUST_GENERAL_LOADING      =
*         IT_CUST_GENERAL_EXPORT       =
*         IT_CUST_GENERAL_ALT_PAYEE    =
          it_cust_company    = lt_cust_company
*         IT_CUST_COMPANY_WTAX         =
*         IT_CUST_COMPANY_TEXTS        =
*         IT_CUST_COMPANY_ALT_PAYEE    =
*         IT_CUST_SALES      =
*         IT_CUST_SALES_TEXTS          =
*         IT_CUST_COMP_DUNNING         =
*         IT_CUST_SALES_FUNCTIONS      =
*         IT_CUST_TAX_INDICATOR        =
*         IT_CUST_CONTACTS   =
*         IT_CUST_CONT_ADDRESS1        =
*         IT_CUST_CONT_ADDRESS2        =
*         IT_CUST_CONT_ADDRESS3        =
*         IT_CUST_CONT_TELENO1         =
*         IT_CUST_CONT_TELENO2         =
*         IT_CUST_CONT_TELENO3         =
*         IT_CUST_CONT_FAX1  =
*         IT_CUST_CONT_FAX2  =
*         IT_CUST_CONT_FAX3  =
*         IT_CUST_CONT_EMAIL1          =
*         IT_CUST_CONT_EMAIL2          =
*         IT_CUST_CONT_EMAIL3          =
*         IT_CUST_CONT_TEXTS =
*         IT_CUST_GENERAL_VAT          =
*         IT_SUP_GENERAL_ALT_PAYEE     =
          it_sup_general     = lt_sup_general
*         IT_SUP_GENERAL_TEXTS         =
          it_sup_company     = lt_sup_company
*         IT_SUP_COMPANY_TEXTS         =
*         IT_SUP_COMPANY_WTAX          =
*         IT_SUP_COMPANY_ALT_PAYEE     =
*         it_sup_purchasing_texts      = lt_sup_text
          it_sup_purchasing  = lt_sup_ekorg
*         IT_SUP_PURCHASING2 =
*         IT_SUP_PURCH_FUNCTIONS       =
*         IT_SUPPLIER_CONTACTS         =
*         IT_SUP_CONT_ADDRESS1         =
*         IT_SUP_CONT_ADDRESS2         =
*         IT_SUP_CONT_ADDRESS3         =
*         IT_SUP_CONT_TELENO1          =
*         IT_SUP_CONT_TELENO2          =
*         IT_SUP_CONT_TELENO3          =
*         IT_SUP_CONT_FAX1   =
*         IT_SUP_CONT_FAX2   =
*         IT_SUP_CONT_FAX3   =
*         IT_SUP_CONT_EMAIL1 =
*         IT_SUP_CONT_EMAIL2 =
*         IT_SUP_CONT_EMAIL3 =
*         IT_SUP_GENERAL_VAT =
          ct_return          = ct_return.


*        READ TABLE CT_RETURN INTO CW_RETURN WITH KEY TYPE = 'S'.
*        IF SY-SUBRC = 0.
      REFRESH lt_return.
      LOOP AT ct_return INTO cw_return.
        CLEAR: lw_return.
        MOVE-CORRESPONDING cw_return TO lw_return.
        APPEND lw_return TO lt_return.
      ENDLOOP.
      PERFORM frm_get_message TABLES   lt_return
                              CHANGING c_output-message
                                       c_output-type.

      IF c_output-type NE 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

*          IF CW_RETURN-OBJECT_KEY IS NOT INITIAL.
*            CONCATENATE   CW_RETURN-OBJECT_KEY '修改成功' INTO c_output-message.
*            PARTNER = CW_RETURN-OBJECT_KEY.
        l_partner = lw_bp-bpartner.
        CONCATENATE l_partner '修改成功' INTO c_output-message.
        c_output-type = 'S'.
        c_output-partner = g_partner.
        PERFORM frm_update_address TABLES   lt_bp_address lt_bp_address_teleno
                                   USING    lw_bp
                                   CHANGING c_output-type c_output-message.
*        PERFORM frm_update_bankinfo TABLES   lt_bp_bank_details
*                                    USING    g_partner
*                                    CHANGING c_output-type c_output-message.
        PERFORM frm_update_bp_after_maintain TABLES   lt_bp_ident lt_cvis_bp_tax_number lt_sup_text1
                                                      lt_ekorg lt_bukrs
                                             USING    g_partner g_mcflag lw_bp-remark
                                             CHANGING c_output-type c_output-message.
        PERFORM frm_update_sales TABLES   lt_sup_text
                                 USING    g_partner
                                 CHANGING c_output-type c_output-message.
*          IF  lw_bp-comp_head  IS NOT INITIAL. "公司注册号
*            UPDATE bp001 SET comp_head = lw_bp-comp_head WHERE partner = partner.
*          ELSEIF lw_bp-comp_head  IS  INITIAL AND lw_bp-comp_headc IS NOT INITIAL. "公司注册号
*            UPDATE bp001 SET comp_head = lw_bp-comp_headc WHERE partner = partner.
*          ENDIF.
*          IF   lw_bp-vbund  IS NOT INITIAL. "公司注册号
**              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
**                EXPORTING
**                  INPUT  = lw_bp-VBUND
**                IMPORTING
**                  OUTPUT = lw_bp-VBUND.
*
*            UPDATE bp001 SET vbund = lw_bp-vbund WHERE partner = partner.
*          ELSEIF lw_bp-vbund  IS  INITIAL AND lw_bp-vbundc  IS NOT INITIAL. "公司注册号
**              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
**                EXPORTING
**                  INPUT  = lw_bp-VBUNDC
**                IMPORTING
**                  OUTPUT = lw_bp-VBUNDC.
*            UPDATE bp001 SET vbund = lw_bp-vbundc WHERE partner = partner.
*          ENDIF.

*          IF lw_bp-vbund  IS NOT INITIAL.
*            UPDATE lfa1 SET  vbund = lw_bp-vbund WHERE lifnr = lw_bp-bpartner.
*          ENDIF.
*          IF  lw_bp-vbundc  IS NOT INITIAL.
*            UPDATE kna1 SET  vbund = lw_bp-vbundc WHERE kunnr = lw_bp-bpartner.
*          ENDIF.

*          ENDIF.

*          "如果勾选了update复选框，则判断导入的name2到name4以及BKREF是否为空，为空则删除原有的值
*          "因急用，先不更改函数，后续修改的时候，可改用CVI_EI_INBOUND_MAIN函数一次完成更新
*          IF p_update = 'X'.
*            PERFORM frm_update_bp_name USING lw_bp-bpartner lw_bp-name2
*                                             lw_bp-name3 lw_bp-name4
*                                      CHANGING c_output-message.
*          ENDIF.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*          READ TABLE CT_RETURN INTO CW_RETURN WITH KEY TYPE = 'E' .
*          IF SY-SUBRC = 0.
*            c_output-message = CW_RETURN-MESSAGE.
*            lw_bp-STATUS = ICON_LED_RED.
*          ELSE.
*            READ TABLE CT_RETURN INTO CW_RETURN WITH KEY TYPE = 'A' .
*            IF SY-SUBRC = 0.
*              c_output-message = CW_RETURN-MESSAGE.
*              lw_bp-STATUS = ICON_LED_RED.
*            ENDIF.
*          ENDIF.

      ENDIF.

  ENDCASE.

  CLEAR: lt_bp_general,ct_return,lt_sup_company,lt_cust_company,lt_bp_role,lt_bp_bank_details,
  lt_bp_address,lt_bp_address_teleno,lt_cvis_bp_tax_number,lt_cvis_bp_address_email,l_partner,cw_return,lw_bp_role.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_EMAIL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> lt_cvis_bp_address_email
*&      --> lw_bp_ZZMC
*&      --> lw_bp_SMTP_ADDR
*&      --> lw_bp_BU_PARTNER
*&---------------------------------------------------------------------*
FORM frm_append_email  TABLES   pt_cvis_bp_address_email STRUCTURE cvis_bp_address_email
                       USING    ps_zzmc
                                ps_smtp_addr "TYPE sza1_d0100-smtp_addr
                                ps_partner. "TYPE but000-partner.
  DATA: ls_adr6                  LIKE adr6,
        lt_adr6                  LIKE TABLE OF adr6,
        ls_adr6_1                LIKE adr6,
        lt_adr6_1                LIKE TABLE OF adr6,
        ls_cvis_bp_address_email LIKE LINE OF pt_cvis_bp_address_email.
  DATA: ls_adrnr LIKE lfa1-adrnr.

  REFRESH: gt_email, lt_adr6, lt_adr6_1, pt_cvis_bp_address_email.

  SPLIT ps_smtp_addr AT cns_sep INTO TABLE gt_email.
  SORT gt_email.
  DELETE ADJACENT DUPLICATES FROM gt_email COMPARING ALL FIELDS.
  IF ps_zzmc = cns_mcflag_modify.
    CLEAR: ls_adrnr.
    SELECT SINGLE addrnumber INTO ls_adrnr FROM but020 WHERE partner = ps_partner.
    PERFORM frm_append_adr6 TABLES lt_adr6_1 lt_adr6 USING ls_adrnr.

    SORT lt_adr6   BY smtp_addr consnumber.
    SORT lt_adr6_1 BY addrnumber consnumber.
    LOOP AT lt_adr6 INTO ls_adr6.
      READ TABLE lt_adr6_1 INTO ls_adr6_1 WITH KEY addrnumber = ls_adr6-addrnumber
          consnumber = ls_adr6-consnumber BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE lt_adr6_1 INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
    IF lt_adr6_1 IS NOT INITIAL.
      "先删除修改后不存在的邮箱地址
      PERFORM frm_delete_email TABLES lt_adr6_1
                               USING  ps_partner.
    ENDIF.
  ENDIF.
  LOOP AT gt_email INTO gs_email.
    CLEAR ls_cvis_bp_address_email.
    READ TABLE lt_adr6 INTO ls_adr6 WITH KEY smtp_addr = gs_email-smtp_addr BINARY SEARCH.
    IF  sy-subrc = 0.
      "已存在,不修改.
      CONTINUE.
    ENDIF.
    ls_cvis_bp_address_email-e_mail = gs_email-smtp_addr.
    APPEND ls_cvis_bp_address_email TO pt_cvis_bp_address_email.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_ADR6
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ADR6_1
*&      --> LT_ADR6
*&---------------------------------------------------------------------*
FORM frm_append_adr6  TABLES   pt_adr6_1 STRUCTURE adr6
                               pt_adr6 STRUCTURE adr6
                      USING    ps_adrnr TYPE adr6-addrnumber.
  DATA: lt_adr6 TYPE TABLE OF adr6.

  REFRESH: lt_adr6.
  SELECT * INTO TABLE lt_adr6 FROM adr6 WHERE addrnumber = ps_adrnr.
  APPEND LINES OF lt_adr6 TO pt_adr6_1.
  IF  ps_adrnr IS NOT INITIAL AND gt_email IS NOT INITIAL.
    REFRESH: lt_adr6.
    SELECT * INTO TABLE lt_adr6 FROM adr6
      FOR ALL ENTRIES IN gt_email
      WHERE addrnumber = ps_adrnr
        AND smtp_addr = gt_email-smtp_addr.
    APPEND LINES OF lt_adr6 TO pt_adr6.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DELETE_EMAIL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ADR6_1
*&---------------------------------------------------------------------*
FORM frm_delete_email  TABLES   pt_adr6 STRUCTURE adr6
                       USING    ps_partner TYPE but000-partner.
  DATA: ls_adr6 TYPE adr6.
  DATA lt_return TYPE bapiret2 OCCURS 0 WITH HEADER LINE.
  DATA email TYPE TABLE OF bapiadsmtp WITH HEADER LINE.
  DATA email_x TYPE TABLE OF bapiadsmtx WITH HEADER LINE.
  DATA: ls_error TYPE c.

  LOOP AT pt_adr6 INTO ls_adr6.
    email-e_mail = ls_adr6-smtp_addr.
    email-std_no = 'X'.
    APPEND email.

    email_x-std_no = 'X'.
    email_x-e_mail = 'X'.
    email_x-updateflag = 'D'.
    APPEND email_x.
  ENDLOOP.

  CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
    EXPORTING
      businesspartner = ps_partner
    TABLES
      bapiadsmtp      = email
      bapiadsmt_x     = email_x
      return          = lt_return.

  CLEAR: ls_error.
  LOOP AT lt_return WHERE type = 'E' OR type = 'A'.
    ls_error = 'E'.
  ENDLOOP.
  IF  ls_error IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATE_REGION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_update_region USING u_bpartner u_country.
  DATA: ls_adrc  TYPE adrc,
        ls_adrnr TYPE lfa1-adrnr.

  CLEAR: ls_adrnr, ls_adrc.
  SELECT SINGLE addrnumber INTO ls_adrnr
    FROM but020
    WHERE partner = u_bpartner.

  SELECT SINGLE * INTO ls_adrc FROM adrc WHERE addrnumber = ls_adrnr.

  IF ls_adrc IS NOT INITIAL.
    IF u_country NE ls_adrc-country AND ls_adrc-region IS NOT INITIAL.
      CLEAR: ls_adrc-region.
      UPDATE adrc FROM ls_adrc.
      SELECT SINGLE adrnr INTO ls_adrnr FROM lfa1 WHERE lifnr = u_bpartner.
      IF sy-subrc = 0.
        UPDATE adrc SET region = '' WHERE addrnumber = ls_adrnr.
      ENDIF.
      SELECT SINGLE adrnr INTO ls_adrnr FROM kna1 WHERE kunnr = u_bpartner.
      IF sy-subrc = 0.
        UPDATE adrc SET region = '' WHERE addrnumber = ls_adrnr.
      ENDIF.
      COMMIT WORK AND WAIT .
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_EKORG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUP_EKORG
*&      --> LT_SUP_TEXT
*&      --> LW_BP_OA_LFM1
*&---------------------------------------------------------------------*
FORM frm_append_ekorg  TABLES  t_sup_ekorg STRUCTURE cvis_supplier_purchasing
                                t_sup_text  STRUCTURE cvis_supplier_porg_texts
                                t_sup_text1 STRUCTURE cvis_supplier_porg_texts
                                t_ekorg TYPE tt_ekorg
                       USING    ut_bp_lfm1  TYPE zdt_oa2sap_bp_oa_lfm1_tab.

  DATA: lw_sup_ekorg TYPE cvis_supplier_purchasing,
        lw_sup_text  TYPE cvis_supplier_porg_texts,
        lw_bp_lfm1   TYPE zdt_oa2sap_bp_oa_lfm1,
        lw_ekorg     LIKE LINE OF t_ekorg.

  LOOP AT ut_bp_lfm1 INTO lw_bp_lfm1.
    CLEAR lw_sup_ekorg.
    lw_sup_ekorg-ekorg = lw_bp_lfm1-ekorg.
    lw_sup_ekorg-waers = lw_bp_lfm1-waers.
    lw_sup_ekorg-verkf = lw_bp_lfm1-verkf.
    lw_sup_ekorg-ekgrp = lw_bp_lfm1-ekgrp.
    lw_sup_ekorg-webre = 'X'. "默认勾选
    lw_sup_ekorg-sperm = lw_bp_lfm1-sperm.
    lw_sup_ekorg-zterm = lw_bp_lfm1-m1_zterm.

    IF lw_sup_ekorg-sperm IS INITIAL.  "从X改为空，需要单独调用BAPI
      CLEAR: lw_ekorg.
      SELECT SINGLE sperm INTO lw_ekorg-sperm
        FROM lfm1
        WHERE ekorg = lw_sup_ekorg-ekorg
          AND lifnr = g_partner
        .
      IF lw_ekorg-sperm  IS NOT INITIAL.
        CLEAR lw_ekorg-sperm.
        lw_ekorg-ekorg = lw_sup_ekorg-ekorg.
        APPEND lw_ekorg TO t_ekorg.
      ENDIF.
    ENDIF.
    APPEND lw_sup_ekorg TO t_sup_ekorg.
    IF lw_bp_lfm1-sales IS NOT INITIAL.
      CLEAR: lw_sup_text.
      lw_sup_text-ekorg = lw_bp_lfm1-ekorg.
      lw_sup_text-langu = '1'.  "固定中文
      lw_sup_text-text_id = cns_tdid_sales.
      lw_sup_text-tdline = lw_bp_lfm1-sales.
      "传多个文本时，会dump，多个采购组织都有业务员时，第一次只传一个，其余的另外调bapi处理
      IF t_sup_text[] IS INITIAL.
        APPEND lw_sup_text TO t_sup_text.
      ELSE.
        APPEND lw_sup_text TO t_sup_text1.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_BUKRS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUP_COMPANY
*&      --> LW_BP_OA_LFB1
*&---------------------------------------------------------------------*
FORM frm_append_bukrs  TABLES   t_sup_company STRUCTURE cvis_supplier_company
                                 t_bukrs TYPE tt_bukrs
                       USING    ut_bp_lfb1 TYPE zdt_oa2sap_bp_oa_lfb1_tab.

  DATA: lw_bp_lfb1     TYPE zdt_oa2sap_bp_oa_lfb1,
        lw_sup_company TYPE cvis_supplier_company,
        lw_bukrs       LIKE LINE OF t_bukrs.

  LOOP AT ut_bp_lfb1 INTO lw_bp_lfb1.

    lw_sup_company-bukrs  = lw_bp_lfb1-bukrs . "输入参数
    lw_sup_company-akont  = lw_bp_lfb1-akont.
    lw_sup_company-zwels = lw_bp_lfb1-zwels .
    lw_sup_company-zterm  = lw_bp_lfb1-zterm .
    lw_sup_company-loevm = lw_bp_lfb1-loevm.
    IF lw_sup_company-loevm IS INITIAL. "从X改为空，需要单独调用BAPI
      CLEAR: lw_bukrs.
      SELECT SINGLE loevm INTO lw_bukrs-loevm
        FROM lfb1
        WHERE bukrs = lw_sup_company-bukrs
          AND lifnr = g_partner
        .
      IF lw_bukrs-loevm  IS NOT INITIAL.
        CLEAR lw_bukrs-loevm.
        lw_bukrs-bukrs = lw_sup_company-bukrs.
        APPEND lw_bukrs TO t_bukrs.
      ENDIF.
    ENDIF.
*    lw_sup_company-togrr   = lw_bp_lfb1-togrr  .
*    lw_sup_company-zuawa   = lw_bp_lfb1-zuawa .
    APPEND  lw_sup_company TO t_sup_company.
    CLEAR lw_sup_company.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_GENERAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BP_GENERAL
*&      --> LW_BP
*&---------------------------------------------------------------------*
FORM frm_append_general  TABLES   t_bp_general STRUCTURE cvis_bp_general
                         USING    u_bp TYPE zdt_oa2sap_bp.

  DATA: lw_bp_general TYPE cvis_bp_general.


  REFRESH t_bp_general.
  IF g_mcflag = cns_mcflag_create.
    lw_bp_general-object_task = 'I'.
    lw_bp_general-grouping = u_bp-grouping.
    lw_bp_general-category = '2'. "u_bp-category.  固定组织
    lw_bp_general-title_key = '3000'. "lw_bp-title_key. 固定公司
  ELSE.
    lw_bp_general-object_task = 'U'.
  ENDIF.

  lw_bp_general-bpartner = g_partner.  "外部给号
  lw_bp_general-partnertype = u_bp-bpkind.   "
  lw_bp_general-searchterm1 = u_bp-searchterm1.  "输入参数
  lw_bp_general-searchterm2 = u_bp-searchterm2.  "输入参数


  PERFORM frm_set_bp_name USING    u_bp-name1
                          CHANGING lw_bp_general-name1
                                   lw_bp_general-name2
                                   lw_bp_general-name3
                                   lw_bp_general-name4.
*      lw_bp_general-titleletter = lw_bp-title_let.
  APPEND lw_bp_general TO t_bp_general.
  CLEAR lw_bp_general.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BP_ADDRESS
*&      --> LW_BP
*&---------------------------------------------------------------------*
FORM frm_append_address  TABLES   t_bp_address STRUCTURE cvis_bp_address
                         USING    u_bp TYPE zdt_oa2sap_bp.

  DATA: lw_bp_address TYPE cvis_bp_address.
  PERFORM frm_set_street USING u_bp-street
                         CHANGING lw_bp_address-street
                                  lw_bp_address-str_suppl1
                                  lw_bp_address-str_suppl2.

  lw_bp_address-country = u_bp-country.  "输入参数

*      IF u_bp-langu = 'ZH'.
*        lw_bp_address-langu = '1'.  "输入参数
*      ELSE.
*        lw_bp_address-langu = 'E'.
*      ENDIF.
  SELECT SINGLE spras INTO lw_bp_address-langu FROM t002 WHERE laiso = u_bp-langu.

  " lw_bp_address-LANGUISO = u_bp-LANGUISO.
*      lw_bp_address-extaddressnumber = u_bp-adext.
*      lw_bp_address-house_no = u_bp-house_no.
*      lw_bp_address-city = u_bp-city.
  lw_bp_address-region = u_bp-region.
  CONDENSE lw_bp_address-region NO-GAPS.
  lw_bp_address-regiogroup = u_bp-regiogroup.
  APPEND lw_bp_address TO t_bp_address.
  CLEAR lw_bp_address.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_TELENO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BP_ADDRESS_TELENO
*&      --> LW_BP
*&---------------------------------------------------------------------*
FORM frm_append_teleno  TABLES   t_bp_address_teleno STRUCTURE cvis_bp_address_teleno
                         USING    u_bp TYPE zdt_oa2sap_bp.
  DATA: lw_bp_address_teleno TYPE cvis_bp_address_teleno.

  lw_bp_address_teleno-telephone = u_bp-telephone. "输入参数
*      lw_bp_address_teleno-caller_no = u_bp-caller_no.
  lw_bp_address_teleno-extension = u_bp-extension.
  lw_bp_address_teleno-country   = u_bp-country.
  APPEND lw_bp_address_teleno TO t_bp_address_teleno.
  CLEAR lw_bp_address_teleno.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_TAX_NUMBER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_CVIS_BP_TAX_NUMBER
*&      --> LW_BP
*&---------------------------------------------------------------------*
FORM frm_append_tax_number  TABLES   t_cvis_bp_tax_number STRUCTURE cvis_bp_tax_number
                            USING    u_taxtype u_taxnumxl.
  DATA: lw_cvis_bp_tax_number TYPE cvis_bp_tax_number.

  IF u_taxtype IS NOT INITIAL.
    lw_cvis_bp_tax_number-taxtype = u_taxtype.
    lw_cvis_bp_tax_number-taxnumber = u_taxnumxl.
    lw_cvis_bp_tax_number-taxnumxl  = u_taxnumxl.
    APPEND lw_cvis_bp_tax_number TO t_cvis_bp_tax_number.
    CLEAR lw_cvis_bp_tax_number.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATE_BP_AFTER_MAINTAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BP_ADDRESS
*&      --> LT_BP_IDENT
*&      --> LT_CVIS_BP_TAX_NUMBER
*&      --> G_PARTNER
*&      --> LW_BP_MCFLAG
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_update_bp_after_maintain  TABLES   t_bp_ident STRUCTURE cvis_bp_ident_numbers
                                            t_cvis_bp_tax_number STRUCTURE cvis_bp_tax_number
                                            t_sup_text  STRUCTURE cvis_supplier_porg_texts
                                            t_ekorg TYPE tt_ekorg
                                            t_bukrs TYPE tt_bukrs
                                   USING    u_partner TYPE but000-partner
                                            u_mcflag
                                            u_remark
                                   CHANGING c_type
                                            c_message.
  DATA: lw_bp_address TYPE cvis_bp_address,
        lw_sup_text   TYPE cvis_supplier_porg_texts,
        lw_stxh       TYPE stxh,
        l_task        TYPE c.

  DATA: ls_master   TYPE vmds_ei_main,
        ls_data_cor TYPE vmds_ei_main,
        ls_msg_cor  TYPE cvis_message,
        ls_data_def TYPE vmds_ei_main,
        ls_msg_def  TYPE cvis_message,
        ls_vendor   TYPE vmds_ei_extern,
        ls_company  TYPE vmds_ei_company,
        ls_purch    TYPE vmds_ei_purchasing.

  DATA: lt_texts TYPE cvis_ei_text_t,
        lt_tline TYPE tline_tab,
        ls_tline TYPE tline.
  DATA: lw_ekorg LIKE LINE OF t_ekorg,
        lw_bukrs LIKE LINE OF t_bukrs.


  PERFORM frm_ident_add TABLES   t_bp_ident
                        USING    u_partner
                                 u_mcflag
                        CHANGING c_type c_message.
  PERFORM frm_tax_modify TABLES t_cvis_bp_tax_number
                            USING u_partner
                                  u_mcflag
                         CHANGING c_type c_message.

  IF t_sup_text[] IS NOT INITIAL OR t_ekorg[] IS NOT INITIAL OR t_bukrs[] IS NOT INITIAL.

    CLEAR: ls_vendor, ls_master.
    ls_vendor = VALUE vmds_ei_extern( header = VALUE vmds_ei_header(     object_task = 'M'
                                                                         object_instance = VALUE vmds_instance( lifnr = u_partner )
                                                                        )
                                         ).
    LOOP AT t_sup_text INTO lw_sup_text.
      PERFORM frm_process_until_next USING u_partner.

      CLEAR: ls_tline, lw_stxh, ls_purch.
      REFRESH: lt_tline, lt_texts.


      ls_tline-tdline = lw_sup_text-tdline."业务员
      APPEND ls_tline TO lt_tline.

      CLEAR: l_task.
      CONCATENATE u_partner lw_sup_text-ekorg INTO lw_stxh-tdname.
      SELECT SINGLE * INTO lw_stxh FROM stxh
        WHERE tdobject = 'LFM1'
          AND tdname = lw_stxh-tdname
          AND tdid = lw_sup_text-text_id
          AND tdspras = lw_sup_text-langu
        .
      IF sy-subrc = 0.
        l_task = 'M'.
      ELSE.
        l_task = cns_modifymode_insert.
      ENDIF.

      APPEND VALUE cvis_ei_text( task = l_task
                              data_key = VALUE cvis_ei_longtext_data_key( text_id = lw_sup_text-text_id
                                                                         langu = lw_sup_text-langu
                                                                         )
                              data  = lt_tline ) TO lt_texts.


      ls_purch = VALUE vmds_ei_purchasing( task = 'M'
                                             data_key = VALUE vmds_ei_purchasing_key( ekorg = lw_sup_text-ekorg ) " 采购组织
                                             texts =  VALUE cvis_ei_cvis_text( texts = lt_texts )
                                             ).
      READ TABLE t_ekorg INTO lw_ekorg WITH KEY ekorg = lw_sup_text-ekorg.
      IF sy-subrc = 0.
        ls_purch-data-sperm = lw_ekorg-sperm.
        ls_purch-datax-sperm = 'X'.
        DELETE t_ekorg WHERE ekorg = lw_sup_text-ekorg.
      ENDIF.
      APPEND ls_purch TO ls_vendor-purchasing_data-purchasing.

    ENDLOOP.
    LOOP AT t_ekorg INTO lw_ekorg.
      CLEAR: ls_purch.
      ls_purch-task = 'M'.
      ls_purch-data_key-ekorg = lw_ekorg-ekorg.
      ls_purch-data-sperm = lw_ekorg-sperm.
      ls_purch-datax-sperm = 'X'.
      APPEND ls_purch TO ls_vendor-purchasing_data-purchasing.
    ENDLOOP.

    LOOP AT t_bukrs INTO lw_bukrs.
      CLEAR: ls_company.
      ls_company-task = 'M'.
      ls_company-data_key-bukrs = lw_bukrs-bukrs.
      ls_company-data-loevm = lw_bukrs-loevm.
      ls_company-datax-loevm = 'X'.
      APPEND ls_company TO ls_vendor-company_data-company.
    ENDLOOP.

    APPEND ls_vendor TO ls_master-vendors.

    CALL METHOD vmd_ei_api=>initialize
*    EXPORTING
*      iv_manual_memory_init =
      .
    CALL METHOD vmd_ei_api=>maintain_bapi
      EXPORTING
*       iv_test_run              = SPACE
        iv_collect_messages      = 'X'
        is_master_data           = ls_master
      IMPORTING
        es_master_data_correct   = ls_data_cor
        es_message_correct       = ls_msg_cor
        es_master_data_defective = ls_data_def
        es_message_defective     = ls_msg_def.
    IF ls_msg_def-is_error IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'
*     IMPORTING
*         RETURN        =
        .

    ELSE.
      c_message =  c_message && ', 更新失败：'.
      LOOP AT ls_msg_def-messages INTO DATA(ls_return) WHERE type = 'E' OR type = 'A' OR type = 'X'.
        CONCATENATE c_message ls_return-message INTO c_message SEPARATED BY space.
*        EXIT.
      ENDLOOP.
      c_type = 'W'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
        .
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATE_REMARK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CW_RETURN_OBJECT_KEY
*&      --> LW_BP_REMARK
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_update_remark  USING    u_bpartner
                                 u_remark
                                 u_langu
                        CHANGING c_type
                                 c_message.

  DATA: lt_ad_rem  TYPE TABLE OF bapiad_rem,
        lt_ad_rex  TYPE TABLE OF bapiad_rex,
        lt_return  TYPE TABLE OF bapiret2,
        lw_ad_rem  TYPE bapiad_rem,
        lw_ad_rex  TYPE bapiad_rex,
        lw_return  TYPE bapiret2,
        l_bpartner TYPE but000-partner,
        l_type     TYPE bapi_mtype,
        l_msg      TYPE bapi_msg.

  l_bpartner = u_bpartner.
  CHECK l_bpartner IS NOT INITIAL AND u_remark NE ''.

  PERFORM frm_process_until_next USING l_bpartner.

  lw_ad_rem-adr_notes = u_remark.
  lw_ad_rem-langu = u_langu.
  APPEND lw_ad_rem TO lt_ad_rem.
  lw_ad_rex-adr_notes = 'X'.
  lw_ad_rex-langu = 'X'.
  lw_ad_rex-updateflag = 'I'.
  APPEND lw_ad_rex TO lt_ad_rex.

  CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
    EXPORTING
      businesspartner = l_bpartner
    TABLES
      bapiad_rem      = lt_ad_rem
      bapiad_re_x     = lt_ad_rex
      return          = lt_return.

  CLEAR: l_type, l_msg.
  PERFORM frm_get_message TABLES   lt_return
                          CHANGING l_msg
                                   l_type.
*  LOOP AT lt_return INTO lw_return WHERE type = 'E' OR type = 'A'.
*    l_type = 'W'.
*    EXIT.
*  ENDLOOP.
  IF l_type = 'E'.
    c_type = 'W'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    CONCATENATE c_message '更新备注失败：' l_msg INTO c_message SEPARATED BY space.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_IDENT_ADD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BP_IDENT
*&      --> L_PARTNER
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_ident_add  TABLES   t_bp_ident STRUCTURE cvis_bp_ident_numbers
                    USING    u_partner TYPE but000-partner
                             u_mcflag
                    CHANGING c_type
                             c_message.
  DATA: lw_bp_ident TYPE cvis_bp_ident_numbers,
        lw_ident    TYPE bapibus1006_identification,
        l_partner   TYPE but000-partner,
        lt_return   TYPE STANDARD TABLE OF bapiret2,
        lw_return   TYPE bapiret2,
        l_type      TYPE bapi_mtype,
        l_msg       TYPE bapi_msg.


  CLEAR: l_type.

  l_partner = u_partner.
*****20230110 修复技术标识没法全部清空的BUG.waymon begin .
* CHECK t_bp_ident[] IS NOT INITIAL AND l_partner IS NOT INITIAL.
 CHECK l_partner IS NOT INITIAL.

*****20230110 修复技术标识没法全部清空的BUG.waymon end .
  PERFORM frm_process_until_next USING l_partner.
  IF u_mcflag = cns_mcflag_modify.
    SELECT partner,
         type,
         idnumber,
         institute,
         entry_date,
         valid_date_from,
         valid_date_to
      FROM   but0id
      WHERE  partner = @l_partner
      INTO TABLE @DATA(lt_but0id).


    "只有一个技术ID，先全部删除，再插入
    LOOP AT  lt_but0id INTO DATA(ls_but0id).
*      READ TABLE t_bp_ident INTO lw_bp_ident WITH KEY identificationcategory = ls_but0id-type.
*      IF sy-subrc = 0.  "相同Type的Ident，先删除后插入
        "更新操作,先移除后插入
        PERFORM frm_remove_ident USING ls_but0id-partner ls_but0id-type ls_but0id-idnumber
                                     CHANGING l_type l_msg.
        IF l_type = 'E'.
          l_type = 'W'.
        ENDIF.
        IF l_type NE 'W'.
*          "移除完毕后新增
*          lw_ident-idvalidfromdate = lw_bp_ident-idvalidfromdate.
*          lw_ident-idvalidtodate   = lw_bp_ident-idvalidtodate.
*
*
*          CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
*            EXPORTING
*              businesspartner        = l_partner
*              identificationcategory = lw_bp_ident-identificationcategory
*              identificationnumber   = lw_bp_ident-identificationnumber
*              identification         = lw_ident
*            TABLES
*              return                 = lt_return.
*          LOOP AT  lt_return INTO lw_return  WHERE type CA 'EA'.
*            l_type = 'W'.
*            EXIT.
*          ENDLOOP.
*          IF l_type = 'W'.
*            EXIT.
*          ELSE.
*            DELETE t_bp_ident WHERE identificationcategory  = lw_bp_ident-identificationcategory AND identificationnumber  =  lw_bp_ident-identificationnumber."更新完毕后删除
        ENDIF.
*      ELSE.
*        EXIT.
*      ENDIF.
*      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT t_bp_ident INTO lw_bp_ident .

    lw_ident-idvalidfromdate = lw_bp_ident-idvalidfromdate.
    lw_ident-idvalidtodate = lw_bp_ident-idvalidtodate.

    CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
      EXPORTING
        businesspartner        = l_partner
        identificationcategory = lw_bp_ident-identificationcategory
        identificationnumber   = lw_bp_ident-identificationnumber
        identification         = lw_ident
      TABLES
        return                 = lt_return.
*    LOOP AT  lt_return INTO lw_return  WHERE type CA 'EA'.
*      l_type = 'W'.
*      EXIT.
*    ENDLOOP.
    CLEAR: l_type, l_msg.
    PERFORM frm_get_message TABLES   lt_return
                            CHANGING l_msg
                                     l_type.

    IF  l_type = 'E'.
      l_type = 'W'.
      EXIT .
    ENDIF.
  ENDLOOP.

  IF  l_type = 'W'.
    c_type = l_type.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    IF u_mcflag = cns_mcflag_modify.
      CONCATENATE c_message '更新技术标识失败' l_msg INTO c_message SEPARATED BY space.
    ELSE.
      CONCATENATE c_message '创建技术标识失败' l_msg INTO c_message SEPARATED BY space.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATE_SALES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUP_TEXT
*&      --> G_PARTNER
*&      --> C_OUTPUT_TYPE
*&      --> C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_update_sales  TABLES   t_sup_text STRUCTURE cvis_supplier_porg_texts
                       USING    u_partner
                                c_type
                                c_message.
  DATA: lw_thead    TYPE thead,
        lw_sup_text TYPE cvis_supplier_porg_texts,
        lt_tline    TYPE TABLE OF tline,
        lw_tline    TYPE tline,
        l_type      TYPE bapi_mtype.

  CLEAR: l_type.

  LOOP AT t_sup_text INTO lw_sup_text.
    lw_thead-tdobject = 'LFM1'.
    lw_thead-tdid     = cns_tdid_sales.
    lw_thead-tdname   = u_partner && lw_sup_text-ekorg.
    lw_thead-tdspras  = '1'.  "固定中文

    REFRESH lt_tline.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = lw_thead-tdid
        language                = lw_thead-tdspras
        name                    = lw_thead-tdname
        object                  = lw_thead-tdobject
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*     IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.

    ENDIF.
    IF  lt_tline[] IS NOT INITIAL.
      READ TABLE lt_tline INTO lw_tline INDEX 1.
      IF lw_tline-tdline = lw_sup_text-tdline.
        CONTINUE. "长文本相同则不更新。
      ENDIF.
    ENDIF.

    REFRESH lt_tline.
    CLEAR: lw_tline.
    lw_tline-tdline = lw_sup_text-tdline.
    APPEND lw_tline TO lt_tline.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*       CLIENT          = SY-MANDT
        header          = lw_thead
*       INSERT          = ' '
        savemode_direct = 'X'
*       OWNER_SPECIFIED = ' '
*       LOCAL_CAT       = ' '
*       KEEP_LAST_CHANGED       = ' '
*     IMPORTING
*       FUNCTION        =
*       NEWHEADER       =
      TABLES
        lines           = lt_tline
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
      l_type = 'W'.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF l_type = 'W'.
    c_type = l_type.
    CONCATENATE c_message '更新业务员失败' INTO c_message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_BUT0ID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BP_IDENT
*&      --> LW_BP_OA_BUT0ID
*&---------------------------------------------------------------------*
FORM frm_append_but0id  TABLES   t_bp_ident STRUCTURE cvis_bp_ident_numbers
                        USING    ut_but0id TYPE zdt_oa2sap_bp_oa_but0id_tab.
  DATA: lw_but0id   TYPE zdt_oa2sap_bp_oa_but0id,
        lw_bp_ident TYPE cvis_bp_ident_numbers.

  LOOP AT ut_but0id INTO lw_but0id.
    CLEAR: lw_bp_ident.
    lw_bp_ident-identificationcategory = lw_but0id-type.
    lw_bp_ident-identificationnumber   = lw_but0id-idnumber.
    lw_bp_ident-idvalidfromdate = lw_but0id-valid_date_from.
    lw_bp_ident-idvalidtodate   = lw_but0id-valid_date_to.
    APPEND lw_bp_ident TO t_bp_ident.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_TAX_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_CVIS_BP_TAX_NUMBER
*&      --> G_PARTNER
*&      --> LW_BP_MCFLAG
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_tax_modify  TABLES   t_cvis_bp_tax_number STRUCTURE cvis_bp_tax_number
                     USING    u_partner TYPE but000-partner
                              u_mcflag
                     CHANGING c_type
                              c_message.
  DATA: l_bpartner TYPE but000-partner,
        lw_bp_tax  TYPE cvis_bp_tax_number,
        lt_return  TYPE TABLE OF bapiret2,
        lw_return  TYPE bapiret2,
        l_type     TYPE bapi_mtype,
        l_msg      TYPE bapi_msg.

  CLEAR: l_type.

  l_bpartner = u_partner.
  CHECK t_cvis_bp_tax_number[] IS NOT INITIAL AND l_bpartner IS NOT INITIAL.
  PERFORM frm_process_until_next USING l_bpartner.

  " 每个BP只能有一个税号：
  SELECT *
    FROM dfkkbptaxnum
   WHERE partner = @l_bpartner
    INTO TABLE @DATA(lt_taxnum).

  " 先删除之前的税号：
  LOOP AT lt_taxnum INTO DATA(ls_taxnum).
    CALL FUNCTION 'BAPI_BUPA_TAX_REMOVE'
      EXPORTING
        businesspartner = l_bpartner
        taxtype         = ls_taxnum-taxtype
      TABLES
        return          = lt_return.

*    LOOP AT lt_return INTO lw_return WHERE type = 'A' OR type = 'E' OR type = 'X'.
*      l_type = 'W'.
*      CONCATENATE c_message '更新税码失败' lw_return-message INTO c_message SEPARATED BY space.
*    ENDLOOP.
    CLEAR: l_type, l_msg.
    PERFORM frm_get_message TABLES   lt_return
                            CHANGING l_msg
                                     l_type.

    IF l_type = 'E'.
      l_type = 'W'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF l_type = 'W'.
    c_type = l_type.
    CONCATENATE c_message '更新税码失败：' l_msg INTO c_message SEPARATED BY space.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
      .
    RETURN.
  ENDIF.

  " 再新增当前的税号：
  REFRESH lt_return.
  READ TABLE t_cvis_bp_tax_number INTO lw_bp_tax INDEX 1.
  CALL FUNCTION 'BUPA_TAX_ADD'
    EXPORTING
      iv_partner            = l_bpartner
      iv_tax_type           = lw_bp_tax-taxtype
      iv_tax_number         = lw_bp_tax-taxnumber
      iv_tax_number_xl      = lw_bp_tax-taxnumxl
*     IV_COMBINED_CHECK     = ' '
*     IV_X_SAVE             = 'X'
    TABLES
      ct_return             = lt_return
    EXCEPTIONS
      record_already_exists = 1
      invalid_data          = 2
      foreign_lock          = 3
      OTHERS                = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*  LOOP AT lt_return INTO lw_return WHERE type = 'A' OR type = 'E' OR type = 'X'.
*    l_type = 'W'.
*    CONCATENATE c_message '更新税码失败' lw_return-message INTO c_message SEPARATED BY space.
*  ENDLOOP.
  CLEAR: l_type, l_msg.
  PERFORM frm_get_message TABLES   lt_return
                          CHANGING l_msg
                                   l_type.
  IF l_type = 'E'.
    c_type = 'W'.
    CONCATENATE c_message '更新税码失败：' l_msg INTO c_message SEPARATED BY space.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
      .
    RETURN.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_PROCESS_UNTIL_NEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_PARTNER
*&---------------------------------------------------------------------*
FORM frm_process_until_next  USING VALUE(u_partner) TYPE bu_partner.
  DATA lt_return TYPE TABLE OF bapiret2.
  DO.
    CALL FUNCTION 'BUPA_ENQUEUE'
      EXPORTING
        iv_partner          = u_partner
        iv_check_not_number = 'X'
      TABLES
        et_return           = lt_return
      EXCEPTIONS
        blocked_partner     = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      WAIT UP TO '0.2' SECONDS.
    ELSE.
      CALL FUNCTION 'BUPA_DEQUEUE'
        EXPORTING
          iv_partner          = u_partner
          iv_check_not_number = 'X'
        TABLES
          et_return           = lt_return.

      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATE_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BP_ADDRESS
*&      --> LT_BP_ADDRESS_TELENO
*&      --> LW_BP
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_update_address  TABLES  t_bp_address STRUCTURE cvis_bp_address
                                  t_bp_address_teleno STRUCTURE cvis_bp_address_teleno
                         USING    u_bp TYPE zdt_oa2sap_bp
                         CHANGING c_type
                                  c_message.
  DATA: lt_tel    TYPE TABLE OF bapiadtel,
        lt_telx   TYPE TABLE OF bapiadtelx,
        lt_ad_rem TYPE TABLE OF bapiad_rem,
        lt_ad_rex TYPE TABLE OF bapiad_rex,
        lt_email  TYPE TABLE OF bapiadsmtp,
        lt_emailx TYPE TABLE OF bapiadsmtx,
        lt_fax    TYPE TABLE OF bapiadfax,
        lt_faxx   TYPE TABLE OF bapiadfaxx,
        lt_return TYPE TABLE OF bapiret2.
  DATA: lw_address  TYPE bapibus1006_address,
        lw_addressx TYPE bapibus1006_address_x,
        lw_tel      TYPE bapiadtel,
        lw_telx     TYPE bapiadtelx,
        lw_ad_rem   TYPE bapiad_rem,
        lw_ad_rex   TYPE bapiad_rex,
        lw_email    TYPE bapiadsmtp,
        lw_emailx   TYPE bapiadsmtx.

  DATA: lw_bp_address TYPE cvis_bp_address.
  DATA: ls_adr6   LIKE adr6,
        lt_adr6   LIKE TABLE OF adr6,
        ls_adr6_1 LIKE adr6,
        lt_adr6_1 LIKE TABLE OF adr6.
  DATA: ls_adrnr     LIKE lfa1-adrnr,
        l_valid_from TYPE but020-addr_valid_from,
        l_valid_to   TYPE but020-addr_valid_to.
  DATA: l_msg  TYPE bapi_msg,
        l_type TYPE bapi_mtype.

  CLEAR: lw_address, lw_addressx.

  LOOP AT t_bp_address INTO lw_bp_address.
    lw_address-country = lw_bp_address-country.
    lw_addressx-country = 'X'.
    lw_address-street  = lw_bp_address-street.
    lw_addressx-street = 'X'.
    IF lw_bp_address-str_suppl1 IS NOT INITIAL .
      lw_address-str_suppl1 = lw_bp_address-str_suppl1.
      lw_addressx-str_suppl1 = 'X'.
    ENDIF.
    IF lw_bp_address-str_suppl2 IS NOT INITIAL .
      lw_address-str_suppl2 = lw_bp_address-str_suppl2.
      lw_addressx-str_suppl2 = 'X'.
    ENDIF.
    lw_address-langu = lw_bp_address-langu.
    lw_addressx-langu = 'X'.
    lw_address-region = lw_bp_address-region.
    lw_addressx-region = 'X'.
    lw_address-regiogroup = lw_bp_address-regiogroup.
    lw_addressx-regiogroup = 'X'.

    EXIT. "地址只有一条记录
  ENDLOOP.

  LOOP AT t_bp_address_teleno INTO DATA(lw_bp_teleno).
    CLEAR: lw_tel, lw_telx.
    lw_tel-telephone = lw_bp_teleno-telephone.
    lw_telx-telephone = 'X'.
    lw_tel-extension = lw_bp_teleno-extension.
    lw_telx-extension = 'X'.

    lw_telx-updateflag = 'U'.
    APPEND lw_tel TO lt_tel.
    APPEND lw_telx TO lt_telx.
    EXIT. "电话只有一条记录
  ENDLOOP.

  REFRESH: gt_email, lt_adr6, lt_adr6_1.

  SPLIT u_bp-e_mail AT cns_sep INTO TABLE gt_email.
  SORT gt_email.
  DELETE ADJACENT DUPLICATES FROM gt_email COMPARING ALL FIELDS.

  CLEAR: ls_adrnr.
  l_valid_from = sy-datum && '235959'.
  l_valid_to = sy-datum && '000000'.
  SELECT SINGLE addrnumber INTO ls_adrnr FROM but020
    WHERE partner = g_partner
      AND addr_valid_from LE l_valid_from
      AND addr_valid_to GE l_valid_to.
  PERFORM frm_append_adr6 TABLES lt_adr6_1 lt_adr6 USING ls_adrnr.

  SORT lt_adr6   BY smtp_addr consnumber.
  SORT lt_adr6_1 BY addrnumber consnumber.
  LOOP AT lt_adr6 INTO ls_adr6.
    READ TABLE lt_adr6_1 INTO ls_adr6_1 WITH KEY addrnumber = ls_adr6-addrnumber
        consnumber = ls_adr6-consnumber BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE lt_adr6_1 INDEX sy-tabix.
    ENDIF.
    READ TABLE gt_email INTO gs_email WITH KEY smtp_addr = ls_adr6-smtp_addr.
    IF sy-subrc = 0.
      DELETE gt_email INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  "修改后不存在的email地址标记删除
  LOOP AT lt_adr6_1 INTO ls_adr6_1.
    lw_email-e_mail = ls_adr6_1-smtp_addr.
    lw_email-std_no = 'X'.
    lw_email-consnumber = ls_adr6_1-consnumber.
    APPEND lw_email TO lt_email.

    lw_emailx-std_no = 'X'.
    lw_emailx-e_mail = 'X'.
    lw_emailx-updateflag = 'D'.
    APPEND lw_emailx TO lt_emailx.
  ENDLOOP.

  "新增的email插入
  LOOP AT gt_email INTO gs_email.
    lw_email-e_mail = gs_email-smtp_addr.
    lw_email-std_no = 'X'.
    APPEND lw_email TO lt_email.

    lw_emailx-std_no = 'X'.
    lw_emailx-e_mail = 'X'.
    lw_emailx-updateflag = 'I'.
    APPEND lw_emailx TO lt_emailx.
  ENDLOOP.

  lw_ad_rem-adr_notes = u_bp-remark.
  lw_ad_rem-langu = lw_address-langu.
  APPEND lw_ad_rem TO lt_ad_rem.
  lw_ad_rex-adr_notes = 'X'.
  lw_ad_rex-langu = 'X'.
  lw_ad_rex-updateflag = 'I'.
  APPEND lw_ad_rex TO lt_ad_rex.

  CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
    EXPORTING
      businesspartner = g_partner
      addressdata     = lw_address
      addressdata_x   = lw_addressx
    TABLES
      bapiadtel       = lt_tel
      bapiadsmtp      = lt_email
      bapiad_rem      = lt_ad_rem
      bapiadtel_x     = lt_telx
      bapiadsmt_x     = lt_emailx
      bapiad_re_x     = lt_ad_rex
      return          = lt_return.

  CLEAR: l_type, l_msg.
  PERFORM frm_get_message TABLES   lt_return
                          CHANGING l_msg
                                   l_type.

*  LOOP AT lt_return INTO DATA(ls_return) WHERE type = 'A' OR type = 'E' OR type = 'X'.
*    c_type = 'W'.
*    CONCATENATE c_message '更新地址失败' INTO c_message SEPARATED BY space.
*    EXIT.
*  ENDLOOP.
  IF l_type IS NOT INITIAL.
    c_type = 'W'.
    CONCATENATE c_message '更新地址失败：' l_msg INTO c_message SEPARATED BY space.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
      .

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'
*     IMPORTING
*       RETURN        =
      .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_UPDATE_BANKINFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BP_BANK_DETAILS
*&      --> G_PARTNER
*&      <-- C_OUTPUT_TYPE
*&      <-- C_OUTPUT_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_update_bankinfo TABLES   t_bp_bank_details STRUCTURE cvis_bp_bank_details
                          USING    u_partner TYPE but000-partner
                          CHANGING c_type
                                   c_message.

  DATA: lt_banks_old   TYPE TABLE OF bapibus1006_bankdetails,
        lw_banks_old   LIKE LINE OF lt_banks_old,
        lw_bank_detail TYPE cvis_bp_bank_details,
        l_type         TYPE c,
        l_msg          TYPE bapi_msg.

  CHECK t_bp_bank_details[] IS NOT INITIAL.
  CLEAR l_msg.

  "用函数取不到过期的
*  CALL FUNCTION 'BAPI_BUPA_BANKDETAILS_GET'
*    EXPORTING
*      businesspartner = u_partner
*    TABLES
*      bankdetails     = lt_banks_old.
  SELECT bkvid AS bankdetailid
         banks AS bank_ctry
         bankl AS bank_key
         bankn AS bank_acct
         bkref AS bank_ref
    INTO CORRESPONDING FIELDS OF TABLE lt_banks_old
    FROM but0bk
    WHERE partner = u_partner.

  "OA每次修改传所有数据过来，没有的就删除
  CLEAR: l_type.
  SORT t_bp_bank_details BY data_key.
  SORT lt_banks_old BY bankdetailid.
  LOOP AT lt_banks_old INTO lw_banks_old.
    READ TABLE t_bp_bank_details INTO lw_bank_detail WITH KEY data_key = lw_banks_old-bankdetailid
                                                              bank_ctry = lw_banks_old-bank_ctry
                                                              bank_key = lw_banks_old-bank_key
                                                              bank_acct = lw_banks_old-bank_acct
                                                              bank_ref = lw_banks_old-bank_ref.
    IF sy-subrc = 0 .
      "银行信息完全一样的不变更
      DELETE t_bp_bank_details INDEX sy-tabix.
    ELSE. "先删除所有银行信息
      PERFORM frm_delete_bp_banks USING u_partner lw_banks_old-bankdetailid CHANGING l_type l_msg.
      IF l_type = 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF l_type IS NOT INITIAL.
    c_type = 'E'.
    CONCATENATE c_message '更新银行信息失败：' l_msg INTO c_message SEPARATED BY space.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    RETURN.
  ELSE.
    "最后一并COMMIT
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'
*     IMPORTING
*       RETURN        =
      .
  ENDIF.

*  "再新增银行信息 主函数一起新增，可以传bkvid的值，单独新增的BAPI不能传BKVID
*  LOOP AT t_bp_bank_details INTO lw_bank_detail.
*    PERFORM frm_add_bp_banks USING u_partner lw_bank_detail CHANGING l_type.
*    IF l_type = 'X'.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*
*  IF l_type IS NOT INITIAL.
*    c_type = 'W'.
*    CONCATENATE c_message '更新银行信息失败' INTO c_message SEPARATED BY space.
*    RETURN.
*  ELSE.
*    "最后一并COMMIT
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'
**     IMPORTING
**       RETURN        =
*      .
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DELETE_BP_BANKS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_PARTNER
*&      --> LW_BANKS_OLD_BANKDETAILID
*&      <-- L_TYPE
*&---------------------------------------------------------------------*
FORM frm_delete_bp_banks  USING    u_partner TYPE but000-partner
                                   u_bkvid TYPE bu_bkvid
                          CHANGING c_type
                                   c_message.


  " 等待之前的操作执行完毕：
  PERFORM frm_process_until_next USING u_partner.

  DATA lt_return TYPE TABLE OF bapiret2.
  CALL FUNCTION 'BUPA_BANKDETAIL_REMOVE'
    EXPORTING
      iv_partner = u_partner
*     IV_PARTNER_GUID       =
      iv_bkvid   = u_bkvid
*     IV_X_SAVE  = 'X'
    TABLES
      et_return  = lt_return.
*  LOOP AT lt_return INTO DATA(ls_return) WHERE type = 'A' OR type = 'E' OR type = 'X'.
*    c_message = ls_return-message.
*    c_type = 'X'.
*    EXIT.
*  ENDLOOP.
  PERFORM frm_get_message TABLES   lt_return
                          CHANGING c_message
                                   c_type.
  IF c_type = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
      .

  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'
**     IMPORTING
**       RETURN        =
*      .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_ADD_BP_BANKS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_PARTNER
*&      --> LW_BANK_DETAIL
*&      <-- L_TYPE
*&---------------------------------------------------------------------*
FORM frm_add_bp_banks  USING   u_partner TYPE but000-partner
                                u_bank_detail TYPE cvis_bp_bank_details
                       CHANGING c_type.

  DATA: ls_bank   TYPE bapibus1006_bankdetail,
        lt_return TYPE TABLE OF bapiret2.
  " 等待之前对该BP的操作执行完毕：
  PERFORM frm_process_until_next USING u_partner.


  CLEAR: ls_bank.
  MOVE-CORRESPONDING u_bank_detail TO ls_bank.

  CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
    EXPORTING
      businesspartner = u_partner
      bankdetaildata  = ls_bank
    TABLES
      return          = lt_return.

  LOOP AT lt_return INTO DATA(ls_return) WHERE type = 'E' OR type = 'A' OR type = 'X'.
    c_type = 'X'.
    EXIT.
  ENDLOOP.
  IF c_type IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
      .
  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'
**     IMPORTING
**       RETURN        =
*      .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_STREET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_BP_STREET
*&      <-- LW_BP_ADDRESS_STREET
*&      <-- LW_BP_ADDRESS_STR_SUPPL1
*&      <-- LW_BP_ADDRESS_STR_SUPPL2
*&---------------------------------------------------------------------*
FORM frm_set_street  USING    u_street
                     CHANGING c_street
                              c_str_suppl1
                              c_str_suppl2.
  DATA: line TYPE string.

  IF strlen( u_street ) <= 60.
    c_street = u_street.  "输入参数
*街道2街道3空的情况下更新 add S4DK905067
    c_str_suppl1 = ''.
    c_str_suppl2 = ''.
*end S4DK905067
  ELSEIF strlen( u_street ) > 60 AND strlen( u_street ) <= 100.
    line = u_street.
    c_street = u_street(60).
    SHIFT line BY 60 PLACES LEFT.
    c_str_suppl1 = line.
*add S4DK905067
    c_str_suppl2 = ''.
*end S4DK905067
  ELSEIF strlen( u_street ) > 100 AND strlen( u_street ) <= 140.
    line = u_street.
    c_street = u_street(60).
    c_str_suppl1 = u_street+60(40).
    SHIFT line BY 100 PLACES LEFT.
    c_str_suppl2 = line.
  ENDIF.
ENDFORM.