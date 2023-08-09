*----------------------------------------------------------------------*
***INCLUDE LZMM006F05.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_MAINTAIN_CUSTOMER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_INPUT
*&      <-- E_OUTPUT
*&---------------------------------------------------------------------*
FORM frm_maintain_customer  USING    u_input TYPE zdt_oa2other_custom
                            CHANGING c_output TYPE zdt_oa2other_custom_ret.


  DATA: lv_table        TYPE char20,
        lv_spras        TYPE char2,
        lv_lifnr        TYPE lfa1-lifnr,
        ls_partner      TYPE bus_ei_extern, "业务合作伙伴
        ls_customer     TYPE cmds_ei_extern, "客户
        lv_mode         TYPE  char1, "操作模式
        l_mode_id       TYPE char1,
        lv_bpartnerguid TYPE bu_partner_guid, "bu_partner_guid_bapi,
        lt_roles        TYPE bus_ei_bupa_roles_t,
        ls_roles        TYPE bus_ei_bupa_roles,
        lt_addresses    TYPE bus_ei_bupa_address_t,
        ls_addresses    TYPE bus_ei_bupa_address,
*        ls_central_data TYPE vmds_ei_central_data,
        lt_bankdetails  TYPE bus_ei_bupa_bankdetail_t,
        ls_bankdetails  TYPE bus_ei_bupa_bankdetail,
*        ls_header       TYPE vmds_ei_header,
*        ls_central      TYPE vmds_ei_vmd_central,
        ls_data         TYPE  cvis_ei_extern,
        lt_data         TYPE  cvis_ei_extern_t,
        lt_return       TYPE  bapiretm,
        ls_return       TYPE  bapireti,
        ls_msg          TYPE  bapiretc.
  DATA:lt_cvis_bp_tax_number TYPE TABLE OF cvis_bp_tax_number,
       lw_ident_number       TYPE bus_ei_bupa_identification.
  DATA: l_partner     TYPE but000-partner,
        lw_but0id_old TYPE but0id,
        lt_but0id_del TYPE TABLE OF but0id,
        l_kunnr       TYPE kna1-kunnr,
        lw_but100     TYPE but100.
  DATA: l_line TYPE n.
  DATA: lv_kunnr TYPE kunnr.

  CLEAR: lv_table,lv_spras,lv_lifnr,ls_partner,ls_customer,lv_bpartnerguid,
         lv_mode,ls_roles,lt_roles,lt_addresses,ls_addresses,
         lt_bankdetails,ls_bankdetails,ls_data,lt_data,
         lt_return,ls_return,ls_msg.
  REFRESH: lt_cvis_bp_tax_number.

*  "语言转换
*  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
*    EXPORTING
*      input  = sy-langu
*    IMPORTING
*      output = lv_spras.

  lv_table = 'BUT000'.  "'CVI_VEND_LINK'.
  SELECT SINGLE partner_guid INTO lv_bpartnerguid FROM (lv_table) WHERE partner EQ g_partner. "获取全局唯一GUI
  IF sy-subrc EQ 0.
    lv_mode = cns_modifymode_update.
  ELSE.
    lv_mode = cns_modifymode_insert.
    DO 10 TIMES.
      TRY .
          lv_bpartnerguid = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
        CATCH cx_uuid_error.
      ENDTRY.
      IF lv_bpartnerguid IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
    IF lv_bpartnerguid IS INITIAL.
      c_output-type = 'E'.
      c_output-message = '获取GUID出错，请重试'.
      RETURN.
    ENDIF.
  ENDIF.

  CHECK ( lv_bpartnerguid IS NOT INITIAL AND u_input-grouping IS NOT INITIAL ).          "业务伙伴组为空时会dump

  ls_partner-header-object_task = lv_mode.
  ls_partner-header-object_instance-bpartner     = g_partner.
  ls_partner-header-object_instance-bpartnerguid = lv_bpartnerguid.
****业务合作伙伴数据的中心数据***********************************
  ls_partner-central_data-common-data-bp_control-category = '2'.              "业务伙伴类别-组织
  ls_partner-central_data-common-data-bp_control-grouping = u_input-grouping.             "业务伙伴组

  ls_partner-central_data-common-data-bp_centraldata-searchterm1 = u_input-searchterm1.      "检索项
  ls_partner-central_data-common-data-bp_centraldata-searchterm2 = u_input-searchterm2.      "检索项
*    ls_partner-central_data-common-data-bp_centraldata-partnertype = u_input-bpkind.      "业务合作伙伴类型
  ls_partner-central_data-common-data-bp_centraldata-partnerlanguageiso = u_input-langu.      "语言
  ls_partner-central_data-common-data-bp_centraldata-titleletter = u_input-title_let. "问候
  ls_partner-central_data-common-data-bp_centraldata-title_key = '3000'.  " 称谓 固定公司
  PERFORM frm_set_bp_name USING    u_input-name1
                        CHANGING ls_partner-central_data-common-data-bp_organization-name1
                                 ls_partner-central_data-common-data-bp_organization-name2
                                 ls_partner-central_data-common-data-bp_organization-name3
                                 ls_partner-central_data-common-data-bp_organization-name4.
  ls_partner-central_data-common-data-bp_group-namegroup1 = ls_partner-central_data-common-data-bp_organization-name1 .      "供应商名称1
  ls_partner-central_data-common-data-bp_group-namegroup2 = ls_partner-central_data-common-data-bp_organization-name2 .      "供应商名称1

  ls_partner-central_data-common-datax-bp_centraldata-searchterm1      = abap_true.
  ls_partner-central_data-common-datax-bp_centraldata-searchterm2      = abap_true.
*    ls_partner-central_data-common-datax-bp_centraldata-partnertype      = abap_true.
  ls_partner-central_data-common-datax-bp_centraldata-partnerlanguageiso      = abap_true.
  ls_partner-central_data-common-datax-bp_group-namegroup1      = abap_true.
  ls_partner-central_data-common-datax-bp_organization-name1           = abap_true.
*  IF ls_partner-central_data-common-data-bp_group-namegroup2 IS NOT INITIAL. S4DK905068
  ls_partner-central_data-common-datax-bp_group-namegroup2      = abap_true.
  ls_partner-central_data-common-datax-bp_organization-name2           = abap_true.
*  ENDIF.
*  IF ls_partner-central_data-common-data-bp_organization-name3 IS NOT INITIAL. S4DK905068
  ls_partner-central_data-common-datax-bp_organization-name3          = abap_true.
*  ENDIF.
*  IF ls_partner-central_data-common-data-bp_organization-name4 IS NOT INITIAL. S4DK905068
  ls_partner-central_data-common-datax-bp_organization-name4           = abap_true.
*  ENDIF.
  ls_partner-central_data-common-datax-bp_centraldata-titleletter = abap_true.
  ls_partner-central_data-common-datax-bp_centraldata-title_key   = abap_true.

***角色
  CLEAR:lt_roles.
  IF lv_mode = cns_modifymode_insert.
    lt_roles = VALUE #( task = lv_mode ( data_key = cns_bp_role_cust_fin )"财务角色
                                     ( data_key = cns_bp_role_cust_sales ) "销售角色
                                     ( data_key = cns_bp_role_cust_ukm ) ). "信贷角色
  ELSE.
    PERFORM frm_append_roles USING g_partner cns_bp_role_cust_fin CHANGING lt_roles.
    PERFORM frm_append_roles USING g_partner cns_bp_role_cust_sales CHANGING lt_roles.
    PERFORM frm_append_roles USING g_partner cns_bp_role_cust_ukm CHANGING lt_roles.
  ENDIF.
  ls_partner-central_data-role-roles = lt_roles.

  "新建时新建技术ID，CNN0会自动变成CN5
*  ls_partner-central_data-taxnumber-taxnumbers = VALUE bus_ei_bupa_taxnumber_t(
*                                                    ( task = lv_mode
*                                                      data_key-taxtype = u_input-taxtype
*                                                      data_key-taxnumxl = u_input-taxnumxl
*                                                      data_key-taxnumber = u_input-taxnumxl ) ).
  PERFORM frm_append_tax_number TABLES lt_cvis_bp_tax_number
                               USING u_input-taxtype u_input-taxnumxl.

  "技术ID信息 技术ID只能有一条 不删除多余的，客商同码时，可能存在2条记录
*  REFRESH: lt_but0id_del.
*  SELECT * INTO TABLE lt_but0id_del FROM but0id
*    WHERE partner = g_partner.
*  SORT lt_but0id_del BY type idnumber.
  LOOP AT u_input-oa_but0id INTO DATA(lw_but0id).
    IF lv_mode = cns_modifymode_update.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_but0id_old
        FROM but0id
        WHERE partner = g_partner
          AND type = lw_but0id-type
*          AND idnumber = lw_but0id-idnumber
*          AND institute = lw_but0id-institute
*          AND valid_date_from = lw_but0id-valid_date_from
*          AND valid_date_to = lw_but0id-valid_date_to
        .
*      READ TABLE lt_but0id_del INTO lw_but0id_old WITH KEY type = lw_but0id-type idnumber = lw_but0id-idnumber.
      IF sy-subrc = 0.
*        DELETE lt_but0id_del INDEX sy-tabix.  "不删除
        IF lw_but0id_old-type = lw_but0id-type AND lw_but0id_old-idnumber = lw_but0id-idnumber AND
           lw_but0id_old-institute = lw_but0id-institute AND  lw_but0id_old-valid_date_from = lw_but0id-valid_date_from AND
           lw_but0id_old-valid_date_to = lw_but0id-valid_date_to.
          CONTINUE. "完全相同则不修改
        ELSE.
          l_mode_id = lv_mode.
        ENDIF.
      ELSE.
        "关键字变更，先新增，如果之前有记录，则新增后删除旧的
        l_mode_id = cns_modifymode_insert.
*          PERFORM frm_remove_ident USING g_partner lw_but0id_old-type lw_but0id_old-idnumber
*                                   CHANGING c_output-type c_output-message.
*          IF c_output-type = 'E'.
*            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*            EXIT.
*          ELSE.
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                wait = 'X'.
*          ENDIF.
      ENDIF.
    ELSE.
      l_mode_id = cns_modifymode_insert.  "没有记录，新增
    ENDIF.

    CLEAR: lw_ident_number.
    lw_ident_number-task = l_mode_id.
    lw_ident_number-data_key-identificationcategory = lw_but0id-type.
    lw_ident_number-data_key-identificationnumber = lw_but0id-idnumber.
    lw_ident_number-data-idinstitute = lw_but0id-institute.
    lw_ident_number-data-idvalidfromdate = lw_but0id-valid_date_from.
    lw_ident_number-data-idvalidtodate = lw_but0id-valid_date_to.
    lw_ident_number-datax-idinstitute = abap_true.
    lw_ident_number-datax-idvalidfromdate = abap_true.
    lw_ident_number-datax-idvalidtodate = abap_true.
    APPEND lw_ident_number TO ls_partner-central_data-ident_number-ident_numbers.
*    ls_partner-central_data-ident_number-ident_numbers = VALUE bus_ei_bupa_identification_t(
*                                                      ( task = l_mode_id
*                                                        data_key-identificationcategory = lw_but0id-type
*                                                        data_key-identificationnumber = lw_but0id-idnumber
*                                                        data-idinstitute = lw_but0id-institute
*                                                        data-idvalidfromdate = lw_but0id-valid_date_from
*                                                        data-idvalidtodate = lw_but0id-valid_date_to
*                                                        datax-idinstitute = abap_true
*                                                        datax-idvalidfromdate = abap_true
*                                                        datax-idvalidtodate = abap_true ) ).

*    EXIT.
  ENDLOOP.
*  "只能先删除，后修改，type相同的ID不能添加多条
*  LOOP AT lt_but0id_del INTO lw_but0id_old.
*    PERFORM frm_process_until_next USING g_partner.
*    PERFORM frm_remove_ident USING g_partner lw_but0id_old-type lw_but0id_old-idnumber
*                                 CHANGING c_output-type c_output-message.
*    IF c_output-type = 'E'.
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*  IF c_output-type NE 'E'.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*  ENDIF.
*  IF c_output-type = 'E'.
*    RETURN.
*  ENDIF.

***地址信息
  "业务或作伙伴
*  PERFORM frm_set_address USING lv_mode lv_bpartnerguid cns_address_type_pi
*                                u_input-street_pi u_input-city1_pi u_input-country_pi
*                                u_input-langu_pi u_input-region_pi ''
*                                u_input-telf1_pi '' ''
*                          CHANGING ls_addresses.
*  APPEND ls_addresses TO lt_addresses.
  CLEAR: ls_addresses.

  PERFORM frm_set_address USING lv_mode lv_bpartnerguid cns_address_type_default
                                u_input-street u_input-city1 u_input-country
                                u_input-langu u_input-region u_input-remark
                                u_input-telf1 u_input-extension u_input-telf2
*-->start add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段  S4DK906218
                                u_input-post_code                  "邮政编码
*-->end add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段
                                u_input-smtp_addr_pi"20230421 Seashell 开票邮箱
                          CHANGING ls_addresses.

*    "邮件
*    ls_addresses-data-communication-smtp-smtp = VALUE bus_ei_bupa_smtp_t(
*                                                      ( contact-task = lv_mode
*                                                        contact-data-e_mail = <lfs_data>-smtp_addr
*                                                        contact-datax-e_mail = abap_true ) ).
  APPEND ls_addresses TO lt_addresses.
  ls_partner-central_data-address-addresses[]  = lt_addresses[].

***银行信息****************************************************
  DATA: lt_banks_old      TYPE TABLE OF but0bk,
        lw_banks_old      LIKE LINE OF lt_banks_old,
        t_bp_bank_details TYPE zdt_bankinfo_tab,
        lw_bank_detail    LIKE LINE OF t_bp_bank_details,
        l_type            TYPE c,
        l_msg             TYPE bapi_msg.

  CLEAR l_msg.

  SELECT bkvid
         banks
         bankl
         bankn
         bkref
    INTO CORRESPONDING FIELDS OF TABLE lt_banks_old
    FROM but0bk
    WHERE partner = g_partner.
  "OA每次修改传所有数据过来，没有的就删除
  CLEAR: l_type.
  t_bp_bank_details = u_input-bankinfo.
  SORT t_bp_bank_details BY bktyp.
  SORT lt_banks_old BY bkvid.
  LOOP AT t_bp_bank_details INTO lw_bank_detail.
    PERFORM frm_maintain_bank_md USING    lw_bank_detail-banka
                                          lw_bank_detail-banks
                                          lw_bank_detail-bankl
                                 CHANGING c_output-type
                                          c_output-message.
    IF c_output-type = 'E'.
      EXIT.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_banks_old INTO lw_banks_old.
    READ TABLE t_bp_bank_details INTO lw_bank_detail WITH KEY bktyp = lw_banks_old-bkvid
                                                              banks = lw_banks_old-banks
                                                              bankl = lw_banks_old-bankl
                                                              banka = lw_banks_old-bankn && lw_banks_old-bkref .
    IF sy-subrc = 0 .
      "银行信息完全一样的不变更
      DELETE t_bp_bank_details INDEX sy-tabix.
    ELSE. "先删除所有银行信息
      PERFORM frm_delete_bp_banks USING g_partner lw_banks_old-bkvid CHANGING l_type l_msg.
      IF l_type = 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF l_type IS NOT INITIAL.
    c_output-type = 'E'.
    CONCATENATE c_output-message '更新银行信息失败：' l_msg INTO c_output-message SEPARATED BY space.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    EXIT.
  ELSE.
    "最后一并COMMIT
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        WAIT = 'X'.
  ENDIF.

  LOOP AT t_bp_bank_details INTO lw_bank_detail.
    PERFORM frm_maintain_bank_md USING    lw_bank_detail-banka
                                          lw_bank_detail-banks
                                          lw_bank_detail-bankl
                                 CHANGING c_output-type
                                          c_output-message.
    IF c_output-type = 'E'.
      EXIT.
    ENDIF.
    CLEAR: ls_bankdetails.
    "银行帐户号码
    PERFORM frm_split_bankn USING lw_bank_detail-bankn
                         CHANGING ls_bankdetails-data-bank_acct
                                  ls_bankdetails-data-bank_ref.
*    IF LV_MODE = CNS_MODIFYMODE_UPDATE.
*      SELECT SINGLE PARTNER INTO L_KUNNR FROM BUT0BK        "S4DK905068
*        WHERE PARTNER = G_PARTNER
*          AND BANKS = LW_BANKINFO-BANKS
*          AND BANKL = LW_BANKINFO-BANKL
*          AND BANKN = LS_BANKDETAILS-DATA-BANK_ACCT
*          AND BKREF = LS_BANKDETAILS-DATA-BANK_REF
*          AND BKVID = LW_BANKINFO-BKTYP.                    "S4DK905068
*      IF SY-SUBRC = 0.  "完全相同，则不修改
*        CONTINUE.
*      ENDIF.
*    ENDIF.
    ls_bankdetails-task = cns_modifymode_insert.
    ls_bankdetails-data_key               = lw_bank_detail-bktyp.
    ls_bankdetails-data-bank_ctry         = lw_bank_detail-banks.               "银行国家代码
    ls_bankdetails-data-bank_key          = lw_bank_detail-bankl.               "银行代码
    ls_bankdetails-datax-bank_ctry         = abap_true.
    ls_bankdetails-datax-bank_key          = abap_true.
    ls_bankdetails-datax-bank_acct         = abap_true.
*    IF ls_bankdetails-data-bank_ref IS NOT INITIAL. "S4DK905068
    ls_bankdetails-datax-bank_ref = abap_true.
*    ENDIF.
    APPEND ls_bankdetails TO lt_bankdetails.
  ENDLOOP.
  IF c_output-type = 'E'.
    RETURN.
  ENDIF.

  ls_partner-central_data-bankdetail-bankdetails[] = lt_bankdetails[].

****客户数据表头**********************************************
  l_kunnr = g_partner.
  SELECT SINGLE kunnr FROM kna1 INTO l_kunnr WHERE kunnr = l_kunnr.
  IF sy-subrc EQ 0.
    lv_mode = cns_modifymode_update.
  ELSE.
    lv_mode = cns_modifymode_insert.
  ENDIF.

  ls_customer-header-object_task = lv_mode.
  ls_customer-header-object_instance-kunnr = l_kunnr.

***贸易伙伴

****中心数据扩展结构****************************************************
  "如果不更新以下数据，LFA1中的数据不会更新
  ls_customer-central_data-address-task = lv_mode.
  ls_customer-central_data-address-postal-data-name     = ls_partner-central_data-common-data-bp_group-namegroup1."名称
  ls_customer-central_data-address-postal-data-name_2   = ls_partner-central_data-common-data-bp_group-namegroup2."名称
  ls_customer-central_data-address-postal-data-name_3   = ls_partner-central_data-common-datax-bp_organization-name3."名称
  ls_customer-central_data-address-postal-data-name_4   = ls_partner-central_data-common-datax-bp_organization-name4."名称
  ls_customer-central_data-address-postal-data-sort1    = u_input-searchterm1."搜索
  ls_customer-central_data-address-postal-data-sort2    = u_input-searchterm2."搜索

  "给ls_address赋值时，先符开票的，后给缺省的赋值，以便于此处用普通地址
  ls_customer-central_data-address-postal-data-street   = ls_addresses-data-postal-data-street."街道
  ls_customer-central_data-address-postal-data-str_suppl1   = ls_addresses-data-postal-data-str_suppl1."街道
  ls_customer-central_data-address-postal-data-str_suppl2   = ls_addresses-data-postal-data-str_suppl2."街道

  ls_customer-central_data-address-postal-data-country  = u_input-country."国家
  ls_customer-central_data-address-postal-data-langu_iso    = u_input-langu."语言

  ls_customer-central_data-address-postal-datax-name    = abap_true.
  ls_customer-central_data-address-postal-datax-sort1   = abap_true.
  ls_customer-central_data-address-postal-datax-sort2   = abap_true.
  ls_customer-central_data-address-postal-datax-street  = abap_true.
  ls_customer-central_data-address-postal-datax-country = abap_true.
  ls_customer-central_data-address-postal-datax-langu_iso   = abap_true.
  IF ls_customer-central_data-address-postal-data-name_2 IS NOT INITIAL.
    ls_customer-central_data-address-postal-datax-name_2      = abap_true.
  ENDIF.
  IF ls_customer-central_data-address-postal-data-name_3 IS NOT INITIAL.
    ls_customer-central_data-address-postal-datax-name_3      = abap_true.
  ENDIF.
  IF ls_customer-central_data-address-postal-data-name_4 IS NOT INITIAL.
    ls_customer-central_data-address-postal-datax-name_4      = abap_true.
  ENDIF.
*add S4DK905067
*  IF ls_customer-central_data-address-postal-data-str_suppl1 IS NOT INITIAL.
  ls_customer-central_data-address-postal-datax-str_suppl1     = abap_true.
*  ENDIF.
*  IF ls_customer-central_data-address-postal-data-str_suppl2 IS NOT INITIAL.
  ls_customer-central_data-address-postal-datax-str_suppl2     = abap_true.
*  ENDIF.
*end *add S4DK905067
  "
  ls_customer-central_data-central-data-kukla = u_input-kukla.
  ls_customer-central_data-central-datax-kukla = abap_true.

  ls_customer-central_data-central-data-aufsd = u_input-aufsd.
  ls_customer-central_data-central-datax-aufsd = abap_true.

***设置税分类
  PERFORM frm_set_tax_ind USING u_input-taxkd g_partner lv_mode CHANGING ls_customer.

***    外部接口：销售数据
  PERFORM frm_set_sales_data USING u_input-oa_knvv g_partner lv_mode CHANGING ls_customer.


****    外部接口：公司代码数据
  PERFORM frm_set_company_data USING u_input-oa_knb1 g_partner lv_mode CHANGING ls_customer.

  "信用额度/信用检查
  PERFORM frm_set_ukmbp_data USING u_input g_partner lv_mode CHANGING ls_partner.

  ls_data-partner = ls_partner.
  ls_data-customer = ls_customer.
  APPEND ls_data TO lt_data.

  TRY .
      PERFORM frm_process_until_next USING g_partner.
      CALL METHOD cl_md_bp_maintain=>maintain
        EXPORTING
          i_data   = lt_data
        IMPORTING
          e_return = lt_return.
    CATCH cx_root INTO DATA(lo_root).
  ENDTRY.

  LOOP AT lt_return INTO ls_return.
    CLEAR: l_line.
    LOOP AT ls_return-object_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
      c_output-type = 'E'.
      l_line = l_line + 1.
      IF c_output-message IS INITIAL.
        CONCATENATE l_line ls_msg-message INTO c_output-message SEPARATED BY space.
      ELSEIF l_line LE 9.
        CONCATENATE c_output-message l_line ls_msg-message INTO c_output-message SEPARATED BY space.
      ELSE.
        CONCATENATE c_output-message ls_msg-message INTO c_output-message SEPARATED BY space.
      ENDIF.
*      EXIT.
    ENDLOOP.
  ENDLOOP.

  IF c_output-type = 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    c_output-type = 'S'.
    IF g_partner IS INITIAL.
      DO 10 TIMES.
        SELECT SINGLE partner INTO g_partner FROM but000
          WHERE partner_guid = lv_bpartnerguid.
        IF sy-subrc NE 0. "等待数据写入数据库
          WAIT UP TO 1 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
***   >>>取自动编号
*    IF G_PARTNER IS NOT INITIAL.
*      IF LV_MODE = CNS_MODIFYMODE_INSERT.
*        C_OUTPUT-MESSAGE = G_PARTNER && '创建成功'.
*      ELSE.
*        C_OUTPUT-MESSAGE = G_PARTNER && '修改成功'.
*      ENDIF.
*      C_OUTPUT-PARTNER = G_PARTNER.
*    ENDIF.
*------>start add by chenhy 20220925 新增外部转换
    lv_kunnr = g_partner.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lv_kunnr
      IMPORTING
        output = lv_kunnr.
    IF g_partner IS NOT INITIAL.
      IF lv_mode = cns_modifymode_insert.
        c_output-message = lv_kunnr && '创建成功'.
      ELSE.
        c_output-message = lv_kunnr && '修改成功'.
      ENDIF.
      c_output-partner = lv_kunnr.
    ENDIF.
*------>end add by chenhy 20220925 新增外部转换
    SELECT SINGLE * INTO @DATA(ls_log) FROM /sappo/order_hdr
      WHERE main_objkey = @g_partner AND msgty = 'E'.
    IF sy-subrc EQ 0.
      SELECT SINGLE kunnr INTO l_kunnr FROM (ls_log-basic_objtype) WHERE kunnr = g_partner.
      IF sy-subrc NE 0 OR g_mcflag = cns_modifymode_update.
        MESSAGE ID ls_log-msgid TYPE ls_log-msgty NUMBER ls_log-msgno
        WITH ls_log-msgv1 ls_log-msgv2 ls_log-msgv3 ls_log-msgv4
        INTO DATA(l_message).
        c_output-type = 'W'.
        IF g_mcflag = cns_modifymode_update.
          c_output-message = g_partner && '部分修改成功，请修正数据并重新导入！' && l_message.
        ELSE.
          c_output-message = g_partner && '部分创建成功，请修正数据并重新导入！' && l_message.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

  IF c_output-type NE 'E' AND g_partner IS NOT INITIAL.
    PERFORM frm_modify_pi_address USING lv_mode g_partner cns_address_type_pi
                                    u_input-street_pi u_input-city1_pi u_input-country_pi
                                    u_input-langu_pi u_input-region_pi u_input-telf1_pi
                                    CHANGING c_output-type c_output-message.


    PERFORM frm_tax_modify TABLES lt_cvis_bp_tax_number
                            USING g_partner
                                  g_mcflag
                        CHANGING c_output-type c_output-message..
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_MODE
*&      --> LV_BPARTNERGUID
*&      --> CNS_ADDRESS_TYPE_DEFAULT
*&      --> U_INPUT_STREET
*&      --> U_INPUT_CITY1
*&      --> U_INPUT_COUNTRY
*&      --> U_INPUT_LANGU
*&      --> U_INPUT_REGION
*&      --> U_INPUT_REMARK
*&      --> U_INPUT_TELF1
*&      --> U_INPUT_EXTENSION
*&      --> U_INPUT_TELF2
*&      <-- LS_ADDRESSES
*&---------------------------------------------------------------------*
FORM frm_set_address  USING    u_mode
                               u_bpartnerguid
                               u_address_type
                               u_street
                               u_city1
                               u_country
                               u_langu
                               u_region
                               u_remark
                               u_telf1
                               u_extension
                               u_telf2
*-->start add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段  S4DK906218
                               u_post_code
*-->end add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段
                               u_smtp_addr_pi"20230421 Seashell 开票邮箱
                      CHANGING cw_addresses TYPE bus_ei_bupa_address.

  DATA: lw_remark    TYPE bus_ei_bupa_addressremark,
        lw_addrusage TYPE bus_ei_bupa_addressusage,
        lw_telf      TYPE bus_ei_bupa_telephone,
        lt_telf      TYPE TABLE OF bus_ei_bupa_telephone,
*        lw_commusage TYPE bus_ei_bupa_commusage,
        lw_smtp      TYPE bus_ei_bupa_smtp, "Seashell 开票邮箱
        lt_smtp      TYPE TABLE OF bus_ei_bupa_smtp, "Seashell 开票邮箱
        lw_but021_fs TYPE but021_fs,
        lt_but021_fs TYPE TABLE OF but021_fs,
        l_adrnr      TYPE adr2-addrnumber,
        l_r3_user    TYPE adr2-r3_user,
        lt_adr2      TYPE TABLE OF adr2.
  DATA: l_valid_from LIKE sy-datum,
        l_valid_to   TYPE sy-datum,
        l_mode       TYPE c,
        l_guid       TYPE but020-address_guid,
        lw_adrct     TYPE adrct.
  DATA: lt_but020_new TYPE TABLE OF busaddrvb,
        lt_but020_old TYPE TABLE OF busaddrvb,
        lt_but021_new TYPE TABLE OF busadvwvb,
        lt_but021_old TYPE TABLE OF busadvwvb,
        lw_but021     TYPE busadvwvb.

  REFRESH: lt_but021_fs.
  CLEAR: l_guid, lw_but021_fs, l_adrnr.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_but021_fs FROM but021_fs
    WHERE partner = g_partner
      AND adr_kind = u_address_type.
  IF sy-subrc = 0.
    l_adrnr = lw_but021_fs-addrnumber.
    SELECT SINGLE address_guid INTO l_guid FROM but020
      WHERE partner = g_partner
        AND addrnumber = lw_but021_fs-addrnumber.

    PERFORM frm_convert_stamp_to_date USING lw_but021_fs-valid_from
                                      CHANGING l_valid_from.
    PERFORM frm_convert_stamp_to_date USING lw_but021_fs-valid_to
                                      CHANGING l_valid_to.
    l_mode = cns_modifymode_update.
    SELECT * INTO TABLE lt_but021_fs  FROM but021_fs
      WHERE partner = g_partner
        AND addrnumber = lw_but021_fs-addrnumber
        AND adr_kind NE u_address_type.
    REFRESH: lt_but020_new, lt_but020_old, lt_but021_new, lt_but021_old.
    LOOP AT lt_but021_fs INTO lw_but021_fs.
      "这样只改有效期，不能直接删除
*      lw_addrusage-task = cns_modifymode_delete.
*      lw_addrusage-data_key-addresstype = lw_but021_fs-adr_kind.
*      PERFORM frm_convert_stamp_to_date USING lw_but021_fs-valid_to
*                                      CHANGING lw_addrusage-data_key-valid_to.
*      APPEND lw_addrusage TO cw_addresses-data-addr_usage-addr_usages.
*      CLEAR lw_addrusage.

      CLEAR lw_but021.
      MOVE-CORRESPONDING lw_but021_fs TO lw_but021.
      lw_but021-chngind = cns_modifymode_delete.
      APPEND lw_but021 TO lt_but021_old.
    ENDLOOP.
    IF lt_but021_old[] IS NOT INITIAL.
      CALL FUNCTION 'BUA_BUPA_UPDATE'
        TABLES
          t_but020_new = lt_but020_new
          t_but020_old = lt_but020_old
          t_but021_new = lt_but021_new
          t_but021_old = lt_but021_old
*         T_BUTADRSEARCH       =
        .
    ENDIF.
  ELSE. "没有标准地址的情况就新建一个
    l_valid_from = sy-datum.
    l_valid_to = '99991231'.
    l_mode = cns_modifymode_insert.
  ENDIF.

  lw_addrusage-task = l_mode.
  lw_addrusage-data_key-addresstype = u_address_type.
  lw_addrusage-data_key-valid_to = l_valid_to.
  lw_addrusage-data-valid_from = l_valid_from.
  lw_addrusage-datax-valid_from = abap_true.
  IF u_address_type = cns_address_type_default.
    lw_addrusage-data-standard = abap_true.
    lw_addrusage-datax-standard = abap_true.
    cw_addresses-data-postal-data-standardaddress = abap_true.
    cw_addresses-data-postal-datax-standardaddress = abap_true.
  ENDIF.
  APPEND lw_addrusage TO cw_addresses-data-addr_usage-addr_usages.

  cw_addresses-task = u_mode.
  IF l_guid IS NOT INITIAL.
    cw_addresses-data_key-guid = l_guid.                           "唯一gui
  ENDIF.
  cw_addresses-data-postal-data-country    = u_country.             "国家
  cw_addresses-data-postal-data-languiso   = u_langu.                    "语言
*    ls_addresses-data-postal-data-extaddressnumber   = <lfs_data>-name_co.                    "联系人
*    ls_addresses-data-postal-data-c_o_name   = <lfs_data>-name_co.                    "联系人
  PERFORM frm_set_street USING u_street                     "供应商地址
                       CHANGING cw_addresses-data-postal-data-street
                                cw_addresses-data-postal-data-str_suppl1
                                cw_addresses-data-postal-data-str_suppl2.
*    ls_addresses-data-postal-data-postl_cod1   = <lfs_data>-postl_cod1.                    "邮政编码
  cw_addresses-data-postal-data-city   = u_city1.                    "城市
  cw_addresses-data-postal-data-region   = u_region.                    "地区
*-->start add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段  S4DK906218
  cw_addresses-data-postal-data-postl_cod1   = u_post_code.                    "邮政编码
  cw_addresses-data-postal-datax-postl_cod1  = abap_true.                      "邮政编码
*-->end add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段

  cw_addresses-data-postal-datax-country   = abap_true.
  cw_addresses-data-postal-datax-langu_iso  = abap_true.
*    ls_addresses-data-postal-datax-extaddressnumber  = abap_true.
*    ls_addresses-data-postal-datax-c_o_name  = abap_true.
  cw_addresses-data-postal-datax-street  = abap_true.
*add S4DK905067
*  IF cw_addresses-data-postal-data-str_suppl1 IS NOT INITIAL.
  cw_addresses-data-postal-datax-str_suppl1 = abap_true.
*  ENDIF.
*  IF cw_addresses-data-postal-data-str_suppl2 IS NOT INITIAL.
  cw_addresses-data-postal-datax-str_suppl2 = abap_true.
*  ENDIF.
*end S4DK905067
*    ls_addresses-data-postal-datax-postl_cod1  = abap_true.
  cw_addresses-data-postal-datax-city  = abap_true.
  cw_addresses-data-postal-datax-region  = abap_true.

*  cw_addresses-data-postal-data-validfromdate = l_valid_from.
*  cw_addresses-data-postal-data-validtodate   = l_valid_to.
*  cw_addresses-data-postal-datax-validfromdate = abap_true.
*  cw_addresses-data-postal-datax-validtodate   = abap_true.

  "20230421 Seashell 开票邮箱


  IF u_remark IS NOT INITIAL.
    IF u_mode = cns_modifymode_update AND l_adrnr IS NOT INITIAL.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_adrct
        FROM adrct
        WHERE addrnumber = l_adrnr.
      IF sy-subrc = 0 AND lw_adrct-remark IS NOT INITIAL.
        lw_remark-task = cns_modifymode_delete.
        lw_remark-data-adr_notes = u_remark.
        lw_remark-datax-adr_notes = abap_true.
        lw_remark-data-langu_iso = u_langu.
        lw_remark-datax-langu_iso = abap_true.
        APPEND lw_remark TO cw_addresses-data-remark-remarks.
      ELSE.
      ENDIF.
    ELSE.
      lw_remark-task = u_mode.
    ENDIF.

    CLEAR lw_remark.
    lw_remark-task = cns_modifymode_insert.
    lw_remark-data-adr_notes = u_remark.
    lw_remark-datax-adr_notes = abap_true.
    lw_remark-data-langu_iso = u_langu.
    lw_remark-datax-langu_iso = abap_true.
    APPEND lw_remark TO cw_addresses-data-remark-remarks.
  ENDIF.

  REFRESH lt_adr2.
  SELECT * INTO TABLE lt_adr2 FROM adr2 WHERE addrnumber = l_adrnr AND r3_user IN ('1', '3').
  LOOP AT  lt_adr2 INTO DATA(lw_adr2).
    CLEAR: lw_telf.
    lw_telf-contact-task = cns_modifymode_delete.
    lw_telf-contact-data-telephone = lw_adr2-tel_number.
    lw_telf-contact-data-extension = lw_adr2-tel_extens.
    lw_telf-contact-data-country = lw_adr2-country.
    lw_telf-contact-data-r_3_user = lw_adr2-r3_user.
    lw_telf-contact-data-valid_from = lw_adr2-valid_from.
    lw_telf-contact-data-valid_to = lw_adr2-valid_to.

    lw_telf-contact-datax-telephone = abap_true.
    lw_telf-contact-datax-extension = abap_true.
    lw_telf-contact-datax-country = abap_true.
    lw_telf-contact-datax-r_3_user = abap_true.
    lw_telf-contact-datax-valid_from = abap_true.
    lw_telf-contact-datax-valid_to = abap_true.
    APPEND lw_telf TO  cw_addresses-data-communication-phone-phone.
  ENDLOOP.

  CLEAR: lw_telf.
*  REFRESH lt_telf.
  l_r3_user = '1'.
*  PERFORM frm_set_update_mode USING l_adrnr l_r3_user CHANGING lw_telf-contact-task.
  lw_telf-contact-task = cns_modifymode_insert.
  lw_telf-contact-data-telephone = u_telf1.
  lw_telf-contact-datax-telephone = abap_true.
  lw_telf-contact-data-r_3_user = l_r3_user.
  lw_telf-contact-datax-r_3_user = abap_true.
  IF u_extension IS NOT INITIAL.
    lw_telf-contact-data-extension = u_extension.
    lw_telf-contact-datax-extension = abap_true.
  ENDIF.
*  lw_commusage-data_key-comm_usage = 'AD_NMBDEFA'.
*  lw_commusage-task = u_mode.
*  lw_commusage-data-def_usage = l_r3_user.
*  lw_commusage-datax-def_usage = abap_true.
*  APPEND lw_commusage TO lw_telf-comm_usage-comm_usages.
  APPEND lw_telf TO cw_addresses-data-communication-phone-phone.

  IF u_telf2 IS NOT INITIAL.
    CLEAR: lw_telf.
    l_r3_user = '3'.   "待定，手机
*    PERFORM frm_set_update_mode USING l_adrnr l_r3_user CHANGING lw_telf-contact-task.
    lw_telf-contact-task = cns_modifymode_insert. "u_mode.
    lw_telf-contact-data-telephone = u_telf2.
    lw_telf-contact-datax-telephone = abap_true.
    lw_telf-contact-data-r_3_user = l_r3_user.
    lw_telf-contact-datax-r_3_user = abap_true.
*    lw_commusage-data_key-comm_usage = 'AD_MBDEFAU'.
*    lw_commusage-task = lw_telf-contact-task.
*    lw_commusage-data-def_usage = l_r3_user.
*    lw_commusage-datax-def_usage = abap_true.
*    APPEND lw_commusage TO lw_telf-comm_usage-comm_usages.
    APPEND lw_telf TO cw_addresses-data-communication-phone-phone.

  ENDIF.

  "Seashell 20230727开票邮箱
  "删除系统内已经存在的数据
  CLEAR  lw_smtp.
  SELECT * FROM adr6 INTO TABLE @DATA(lt_adr6) WHERE addrnumber = @l_adrnr.

  LOOP AT lt_adr6 INTO DATA(ls_adr6).
    lw_smtp-contact-task = cns_modifymode_delete.
    lw_smtp-contact-data-e_mail = ls_adr6-smtp_addr.
    lw_smtp-contact-datax-e_mail = abap_true.
    lw_smtp-contact-data-consnumber = ls_adr6-consnumber.
    lw_smtp-contact-datax-consnumber = abap_true.
    lw_smtp-contact-data-r_3_user = ls_adr6-r3_user.
    lw_smtp-contact-datax-r_3_user = abap_true.
    lw_smtp-contact-data-email_srch = ls_adr6-smtp_srch.
    lw_smtp-contact-datax-email_srch = abap_true.
    lw_smtp-contact-data-valid_from = ls_adr6-valid_from.
    lw_smtp-contact-datax-valid_from = abap_true.
    lw_smtp-contact-data-valid_to = ls_adr6-valid_to.
    lw_smtp-contact-datax-valid_to = abap_true.
    lw_smtp-contact-data-std_recip = ls_adr6-dft_receiv.
    lw_smtp-contact-datax-std_recip = abap_true.
    APPEND lw_smtp TO cw_addresses-data-communication-smtp-smtp.
    CLEAR ls_adr6.
  ENDLOOP.


  "插入新的数据
  lw_smtp-contact-task = cns_modifymode_insert.
  lw_smtp-contact-data-e_mail = u_smtp_addr_pi.
  lw_smtp-contact-datax-e_mail = abap_true.
  lw_smtp-contact-data-r_3_user = 'X'.
  lw_smtp-contact-datax-r_3_user = abap_true.
  lw_smtp-contact-data-std_recip = 'X'.
  lw_smtp-contact-datax-std_recip = abap_true.
  APPEND lw_smtp TO cw_addresses-data-communication-smtp-smtp.
**  "20230727  表层研究用
**  lw_smtp-contact-task = cns_modifymode_insert.
**  lw_smtp-contact-data-e_mail = 'A123@QQ.COM'.
**  lw_smtp-contact-datax-e_mail = abap_true.
**
**                                                            "20230703
**  APPEND lw_smtp TO cw_addresses-data-communication-smtp-smtp.
**  "20230727  表层研究用
**  lw_smtp-contact-task = cns_modifymode_insert.
**  lw_smtp-contact-data-e_mail = 'B123@QQ.COM'.
**  lw_smtp-contact-datax-e_mail = abap_true.
**
**                                                            "20230703
**  APPEND lw_smtp TO cw_addresses-data-communication-smtp-smtp.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_UPDATE_MODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> L_ADRNR
*&      --> L_R3_USER
*&      <-- LW_TELF_CONTACT_TASK
*&---------------------------------------------------------------------*
FORM frm_set_update_mode  USING    u_adrnr TYPE adr2-addrnumber
                                   u_r3_user
                          CHANGING c_mode.

  IF u_adrnr IS INITIAL.
    c_mode = cns_modifymode_insert.
    RETURN.
  ENDIF.

  DATA: l_adr2 TYPE adr2.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF l_adr2
    FROM adr2
    WHERE addrnumber = u_adrnr
      AND r3_user = u_r3_user
    .
  IF sy-subrc = 0.
    c_mode = cns_modifymode_update.
  ELSE.
    c_mode = cns_modifymode_insert.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_TAX_IND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_INPUT_TAXKD
*&      --> G_PARTNER
*&      --> LV_MODE
*&      <-- LS_CUSTOMER
*&---------------------------------------------------------------------*
FORM frm_set_tax_ind  USING    u_taxkd
                               u_partner
                               u_mode
                      CHANGING cw_customer TYPE cmds_ei_extern.
  DATA: l_mode      TYPE c,
        lw_tax_ind  TYPE cmds_ei_tax_ind,
        lw_knvi_old TYPE knvi.

  CHECK u_taxkd IS NOT INITIAL.

  l_mode = u_mode.
  IF l_mode = cns_modifymode_update.
    SELECT SINGLE * INTO lw_knvi_old FROM knvi
      WHERE kunnr = u_partner
        AND aland = cns_aland_cn
        AND tatyp = cns_tatyp_mwst
    .
    IF sy-subrc NE 0.
      l_mode = cns_modifymode_insert.
    ENDIF.
  ENDIF.

  CLEAR: lw_tax_ind.
  lw_tax_ind-task = l_mode.
  lw_tax_ind-data_key-aland = cns_aland_cn.
  lw_tax_ind-data_key-tatyp = cns_tatyp_mwst.
  lw_tax_ind-data-taxkd = u_taxkd.
  lw_tax_ind-datax-taxkd = abap_true.

  APPEND lw_tax_ind TO cw_customer-central_data-tax_ind-tax_ind.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_SALES_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_INPUT_OA_KNVV
*&      <-- LS_CUSTOMER
*&---------------------------------------------------------------------*
FORM frm_set_sales_data  USING    ut_knvv TYPE zdt_knvv_tab
                                  u_kunnr
                                  u_mode
                         CHANGING cw_customer TYPE cmds_ei_extern.
  DEFINE append_functions.
    CLEAR: lw_functions.
    lw_functions-task = l_mode.
    lw_functions-data_key-parvw = &1.
    lw_functions-data-partner = &2.
    CONDENSE lw_functions-data-partner.
    IF lw_functions-data-partner IS INITIAL.
      lw_functions-data-partner = &3.
    ENDIF.
    IF lw_functions-data-partner IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lw_functions-data-partner
      IMPORTING
        output = lw_functions-data-partner.

    lw_functions-datax-partner = abap_true.
    ENDIF.
    APPEND lw_functions TO lw_salesdata-functions-functions.
  END-OF-DEFINITION.
  DATA: lw_salesdata TYPE cmds_ei_sales,
        lw_functions TYPE cmds_ei_functions,
        lw_knvv_old  TYPE knvv,
        lw_knvp_old  TYPE knvp,
        l_mode       TYPE c,
        l_kunnr      TYPE knvp-kunnr.

  LOOP AT ut_knvv INTO DATA(lw_knvv).
    CLEAR:lw_salesdata.
    l_mode = u_mode.
    IF l_mode = cns_modifymode_update.
      SELECT SINGLE * INTO lw_knvv_old FROM knvv
        WHERE kunnr = u_kunnr
          AND vkorg = lw_knvv-vkorg
          AND vtweg = lw_knvv-vtweg
          AND spart = lw_knvv-spart.
      IF sy-subrc NE 0.
        l_mode = cns_modifymode_insert.
      ENDIF.
    ENDIF.
    lw_salesdata-task = l_mode.
    lw_salesdata-data_key-vkorg = lw_knvv-vkorg.
    lw_salesdata-data_key-vtweg = lw_knvv-vtweg.
    lw_salesdata-data_key-spart = lw_knvv-spart.

    lw_salesdata-data-bzirk = lw_knvv-bzirk.
    lw_salesdata-data-vkbur = lw_knvv-vkbur.
    lw_salesdata-data-waers = lw_knvv-waers.
    lw_salesdata-data-kalks = lw_knvv-kalks.
    lw_salesdata-data-vsbed = lw_knvv-vsbed.
    lw_salesdata-data-incov = lw_knvv-incov.
    lw_salesdata-data-inco1 = lw_knvv-inco1.
    lw_salesdata-data-inco2_l = lw_knvv-inco2_l.
    lw_salesdata-data-inco3_l = lw_knvv-inco3_l.
    lw_salesdata-data-zterm = lw_knvv-zterm.
    lw_salesdata-data-kkber = lw_knvv-kkber.
    lw_salesdata-data-ktgrd = lw_knvv-ktgrd.
    lw_salesdata-data-kvgr1 = lw_knvv-kvgr1.
    lw_salesdata-data-kvgr2 = lw_knvv-kvgr2.
*-->start add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段  S4DK906218
* SAP默认勾选KNVV- KZAZU值
    lw_salesdata-data-kzazu  = 'X'.
    lw_salesdata-datax-kzazu = abap_true.
*-->end add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段
    lw_salesdata-datax-bzirk = abap_true.
    lw_salesdata-datax-vkbur = abap_true.
    lw_salesdata-datax-waers = abap_true.
    lw_salesdata-datax-kalks = abap_true.
    lw_salesdata-datax-vsbed = abap_true.
    lw_salesdata-datax-incov = abap_true.
    lw_salesdata-datax-inco1 = abap_true.
    lw_salesdata-datax-inco2_l = abap_true.
    lw_salesdata-datax-inco3_l = abap_true.
    lw_salesdata-datax-zterm = abap_true.
    lw_salesdata-datax-kkber = abap_true.
    lw_salesdata-datax-ktgrd = abap_true.
    lw_salesdata-datax-kvgr1 = abap_true.
    lw_salesdata-datax-kvgr2 = abap_true.

    l_mode = u_mode.
    IF u_mode = cns_modifymode_update.
      SELECT SINGLE * INTO lw_knvp_old FROM knvp
        WHERE kunnr = u_kunnr
          AND vkorg = lw_knvv-vkorg
          AND vtweg = lw_knvv-vtweg
          AND spart = lw_knvv-spart
          AND parvw = cns_parvw_z3.
      IF sy-subrc NE 0.
        l_mode = cns_modifymode_insert.
      ENDIF.
    ENDIF.

    append_functions cns_parvw_z3 lw_knvv-pernr ''.
    append_functions cns_parvw_ag lw_knvv-sp_pernr u_kunnr.
    append_functions cns_parvw_re lw_knvv-bp_pernr u_kunnr.
    append_functions cns_parvw_rg lw_knvv-py_pernr u_kunnr.
    append_functions cns_parvw_we lw_knvv-sh_pernr u_kunnr.
    append_functions cns_parvw_z1 lw_knvv-z1_pernr u_kunnr.
    append_functions cns_parvw_z2 lw_knvv-z2_pernr u_kunnr.

    APPEND lw_salesdata TO cw_customer-sales_data-sales.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_COMPANY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_INPUT_OA_KNB1
*&      --> G_PARTNER
*&      --> LV_MODE
*&      <-- LS_CUSTOMER
*&---------------------------------------------------------------------*
FORM frm_set_company_data  USING   ut_knb1 TYPE zdt_oa_knb1_tab
                                    u_kunnr
                                    u_mode
                           CHANGING cw_customer TYPE cmds_ei_extern.
  DATA: lw_company  TYPE cmds_ei_company,
        lw_knb1_old TYPE knb1,
        l_mode      TYPE c.

  LOOP AT ut_knb1 INTO DATA(lw_knb1).
    CLEAR:lw_company.
    l_mode = u_mode.
    IF l_mode = cns_modifymode_update.
      SELECT SINGLE * INTO lw_knb1_old FROM knb1
        WHERE kunnr = u_kunnr
          AND bukrs = lw_knb1-bukrs.
      IF sy-subrc NE 0.
        l_mode = cns_modifymode_insert.
      ENDIF.
    ENDIF.
    lw_company-task = l_mode.
    lw_company-data_key-bukrs = lw_knb1-bukrs.
    lw_company-data-akont = lw_knb1-akont.
    lw_company-datax-akont = abap_true.
*-->start add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段  S4DK906218
*   付款方式
    lw_company-data-zwels  = lw_knb1-zwels.
    lw_company-datax-zwels = abap_true.
*-->end add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段


    APPEND lw_company TO cw_customer-company_data-company.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_UKMBP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> U_INPUT
*&      --> G_PARTNER
*&      --> LV_MODE
*&      <-- LS_PARTNER
*&---------------------------------------------------------------------*
FORM frm_set_ukmbp_data  USING    u_input TYPE zdt_oa2other_custom
                                  u_partner
                                  u_mode
                         CHANGING cw_partner TYPE bus_ei_extern.
  DATA: lw_cms_sgm     TYPE ukm_ei_bp_cms_sgm,
        lw_cms_sgm_old TYPE ukmbp_cms_sgm,
        l_mode         TYPE c.

  cw_partner-ukmbp_data-profile-data-check_rule = u_input-check_rule.
  cw_partner-ukmbp_data-profile-data-limit_rule = u_input-limit_rule.
  cw_partner-ukmbp_data-profile-data-risk_class = u_input-risk_class.
  cw_partner-ukmbp_data-profile-datax-check_rule = abap_true.
  cw_partner-ukmbp_data-profile-datax-limit_rule = abap_true.
  cw_partner-ukmbp_data-profile-datax-risk_class = abap_true.

  LOOP AT u_input-oa_ukmbp_cms_sgm INTO DATA(lw_ukmbp_sgm).
    CLEAR:lw_cms_sgm.
    l_mode = u_mode.
    IF l_mode = cns_modifymode_update.
      SELECT SINGLE * INTO lw_cms_sgm_old FROM ukmbp_cms_sgm
        WHERE partner = u_partner
          AND credit_sgmnt = lw_ukmbp_sgm-credit_sgmnt.
      IF sy-subrc NE 0.
        l_mode = cns_modifymode_insert.
      ENDIF.
    ENDIF.
    lw_cms_sgm-task = l_mode.
    lw_cms_sgm-data_key-partner = u_partner."add S4DK905068
    lw_cms_sgm-data_key-credit_sgmnt = lw_ukmbp_sgm-credit_sgmnt.
    lw_cms_sgm-data-credit_limit = lw_ukmbp_sgm-credit_limit.
    lw_cms_sgm-datax-credit_limit = abap_true.

    APPEND lw_cms_sgm TO cw_partner-ukmbp_data-segments-segments.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_MODIFY_PI_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_MODE
*&      --> LV_BPARTNERGUID
*&      --> CNS_ADDRESS_TYPE_PI
*&      --> U_INPUT_STREET_PI
*&      --> U_INPUT_CITY1_PI
*&      --> U_INPUT_COUNTRY_PI
*&      --> U_INPUT_LANGU_PI
*&      --> U_INPUT_REGION_PI
*&      --> P_
*&      --> U_INPUT_TELF1_PI
*&      <-- C_OUTPUT_TYPE
*&      <-- L_MESSAGE
*&---------------------------------------------------------------------*
FORM frm_modify_pi_address  USING    u_mode
                                      u_bpartner
                                      u_address_type
                                      u_street
                                      u_city1
                                      u_country
                                      u_langu
                                      u_region
                                      u_telf1
                            CHANGING  c_type
                                      c_message.
  DATA: lw_but021_fs  TYPE but021_fs,
        lw_addrusage  TYPE bapibus1006_addressusage,
        lt_addrusage  TYPE TABLE OF bapibus1006_addressusage,
        lw_telf       TYPE bapiadtel,
        lt_telf       TYPE TABLE OF bapiadtel,
        lw_addrusagex TYPE bapibus1006_addressusage_x,
        lt_addrusagex TYPE TABLE OF bapibus1006_addressusage_x,
        lw_telfx      TYPE bapiadtelx,
        lt_telfx      TYPE TABLE OF bapiadtelx.
  DATA: lw_address    TYPE bapibus1006_address,
        lw_addressx   TYPE bapibus1006_address_x,
        lt_return     TYPE TABLE OF bapiret2,
        l_addressguid TYPE but020-address_guid,
        l_guid        TYPE but020-guid,
        lt_but020     TYPE TABLE OF but020,
        lw_but020     TYPE but020,
        l_errflag     TYPE c.

  DATA: l_valid_from LIKE sy-datum,
        l_valid_to   TYPE sy-datum,
        l_mode       TYPE c,
        l_partner    TYPE but000-partner.

  CLEAR: l_guid, l_addressguid.
  l_partner = u_bpartner.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_but021_fs FROM but021_fs
    WHERE partner = l_partner
      AND adr_kind = u_address_type.
  IF sy-subrc = 0.
    SELECT SINGLE address_guid INTO l_addressguid FROM but020
      WHERE partner = l_partner
        AND addrnumber = lw_but021_fs-addrnumber.
    l_guid = l_addressguid.
    PERFORM frm_convert_stamp_to_date USING lw_but021_fs-valid_from
                                      CHANGING l_valid_from.
    PERFORM frm_convert_stamp_to_date USING lw_but021_fs-valid_to
                                      CHANGING l_valid_to.
    l_mode = cns_modifymode_update.
  ELSE.
    l_valid_from = sy-datum.
    l_valid_to = '99991231'.
    l_mode = cns_modifymode_insert.
  ENDIF.

  CLEAR: lw_addrusage, lw_address, lw_telf, lw_addressx, lw_telfx, lw_addrusagex.
  REFRESH: lt_addrusage, lt_telf, lt_telfx, lt_addrusagex, lt_return.

  lw_addrusage-addresstype = u_address_type.
  lw_addrusage-usagevalidto = l_valid_to.
  lw_addrusage-usagevalidfrom = l_valid_from.
  APPEND lw_addrusage TO lt_addrusage.
  lw_addrusagex-addresstype = abap_true.
  lw_addrusagex-usagevalidto = abap_true.
  lw_addrusagex-usagevalidfrom = abap_true.
  APPEND lw_addrusagex TO lt_addrusagex.

  lw_address-country    = u_country.             "国家
  lw_address-languiso   = u_langu.                    "语言
*    LS_ADDRESSES-DATA-POSTAL-DATA-EXTADDRESSNUMBER   = <LFS_DATA>-NAME_CO.                    "联系人
*    LS_ADDRESSES-DATA-POSTAL-DATA-C_O_NAME   = <LFS_DATA>-NAME_CO.                    "联系人
  PERFORM frm_set_street USING u_street                     "供应商地址
                       CHANGING lw_address-street
                                lw_address-str_suppl1
                                lw_address-str_suppl2.
*    LS_ADDRESSES-DATA-POSTAL-DATA-POSTL_COD1   = <LFS_DATA>-POSTL_COD1.                    "邮政编码
  lw_address-city   = u_city1.                    "城市
  lw_address-region   = u_region.                    "地区

  lw_addressx-country    = abap_true.             "国家
  lw_addressx-langu_iso   = abap_true.                    "语言
  lw_addressx-street  = abap_true.
*  IF LW_ADDRESS-STR_SUPPL1 IS NOT INITIAL.
  lw_addressx-str_suppl1 = abap_true.
*  ENDIF.
*  IF LW_ADDRESS-STR_SUPPL2 IS NOT INITIAL.
  lw_addressx-str_suppl2 = abap_true.
*  ENDIF.
  lw_addressx-city   = abap_true.                    "城市
  lw_addressx-region   = abap_true.                    "地区

  IF u_telf1 IS NOT INITIAL.
    lw_telf-telephone = u_telf1.
    APPEND lw_telf TO lt_telf.
    lw_telfx-telephone = abap_true.
    APPEND lw_telfx TO lt_telfx.
  ENDIF.

  IF l_mode = cns_modifymode_update.
    CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
      EXPORTING
        businesspartner = l_partner
        addressguid     = l_guid
        addressdata     = lw_address
        addressdata_x   = lw_addressx
*       DUPLICATE_MESSAGE_TYPE       =
*       ACCEPT_ERROR    = ' '
      TABLES
        bapiadtel       = lt_telf
*       BAPIADFAX       =
*       BAPIADTTX       =
*       BAPIADTLX       =
*       BAPIADSMTP      =
*       BAPIADRML       =
*       BAPIADX400      =
*       BAPIADRFC       =
*       BAPIADPRT       =
*       BAPIADSSF       =
*       BAPIADURI       =
*       BAPIADPAG       =
*       BAPIAD_REM      =
*       BAPICOMREM      =
        addressusage    = lt_addrusage
*       BAPIADVERSORG   =
*       BAPIADVERSPERS  =
*       BAPIADUSE       =
        bapiadtel_x     = lt_telfx
*       BAPIADFAX_X     =
*       BAPIADTTX_X     =
*       BAPIADTLX_X     =
*       BAPIADSMT_X     =
*       BAPIADRML_X     =
*       BAPIADX40_X     =
*       BAPIADRFC_X     =
*       BAPIADPRT_X     =
*       BAPIADSSF_X     =
*       BAPIADURI_X     =
*       BAPIADPAG_X     =
*       BAPIAD_RE_X     =
*       BAPICOMRE_X     =
        addressusage_x  = lt_addrusagex
*       BAPIADVERSORG_X =
*       BAPIADVERSPERS_X             =
*       BAPIADUSE_X     =
        return          = lt_return
*       ADDRESSDUPLICATES            =
      .
*   add S4DK905067
    SELECT * INTO TABLE @DATA(lt_adr2)
      FROM adr2
    WHERE addrnumber = @lw_but021_fs-addrnumber.
    IF lt_adr2 IS INITIAL.
*     start S4DK905218
      CLEAR lt_addrusage.
      l_valid_from = sy-datum.
      l_valid_to = '99991231'.
      lw_addrusage-addresstype = u_address_type.
      lw_addrusage-usagevalidto = l_valid_to.
      lw_addrusage-usagevalidfrom = l_valid_from.
      APPEND lw_addrusage TO lt_addrusage.
*     end S4DK905218
      CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
        EXPORTING
          businesspartner = l_partner
          addressdata     = lw_address
        TABLES
          bapiadtel       = lt_telf
          addressusage    = lt_addrusage
          return          = lt_return.
    ENDIF.
*   end S4DK905067
*-->start add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段  S4DK906218
  ELSEIF u_country IS NOT INITIAL AND u_street IS NOT INITIAL .
* ELSE.
*-->end add by chenhy 20220816 新增OA客户BP上传SAP接口优化字段
    CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
      EXPORTING
        businesspartner = l_partner
        addressdata     = lw_address
*       DUPLICATE_MESSAGE_TYPE       =
*       ACCEPT_ERROR    = ' '
*     IMPORTING
*       ADDRESSGUID     =
      TABLES
        bapiadtel       = lt_telf
*       BAPIADFAX       =
*       BAPIADTTX       =
*       BAPIADTLX       =
*       BAPIADSMTP      =
*       BAPIADRML       =
*       BAPIADX400      =
*       BAPIADRFC       =
*       BAPIADPRT       =
*       BAPIADSSF       =
*       BAPIADURI       =
*       BAPIADPAG       =
*       BAPIAD_REM      =
*       BAPICOMREM      =
        addressusage    = lt_addrusage
*       BAPIADUSE       =
        return          = lt_return
*       ADDRESSDUPLICATES            =
      .

  ENDIF.

  LOOP AT lt_return INTO DATA(lw_return) WHERE type = 'E' OR type = 'A'.
    c_type = 'W'.
    CONCATENATE c_message '更新PI地址失败：' lw_return-message INTO c_message SEPARATED BY space.
    EXIT.
  ENDLOOP.
  IF  sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    "删除无用的地址
    CLEAR l_errflag.
    SELECT * INTO TABLE lt_but020 FROM but020
      WHERE partner = l_partner
        AND NOT EXISTS ( SELECT * FROM but021_fs WHERE partner = l_partner AND addrnumber = but020~addrnumber
        AND ( adr_kind = cns_address_type_default OR adr_kind = cns_address_type_pi ) ).
    LOOP AT lt_but020 INTO lw_but020.
      REFRESH lt_return.
      l_guid = lw_but020-address_guid.
      CALL FUNCTION 'BAPI_BUPA_ADDRESS_REMOVE'
        EXPORTING
          businesspartner = l_partner
          addressguid     = l_guid
        TABLES
          return          = lt_return.
      LOOP AT lt_return INTO lw_return WHERE type = 'E' OR type = 'A'.
        c_type = 'W'.
        l_errflag = 'X'.
*        CONCATENATE c_message lw_return-message INTO c_message SEPARATED BY c_message. "删除无关的地址，不返回错误消息
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
    ENDLOOP.
    IF sy-subrc = 0 AND l_errflag IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_APPEND_ROLES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> G_PARTNER
*&      --> CNS_BP_ROLE_CUST_FIN
*&      <-- LT_ROLES
*&---------------------------------------------------------------------*
FORM frm_append_roles  USING    u_partner TYPE but000-partner
                                u_rltyp
                       CHANGING ct_roles TYPE bus_ei_bupa_roles_t.
  DATA: lw_roles  TYPE bus_ei_bupa_roles,
        lw_but100 TYPE but100,
        l_mode    TYPE c.

  CLEAR: lw_roles.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF lw_but100 FROM but100
      WHERE partner = u_partner AND rltyp = u_rltyp.
  IF sy-subrc = 0.
    lw_roles-task = cns_modifymode_update.
  ELSE.
    lw_roles-task = cns_modifymode_insert.
  ENDIF.
  lw_roles-data_key = u_rltyp.
  APPEND lw_roles TO ct_roles.

ENDFORM.