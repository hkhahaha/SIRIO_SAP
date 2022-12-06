FUNCTION z_sd_aps_so.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(I_VBAK) TYPE  VBAK
*"  EXPORTING
*"     VALUE(E_OUTPUT) TYPE  BAPIRET2
*"----------------------------------------------------------------------

  DATA l_intid TYPE ze_intf_id VALUE 'SD011'.

*----------------------------------------------------------------------
*   业务逻辑处理
*----------------------------------------------------------------------
  DATA:
    lv_error        TYPE c,
    lv_str          TYPE c,
    ls_zpitlog0008  TYPE zpitlog0008,
    lt_zpitlog0008  TYPE TABLE OF zpitlog0008,
    ls_zpitlog0008a TYPE zpitlog0008a,
    lt_zpitlog0008a TYPE TABLE OF zpitlog0008a,
    ls_vbak         TYPE vbak,
    ls_vbap         TYPE vbapvb,
    lt_vbap         TYPE TABLE OF vbap,
    ls_vbep         TYPE vbepvb,
    lt_vbep         TYPE TABLE OF vbep,
    lr_proxy        TYPE REF TO zco_si_sap2aps_so_out, "代理类
    lr_protocol     TYPE REF TO if_wsprotocol_message_id,
    lt_so           TYPE zsap_order_tab,
    ls_so           TYPE zsap_order,
    lt_item         TYPE zsap_order_detail_tab,
    ls_item         TYPE zsap_order_detail,
    ls_output       TYPE zsap_to_aps_sap_order_soap_in.

* 异步确认的使用对象
  DATA:go_sys_fault   TYPE REF TO cx_ai_system_fault,
       go_asyn_msg    TYPE REF TO if_wsprotocol_async_messaging,
       go_protocol    TYPE REF TO if_wsprotocol_message_id,
       gv_ack_request TYPE prx_ack_request_details.


  WAIT UP TO 10 SECONDS."异步调用，保持数据是最新的




*----------------------------------------------------------------------
*   日志记录-记录输入参数
*----------------------------------------------------------------------
  DATA(l_guid) = zcl_bc_public=>get_guid( )."生成本次调用唯一标识
  DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = l_guid iv_intid = l_intid iv_ptype = 'I' is_data = i_vbak ). "记录输入参数

**** 开关处理
  IF l_flag EQ 'A' OR   l_flag EQ 'E'.
    RETURN.
  ENDIF.

  TRY.
      CREATE OBJECT lr_proxy.
      CLEAR ls_output.

      ls_vbak  = i_vbak.



      CHECK lv_error IS INITIAL.

      SELECT * INTO TABLE lt_vbap FROM vbap
        WHERE vbeln = ls_vbak-vbeln.

      SELECT * INTO TABLE lt_vbep FROM vbep
         WHERE vbeln = ls_vbak-vbeln.


      CHECK ls_vbak-auart = '01' OR  ls_vbak-auart+0(2) = 'ZA'.

      IF sy-subrc = 0.

        SELECT spras, msehi, msehl
          FROM t006a
           FOR ALL ENTRIES IN @lt_vbap[]
         WHERE msehi = @lt_vbap-vrkme
           AND spras = @sy-langu
          ORDER BY PRIMARY KEY
          INTO TABLE @DATA(lt_t006a).

        SELECT * INTO TABLE @DATA(lt_vbkd) FROM vbkd
          FOR ALL ENTRIES IN @lt_vbap[]
          WHERE vbeln = @lt_vbap-vbeln
            AND posnr = @lt_vbap-posnr.

        SELECT matnr,maktx INTO TABLE @DATA(lt_makt) FROM makt
          FOR ALL ENTRIES IN @lt_vbap[]
          WHERE matnr =  @lt_vbap-matnr.


        SELECT SINGLE * INTO @DATA(ls_t005t) FROM t005t
          WHERE land1 = @ls_vbak-zckgj
            AND spras = @sy-langu.

        SELECT SINGLE * INTO @DATA(ls_tvtwt) FROM tvtwt
           WHERE vtweg = @ls_vbak-vtweg
             AND spras = @sy-langu.


        SELECT SINGLE * INTO @DATA(ls_but)   FROM but000
          WHERE partner = @ls_vbak-kunnr.

        SELECT SINGLE * INTO @DATA(ls_user)  FROM user_addr
          WHERE bname = @ls_vbak-ernam.

        "20220822 Seashell 获取客户分类描述
        SELECT * FROM tkukt INTO TABLE @DATA(lt_tkukt) WHERE spras = '1'.


        SELECT SINGLE * INTO @DATA(ls_kna1)  FROM kna1
          WHERE kunnr = @ls_vbak-kunnr.
        "业务合作伙伴-业务员
        SELECT SINGLE * INTO @DATA(ls_vbpa)  FROM vbpa
           WHERE vbeln = @ls_vbak-vbeln
             AND parvw = 'Z3'.
        IF ls_vbpa IS NOT INITIAL.
          SELECT SINGLE * INTO @DATA(ls_adrc)  FROM adrc
             WHERE addrnumber = @ls_vbpa-adrnr.
        ENDIF.
        "业务合作伙伴-实际客户名称
*        SELECT SINGLE kna1~name1 FROM vbpa
*          INNER JOIN kna1 ON vbpa~kunnr = kna1~kunnr
*          WHERE vbpa~vbeln = @ls_vbak-vbeln
*          AND vbpa~parvw = 'Z1'
*          INTO @DATA(lv_z1_name1).

        SELECT SINGLE * INTO @DATA(ls_knvv)  FROM knvv
          WHERE kunnr = @ls_vbak-kunnr
           AND vkorg = @ls_vbak-vkorg
           AND vtweg = @ls_vbak-vtweg
           AND spart = @ls_vbak-spart.
        IF sy-subrc = 0.
          SELECT SINGLE * INTO @DATA(ls_tvv1t)  FROM tvv1t
              WHERE kvgr1 = @ls_knvv-kvgr1
                AND spras = @sy-langu.
        ENDIF.

        "非01类型的订单，只保留 ZA/ZB计划行
        IF ls_vbak-auart NE '01'.
          LOOP AT lt_vbep INTO ls_vbep.
            IF ls_vbep-ettyp = 'ZA' OR ls_vbep-ettyp = 'ZB' OR ls_vbep-ettyp = 'ZC' .
            ELSE.
              DELETE lt_vbep[] INDEX sy-tabix.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF lt_vbep[] IS INITIAL.
          RETURN.
        ENDIF.
        "抬头赋值
        MOVE-CORRESPONDING ls_vbak TO ls_so.
        PERFORM frm_read_long_text USING 'Z001' ls_vbak-vbeln  '' 'VBBK'  CHANGING  ls_so-zscwb."生产抬头备注
        ls_so-ernam = ls_user-name_last && ls_user-name_first."制单人
        ls_so-bu_sort1 = ls_but-bu_sort1."客户简称
        ls_so-zxsqd = ls_tvtwt-vtext && '-' && ls_tvv1t-bezei."渠道
        ls_so-zkhjb = ls_kna1-kukla."客户级别
        "20220822 Seashell 客户分类取描述
        READ TABLE lt_tkukt INTO DATA(ls_tkukt) WITH KEY kukla = ls_kna1-kukla.
        IF sy-subrc = 0.
          ls_so-zkhjb = ls_tkukt-vtext.
        ENDIF.


        ls_so-zywy = ls_adrc-name1."业务员
        ls_so-inco2_l = ls_t005t-landx."出口国家
        "业务员名称、实际客户名称
        SELECT SINGLE kunnr FROM vbpa WHERE vbeln = @ls_vbak-vbeln AND posnr IS INITIAL AND parvw = 'Z1' INTO @DATA(lv_kunnr).
        IF lv_kunnr IS NOT INITIAL.
          SELECT SINGLE tvk1t~vtext kna1~name1 INTO ( ls_so-additional1 ,ls_so-additional2 )
            FROM kna1
            LEFT JOIN tvk1t ON kna1~katr1 = tvk1t~katr1 AND tvk1t~spras = sy-langu
            WHERE kna1~kunnr = lv_kunnr.
        ENDIF.

*       创建日期取二审日期S4DK905628-S
        SELECT SINGLE udate FROM jcds
        WHERE objnr = @ls_vbak-objnr AND stat ='E0003'
           INTO @DATA(lv_erdat).
        IF lv_erdat IS NOT INITIAL.                         "S4DK905838
          ls_so-erdat = lv_erdat.
        ENDIF.
        CLEAR lv_erdat.
*        S4DK905628-E
*        ls_so-additional1 = ls_kna1-katr1."属性1
        MOVE-CORRESPONDING ls_so TO ls_zpitlog0008." 抬头

        "项目赋值
        LOOP AT lt_vbap INTO ls_vbap.
          IF ls_vbak-auart <> '01' AND  ls_vbap-abgru <> '' .
            CONTINUE.
          ENDIF.
          READ TABLE lt_vbep INTO ls_vbep WITH KEY vbeln = ls_vbap-vbeln
                                                   posnr = ls_vbap-posnr.
          IF sy-subrc <>  0.
            CONTINUE.
          ENDIF.

          MOVE-CORRESPONDING ls_vbap TO ls_item.
          ls_item-zscbldh   = ls_vbap-zscbldh."生产备料单
          ls_item-zcpgg     = ls_vbap-zcpgg."规格
          ls_item-zbzgg     = ls_vbap-zbzgg."包装规格
          ls_item-kwmeng    = ls_vbap-kwmeng."数量

          IF ls_vbap-zypsl IS INITIAL.
            ls_item-zypsl = 'NULL'."样品数量
          ELSE.
            ls_item-zypsl = ls_vbap-zypsl."样品数量
          ENDIF.

          ls_item-zzhxs  = ls_vbap-umvkz / ls_vbap-umvkn."销售单位转基本单位系数
****************************20191021 短益条款转换******************************************
*          LS_ITEM-ZDYTK  = LS_VBAP-ZDYTK."短益条款
          PERFORM frm_read_domain    USING 'ZD_DYTK' ls_vbap-zdytk   CHANGING  ls_item-zdytk ."短益条款
          ls_item-werks  = ls_vbap-werks."发货工厂
          ls_item-zysjhq = ls_vbap-zysjhq." 原始交货期
          ls_item-znbjhq = ls_vbap-znbjhq." 内部交货期
          ls_item-zzztl  = ls_vbap-zzztl." 最早投料日期
          ls_item-zzwtl  = ls_vbap-zzwtl." 最晚投料日期
          ls_item-zzzbz  = ls_vbap-zzzbz." 最早包装时间
          ls_item-zzwbz  = ls_vbap-zzwbz." 最晚包装时间
          ls_item-zbomqq = ls_vbap-zbomqq." BOM欠缺物料
          READ TABLE lt_vbkd INTO DATA(ls_vbkd) WITH KEY vbeln = ls_vbap-vbeln
                                                         posnr = ls_vbap-posnr.
          IF sy-subrc = 0.
            ls_item-bstkd_e = ls_vbkd-bstkd_e." 客户合同单号
            ls_item-posex_e = ls_vbkd-posex_e." 客户合同分录
          ENDIF.

          READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_vbap-matnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            ls_item-zcpms = ls_makt-maktx.
          ENDIF.

          READ TABLE lt_t006a INTO DATA(ls_t006a) WITH KEY  msehi = ls_vbap-vrkme.
          IF sy-subrc IS INITIAL.
            ls_item-zvrkme = ls_t006a-msehl."销售单位
          ENDIF.

          PERFORM frm_read_long_text USING 'Z001'  ls_vbak-vbeln ls_vbap-posnr 'VBBP'  CHANGING  ls_item-zschxmbz."生产行项目备注
          PERFORM frm_read_domain    USING 'ZD_SFXBZ' ls_vbap-zsfxbz   CHANGING  ls_item-zsfxbz."是否新包装
          PERFORM frm_read_domain    USING 'ZD_XFLAG' ls_vbap-zsfxcp   CHANGING  ls_item-zsfxcp."是否新产品
          PERFORM frm_read_domain    USING 'ZD_XFLAG' ls_vbap-zsfbj    CHANGING  ls_item-zsfbj."是否报检
          PERFORM frm_read_domain    USING 'ZD_ZDSCQY' ls_vbap-zzdscqy CHANGING  ls_item-zzdscqy."指定生产区域
          PERFORM frm_read_domain    USING 'ZD_PHYQ' ls_vbap-zphyq     CHANGING  ls_item-zphyq."批号要求
          PERFORM frm_read_domain    USING 'ZD_BHCL' ls_vbap-zbhcl     CHANGING  ls_item-zbhcl."备货策略

          "需求计划新增字段 20191014
          PERFORM frm_read_long_text USING  '0002'  ls_vbak-vbeln ls_vbap-posnr 'VBBP'  CHANGING  ls_item-additional2."项目注释文本
          PERFORM frm_read_long_text USING  '0003'  ls_vbak-vbeln ls_vbap-posnr 'VBBP'  CHANGING  ls_item-additional3."包装注释文本
          PERFORM frm_read_long_text USING  '0004'  ls_vbak-vbeln ls_vbap-posnr 'VBBP'  CHANGING  ls_item-additional4."交货注释文本


          READ TABLE lt_vbep INTO ls_vbep WITH KEY vbeln = ls_vbap-vbeln posnr = ls_vbap-posnr.
          IF sy-subrc = 0.
            IF ls_vbak-auart = '01'.  "备用字段1  计划行类别
              ls_item-additional1 = ls_vbap-abgru.
              ls_item-zysjhq = ls_vbep-edatu." 原始交货期
            ELSE.
              ls_item-additional1 = ls_vbep-ettyp.
            ENDIF.
          ENDIF.
*          获取物料分类ZQUICK S4DK905628-S
          SELECT SINGLE a~atwrt
            INTO ls_item-additional5
            FROM ausp AS a
            INNER JOIN cabn AS b ON a~atinn = b~atinn
          WHERE objek = ls_vbap-matnr AND b~atnam = 'ZQUICK'." 快速放行
*        S4DK905628-E
          "20220822 Seashell 启用备用字段
          ls_item-additional5 = ls_vbap-zzqlx."周期类型
          ls_item-additional4 = ls_vbap-zkhfwdj."客户服务等级
          "20220822 end

          MOVE-CORRESPONDING ls_vbap TO ls_zpitlog0008a.
          ls_zpitlog0008a-zvrkme = ls_vbap-vrkme.
          ls_zpitlog0008a-zcpms  = ls_makt-maktx.
          APPEND ls_item TO lt_item.
        ENDLOOP.

        ls_so-sap_order_detail_list-sap_order_detail = lt_item.
        APPEND ls_so TO lt_so.
        ls_output-sap_order_list-sap_order =  lt_so.

*** 设置确认状态
        go_asyn_msg ?= lr_proxy->get_protocol(
                          if_wsprotocol=>async_messaging ).
        gv_ack_request =
          if_wsprotocol_async_messaging=>co_transport_acknowledgment.
        go_asyn_msg->set_acknowledgment_requested( gv_ack_request ).

*** 接口数据下发
        lr_proxy->si_sap2aps_so_out( ls_output )."调用pi接口

      ENDIF.

      lr_protocol ?= lr_proxy->get_protocol( if_wsprotocol=>message_id ). "获取message_id
      DATA(l_messageid) = lr_protocol->get_message_id( ).
      ls_zpitlog0008-message_id = l_messageid.
      ls_zpitlog0008a-message_id = l_messageid.
      ls_zpitlog0008-erdat = sy-datum.
      ls_zpitlog0008-ernam = sy-uname.
      ls_zpitlog0008-erzet = sy-uzeit.
      ls_zpitlog0008-type  = 'S'.
      ls_zpitlog0008-message  = '下发成功'.
      e_output-type    = ls_zpitlog0008-type .
      e_output-message = ls_zpitlog0008-message.
    CATCH cx_root INTO DATA(lr_root).
      ls_zpitlog0008-type  = 'E'.
      ls_zpitlog0008-message  = '下发失败'.
      ls_zpitlog0008-erdat = sy-datum.
      ls_zpitlog0008-ernam = sy-uname.
      ls_zpitlog0008-erzet = sy-uzeit.
      e_output-type    = ls_zpitlog0008-type .
      e_output-message = ls_zpitlog0008-message.
  ENDTRY.

  APPEND ls_zpitlog0008 TO lt_zpitlog0008.
  APPEND ls_zpitlog0008a TO lt_zpitlog0008a.
  CLEAR:ls_zpitlog0008,ls_zpitlog0008a.


*----------------------------------------------------------------------
*   日志记录-记录返回参数
*----------------------------------------------------------------------
  IF l_messageid IS INITIAL. l_messageid = l_guid. ENDIF.
  zcl_bc_public=>write_log( iv_logid = CONV #( l_messageid ) iv_intid = l_intid iv_ptype = 'E' is_data = e_output ). "记录输出参数

  IF lt_zpitlog0008 IS NOT  INITIAL.
    MODIFY zpitlog0008 FROM TABLE lt_zpitlog0008.
    IF sy-subrc = 0.
      lv_error = ''.
    ELSE.
      lv_error = 'X'.
    ENDIF.
  ENDIF.
  IF lt_zpitlog0008a IS NOT  INITIAL.
    MODIFY zpitlog0008a FROM TABLE lt_zpitlog0008a.
    IF sy-subrc = 0.
      lv_error = ''.
    ELSE.
      lv_error = 'X'.
    ENDIF.
  ENDIF.

  IF lv_error = ''.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFUNCTION.

FORM frm_read_long_text USING p_id p_vbeln p_posnr  p_object
                      CHANGING p_ltext.
  DATA tdlines TYPE TABLE OF tline.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = p_id
      language                = sy-langu
      name                    = CONV tdobname( p_vbeln && p_posnr )
      object                  = p_object
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

  CLEAR p_ltext.
  LOOP AT tdlines ASSIGNING FIELD-SYMBOL(<fs_line>).
    p_ltext = p_ltext && <fs_line>-tdline.
  ENDLOOP.

ENDFORM.

FORM frm_read_domain USING u_doman u_name
                     CHANGING c_text.
  DATA:

    lv_return TYPE sy-subrc,
    lt_domtab TYPE STANDARD TABLE OF dd07v,
    ls_domtab TYPE dd07v.


  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = u_doman
      text           = 'X'
      langu          = sy-langu
    IMPORTING
      rc             = lv_return
    TABLES
      dd07v_tab      = lt_domtab
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_domtab INTO ls_domtab WITH KEY domvalue_l = u_name.
  IF sy-subrc = 0.
    c_text = ls_domtab-ddtext.
  ENDIF.


ENDFORM.