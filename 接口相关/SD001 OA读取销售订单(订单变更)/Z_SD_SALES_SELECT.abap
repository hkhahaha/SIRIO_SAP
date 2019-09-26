FUNCTION z_sd_sales_select.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_OA2SAP_SALESORDER
*"     REFERENCE(MESSAGE_ID) TYPE  SXMSGUID
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_OA2SAP_SALESORDER_RET
*"----------------------------------------------------------------------



  DATA: l_success.
  DATA:ls_input  TYPE zdt_oa2sap_salesorder,
       lt_input2 TYPE TABLE OF zdt_oa2sap_salesorder_header,
       ls_input2 TYPE zdt_oa2sap_salesorder_header.
  ls_input = input-mt_oa2sap_salesorder.
  lt_input2 = ls_input-header." 可以取到第一个值
  READ TABLE lt_input2 INTO ls_input2 INDEX 1.


  DATA:ls_output TYPE zdt_oa2sap_salesorder_ret.




*检查输入条件是否都满足
  PERFORM frm_check_input_data USING    ls_input2
  CHANGING l_success.
*  CHECK l_success IS INITIAL.

*获取数据
  PERFORM frm_get_info_data USING    ls_input2
  CHANGING ls_output.
  output-mt_oa2sap_salesorder_ret = ls_output.
ENDFUNCTION.

FORM frm_check_input_data  USING u_input TYPE zdt_oa2sap_salesorder_header
CHANGING c_success.
  DATA:u_input_one  TYPE zdt_oa2sap_salesorder_header,
       u_input_two  TYPE zdt_oa2sap_salesorder_item_tab,
       u_input_two2 TYPE zdt_oa2sap_salesorder_item.
  u_input_two = u_input-item.
  READ TABLE u_input_two INTO u_input_two2 INDEX 1.
  IF  u_input-vbeln IS INITIAL
  OR  u_input_two2-posnr IS INITIAL.
    c_success = 'E'.
  ENDIF.
ENDFORM.

FORM frm_get_info_data USING    u_input TYPE zdt_oa2sap_salesorder_header
CHANGING c_output TYPE zdt_oa2sap_salesorder_ret.
  TYPES: BEGIN OF ty_vbap1,
           posnr    TYPE  vbap-posnr, "行项目号
           zdjzsj   TYPE  kzwi1, "单价（主数据）
           zzjg     TYPE  kzwi1, "正价格（单价）
           zfjg     TYPE  kzwi1, "负价格（单价）
           zzkl     TYPE  kzwi1, "折扣率
           werks    TYPE  werks_ext, "工厂
           zcpgg    TYPE  zsd_cpgg, "规格
           zbzgg    TYPE  zsd_bzgg, "包装规格
           kwmeng   TYPE kwmeng , "订单数量
           vrkme    TYPE  vrkme, "单位
           zysjhq   TYPE zsd_ysjhq , "原始交货期
           znbjhq   TYPE zsd_nbjhq , "内部交货期
           zsfbj    TYPE zsd_sfbj , "是否报检
           zypsl    TYPE  zsd_ypsl, "样品数量
           zdytk    TYPE  zsd_dytk, "短溢条款
           zphyq    TYPE zsd_phyq , "批号要求
           zbhcl    TYPE zsd_bhcl , "备货策略
           zbomqq   TYPE zsd_bomqq , "BOM欠缺物料
           zzztl    TYPE zsd_zztl , "最早投料日期
           zzwtl    TYPE zsd_zwtl , "最晚投料日期
           zzzbz    TYPE zsd_zzbz , "最早包装时间
           zzwbz    TYPE zsd_zwbz , "最晚包装时间
           bstkd_e  TYPE vbkd-bstkd_e , "客户合同单号
           posex_e  TYPE vbkd-posex_e , "客户合同分录
           zschxmbz TYPE c400 , "生产行项目备注
         END OF ty_vbap1.
  DATA:ls_vbap1 TYPE ty_vbap1,
       lt_vbap1 TYPE TABLE OF ty_vbap1.
  DATA: lt_vbak TYPE TABLE OF vbak,
        ls_vbak TYPE vbak,
        lt_vbap TYPE TABLE OF vbap,
        ls_vbap TYPE vbap,
        lt_kna1 TYPE TABLE OF kna1,
        ls_kna1 TYPE kna1,
        lt_vbkd TYPE TABLE OF vbkd,
        ls_vbkd TYPE vbkd,
        lt_makt TYPE TABLE OF makt,
        ls_makt TYPE makt.
  DATA: BEGIN OF ls_vbak_ext,
          vbeln TYPE vbak-vbeln,
        END OF ls_vbak_ext.
  DATA: BEGIN OF ls_vbap_ext,
          vbeln TYPE vbak-vbeln,
          posnr TYPE vbap-posnr,
        END OF ls_vbap_ext.
  DATA: lt_vbak_ext LIKE TABLE OF ls_vbak_ext,
        lt_vbak_tmp LIKE TABLE OF ls_vbak_ext,
        lt_vbap_ext LIKE TABLE OF ls_vbap_ext,
        lt_vbap_tmp LIKE TABLE OF ls_vbap_ext.
  DATA: lt_so   TYPE zdt_oa2sap_salesorder_ret_tab1,
        ls_so   TYPE zdt_oa2sap_salesorder_ret_so,

        lt_item TYPE zdt_oa2sap_salesorder_ret_tab2,
        ls_item TYPE zdt_oa2sap_salesorder_ret_item.
  DATA:lt_vbpa   TYPE TABLE OF vbpa,
       ls_vbpa   TYPE vbpa,
       lt_adrc   TYPE TABLE OF adrc,
       ls_adrc   TYPE adrc,
       lt_but000 TYPE TABLE OF but000,
       ls_but000 TYPE but000.


  DATA: lv_str  TYPE string,
        lv_str2 LIKE lv_str.

  DATA:u_input_one  TYPE zdt_oa2sap_salesorder,
       u_input_two  TYPE  zdt_oa2sap_salesorder2_ite_tab, "zdt_oa2sap_salesorder2_item,
       u_input_two2 TYPE zdt_oa2sap_salesorder2_item,
       lv_posnr     TYPE string.
  u_input_two = u_input-item.
  READ TABLE u_input_two INTO u_input_two2 INDEX 1.
  "获取查询的数据
  lv_str = '%' && u_input-vbeln && '%'.

*  修改开始，日期2019.9.3，修改人HK

*抬头字段
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_vbak
  FROM vbak
  WHERE vbak~vbeln LIKE lv_str.

  IF lt_vbak IS NOT INITIAL.

    SELECT
    *
    INTO CORRESPONDING FIELDS OF TABLE lt_kna1
    FROM kna1
    FOR ALL ENTRIES IN lt_vbak
    WHERE kna1~kunnr = lt_vbak-kunnr.

    SELECT
    *
    INTO CORRESPONDING FIELDS OF TABLE lt_vbkd
    FROM vbkd
    FOR ALL ENTRIES IN lt_vbak
    WHERE vbkd~vbeln = lt_vbak-vbeln.



    DATA: i_posnr TYPE string.
*  抬头数据查询完毕，下面是行项目的数据
    IF u_input_two2 IS NOT INITIAL.
      i_posnr = u_input_two2-posnr.
      SELECT
      *
      INTO CORRESPONDING FIELDS OF TABLE lt_vbap
      FROM vbap
      FOR ALL ENTRIES IN lt_vbak
      WHERE vbap~posnr = i_posnr
      AND   vbap~vbeln = lt_vbak-vbeln.
    ELSE.
      SELECT
      *
      INTO CORRESPONDING FIELDS OF TABLE lt_vbap
      FROM vbap
      FOR ALL ENTRIES IN lt_vbak
      WHERE vbap~vbeln = lt_vbak-vbeln.
    ENDIF.
    IF lt_vbap IS NOT INITIAL.
      SELECT
      *
      INTO CORRESPONDING FIELDS OF TABLE lt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_vbap
      WHERE makt~matnr = lt_vbap-matnr.
    ENDIF.

    " 下面进行读内表插数据操作
    LOOP AT lt_vbak INTO ls_vbak.
      MOVE-CORRESPONDING ls_vbak TO ls_so.
      READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_vbak-kunnr.
      IF sy-subrc = 0.
        ls_so-name = ls_kna1-name1 &&  ls_kna1-name1 && ls_kna1-name1 &&  ls_kna1-name1 .
        MOVE-CORRESPONDING ls_kna1 TO ls_so.
      ENDIF.
      READ TABLE lt_vbkd INTO ls_vbkd WITH KEY vbeln = ls_vbak-vbeln.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_vbkd TO ls_so.
        MOVE ls_vbkd-bstkd_e TO ls_so-zkhpo.
      ENDIF.
*      取ZZSKKNO字段
      IF sy-subrc = 0.
        SELECT SINGLE
          *
        FROM vbpa
        INTO CORRESPONDING FIELDS OF ls_vbpa
        WHERE vbpa~vbeln = ls_vbak-vbeln
        AND vbpa~parvw = 'Z1'.
        MOVE ls_vbpa-kunnr TO ls_so-zzskkno.
      ENDIF.
*     取ZZSKK字段
      SELECT SINGLE
      *
      INTO CORRESPONDING FIELDS OF ls_but000
      FROM but000
      WHERE but000~partner = ls_vbpa-kunnr.
      ls_so-zzskk = ls_but000-name_org1 && ls_but000-name_org2
      && ls_but000-name_org3 && ls_but000-name_org4.

      "上面是抬头字段的信息
      LOOP AT lt_vbap INTO ls_vbap WHERE vbeln = ls_vbak-vbeln.
        MOVE-CORRESPONDING ls_vbap TO ls_vbap1.
        APPEND ls_vbap1 TO lt_vbap1.
        LOOP AT lt_vbap1 INTO ls_vbap1.
          ls_item-posnr = ls_vbap-posnr.
          ls_item-matnr = ls_vbap-matnr.
          IF sy-subrc = 0.
            READ TABLE lt_makt INTO ls_makt WITH KEY maktx = ls_vbap-matnr.
            ls_item-maktx = ls_makt-maktx.
          ENDIF.
*          接下来一行一行的插入字段名、字段值还有字段描述。
*          ls_item-fieldname = 'zdjzsj'.
*          ls_item-fielddes = '单价（主数据）'.
*          ls_item-fieldvalue = ls_vbap1-zdjzsj.
*          APPEND ls_item TO lt_item.
*          ls_item-fieldname = 'zzjg'.
*          ls_item-fielddes = '正价格（单价）'.
*          ls_item-fieldvalue = ls_vbap1-zzjg.
*          APPEND ls_item TO lt_item.
*          ls_item-fieldname = 'zfjg'.
*          ls_item-fielddes = '负价格（单价）'.
*          ls_item-fieldvalue = ls_vbap1-zfjg.
*          APPEND ls_item TO lt_item.
*          ls_item-fieldname = 'zzkl'.
*          ls_item-fielddes = '折扣率'.
*          ls_item-fieldvalue = ls_vbap1-zzkl.
*          APPEND ls_item TO lt_item.
          ls_item-fieldname = 'werks'.
          ls_item-fielddes = '工厂'.
          ls_item-fieldvalue = ls_vbap1-werks.
*          IF ls_vbap1-werks IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zcpgg'.
          ls_item-fielddes = '规格'.
          ls_item-fieldvalue = ls_vbap1-zcpgg.
*          IF ls_vbap1-zcpgg IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zbzgg'.
          ls_item-fielddes = '包装规格'.
          ls_item-fieldvalue = ls_vbap1-zbzgg.
*          IF ls_vbap1-zbzgg IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'kwmeng'.
          ls_item-fielddes = '订单数量'.
          ls_item-fieldvalue = ls_vbap1-kwmeng.
*          IF ls_vbap1-kwmeng IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'vrkme'.
          ls_item-fielddes = '单位'.
          ls_item-fieldvalue = ls_vbap1-vrkme.
*          IF ls_vbap1-vrkme IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zysjhq'.
          ls_item-fielddes = '原始交货期'.
          ls_item-fieldvalue = ls_vbap1-zysjhq.
*          IF ls_vbap1-zysjhq IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'znbjhq'.
          ls_item-fielddes = '内部交货期'.
          ls_item-fieldvalue = ls_vbap1-znbjhq.
*          IF ls_vbap1-znbjhq IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zsfbj'.
          ls_item-fielddes = '是否报检'.
          ls_item-fieldvalue = ls_vbap1-zsfbj.
*          IF ls_vbap1-zsfbj IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zypsl'.
          ls_item-fielddes = '样品数量'.
          ls_item-fieldvalue = ls_vbap1-zypsl.
*          IF ls_vbap1-zypsl IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zdytk'.
          ls_item-fielddes = '短溢条款'.
          ls_item-fieldvalue = ls_vbap1-zdytk.
*          IF ls_vbap1-zdytk IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zphyq'.
          ls_item-fielddes = '批号要求'.
          ls_item-fieldvalue = ls_vbap1-zphyq.
*          IF ls_vbap1-zphyq IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zbhcl'.
          ls_item-fielddes = '备货策略'.
          ls_item-fieldvalue = ls_vbap1-zbhcl.
*          IF ls_vbap1-zbhcl IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zbomqq'.
          ls_item-fielddes = 'BOM欠缺物料'.
          ls_item-fieldvalue = ls_vbap1-zbomqq.
*          IF ls_vbap1-zbomqq IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zzztl'.
          ls_item-fielddes = '最早投料日期'.
          ls_item-fieldvalue = ls_vbap1-zzztl.
*          IF ls_vbap1-zzztl IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zzwtl'.
          ls_item-fielddes = '最晚投料日期'.
          ls_item-fieldvalue = ls_vbap1-zzwtl.
*          IF ls_vbap1-zzwtl IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zzzbz'.
          ls_item-fielddes = '最早包装时间'.
          ls_item-fieldvalue = ls_vbap1-zzzbz.
*          IF ls_vbap1-zzzbz IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zzwbz'.
          ls_item-fielddes = '最晚包装时间'.
          ls_item-fieldvalue = ls_vbap1-zzwbz.
*          IF ls_vbap1-zzwbz IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'bstkd_e'.
          ls_item-fielddes = '客户合同单号'.
          ls_item-fieldvalue = ls_vbap1-bstkd_e.
*          IF ls_vbap1-bstkd_e IS NOT INITIAL.
             APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'posex_e'.
          ls_item-fielddes = '客户合同分录'.
          ls_item-fieldvalue = ls_vbap1-posex_e.
*          IF ls_vbap1-posex_e IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
          ls_item-fieldname = 'zschxmbz'.
          ls_item-fielddes = '生产行项目备注'.
          ls_item-fieldvalue = ls_vbap1-zschxmbz.
*          IF ls_vbap1-zschxmbz IS NOT INITIAL.
            APPEND ls_item TO lt_item.
*          ENDIF.
        ENDLOOP.
      ENDLOOP.




      "下面是行内容信息
*        LOOP AT lt_vbap INTO ls_vbap WHERE vbeln = ls_vbak-vbeln.
*          MOVE-CORRESPONDING ls_vbap TO ls_item.
*          MOVE-CORRESPONDING ls_vbkd TO ls_item.
*          IF sy-subrc = 0.
*            READ TABLE lt_makt INTO ls_makt WITH KEY maktx = ls_vbap-matnr.
*            MOVE ls_makt-maktx TO ls_item-zcpms.
*          ENDIF.
*          CLEAR ls_vbkd.
*          CLEAR ls_vbap.
*          CLEAR ls_makt.
*          APPEND ls_item TO lt_item.
*          CLEAR ls_item.
*        ENDLOOP.
      "下面需要往抬头注入item，否则的话是没有数据的。

      "下面进行ITEM内部操作



      CLEAR ls_kna1.
      ls_so-item = lt_item.
      APPEND ls_so TO lt_so.
      CLEAR ls_so.
    ENDLOOP.
    c_output-so = lt_so.
    IF c_output-so IS NOT INITIAL.
      c_output-message = '成功'.
      c_output-type = 'Y'.
    ENDIF.

  ENDIF.
ENDFORM.