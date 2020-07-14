*&---------------------------------------------------------------------*
*& 包含               ZMMR0017_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FRM_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*


FORM frm_f4_resnum .

    CALL TRANSACTION 'ME2N'.
    GET PARAMETER ID 'BES' FIELD s_ebeln.
  
  ENDFORM.
  
  FORM frm_get_data TABLES gt_detail STRUCTURE zmms_zmmr0017_detail
                      USING uv_ebeln uv_werks
                      CHANGING gs_head STRUCTURE zmms_zmmr0017_head
                              cv_message.
  
  *  DATA:aland   LIKE rf82t-land1,
  *       datab   LIKE rf82t-datab,
  *       mwskz   LIKE rf82t-mwskz,
  *       txjcd   LIKE rf82t-txjcd,
  *       t_ftaxp TYPE TABLE OF ftaxp WITH HEADER LINE.
  
    DATA: ls_ekpo    TYPE ekpo,
          lt_ekpo    TYPE TABLE OF ekpo,
          ls_adrcts2 TYPE adrcts2.
  
    DATA: lr_matnr TYPE RANGE OF matnr,
          lw_matnr LIKE LINE OF lr_matnr.
  
    DATA: lt_t007v TYPE STANDARD TABLE OF t007v,
          ls_t007v TYPE t007v.
    DATA: BEGIN OF wa_konp,
            mwskz LIKE konp-mwsk1,
            kbetr LIKE konp-kbetr,
            aland LIKE a003-aland,
          END OF wa_konp.
  
    IF uv_ebeln IS NOT INITIAL.
      s_ebeln = uv_ebeln.
    ENDIF.
    IF uv_werks IS NOT INITIAL.
      p_werks = uv_werks.
    ENDIF.
  
  * 单据审核状态判断
    SELECT SINGLE a~ebeln , a~frgke
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM ekko AS a
      INNER JOIN ekpo AS b
      ON b~ebeln = a~ebeln
      WHERE a~ebeln = @s_ebeln
        AND b~werks = @p_werks
        AND B~loekz = ''.
  
  *  IF  gs_head-frgke NE 'F'
  *    AND gs_head-frgke NE 'G'
  *    AND gs_head-frgke NE 'H'
  *    AND gs_head-frgke NE 'R'.
  *    cv_message = '采购订单未审批允许打印！'.
  *    EXIT.
  **    MESSAGE '采购订单未审批允许打印！' TYPE 'E'.
  *  ENDIF.
    IF gs_head IS INITIAL.
      cv_message = '采购订单不存在！'.
      EXIT.
    ENDIF.
  
    IF  gs_head-frgke EQ ''
      OR gs_head-frgke EQ 'R'
      OR gs_head-frgke EQ 'Z'."add by lanjingcun 20200702 新增审批通过状态Z
  
    ELSE.
      cv_message = '采购订单未审批允许打印！'.
      EXIT.
  *    MESSAGE '采购订单未审批允许打印！' TYPE 'E'.
    ENDIF.
  
  
  *  订单和供应商信息      供应商代码   供应商名称， 卖方公司代码，  订单日期 ，订单号，订单类型, 审核状态,  供应商地址，供应商传真, 供应商邮编
    SELECT SINGLE e~lifnr, l~name1 AS lifnr_n, bukrs, bedat, ebeln ,e~bsart,  frgke, l~stras, l~telfx, pstl2 AS pstl2_n
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM ekko AS e
      INNER JOIN lfa1 AS l ON e~lifnr = l~lifnr
  *    LEFT JOIN adr2 AS a ON a~addrnumber = l~adrnr
      WHERE e~ebeln =  @s_ebeln .
  *    AND a~consnumber EQ 1 .
  
  
  * 文件编号
    SELECT SINGLE ztxbh
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM zqmt007
      WHERE werks = @gs_head-bukrs AND zbddh ='ZMM017'.
  
  * 供应商电话
    SELECT SINGLE a~tel_number AS telf1
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM ekko AS e
      INNER JOIN lfa1 AS l ON e~lifnr = l~lifnr
      LEFT JOIN adr2 AS a ON a~addrnumber = l~adrnr
      WHERE e~ebeln =  @s_ebeln
      AND a~consnumber EQ 1 .
  
  * 供应商联系人
    SELECT SINGLE l~verkf
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM ekko AS e
      INNER JOIN lfm1 AS l ON e~lifnr = l~lifnr
      WHERE e~ebeln =  @s_ebeln.
  
  * 供应商开户银行，银行账号(银行账号+参考明细)
    SELECT SINGLE b~banka AS banka_n, l~bankn AS bankn_n, l~bkref
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM lfbk AS l
      INNER JOIN bnka AS b ON b~bankl = l~bankl
      WHERE l~lifnr = @gs_head-lifnr
      AND l~bvtyp ='0001'.
  
    gs_head-bankn_n = gs_head-bankn_n && gs_head-bkref .
  
  * 供应商税号
    SELECT SINGLE taxnum AS taxnum_n
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM dfkkbptaxnum
      WHERE partner = @gs_head-lifnr
      AND taxtype ='CN0'.
  
  * 结算方式
    SELECT SINGLE v~text1 AS zterm_t
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM ekko AS e
      INNER JOIN lfm1 AS l ON e~lifnr = l~lifnr
      INNER JOIN t052u AS v ON l~zterm = v~zterm
      WHERE e~ebeln =  @s_ebeln.
  
  * 币别
    SELECT SINGLE t~ktext AS waers_k
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM ekko AS e
      INNER JOIN tcurt AS t ON e~waers = t~waers
      WHERE e~ebeln =  @s_ebeln.
  
    CASE  gs_head-waers_k .
      WHEN '人民币'.
        gs_head-zcurr ='￥'.
      WHEN '美元'.
        gs_head-zcurr = '$'.
    ENDCASE.
  
  
  * 买方公司名称
    SELECT SINGLE t~butxt
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM ekko AS e
      INNER JOIN t001 AS t ON e~bukrs = t~bukrs
      WHERE e~ebeln =  @s_ebeln.
  
    SELECT SINGLE a~text1
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM t052u AS a
      INNER JOIN ekko AS b
      ON b~zterm = a~zterm
      WHERE b~ebeln = @gs_head-ebeln
        AND a~spras = 1.
  
  * 卖方银行 基本户 银行账号
  
  *  SELECT SINGLE t~butxt
  *    INTO CORRESPONDING FIELDS OF @gs_head
  *    FROM FCLM_BAM_AMD AS e
  *    INNER JOIN t001 AS t ON e~bukrs = t~bukrs
  *    WHERE e~ebeln =  @s_ebeln.
  
    SELECT SINGLE f~acc_num AS bankn, b~banka
      INTO CORRESPONDING FIELDS OF @gs_head
      FROM fclm_bam_amd AS f
      INNER JOIN bnka AS b ON b~bankl = f~bankl
      WHERE f~bukrs =  @gs_head-bukrs
      AND f~acc_type_id ='01'AND f~status ='02'.
  
  * 卖方 税号
  
    SELECT *
      INTO TABLE @DATA(lt_adrcts2)
      FROM adrcts2.
  
    LOOP AT lt_adrcts2 INTO ls_adrcts2.
      IF ls_adrcts2-objkey+3(4) = gs_head-bukrs.
        gs_head-taxnum = ls_adrcts2-remark.
      ENDIF.
    ENDLOOP.
  
  
  * 送货地址（工厂）
    CASE gs_head-bukrs.
      WHEN '8100'.
        gs_head-adrnr_t ='一厂：汕头市泰山路83号；二厂：汕头市黄山路珠业南街11号'.
  *      gs_head-reccadr ='汕头市泰山路83号'.
      WHEN '8200'.
        gs_head-adrnr_t ='马鞍山地址：安徽省马鞍山经济技术开发区红旗南路1980号'.
  *      gs_head-reccadr ='马鞍山地址：安徽省马鞍山经济技术开发区红旗南路1980号'.
    ENDCASE.
  
  *                     卖方公司地址   卖方公司传真   卖方公司邮编
    SELECT SINGLE a~street,a~fax_number, a~post_code1
    INTO CORRESPONDING FIELDS OF @gs_head
    FROM adrc AS a
    INNER JOIN t001 AS t ON  t~adrnr = a~addrnumber
    WHERE t~bukrs = @gs_head-bukrs.
  
  *  IF gs_head-bukrs = '8100'.
  *    CONCATENATE '一厂：' gs_head-street INTO gs_head-street.
  *  ENDIF.
  
  
  *  CASE gs_head-bukrs.
  *    WHEN '8100'.
  *      SELECT SINGLE e~street,e~fax_number, e~post_code1
  *      INTO CORRESPONDING FIELDS OF @gs_head
  *      FROM adrc AS a
  *      INNER JOIN t001 AS t ON  t~adrnr = a~addrnumber
  *      WHERE t~bukrs ='8100'.
  *    WHEN '8200'.
  *      SELECT SINGLE e~street,e~fax_number, e~post_code1
  *      INTO CORRESPONDING FIELDS OF @gs_head
  *      FROM adrc AS a
  *      INNER JOIN t001 AS t ON  t~adrnr = a~addrnumber
  *      WHERE t~bukrs ='8200'.
  *  ENDCASE.
  
  
  * 分录
    SELECT *
      INTO TABLE @lt_ekpo
      FROM ekpo
      WHERE ebeln =  @s_ebeln
        and loekz = ''.
  
    CLEAR: lr_matnr, lw_matnr.
  
    IF gs_head-bsart NE 'ZNF'.
  *          物料编码  物料名称  规格型号
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
  *  SELECT m~matnr, m~maktx, a~matkl, a~groes
  *    INTO TABLE @DATA(lt_matnr)
  *    FROM makt AS m
  *    INNER JOIN mara AS a
  *    ON m~matnr = a~matnr
  *    INNER JOIN ekpo AS e
  *    ON e~matnr  = m~matnr or e~ematn = m~matnr
  *    FOR ALL ENTRIES IN @lt_ekpo
  *    WHERE m~matnr = @lt_ekpo-matnr.
  
    ENDIF.
  
  *                      要求到货日期
    SELECT t~ebeln, t~ebelp, t~eindt
      INTO TABLE @DATA(lt_eket)
      FROM eket AS t
      INNER JOIN ekpo AS o ON o~ebeln = t~ebeln AND o~ebelp = t~ebelp
      FOR ALL ENTRIES IN @lt_ekpo
      WHERE t~ebeln = @lt_ekpo-ebeln AND t~ebelp = @lt_ekpo-ebelp.
  
  *                   税码  税率  国家
    SELECT k~mwsk1 AS mwskz, k~kbetr, a~aland
      INTO TABLE @DATA(lt_konp)
      FROM konp AS k
      INNER JOIN ekpo AS o ON o~mwskz = k~mwsk1
      INNER JOIN a003 AS a ON a~knumh = k~knumh
      FOR ALL ENTRIES IN @lt_ekpo
      WHERE o~ebeln = @lt_ekpo-ebeln AND o~ebelp = @lt_ekpo-ebelp
      AND k~kappl = 'TX'.
  
  *  SELECT stpo~stlnr, stpo~stlkn, stpo~stpoz, stpo~meins, stpo~menge
  *    INTO TABLE @DATA(lt_stpo)
  *    FROM stpo
  *    INNER JOIN resb ON stpo~stlnr = resb~stlnr AND stpo~stlkn = resb~stlkn AND stpo~stpoz = resb~stpoz
  *    FOR ALL ENTRIES IN @lt_resb
  *    WHERE stpo~stlnr = @lt_resb-stlnr AND stpo~stlkn = @lt_resb-stlkn AND stpo~stpoz = @lt_resb-stpoz.
    SELECT e~infnr, e~ekorg, e~esokz, e~werks,
           e~netpr, e~peinh, e~mwskz, a~matnr            "采购信息记录中的净价 价格单位 税码 物料编码
      INTO TABLE @DATA(lt_eine)
      FROM eine AS e
      INNER JOIN eina AS a
      ON e~infnr = a~infnr
      FOR ALL ENTRIES IN @lt_ekpo
      WHERE e~esokz = '2'
      AND e~infnr = @lt_ekpo-infnr .
  
    READ TABLE lt_konp INTO wa_konp INDEX 1.
    IF sy-subrc = 0.
      CLEAR: lt_t007v.
      SELECT *
        FROM t007v
        INTO TABLE lt_t007v
        WHERE aland = wa_konp-aland  "国家
          AND datam <> '00000000'.   "生效日期不为空
  
      IF sy-subrc = 0.
        SORT lt_t007v BY datum uzeit DESCENDING.
      ENDIF.
    ENDIF.
  
    SELECT a~kappl, a~kschl, a~lifnr, a~matnr, a~ekorg,   "应用程序, 条件类型,,
           a~werks, a~esokz, a~datbi, a~datab,            "       ,采购信息记录分类, 条件记录有效截止日期, 条件记录有效起始日
           b~knumh, b~kopos, b~kbetr, b~kpein             "条件记录号, 条件的序列号, 不存在等级的条件金额或百分比, 条件定价单位
      FROM a017 AS a
      INNER JOIN konp AS b
      ON b~knumh = a~knumh
      INTO TABLE @DATA(lt_a017)
      FOR ALL ENTRIES IN @lt_ekpo
      WHERE a~kappl = 'M'
        AND a~esokz = '2'
        AND a~matnr = @lt_ekpo-matnr
        AND a~werks = @lt_ekpo-werks.
  
    SELECT a~ebeln, a~frgke, a~bedat, a~bukrs, a~ekorg, a~waers,
           b~lifnr, b~name1, b~telfx, b~stras, b~adrnr            "供应商,名称 1,传真号,街道和房屋号,地址
      FROM ekko AS a
      INNER JOIN lfa1 AS b
      ON b~lifnr = a~lifnr
      INTO TABLE @DATA(lt_ekko)
  *    FOR ALL ENTRIES IN @IT_EBELN
      WHERE a~ebeln = @gs_head-ebeln.
  
    "设置明细信息
  
    LOOP AT lt_ekpo INTO ls_ekpo.
  
      IF ls_ekpo-pstyp EQ  '2' . " EKPO- PSTYP EQ 2 寄售
  *                     含税单价 不含税单价  税率
  *      SELECT a~matnr, e~netpr, e~peinh, e~mwskz
  *      INTO @DATA(lt_eine)
  *      FROM eine AS e
  *      INNER JOIN eina AS a ON e~infnr = a~infnr
  *      WHERE e~ekorg = '8000' AND e~werks = '8100' AND e~esokz = '2'
  *      AND e~infnr = @ls_ekpo-infnr .
  *      ENDSELECT.
        READ TABLE lt_eine INTO DATA(ls_eine) WITH KEY infnr = ls_ekpo-infnr.
        IF sy-subrc = 0.
          ls_ekpo-netpr = ls_eine-netpr .
          ls_ekpo-peinh = ls_eine-peinh .
  
        ENDIF.
      ENDIF.
  
      CLEAR gs_detail.
      MOVE-CORRESPONDING ls_ekpo TO gs_detail.
  
  * 原辅料 取MPN 物料编码
  
      IF lt_ekpo IS NOT INITIAL.
  *      IF gs_detail-matkl+0(1) = 1.
  *        gs_detail-matnr = ls_ekpo-ematn.
  *        IF lt_matnr IS NOT INITIAL.
  *          gs_detail-maktx = lt_matnr[ matnr = ls_ekpo-ematn ]-maktx.
  *          gs_detail-groes = lt_matnr[ matnr = ls_ekpo-ematn ]-groes.
  **        gs_detail-matnr = lt_matnr[ matnr = ls_ekpo-matnr ]-ematn.
  *        ENDIF.
  *      ELSE.
  *        gs_detail-matnr = ls_ekpo-matnr.
  *        IF lt_matnr IS NOT INITIAL.
  *          gs_detail-maktx = lt_matnr[ matnr = ls_ekpo-matnr ]-maktx.
  *          gs_detail-groes = lt_matnr[ matnr = ls_ekpo-matnr ]-groes.
  **        gs_detail-matnr = lt_matnr[ matnr = ls_ekpo-matnr ]-matnr.
  *
  *          IF gs_detail-matkl+0(1) = 2.
  *            DATA(lt_value) = zcl_bc_public=>get_class_values( key = gs_detail-matnr objtable = 'MARA' classnum = gs_detail-matkl classtype = '001' ).
  *            READ TABLE lt_value INTO DATA(ls_value) WITH KEY charact = 'ZMATDET'. "包材取 材质明细
  *            IF sy-subrc EQ 0.
  *              IF ls_value-value_char IS NOT INITIAL.
  *                gs_detail-groes = ls_value-value_char.        "30字符
  *              ELSE.
  *                gs_detail-groes = ls_value-value_char_long.   "70字符  Zhanghb_20191220
  *              ENDIF.
  *            ENDIF.
  *          ENDIF.
  *
  *        ENDIF.
  *      ENDIF.
  
  * 原辅料仍取按物料编码大小量刚      Zhanghb_20191220
        gs_detail-matnr = ls_ekpo-matnr.
        IF lt_matnr IS NOT INITIAL.
          gs_detail-maktx = lt_matnr[ matnr = ls_ekpo-matnr ]-maktx.
          gs_detail-groes = lt_matnr[ matnr = ls_ekpo-matnr ]-groes.
          IF gs_detail-matkl+0(1) = 2.        "包材
            DATA(lt_value) = zcl_bc_public=>get_class_values( key = gs_detail-matnr objtable = 'MARA' classnum = gs_detail-matkl classtype = '001' ).
            READ TABLE lt_value INTO DATA(ls_value) WITH KEY charact = 'ZMATDET'. "包材取 材质明细
            IF sy-subrc EQ 0.
              IF ls_value-value_char IS NOT INITIAL.
                gs_detail-groes = ls_value-value_char.        "30字符
              ELSE.
                gs_detail-groes = ls_value-value_char_long.   "70字符  Zhanghb_20191220
              ENDIF.
            ENDIF.
          ELSE.
            IF gs_detail-matkl+0(1) = 1.      "原辅料组
              gs_detail-matnr = ls_ekpo-ematn.
              IF lt_matnr IS NOT INITIAL.
                gs_detail-maktx = lt_matnr[ matnr = ls_ekpo-ematn ]-maktx.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
  
  *      gs_detail-maktx = lt_matnr[ matnr = ls_ekpo-matnr ]-maktx.
  *      gs_detail-groes = lt_matnr[ matnr = ls_ekpo-matnr ]-groes.
        gs_detail-eindt = lt_eket[ ebeln = ls_ekpo-ebeln ebelp = ls_ekpo-ebelp ]-eindt.
  
  **        gs_detail-netpr = lt_eine[ matnr = gs_detail-matnr ]-netpr / lt_eine[ matnr = gs_detail-matnr ]-peinh .                "不含税单价
  **        gs_detail-kbetr = ls_konp[ mwskz = ls_eine-mwskz ]-kbetr.           " 税率
  **        gs_detail-bsprs = gs_detail-netpr * ( 1 + gs_detail-kbetr / 1000 ). "含税单价
  *      ELSE .
  *      gs_detail-netpr = gs_detail-netpr / gs_detail-peinh.                "含税单价
  *      gs_detail-kbetr = lt_konp[ mwskz = ls_ekpo-mwskz ]-kbetr.           " 税率
  
        IF ls_ekpo-pstyp EQ  '2'.
  *        CLEAR: LS_EKKO.
          READ TABLE lt_ekko INTO DATA(ls_ekko) WITH KEY ebeln = ls_ekpo-ebeln.
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
              gs_detail-brtwr = gs_detail-bsprs * ls_ekpo-menge.
              EXIT.
            ENDLOOP.
          ENDIF.
        ELSE.
          CLEAR: ls_t007v.
          READ TABLE lt_t007v INTO ls_t007v WITH KEY mwskz = ls_ekpo-mwskz.
          IF sy-subrc = 0.
            gs_detail-kbetr = ls_t007v-kbetr.
          ENDIF.
          gs_detail-bsprs = ls_ekpo-brtwr / ls_ekpo-menge.
          gs_detail-netpr = gs_detail-bsprs / ( 1 + gs_detail-kbetr / 1000 ).
        ENDIF.
  *      IF ls_ekpo-pstyp EQ  '2'.   "寄售 含税金额  不含税金额
  **        gs_detail-bsprs = gs_detail-netpr / ( 1 + gs_detail-kbetr / 1000 ) ."不含税单价
  *        gs_detail-bsprs = gs_detail-netpr * ( 1 + gs_detail-kbetr / 1000 ) ."含税单价
  *        gs_detail-netwr = gs_detail-netpr * gs_detail-menge.                "不含税单价
  *        gs_detail-brtwr = gs_detail-bsprs * gs_detail-menge.                "含税单价
  *      ELSE.
  *        gs_detail-bsprs = gs_detail-brtwr / gs_detail-menge.                "含税单价
  *        gs_detail-netpr = gs_detail-netpr / ( 1 + gs_detail-kbetr / 1000 ). "不含税单价
  *      ENDIF.
  *      SELECT single kalsm INTO TABLE @DATA(l_kalsm)
  *         FROM t005
  *         WHERE land1 = 'CN'.
  ** SELECT SINGLE txjcd INTO l_txjcd FROM ttxj WHERE kalsm = l_kalsm.
  *
  *      CALL FUNCTION 'GET_TAX_PERCENTAGE'
  *        EXPORTING
  *          aland   = 'CN'
  *          datab   = '2019-10-12'
  *          mwskz   = 'J1'
  *          txjcd   =
  *        tables
  *          t_ftaxp = lt_ftaxp.
  
  
  *      CALL FUNCTION 'GET_TAX_PERCENTAGE'
  *        EXPORTING
  *          aland   = 'CN'
  *          datab   = '2019/12/31'
  *          mwskz   = 'J2'
  **          txjcd   = ''
  **         export
  *        tables
  *          t_ftaxp = t_ftaxp.
  *
  *      IF t_ftaxp IS NOT INITIAL.
  **    p_line = header[ 1 ]-system_status.
  *      ENDIF.
  
  *      gs_detail-maktxi = lt_matnr[ matnr = ls_resb-matnr ]-maktx.
  *      gs_detail-menge = lt_stpo[ stlnr = ls_resb-stlnr stlkn = ls_resb-stlkn stpoz = ls_resb-stpoz ]-menge.
  
  
  * 生产厂家（制造商）
  
        IF gs_detail-matkl+0(1) = 1.
          SELECT SINGLE b~name_org1
          INTO CORRESPONDING FIELDS OF @gs_detail
          FROM ekpo AS e
          INNER JOIN mara AS m ON e~ematn = m~matnr
          INNER JOIN but000 AS b ON b~partner = m~mfrnr
          WHERE e~ebeln =  @gs_detail-ebeln
             AND e~ebelp = @gs_detail-ebelp .
        ELSE.
          SELECT SINGLE b~name_org1
          INTO CORRESPONDING FIELDS OF @gs_detail
          FROM ekpo AS e
          INNER JOIN mara AS m ON e~matnr = m~matnr
          INNER JOIN but000 AS b ON b~partner = m~mfrnr
          WHERE e~ebeln =  @gs_detail-ebeln
             AND e~ebelp = @gs_detail-ebelp .
  *         m~matnr = @gs_detail-matnr
        ENDIF.
  
  *    送货地址    EKPO-LGORT
        CASE ls_ekpo-lgort+0(1).
          WHEN '1'.
            gs_detail-adrnr = '一厂'.
          WHEN '2'.
            gs_detail-adrnr = '二厂'.
        ENDCASE.
  
  *   采购订单行项目文本
        PERFORM frm_read_txtl USING gs_detail-ebeln gs_detail-ebelp CHANGING p_ltext.
  
  
      ENDIF.
  
      IF gs_head-bsart = 'ZNF'.  "非库存物资订单类型，无物料编码 Zhanghb_20191211
        gs_detail-maktx = ls_ekpo-txz01 .
      ENDIF.
  
  
  *      READ TABLE lt_charg INTO DATA(ls_charg) WITH KEY charg = ls_resb-charg.
  *      IF sy-subrc = 0.
  *        gs_detail-hsdat = ls_charg-hsdat.
  *      ENDIF.
  
  *      PERFORM frm_get_charg_character USING 'ZPSPE'
  *        PERFORM frm_get_charg_character USING 'ZLIFNR'
  *                                            ls_resb
  *                                      CHANGING gs_detail.
  
      APPEND gs_detail TO gt_detail.
      gs_head-amount = gs_head-amount + gs_detail-brtwr.  "含税金额合计
  
    ENDLOOP.
  
  
  
  * 设置总行数17行
    CLEAR gs_line.
    CLEAR gs_detail.
    DESCRIBE TABLE gt_detail LINES gs_line.
  
    p_page = gs_line / 11.
  
    IF gs_line <> 0 AND floor( p_page ) = 0 .
      DO 11 - gs_line  TIMES.
        APPEND gs_detail TO gt_detail.
      ENDDO.
    ENDIF.
  
    IF gs_line <> 0 AND floor( p_page ) >= 1 .
      DO ( floor( p_page ) + 1 ) * 11 + 1  - gs_line  TIMES.
        APPEND gs_detail TO gt_detail.
      ENDDO.
  
    ENDIF.
  
  
  
  *  gt_head-amount = sum(gt_detail-netwr).
  
  
  
  
  ENDFORM.
  
  
  
  
  FORM frm_print TABLES t_gt_detail STRUCTURE zmms_zmmr0017_detail
          USING u_gs_head LIKE zmms_zmmr0017_head
          u_form_name TYPE tdsfname.
    DATA: fm_name TYPE rs38l_fnam.
  
  
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
  
    CALL FUNCTION fm_name
      EXPORTING
  *     ARCHIVE_INDEX    =
  *     ARCHIVE_INDEX_TAB          =
  *     ARCHIVE_PARAMETERS         =
  *     CONTROL_PARAMETERS         =
  *     MAIL_APPL_OBJ    =
  *     MAIL_RECIPIENT   =
  *     MAIL_SENDER      =
  *     OUTPUT_OPTIONS   =
  *     USER_SETTINGS    = 'X'
        gs_head          = u_gs_head
        gs_line          = gs_line
  *   IMPORTING
  *     DOCUMENT_OUTPUT_INFO       =
  *     JOB_OUTPUT_INFO  =
  *     JOB_OUTPUT_OPTIONS         =
      TABLES
        gt_detail        = t_gt_detail
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
  
  
  
  
  ENDFORM.
  
  
  
  FORM frm_read_txtl USING p_ebeln p_ebelp
                          CHANGING  p_ltext .
    DATA tdlines TYPE TABLE OF tline.
    DATA p_tdname LIKE stxl-tdname.
  
  *DATA :lc_name          TYPE thead-tdname,
  *        lit_lines        TYPE TABLE OF tline,
  *        lwa_lines        TYPE tline.
  *CLEAR: lit_lines ,
  *       lc_name,
  *       lwa_lines.
  *    lc_name = vbeln.
  
    CONCATENATE p_ebeln p_ebelp INTO p_tdname.
  
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
  *     CLIENT                  = SY-MANDT
        id                      = 'F01'
        language                = sy-langu
        name                    = CONV tdobname( p_tdname )
        object                  = 'EKPO'
  *     ARCHIVE_HANDLE          = 0
  *     LOCAL_CAT               = ' '
  *     IMPORTING
  *     HEADER                  =
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
  
    IF sy-subrc = 4.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
  *       CLIENT                  = SY-MANDT
          id                      = 'F01'
          language                = sy-langu
          name                    = CONV tdobname( p_tdname )
          object                  = 'EKPO'
  *       ARCHIVE_HANDLE          = 0
  *       LOCAL_CAT               = ' '
  *     IMPORTING
  *       HEADER                  =
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
    ENDIF.
  
  
  *    LOOP AT tdlines INTO lwa_lines.
  **      GWA_OUTPUT-RISE_TEXT = LWA_LINES-TDLINE .
  *      CONCATENATE txt lwa_lines-tdline INTO txt.
  *    ENDLOOP.
  
    IF tdlines IS NOT INITIAL.
      LOOP AT tdlines ASSIGNING FIELD-SYMBOL(<fs_line>).
        p_ltext = p_ltext && <fs_line>-tdline.
      ENDLOOP.
  
      gs_detail-ltxts = p_ltext.  "备注
  
      CLEAR p_ltext.
    ENDIF.
  
  ENDFORM.
  
  
  *&---------------------------------------------------------------------*
  *& Form frm_conv_amount
  *&---------------------------------------------------------------------*
  *& text
  *&---------------------------------------------------------------------*
  *& -->  p1        text
  *& <--  p2        text
  *&---------------------------------------------------------------------*
  FORM frm_conv_amount USING p_source
                   CHANGING c_result.
    DATA: lv_scr(30) TYPE c, lv_res(60) TYPE c,lv_fen(2) TYPE c .
    DATA: lv_len TYPE i, lv_c1 TYPE i, lv_c2 TYPE i, lv_c3 TYPE i, lv_c4 TYPE i.
    DATA: lv_d1(1) TYPE c, lv_d2(1) TYPE c, lv_d3 TYPE i.
    DATA: lv_digit(2)  TYPE c, lv_weight(2) TYPE c.
    DATA: lv_rule1(20) TYPE c VALUE '零壹贰叁肆伍陆柒捌玖'.
    DATA: lv_rule2(30) TYPE c VALUE '分角元拾佰仟万拾佰仟亿拾佰仟万'.
  
    lv_scr = abs( p_source ) * 100.
    CONDENSE lv_scr NO-GAPS.
    IF lv_scr = '0'.
      lv_res = '零元'.
    ELSE.
      lv_len = strlen( lv_scr ).
      lv_c1 = 0.
      lv_d1 = '0'.
      CLEAR lv_res.
      DO lv_len TIMES.
        lv_c1 = lv_c1 + 1.
        lv_c2 = lv_len - lv_c1.
        lv_d2 = lv_scr+lv_c2(1) .
        IF lv_d2 = '0'.
          lv_d3 = 0.
        ELSE.
          lv_d3 = lv_d2.
        ENDIF.
        lv_digit = lv_rule1+lv_d3(1) .
        lv_c3 = ( lv_c1 - 1 ) .
        lv_weight = lv_rule2+lv_c3(1) .
        IF lv_d2 = '0'.
          IF lv_c1 = 3.
            lv_digit = ''.
          ELSEIF lv_c1 = 7.
            lv_digit = ''.
            IF lv_len > 10 .
              lv_c4 = lv_len - 10.
              IF lv_scr+lv_c4(4) = '0000'.
                lv_weight = ''.
              ENDIF.
            ENDIF.
          ELSEIF lv_c1 = 11.
            lv_digit = ''.
          ELSEIF lv_d1 = '0'.
            lv_digit = ''.
            lv_weight = ''.
          ELSE.
            lv_weight = ''.
          ENDIF.
        ENDIF.
        CONCATENATE lv_digit lv_weight lv_res INTO lv_res .
        lv_d1 = lv_d2.
      ENDDO.
    ENDIF.
    lv_len = strlen( lv_res ) - 1.
    lv_fen = lv_res+lv_len(1).
    IF lv_fen <> '分' .
      CONCATENATE lv_res '整' INTO c_result.
    ELSE.
      c_result = lv_res.
    ENDIF.
  ENDFORM. "conv_amount
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