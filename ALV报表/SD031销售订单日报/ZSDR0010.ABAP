*&---------------------------------------------------------------------*
*& Report ZSDR0010
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0010.
*&---------------------------------------------------------------------*
*& 程序名：ZSDR0010
*&作者：Seashell Huang
*&模块：SD模块
*&创建日期：15.10.2019 10:51:57
*&功能描述：销售订单日报表
*&---------------------------------------------------------------------*
TYPE-POOLS:slis."调用系统存在的类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE, "调用系统存在的FIELDCAT类型。
     layout   TYPE slis_layout_alv, "调用系统存在的LAYOUT
     "FIELDCAT用于ALV的结构定义，LAYOUT用于定义ALV的输出格式，后者可以覆盖前者，后者是可选的
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:vbap,vbak."定义系统存在的透明标
TYPES:BEGIN OF ty_all,"定义结构，用于接收VBAP、VBAK所需要的全部字段
        zapszt   TYPE    vbap-zapszt , "  APS状态
        vbeln    TYPE    vbak-vbeln  , "  单据号
        posnr    TYPE    vbap-posnr  , "  行号
        ernam    TYPE    vbak-ernam  , "  制单人
        vkorg    TYPE    vbak-vkorg  , "  销售组织
        auart    TYPE    string  , "  订单类型
        vtweg    TYPE    string , "  分销渠道
        werks    TYPE    vbap-werks  , "  工厂
        vkbur    TYPE    string  , "  二级部门
        mandt    TYPE    vbak-mandt  , "  集团名称
        kunnr    TYPE    string      , "  售达方
        kunnr2   TYPE    vbak-kunnr      , "  售达方编号
        zckgj    TYPE    string  , "  出口国家
        waerk    TYPE    vbak-waerk  , "  订单币种
        matnr    TYPE    vbap-matnr  , "  产品编码
        arktx    TYPE    vbap-arktx  , "  产品名称
*        matwa    TYPE    vbap-matwa  , "  产品名称
        zkhwl    TYPE    vbap-zkhwl  , "  客户产品编码
        zkhwlms  TYPE    vbap-zkhwlms  , "  客户产品名称
        zcpgg    TYPE    vbap-zcpgg  , "  规格型号
        zbzgg    TYPE    vbap-zbzgg  , "  包装规格
        meins    TYPE    vbap-meins  , "  基本计量单位
        kwmeng   TYPE    vbap-kwmeng , "  销售数量
        zypsl    TYPE    vbap-zypsl  , "  样品数量
        vrkme    TYPE    vbap-vrkme  , "  销售单位
        mwsbp    TYPE    vbap-mwsbp  , "  销项税额
        netwr    TYPE    vbap-netwr  , "  不含税金额
        zdytk    TYPE    vbap-zdytk  , "  短溢条款
        zysjhq   TYPE    vbap-zysjhq , "  客户要求交货日期
        zatpdfrq TYPE    vbap-zatpdfrq , "  ATP答复日期
        znbjhq   TYPE    vbap-znbjhq , "  实际交货日期
        zsfxbz   TYPE    vbap-zsfxbz , "  新包装
        zsfxcp   TYPE    vbap-zsfxcp , "  新产品
        zsfbj    TYPE    vbap-zsfbj  , "  是否报检
        zphyq    TYPE    vbap-zphyq  , "  批号要求
        zzztl    TYPE    vbap-zzztl  , "  最早投料日期
        zzwtl    TYPE    vbap-zzwtl  , "  最晚投料日期
        zzzbz    TYPE    vbap-zzzbz  , "  最早包装时间
        zzwbz    TYPE    vbap-zzwbz  , "  最晚包装时间
        zbhcl    TYPE    vbap-zbhcl  , "  备货策略
        zbomqq   TYPE    vbap-zbomqq , "  BOM欠缺物料
        zblxy    TYPE    vbak-zblxy  , "  是否有备料协议
        zsfxkh   TYPE    vbak-zsfxkh , "  新客户
*        auart    TYPE    vbak-auart  , "  订单类型
*        vkorg    TYPE    vbak-vkorg  , "  销售组织
*        vtweg    TYPE    vbak-vtweg  , "  分销渠道
        spart    TYPE    vbak-spart  , "  产品组
*        werks    TYPE    vbap-werks  , "  工厂
*        kunnr    TYPE    vbak-kunnr  , "  客户
*        ernam    TYPE    vbak-ernam  , "  创建人
        erdat    TYPE    vbak-erdat  , "  创建日期
*        matnr    TYPE    vbap-matnr  , "  物料编码
        objnr    TYPE    vbap-objnr  , " 对象编号
        kvgr1    TYPE    string  , "  销售渠道/客户类型
        umziz    TYPE    vbap-umziz  , "转换因子（分子）
        umzin    TYPE    vbap-umzin  ,  "转换因此（分母）
        knumv    TYPE vbak-knumv, "凭证条件号
      END OF ty_all.
TYPES:BEGIN OF ty_out,"定义结构，用于接收所有的ALV字段数据
        zapszt      TYPE    vbap-zapszt , "  APS状态
        state       TYPE    string      , "  订单状态
        vbeln       TYPE    vbak-vbeln  , "  单据号
        posnr       TYPE    vbap-posnr  , "  行号
        ernam       TYPE    string      , "  制单人
        vkorg       TYPE    vbak-vkorg  , "  销售组织
        auart       TYPE    string  , "  订单类型
        vtweg       TYPE    string  , "  分销渠道
        kvgr1       TYPE    string  , "  销售渠道/客户类型
        werks       TYPE    vbap-werks  , "  工厂
        lgort       TYPE    lips-lgort  , "  库存地点
        vkbur       TYPE    string  , "  二级部门
        saleman     TYPE    string      , "  业务员
        mc_name1    TYPE    but000-mc_name1, "检索项
        mandt       TYPE    vbak-mandt  , "  集团名称
        kunnr       TYPE    vbak-kunnr  , "  售达方(客户名称)
        kunnr2      TYPE    vbak-kunnr  , "  售达方(编号)
        custname    TYPE    string      , "  实际客户名称
        custname2   TYPE    string      , "  实际客户编号
        kukla       TYPE    kna1-kukla  , "  实际客户等级
        zterm       TYPE    string  , "  付款方式
        zckgj       TYPE    string  , "  出口国家
        bstkd_e     TYPE    vbkd-bstkd_e, "  客户PO
        waerk       TYPE    vbak-waerk  , "  订单币种
        kursk       TYPE    vbkd-kursk  , "  汇率
        matnr       TYPE    vbap-matnr  , "  产品编码
        arktx       TYPE    vbap-arktx  , "  产品名称
        zstdname    TYPE    string      , "  标准名称
        zkhwl       TYPE    vbap-zkhwl  , "  客户产品编码
        zkhwlms     TYPE    vbap-zkhwlms, "  客户产品名称
        pmatn       TYPE    mvke-pmatn  , "  定价参考物料编码
        maktx       TYPE    makt-maktx  , "  品名
        zpro        TYPE    string      , "  是否益生菌
        matkl       TYPE    mara-matkl  , "  物料小类编码
        maktxs      TYPE    makt-maktx  , "  物料小类名称
        zcpgg       TYPE    vbap-zcpgg  , "  规格型号
        zbzgg       TYPE    vbap-zbzgg  , "  包装规格
        zpwsx       TYPE    zsdt0013    , "  批文属性
        zprotype    TYPE    string      , "  产品类型
        zappsou     TYPE    string      , "  批文来源
        zcotype     TYPE    string      , "  合作类型
        meins       TYPE    vbap-meins  , "  基本计量单位
        number      TYPE    string      , "  基本计量数量
        kwmeng      TYPE    vbap-kwmeng , "  销售数量
        zypsl       TYPE    vbap-zypsl  , "  样品数量
        vrkme       TYPE    vbap-vrkme  , "  销售单位
        kbetr       TYPE    p DECIMALS 2     , "  含税单价
        kwert       TYPE    string      , "  折扣率
        mwsbp2      TYPE    vbap-mwsbp  , "  折扣金额
        kwertdis    TYPE    p DECIMALS 2     , "  税率
        mwsbp       TYPE    vbap-mwsbp  , "  销项税额
        netwr       TYPE    vbap-netwr  , "  不含税金额
        priceall    TYPE    string      , "  价税合计
        zdytk       TYPE    vbap-zdytk  , "  短溢条款
        zysjhq      TYPE    vbap-zysjhq , "  客户要求交货日期
        zatpdfrq    TYPE    vbap-zatpdfrq, "  ATP答复日期
        znbjhq      TYPE    vbap-znbjhq , "  实际交货日期
        innum       TYPE    string      , "  入库量
        outnum      TYPE    string      , "  出库量
        invoice     TYPE    string      , "  开票量
        invoiced    TYPE    string      , "  开票日期
        zsfxbz      TYPE    vbap-zsfxbz , "  新包装
        zsfxcp      TYPE    vbap-zsfxcp , "  新产品
        zsfbj       TYPE    vbap-zsfbj  , "  是否报检
        zphyq       TYPE    vbap-zphyq  , "  批号要求
        zzztl       TYPE    vbap-zzztl  , "  最早投料日期
        zzwtl       TYPE    vbap-zzwtl  , "  最晚投料日期
        zzzbz       TYPE    vbap-zzzbz  , "  最早包装时间
        zzwbz       TYPE    vbap-zzwbz  , "  最晚包装时间
        bstkd_e2    TYPE    vbkd-bstkd_e, "  客户合同单号
        posex_e     TYPE    vbkd-posex_e, "  客户合同分录
        note        TYPE    string      , "  生产行项目备注
        zbhcl       TYPE    vbap-zbhcl  , "  备货策略
        zbomqq      TYPE    vbap-zbomqq , "  BOM欠缺物料
        zblxy       TYPE    vbak-zblxy  , "  是否有备料协议
        zsfxkh      TYPE    vbak-zsfxkh , "  新客户
        pronote     TYPE    string      , "  生产相关文本
        salenot     TYPE    string      , "  销售文本
        voicenot    TYPE    string      , "  发票文本
        invoicenote TYPE string         , "  开票文本
      END OF ty_out.

***********  ALV 定义 **********************************************

DATA: lt_out TYPE TABLE OF ty_out, "用于存储ALV内表的数据
      ls_out TYPE ty_out.

"开始选择界面的数据操作
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
sauart    FOR   vbak-auart  ,"  订单类型
svbeln FOR vbap-vbeln,"订单号
sposnr FOR vbap-posnr,"行号
pvkorg    FOR   vbak-vkorg  NO INTERVALS,"  销售组织
svtweg    FOR   vbak-vtweg  ,"  分销渠道
sspart    FOR   vbak-spart  ,"  产品组
swerks    FOR   vbap-werks  ,"  工厂
skunnr    FOR   vbak-kunnr  ,"  客户
sernam    FOR   vbak-ernam  ,"  创建人
serdat    FOR   vbak-erdat  ,"  创建日期
smatnr    FOR   vbap-matnr  ."  物料编码
SELECTION-SCREEN END OF BLOCK blk.


START-OF-SELECTION.
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.

END-OF-SELECTION.




FORM getdata.
  TYPES:BEGIN OF ty_all2,
          matnr90(90) TYPE c,
        END OF ty_all2.
*  TYPES:BEGIN OF ty_test,
*          flag TYPE c,
*        END OF ty_test.
*  DATA:lt_test TYPE TABLE OF ty_test,
*       ls_test TYPE ty_test.
  DATA:lt_vbak          TYPE TABLE OF vbak,
       ls_vbak          TYPE vbak,
       lt_vbap          TYPE TABLE OF vbap,
       ls_vbap          TYPE vbap,
       lt_all           TYPE TABLE OF ty_all, "用于存储VBAP、VBAK相关的数据
       ls_all           TYPE ty_all,
       ls_jest          TYPE jest,
       ls_adrp          TYPE adrp,
       ls_knvv          TYPE knvv,
       ls_lips          TYPE lips,
       ls_but000        TYPE but000,
       lt_but000        TYPE TABLE OF but000,
       ls_vbkd          TYPE vbkd,
       ls_mvke          TYPE mvke,
       ls_makt          TYPE makt,
       lt_vbfa          TYPE TABLE OF vbfa,
       ls_vbfa          TYPE vbfa,
       ls_mara          TYPE mara,
       ls_prcd_elements TYPE prcd_elements,
       ls_mska          TYPE mska,
       lt_ausp          TYPE TABLE OF ausp,
       ls_ausp          TYPE ausp,
       lt_tvakt         TYPE TABLE OF tvakt,
       ls_tvakt         TYPE tvakt,
       lt_tvtwt         TYPE TABLE OF tvtwt,
       ls_tvtwt         TYPE tvtwt,
       lt_tvv1t         TYPE TABLE OF tvv1t,
       ls_tvv1t         TYPE tvv1t,
       lt_t005t         TYPE TABLE OF t005t,
       ls_t005t         TYPE t005t,
       lt_tvkbt         TYPE TABLE OF tvkbt,
       ls_tvkbt         TYPE tvkbt,
       ls_kna1          TYPE kna1.
  DATA: lt_lines LIKE TABLE OF tline,
        ls_lines LIKE tline.
  DATA:lt_all2 TYPE TABLE OF ty_all2,
       ls_all2 TYPE ty_all2.
*&---------------------------------------------------------------------*
*& 程序名：ZSDR0010
*&作者：Seashell Huang
*&模块：
*&创建日期：30.10.2019 19:01:42
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：添加中文描述信息，对应分别是订单类型、分销渠道、销售渠道、二级部门、出口国家
*&
*&---------------------------------------------------------------------*
  "取出所有的数据到对应的内表中
  SELECT
    auart
    bezei
  INTO CORRESPONDING FIELDS OF TABLE lt_tvakt
  FROM tvakt
  WHERE tvakt~spras = '1'.
  "取出所有的分销渠道的中文描述
  SELECT
    vtweg
    vtext
  INTO CORRESPONDING FIELDS OF TABLE lt_tvtwt
  FROM tvtwt
  WHERE tvtwt~spras = '1'.
  "获取销售渠道的全部中文描述
  SELECT
    bezei
    kvgr1
  INTO CORRESPONDING FIELDS OF TABLE lt_tvv1t
  FROM tvv1t
  WHERE spras = '1'.
  "获取出口国家的全部中文描述
  SELECT
    land1
    landx
  INTO CORRESPONDING FIELDS OF TABLE lt_t005t
  FROM t005t
  WHERE spras = '1'.
  "取二级部门的全部中文描述
  SELECT
    vkbur
    bezei
  INTO CORRESPONDING FIELDS OF TABLE lt_tvkbt
  FROM tvkbt
  WHERE spras = '1'.


  "首先先查询VBAK
  SELECT
  zapszt    "  APS状态
  vbak~vbeln     "  单据号
  posnr     "  行号
  vbak~ernam     "  制单人
  vkorg     "  销售组织
  auart     "  订单类型
  vtweg     "  分销渠道
  werks     "  工厂
  vkbur     "  二级部门
  vbak~mandt     "  集团名称
  kunnr     "  售达方
  zckgj     "  出口国家
  vbak~waerk     "  订单币种
  matnr     "  产品编码
  arktx     "  产品名称
  zkhwl     "  客户产品编码
  zkhwlms   "  客户产品名称
  zcpgg     "  规格型号
  zbzgg     "  包装规格
  meins     "  基本计量单位
  kwmeng    "  销售数量
  zypsl     "  样品数量
  vrkme     "  销售单位
  mwsbp     "  销项税额
  vbap~netwr     "  不含税金额
  zdytk     "  短溢条款
  zysjhq    "  客户要求交货日期
  zatpdfrq  "  ATP答复日期
  znbjhq    "  实际交货日期
  zsfxbz    "  新包装
  zsfxcp    "  新产品
  zsfbj     "  是否报检
  zphyq     "  批号要求
  zzztl     "  最早投料日期
  zzwtl     "  最晚投料日期
  zzzbz     "  最早包装时间
  zzwbz     "  最晚包装时间
  zbhcl     "  备货策略
  zbomqq    "  BOM欠缺物料
  zblxy     "  是否有备料协议
  zsfxkh    "  新客户
  vbak~spart     "  产品组
  vbak~erdat     "  创建日期
  vbak~objnr     " 抬头对象编号
  vbap~umziz     "转换因子
  vbap~umzin     "转换因子
  INTO CORRESPONDING FIELDS OF TABLE lt_all
  FROM vbak
  INNER JOIN vbap
  ON vbak~vbeln = vbap~vbeln
  WHERE vbap~matnr IN smatnr
    AND vbap~werks IN swerks
    AND vbap~vbeln IN svbeln
    AND vbap~posnr IN sposnr
    AND vbak~auart IN sauart
    AND vbak~vkorg IN pvkorg
    AND vbak~vtweg IN svtweg
    AND vbak~spart IN sspart
    AND vbak~kunnr IN skunnr
    AND vbak~ernam IN sernam
*    AND vbak~vbeln = '0200000829'
    AND vbak~erdat IN serdat.

  "上面获取到的是所有有关VBAP和VBAK的数据

  "将物料编码转类型
  LOOP AT lt_all INTO ls_all.
    ls_all2-matnr90 = ls_all-matnr.
    APPEND ls_all2 TO lt_all2.
  ENDLOOP.

  "查询出全部物料的所需要的全部批次属性特征值
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_ausp
  FROM ausp
  FOR ALL ENTRIES IN lt_all2
  WHERE ausp~objek = lt_all2-matnr90
    AND ausp~atinn IN ( '0000000075','0000000052','0000000074','0000000067','0000000035' ).

  "将一部分可以直接插入的数据插入到内表中
  LOOP AT lt_all INTO ls_all.

    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = '0000000075'.
    ls_out-zappsou = ls_ausp-atwrt.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = '0000000052'.
    ls_out-zcotype = ls_ausp-atwrt.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = '0000000074'.
    ls_out-zprotype = ls_ausp-atwrt.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = '0000000067'.
    ls_out-zpro = ls_ausp-atwrt.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = '0000000035'.
    ls_out-zstdname = ls_ausp-atwrt.

    "因为TY_OUT的结构包含了TY_ALL的结构，所以直接MOVE
    MOVE-CORRESPONDING ls_all TO ls_out.
    "售达方编号
    ls_out-kunnr2 = ls_all-kunnr.
    "订单状态
    SELECT SINGLE
      stat
    INTO CORRESPONDING FIELDS OF ls_jest
    FROM jest
    WHERE jest~objnr = ls_all-objnr
      AND jest~stat LIKE 'E%'.
    IF ls_jest-stat = 'E0001'.
      ls_out-state = '创建'.
    ELSEIF ls_jest-stat = 'E0002'.
      ls_out-state = '业务一审'.
    ELSEIF ls_jest-stat = 'E0003'.
      ls_out-state = '财务二审'.
    ENDIF.
    "制单人(取描述)
    SELECT SINGLE
      name_text
    INTO CORRESPONDING FIELDS OF ls_adrp
    FROM adrp
    INNER JOIN usr21
    ON adrp~persnumber = usr21~persnumber
    WHERE usr21~bname = ls_all-ernam.
    ls_out-ernam = ls_adrp-name_text.
    CLEAR ls_adrp.
    " 销售渠道/客户类型
    SELECT SINGLE
      kvgr1
    INTO CORRESPONDING FIELDS OF ls_knvv
    FROM knvv
    WHERE knvv~kunnr = ls_all-kunnr
      AND knvv~vkorg = ls_all-vkorg
      AND knvv~vtweg = ls_all-vtweg
      AND knvv~spart = ls_all-spart.
    ls_out-kvgr1 = ls_knvv-kvgr1.
    CLEAR ls_knvv.
    "库存地点
    SELECT SINGLE
      lgort
    INTO CORRESPONDING FIELDS OF ls_lips
    FROM lips
    WHERE lips~vgbel = ls_all-vbeln
      AND lips~vgpos = ls_all-posnr.
    ls_out-lgort = ls_lips-lgort.
    CLEAR ls_lips.
    "业务员
    SELECT SINGLE
      name_org1
      name_org2
      name_org3
      name_org4
    INTO CORRESPONDING FIELDS OF ls_but000
    FROM but000
    INNER JOIN vbpa
    ON but000~partner = vbpa~kunnr
    WHERE vbpa~parvw = 'Z3'
      AND vbpa~vbeln = ls_all-vbeln
      AND vbpa~posnr = ''.
    ls_out-saleman = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
    CLEAR ls_but000.
    "检索项
    SELECT SINGLE
      mc_name1
      name_org1
      name_org2
      name_org3
      name_org4
    INTO CORRESPONDING FIELDS OF ls_but000
    FROM but000
    WHERE but000~partner = ls_all-kunnr.
    ls_out-mc_name1 = ls_but000-mc_name1.
    "客户名称
    ls_out-kunnr = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
    "集团名称
    "实际客户名称
    SELECT SINGLE
    partner
    name_org1
    name_org2
    name_org3
    name_org4
    INTO CORRESPONDING FIELDS OF ls_but000
    FROM but000
    INNER JOIN vbpa
    ON but000~partner = vbpa~kunnr
    WHERE vbpa~parvw = 'Z1'
    AND vbpa~vbeln = ls_all-vbeln
    AND vbpa~posnr = ''.
    ls_out-custname = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
    ls_out-custname2 = ls_but000-partner.
    CLEAR ls_but000.
    "实际客户等级
    SELECT SINGLE
      kukla
    INTO CORRESPONDING FIELDS OF ls_kna1
    FROM kna1
    INNER JOIN vbpa
    ON vbpa~kunnr = kna1~kunnr
    WHERE vbpa~parvw = 'Z1'
      AND vbpa~vbeln = ls_all-vbeln
      AND vbpa~posnr = ''.
    ls_out-kukla = ls_kna1-kukla.
    IF ls_out-kukla IS NOT INITIAL.
      SELECT SINGLE
        vtext
      INTO ls_out-kukla
      FROM tkukt
      WHERE tkukt~spras = '1'
      AND tkukt~kukla = ls_out-kukla.
    ENDIF.
    CLEAR ls_kna1.
    "客户PO、付款方式、汇率
    SELECT SINGLE
      bstkd_e
      kursk
      zterm
    INTO CORRESPONDING FIELDS OF ls_vbkd
    FROM vbkd
    WHERE vbkd~posnr = ls_all-posnr
      AND vbkd~vbeln = ls_all-vbeln.
    IF ls_vbkd-bstkd_e IS INITIAL.
      SELECT SINGLE
      bstkd_e
      INTO CORRESPONDING FIELDS OF ls_vbkd
      FROM vbkd
      WHERE vbkd~posnr = ''
      AND vbkd~vbeln = ls_all-vbeln.
    ENDIF.
    IF ls_vbkd-zterm IS INITIAL.
      SELECT SINGLE
      zterm
      INTO CORRESPONDING FIELDS OF ls_vbkd
      FROM vbkd
      WHERE vbkd~posnr = ''
      AND vbkd~vbeln = ls_all-vbeln.
    ENDIF.
    IF ls_vbkd-kursk IS INITIAL.
      SELECT SINGLE
      kursk
      INTO CORRESPONDING FIELDS OF ls_vbkd
      FROM vbkd
      WHERE vbkd~posnr = ''
      AND vbkd~vbeln = ls_all-vbeln.
    ENDIF.
    "付款方式、客户PO
*    ls_out-zterm = ls_vbkd-zterm.
    IF ls_vbkd-zterm IS NOT INITIAL.
      SELECT SINGLE
        text1
      INTO ls_out-zterm
      FROM t052u
      WHERE t052u~spras = '1'
      AND t052u~zterm = ls_vbkd-zterm.
    ENDIF.
    ls_out-bstkd_e = ls_vbkd-bstkd_e.
    "汇率
    ls_out-kursk = ls_vbkd-kursk.


    " 定价参考物料编码
    SELECT SINGLE
      pmatn
    INTO CORRESPONDING FIELDS OF ls_mvke
    FROM mvke
    WHERE mvke~matnr = ls_all-matnr.
    ls_out-pmatn = zcl_bc_public=>conv_by_ddic( i_input = ls_mvke-pmatn i_out = 'X' ).
    CLEAR ls_all2-matnr90.
    CLEAR ls_out-zappsou.
    ls_all2-matnr90 = ls_out-pmatn.
    SELECT SINGLE
      atwrt
    INTO ls_out-zappsou
    FROM  ausp
    WHERE ausp~atinn  = '0000000075'
    AND ausp~objek = ls_all2-matnr90.

*    ls_out-pmatn = ls_mvke-pmatn.
    IF ls_mvke-pmatn IS NOT INITIAL.
      SELECT SINGLE
        maktx
      INTO CORRESPONDING FIELDS OF ls_makt
      FROM makt
      WHERE makt~matnr = ls_mvke-pmatn.
      ls_out-maktx = ls_makt-maktx.
    ENDIF.

    "物料小类编码
    SELECT SINGLE
      matkl
    INTO CORRESPONDING FIELDS OF ls_mara
    FROM mara
    WHERE mara~matnr = ls_all-matnr.
    ls_out-matkl = ls_mara-matkl.
    "物料小类名称
    CLEAR ls_makt.
    SELECT SINGLE
      wgbez
    INTO ls_out-maktxs
    FROM t023t
    WHERE t023t~matkl = ls_mara-matkl
    AND t023t~spras = '1'.



    "基本计量数量
    IF ls_all-umzin IS NOT INITIAL.
      ls_out-number = ls_all-kwmeng * ( ls_all-umziz / ls_all-umzin ).
    ENDIF.

    "含税单价
    SELECT SINGLE
      kbetr"含税单价
      kwert"含税金额
    INTO CORRESPONDING FIELDS OF ls_prcd_elements
    FROM prcd_elements
    INNER JOIN vbak
    ON vbak~knumv = prcd_elements~knumv
    WHERE vbak~vbeln = ls_all-vbeln
    AND prcd_elements~kschl IN ( 'ZP00','ZP02' )
    AND prcd_elements~kinak = ''.
    ls_out-kbetr = ls_prcd_elements-kbetr.

    "折扣金额
    ls_out-mwsbp2 = ls_prcd_elements-kwert - ls_all-netwr - ls_all-mwsbp.
    "折扣率
    IF ls_prcd_elements-kwert IS NOT INITIAL AND ls_all-kwmeng IS NOT INITIAL.
      ls_out-kwert = ls_out-mwsbp2 / ( ls_prcd_elements-kwert * ls_all-kwmeng ).
    ENDIF.

    "税率
    CLEAR ls_prcd_elements.
    SELECT SINGLE
      kbetr
    INTO CORRESPONDING FIELDS OF ls_prcd_elements
    FROM prcd_elements
    INNER JOIN vbak
    ON vbak~knumv = prcd_elements~knumv
    WHERE vbak~vbeln = ls_all-vbeln
    AND prcd_elements~kschl = 'MWSI'
    AND prcd_elements~kposn = ls_out-posnr.
    ls_out-kwertdis = ls_prcd_elements-kbetr .
    "价格合计
    ls_out-priceall = ls_all-netwr + ls_all-mwsbp.
    "入库量（当前库存）
    SELECT SINGLE
      kalab
      kains
      kaspe
      kavla
      kavin
      kavsp
    INTO CORRESPONDING FIELDS OF ls_mska
    FROM mska
    WHERE mska~vbeln = ls_all-vbeln
      AND mska~posnr = ls_all-posnr
      AND mska~matnr = ls_all-matnr.
    ls_out-innum = ls_mska-kalab + ls_mska-kains + ls_mska-kaspe + ls_mska-kavla + ls_mska-kavin + ls_mska-kavsp.
    IF ls_out-number IS NOT INITIAL AND ls_out-kwmeng IS NOT INITIAL.
      ls_out-innum = ls_out-innum / ( ls_out-number / ls_out-kwmeng  ).
    ENDIF.

    "出库量
    SELECT
      rfmng
      vbtyp_v
    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
    FROM vbfa
    WHERE vbfa~vbelv = ls_all-vbeln
      AND vbfa~posnv = ls_all-posnr
      AND vbfa~vbtyp_n IN ( 'R','h' ).
    LOOP AT lt_vbfa INTO ls_vbfa.
      ls_out-outnum = ls_out-outnum + ls_vbfa-rfmng.
    ENDLOOP.
    CLEAR ls_vbfa.
    "开票量
    SELECT
    rfmng
    vbtyp_v
    erdat
    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
    FROM vbfa
    WHERE vbfa~vbelv = ls_all-vbeln
    AND vbfa~posnv = ls_all-posnr
    AND vbfa~vbtyp_v = 'M'
    ORDER BY erdat DESCENDING.
    LOOP AT lt_vbfa INTO ls_vbfa.
      ls_out-invoice = ls_out-invoice + ls_vbfa-rfmng.
    ENDLOOP.
    CLEAR ls_vbfa.
    "开票日期
    LOOP AT lt_vbfa INTO ls_vbfa.
      ls_out-invoiced = ls_vbfa-erdat.
      CHECK ls_out-invoiced IS NOT INITIAL.
    ENDLOOP.
    "是否报检
    IF ls_all-zsfbj = 'X'.
      ls_out-zsfbj = '是'.
    ELSE.
      ls_out-zsfbj = '否'.
    ENDIF.
    "批号要求
    IF ls_all-zphyq = 'X'.
      ls_out-zphyq = '是'.
    ELSE.
      ls_out-zphyq = '否'.
    ENDIF.
    "最早投料日期
*    IF ls_all-zzztl = 'X'.
*      ls_out-zzztl = '是'.
*    ELSE.
*      ls_out-zzztl = '否'.
*    ENDIF.
    ls_out-zzwtl = ls_all-zzztl.
    "生产相关文本
    DATA: i_name LIKE thead-tdname.
    i_name = ls_all-vbeln.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'Z001'
        language                = sy-langu
        name                    = i_name
        object                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*       IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_lines
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
* Implement suitable error handling here
    ENDIF.
    READ TABLE lt_lines INTO ls_lines INDEX 1.
    ls_out-pronote = ls_lines-tdline.
    CLEAR ls_lines.
    CLEAR  lt_lines.
    "生产行项目备注
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'Z001'
        language                = sy-langu
        name                    = i_name
        object                  = 'VBBP'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*       IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_lines
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
* Implement suitable error handling here
    ENDIF.
    READ TABLE lt_lines INTO ls_lines INDEX 1.
    ls_out-note = ls_lines-tdline.
    CLEAR ls_lines.
    CLEAR  lt_lines.
    "销售文本
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'Z003'
        language                = sy-langu
        name                    = i_name
        object                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*       IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_lines
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
* Implement suitable error handling here
    ENDIF.
    READ TABLE lt_lines INTO ls_lines INDEX 1.
    ls_out-salenot = ls_lines-tdline.
    CLEAR ls_lines.
    CLEAR  lt_lines.
    "发货文本
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'Z004'
        language                = sy-langu
        name                    = i_name
        object                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*       IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_lines
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
* Implement suitable error handling here
    ENDIF.
    READ TABLE lt_lines INTO ls_lines INDEX 1.
    ls_out-voicenot = ls_lines-tdline.
    CLEAR ls_lines.
    CLEAR  lt_lines.
    "开票文本
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = 'Z005'
        language                = sy-langu
        name                    = i_name
        object                  = 'VBBK'
*       ARCHIVE_HANDLE          = 0
*       LOCAL_CAT               = ' '
*       IMPORTING
*       HEADER                  =
*       OLD_LINE_COUNTER        =
      TABLES
        lines                   = lt_lines
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
* Implement suitable error handling here
    ENDIF.
    READ TABLE lt_lines INTO ls_lines INDEX 1.
    ls_out-invoicenote = ls_lines-tdline.
    CLEAR ls_lines.
    CLEAR  lt_lines.
    "订单类型取描述
    READ TABLE lt_tvakt INTO ls_tvakt WITH KEY auart = ls_out-auart.
    ls_out-auart = ls_tvakt-bezei.
    "分销渠道取描述
    READ TABLE lt_tvtwt INTO ls_tvtwt WITH KEY vtweg = ls_out-vtweg.
    ls_out-vtweg = ls_tvtwt-vtext.
    "销售渠道取描述
    READ TABLE lt_tvv1t INTO ls_tvv1t WITH KEY kvgr1 = ls_out-kvgr1.
    ls_out-kvgr1 = ls_tvv1t-bezei.
    "出口国家取描述
    READ TABLE lt_t005t INTO ls_t005t WITH KEY land1 = ls_out-zckgj.
    ls_out-zckgj = ls_t005t-landx.
    "二级部门取描述
    READ TABLE lt_tvkbt INTO ls_tvkbt WITH KEY vkbur = ls_out-vkbur.
    ls_out-vkbur = ls_tvkbt-bezei.


    "产品编码
    DATA flag TYPE c. "用于标记物料是否为大M码，如果为大M码的话，这个字段标记为'X‘
    CLEAR ls_mara.
    SELECT SINGLE
      matkl
    INTO CORRESPONDING FIELDS OF ls_mara
    FROM mara
    WHERE mara~matnr = ls_all-matnr.

    "单位的转换
    DEFINE changemeins.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = &1
        language       = sy-langu
      IMPORTING
*       LONG_TEXT      =
        output         = &2
*       SHORT_TEXT     =
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    END-OF-DEFINITION.

    changemeins ls_out-meins ls_out-meins.
    changemeins ls_out-vrkme ls_out-vrkme.



    IF ls_mara-matkl = '501601'."组合装
      flag = 'X'.
    ENDIF.
    DATA: lt_stb LIKE TABLE OF stpox,
          ls_stb LIKE LINE OF lt_stb.

    IF flag = 'X'.

      CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
        EXPORTING
*         FTREL                 = ' '
*         ALEKZ                 = ' '
*         ALTVO                 = ' '
*         AUFSW                 = ' '
*         AUMGB                 = ' '
*         AUMNG                 = 0
*         AUSKZ                 = ' '
*         AMIND                 = ' '
*         BAGRP                 = ' '
*         BEIKZ                 = ' '
*         BESSL                 = ' '
*         BGIXO                 = ' '
*         BREMS                 = ' '
          capid                 = 'PP01'
*         CHLST                 = ' '
*         COSPR                 = ' '
*         CUOBJ                 = 000000000000000
*         CUOVS                 = 0
*         CUOLS                 = ' '
          datuv                 = sy-datum
*         DELNL                 = ' '
*         DRLDT                 = ' '
*         EHNDL                 = ' '
          emeng                 = 1000
*         ERSKZ                 = ' '
*         ERSSL                 = ' '
*         FBSTP                 = ' '
*         KNFBA                 = ' '
*         KSBVO                 = ' '
*         MBWLS                 = ' '
          mktls                 = 'X'
*         MDMPS                 = ' '
          mehrs                 = 'X'
*         MKMAT                 = ' '
*         MMAPS                 = ' '
*         SALWW                 = ' '
*         SPLWW                 = ' '
*         MMORY                 = ' '
          mtnrv                 = ls_all-matnr
*         NLINK                 = ' '
*         POSTP                 = ' '
*         RNDKZ                 = ' '
*         RVREL                 = ' '
*         SANFR                 = ' '
*         SANIN                 = ' '
*         SANKA                 = ' '
*         SANKO                 = ' '
*         SANVS                 = ' '
*         SCHGT                 = ' '
*         STKKZ                 = ' '
*         STLAL                 = ' '
*         STLAN                 = ' '
*         STPST                 = 0
          svwvo                 = 'X'
          werks                 = ls_all-werks
*         NORVL                 = ' '
*         MDNOT                 = ' '
*         PANOT                 = ' '
*         QVERW                 = ' '
*         VERID                 = ' '
          vrsvo                 = 'X'
          sgt_scat              = 'X'
*         SGT_REL               =
*         CALLER_APP            =
*         BOM_VERSN             =
* IMPORTING
*         TOPMAT                =
*         DSTST                 =
        TABLES
          stb                   = lt_stb
*         MATCAT                =
        EXCEPTIONS
          alt_not_found         = 1
          call_invalid          = 2
          material_not_found    = 3
          missing_authorization = 4
          no_bom_found          = 5
          no_plant_data         = 6
          no_suitable_bom_found = 7
          conversion_error      = 8
          OTHERS                = 9.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      LOOP AT lt_stb INTO ls_stb.
        SELECT SINGLE
          mtart
        INTO ls_mara-mtart
        FROM mara
        WHERE mara~matnr = ls_stb-idnrk.
        IF ls_mara-mtart = 'Z050'.
          ls_out-matnr = ls_stb-idnrk."BOM的物料编码
          ls_out-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_out-matnr i_out = 'X' ).
          APPEND ls_out TO lt_out." 输出BOM子件的行数
        ENDIF.
      ENDLOOP.
*      APPEND ls_out to lt_out.
    ELSE.
      ls_out-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_out-matnr i_out = 'X' ).
      APPEND ls_out TO lt_out."输出不含BOM的行数
    ENDIF.
*    APPEND ls_out to lt_out.
    CLEAR ls_out.
    CLEAR ls_all.
  ENDLOOP.
ENDFORM.

FORM catalog.
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'L'."字段居中显示
    fieldcat-outputlen = 12."自定义字段的长度
*    fieldcat-ref_tabname = 'BKPF'."调用透明表的数据结构
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.
  fieldcatset 'zapszt'   'APS状态' sy-tabix.
  fieldcatset 'state'   '订单状态' sy-tabix.
  fieldcatset 'vbeln'   '单据号' sy-tabix.
  fieldcatset 'posnr'   '行号' sy-tabix.
  fieldcatset 'ernam'   '制单人' sy-tabix.
  fieldcatset 'vkorg'   '销售组织' sy-tabix.
  fieldcatset 'auart'   '订单类型' sy-tabix.
  fieldcatset 'vtweg'   '分销渠道' sy-tabix.
  fieldcatset 'kvgr1'   '销售渠道/客户类型' sy-tabix.
  fieldcatset 'werks'   '工厂' sy-tabix.
  fieldcatset 'lgort'   '库存地点' sy-tabix.
  fieldcatset 'vkbur'   '二级部门' sy-tabix.
  fieldcatset 'saleman'   '业务员' sy-tabix.
  fieldcatset 'mc_name1'   '检索项' sy-tabix.
  fieldcatset 'werks'   '公司名称' sy-tabix.
  fieldcatset 'kunnr'   '售达方(客户名称)' sy-tabix.
  fieldcatset 'kunnr2'   '售达方(编号)' sy-tabix.
  fieldcatset 'custname'   '实际客户名称' sy-tabix.
  fieldcatset 'custname2'   '实际客户编号' sy-tabix.
  fieldcatset 'kukla'   '实际客户等级' sy-tabix.
  fieldcatset 'zterm'   '付款方式' sy-tabix.
  fieldcatset 'zckgj'   '出口国家' sy-tabix.
  fieldcatset 'bstkd_e'   '客户PO' sy-tabix.
  fieldcatset 'waerk'   '订单币种' sy-tabix.
  fieldcatset 'kursk'   '汇率' sy-tabix.
  fieldcatset 'matnr'   '产品编码' sy-tabix.
  fieldcatset 'arktx'   '产品名称' sy-tabix.
  fieldcatset 'zstdname'   '标准名称' sy-tabix.
  fieldcatset 'zkhwl'   '客户产品编码' sy-tabix.
  fieldcatset 'zkhwlms'   '客户产品名称' sy-tabix.
  fieldcatset 'pmatn'   '定价参考物料编码' sy-tabix.
  fieldcatset 'maktx'   '品名' sy-tabix.
  fieldcatset 'zpro'   '是否益生菌' sy-tabix.
  fieldcatset 'matkl'   '物料小类编码' sy-tabix.
  fieldcatset 'maktxs'   '物料小类名称' sy-tabix.
  fieldcatset 'zcpgg'   '规格型号' sy-tabix.
  fieldcatset 'zbzgg'   '包装规格' sy-tabix.
  fieldcatset 'zpwsx'   '批文属性' sy-tabix.
  fieldcatset 'zprotype'   '产品类型' sy-tabix.
  fieldcatset 'zappsou'   '批文来源' sy-tabix.
  fieldcatset 'zcotype'   '合作类型' sy-tabix.
  fieldcatset 'meins'   '基本计量单位' sy-tabix.
  fieldcatset 'number'   '基本计量数量' sy-tabix.
  fieldcatset 'kwmeng'   '销售数量' sy-tabix.
  fieldcatset 'zypsl'   '样品数量' sy-tabix.
  fieldcatset 'vrkme'   '销售单位' sy-tabix.
  fieldcatset 'kbetr'   '含税单价' sy-tabix.
  fieldcatset 'kwert'   '折扣率' sy-tabix.
  fieldcatset 'mwsbp2'   '折扣金额' sy-tabix.
  fieldcatset 'kwertdis'   '税率' sy-tabix.
  fieldcatset 'mwsbp'   '销项税额' sy-tabix.
  fieldcatset 'netwr'   '不含税金额' sy-tabix.
  fieldcatset 'priceall'   '价税合计' sy-tabix.
  fieldcatset 'zdytk'   '短溢条款' sy-tabix.
  fieldcatset 'zysjhq'   '客户要求交货日期' sy-tabix.
  fieldcatset 'zatpdfrq'   'ATP答复日期' sy-tabix.
  fieldcatset 'znbjhq'   '实际交货日期' sy-tabix.
  fieldcatset 'innum'   '当前库存' sy-tabix.
  fieldcatset 'outnum'   '出库量' sy-tabix.
  fieldcatset 'invoic'   '开票量' sy-tabix.
  fieldcatset 'invoiced'   '开票日期' sy-tabix.
  fieldcatset 'zsfxbz'   '新包装' sy-tabix.
  fieldcatset 'zsfxcp'   '新产品' sy-tabix.
  fieldcatset 'zsfbj'   '是否报检' sy-tabix.
  fieldcatset 'zphyq'   '批号要求' sy-tabix.
  fieldcatset 'zzztl'   '最早投料日期' sy-tabix.
  fieldcatset 'zzwtl'   '最晚投料日期' sy-tabix.
  fieldcatset 'zzzbz'   '最早包装时间' sy-tabix.
  fieldcatset 'zzwbz'   '最晚包装时间' sy-tabix.
  fieldcatset 'bstkd_e2'   '客户合同单号' sy-tabix.
  fieldcatset 'posex_e'   '客户合同分录' sy-tabix.
  fieldcatset 'note'   '生产行项目备注' sy-tabix.
  fieldcatset 'zbhcl'   '备货策略' sy-tabix.
  fieldcatset 'zbomqq'   'BOM欠缺物料' sy-tabix.
  fieldcatset 'zblxy'   '是否有备料协议' sy-tabix.
  fieldcatset 'zsfxkh'   '新客户 ' sy-tabix.
  fieldcatset 'pronote'   '生产相关文本' sy-tabix.
  fieldcatset 'salenot'   '销售文本' sy-tabix.
  fieldcatset 'voicenot'   '发票文本' sy-tabix.
  fieldcatset 'invoicenote' '开票文本' sy-tabix.
ENDFORM.

FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = w_repid "程序名称
*     i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
      i_grid_title       = '销售订单日报表' "标题名
*     I_GRID_SETTINGS    =
      is_layout          = layout "程序所定义的layout名称
      it_fieldcat        = fieldcat[] "定义fieldcat数据
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
*     I_SAVE             = ' '
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab           = lt_out
    EXCEPTIONS "下面都是默认的
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.