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
        auart    TYPE    vbak-auart  , "  订单类型
        vtweg    TYPE    vbak-vtweg  , "  分销渠道
        werks    TYPE    vbap-werks  , "  工厂
        vkbur    TYPE    vbak-vkbur  , "  二级部门
        mandt    TYPE    vbak-mandt  , "  集团名称
        kunnr    TYPE    vbak-kunnr  , "  售达方
        zckgj    TYPE    vbak-zckgj  , "  出口国家
        waerk    TYPE    vbak-waerk  , "  订单币种
        matnr    TYPE    vbap-matnr  , "  产品编码
        matwa    TYPE    vbap-matwa  , "  产品名称
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
        netpr    TYPE    vbap-netpr  , "  不含税金额
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
        kvgr1    TYPE    knvv-kvgr1  , "  销售渠道/客户类型
      END OF ty_all.
TYPES:BEGIN OF ty_out,"定义结构，用于接收所有的ALV字段数据
        zapszt      TYPE    vbap-zapszt , "  APS状态
        state       TYPE    string      , "订单状态
        vbeln       TYPE    vbak-vbeln  , "  单据号
        posnr       TYPE    vbap-posnr  , "  行号
        ernam       TYPE    string      , "  制单人
        vkorg       TYPE    vbak-vkorg  , "  销售组织
        auart       TYPE    vbak-auart  , "  订单类型
        vtweg       TYPE    vbak-vtweg  , "  分销渠道
        kvgr1       TYPE    knvv-kvgr1  , "销售渠道/客户类型
        werks       TYPE    vbap-werks  , "  工厂
        lgort       TYPE    lips-lgort  , "库存地点
        vkbur       TYPE    vbak-vkbur  , "  二级部门
        saleman     TYPE    string      , " 业务员
        mc_name1    TYPE    but000-mc_name1, "检索项
        mandt       TYPE    vbak-mandt  , "  集团名称
        kunnr       TYPE    vbak-kunnr  , "  售达方(客户名称)
        custname    TYPE    string      , "  实际客户名称
        kukla       TYPE    kna1-kukla  , "  实际客户等级
        zterm       TYPE    vbkd-zterm  , "  付款方式
        zckgj       TYPE    vbak-zckgj  , "  出口国家
        bstkd_e     TYPE    vbkd-bstkd_e, "  客户PO
        waerk       TYPE    vbak-waerk  , "  订单币种
        kursk       TYPE    vbkd-kursk  , "  汇率
        matnr       TYPE    vbap-matnr  , "  产品编码
        matwa       TYPE    vbap-matwa  , "  产品名称
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
        meins       TYPE    vbap-meins  , "  基本计量单位
        number      TYPE    string      , "  基本计量数量
        kwmeng      TYPE    vbap-kwmeng , "  销售数量
        zypsl       TYPE    vbap-zypsl  , "  样品数量
        vrkme       TYPE    vbap-vrkme  , "  销售单位
        kbetr       TYPE    string      , "  含税单价
        kwert       TYPE    string      , "  折扣率
        mwsbp       TYPE    vbap-mwsbp  , "  折扣金额
        kwertdis    TYPE    string      , "  税率
        mwsbp2      TYPE    vbap-mwsbp  , "  销项税额
        netpr       TYPE    vbap-netpr  , "  不含税金额
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




"开始选择界面的数据操作
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
sauart    FOR   vbak-auart  ,"  订单类型
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
  DATA:lt_vbak TYPE TABLE OF vbak,
       ls_vbak TYPE vbak,
       lt_vbap TYPE TABLE OF vbap,
       ls-vbap TYPE vbap,
       lt_all  TYPE TABLE OF ty_all, "用于存储VBAP、VBAK相关的数据
       ls_all  TYPE ty_all,
       lt_out  TYPE TABLE OF ty_out, "用于存储ALV内表的数据
       ls_out  TYPE ty_out,
       ls_jest TYPE jest,
       ls_adrp TYPE adrp,
       ls_knvv TYPE knvv,
       ls_lips TYPE lips.
  "取出所有的数据到对应的内表中
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
  matwa     "  产品名称
  zkhwl     "  客户产品编码
  zkhwlms   "  客户产品名称
  zcpgg     "  规格型号
  zbzgg     "  包装规格
  meins     "  基本计量单位
  kwmeng    "  销售数量
  zypsl     "  样品数量
  vrkme     "  销售单位
  mwsbp     "  销项税额
  netpr     "  不含税金额
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
  vbap~objnr     " 抬头对象编号
  INTO CORRESPONDING FIELDS OF TABLE lt_all
  FROM vbak
  INNER JOIN vbap
  ON vbak~vbeln = vbap~vbeln
  WHERE vbap~matnr IN smatnr
    AND vbap~werks IN swerks
    AND vbak~auart IN sauart
    AND vbak~vkorg IN pvkorg
    AND vbak~vtweg IN svtweg
    AND vbak~spart IN sspart
    AND vbak~kunnr IN skunnr
    AND vbak~ernam IN sernam
    AND vbak~erdat IN serdat.
  "上面获取到的是所有有关VBAP和VBAK的数据
  "将一部分可以直接插入的数据插入到内表中
  LOOP AT lt_all INTO ls_all.
    "因为TY_OUT的结构包含了TY_ALL的结构，所以直接MOVE
    MOVE-CORRESPONDING ls_all TO ls_out.
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

    "库存地点
    SELECT SINGLE
      lgort
    INTO CORRESPONDING FIELDS OF ls_lips
    FROM lips
    WHERE lips~vgbel = ls_all-vbeln
      AND lips~vgpos = ls_all-posnr.
    ls_out-lgort = ls_lips-lgort.









    APPEND ls_out TO lt_out.
    CLEAR ls_out.
    CLEAR ls_all.
  ENDLOOP.
  BREAK-POINT.
ENDFORM.

FORM catalog.
ENDFORM.

FORM alvshow.
ENDFORM.