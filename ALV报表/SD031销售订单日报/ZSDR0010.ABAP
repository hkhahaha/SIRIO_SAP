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
DATA:fieldcat TYPE lvc_t_fcat WITH HEADER LINE, "调用系统存在的FIELDCAT类型。
     layout   TYPE lvc_s_layo, "调用系统存在的LAYOUT
     "FIELDCAT用于ALV的结构定义，LAYOUT用于定义ALV的输出格式，后者可以覆盖前者，后者是可选的
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:vbap,vbak,vbep,jcds,knvp."定义系统存在的透明标
"定义合作类型描述相关的字段
TYPES:BEGIN OF ty_zco,
        atinn TYPE cawn-atinn, "内部特性号
        atzhl TYPE cawn-atzhl, "内部计数器值
        atwrt TYPE cawn-atwrt, "特性值
        atwtb TYPE cawnt-atwtb, "特性值描述
      END OF ty_zco.
DATA gv_sel_error TYPE flag.
TYPES:BEGIN OF ty_all,"定义结构，用于接收VBAP、VBAK所需要的全部字段
        zapszt(10) , "  APS状态
        vbeln          TYPE    vbak-vbeln  , "  单据号
        posnr          TYPE    vbap-posnr  , "  行号
        ernam          TYPE    vbak-ernam  , "  制单人
        vkorg          TYPE    vbak-vkorg  , "  销售组织
        auart          TYPE    string  , "  订单类型
        vtweg          TYPE    string , "  分销渠道
        werks          TYPE    vbap-werks  , "  工厂
        vkbur          TYPE    string  , "  二级部门
        mandt          TYPE    vbak-mandt  , "  集团名称
        kunnr          TYPE    string      , "  售达方
        kunnr2         TYPE    vbak-kunnr      , "  售达方编号
        zckgj          TYPE    string  , "  出口国家简称
        zckgj2         TYPE    string  , "  出口国家
        waerk          TYPE    vbak-waerk  , "  订单币种
        matnr          TYPE    vbap-matnr  , "  产品编码
        arktx          TYPE    vbap-arktx  , "  产品名称
*        matwa    TYPE    vbap-matwa  , "  产品名称
        zkhwl          TYPE    vbap-zkhwl  , "  客户产品编码
        zkhwlms        TYPE    vbap-zkhwlms  , "  客户产品名称
        zcpgg          TYPE    vbap-zcpgg  , "  规格型号
        zbzgg          TYPE    vbap-zbzgg  , "  包装规格
        meins(10)      TYPE    c, "  基本计量单位
        kwmeng         TYPE    vbap-kwmeng , "  销售数量
        zypsl          TYPE    vbap-zypsl  , "  样品数量
        vrkme(10)      TYPE    c, "  销售单位
        mwsbp          TYPE    vbap-mwsbp  , "  销项税额
        netwr          TYPE    vbap-netwr  , "  不含税金额
        zdytk(8)       TYPE    c  , "  短溢条款
        zysjhq         TYPE    vbap-zysjhq , "  客户要求交货日期
        zatpdfrq       TYPE    vbap-zatpdfrq , "  ATP答复日期
        znbjhq         TYPE    vbap-znbjhq , "  实际交货日期
        zsfxbz         TYPE    vbap-zsfxbz , "  新包装
        zsfxcp         TYPE    vbap-zsfxcp , "  新产品
        zsfbj          TYPE    vbap-zsfbj  , "  是否报检
        zphyq          TYPE    vbap-zphyq  , "  批号要求
        zzztl          TYPE    vbap-zzztl  , "  最早投料日期
        zzwtl          TYPE    vbap-zzwtl  , "  最晚投料日期
        zzzbz          TYPE    vbap-zzzbz  , "  最早包装时间
        zzwbz          TYPE    vbap-zzwbz  , "  最晚包装时间
        zbhcl          TYPE    string  , "  备货策略
        zbomqq         TYPE    vbap-zbomqq , "  BOM欠缺物料
        zblxy          TYPE    vbak-zblxy  , "  是否有备料协议
        zsfxkh         TYPE    vbak-zsfxkh , "  新客户
*        auart    TYPE    vbak-auart  , "  订单类型
*        vkorg    TYPE    vbak-vkorg  , "  销售组织
*        vtweg    TYPE    vbak-vtweg  , "  分销渠道
        spart          TYPE    vbak-spart  , "  产品组
*        werks    TYPE    vbap-werks  , "  工厂
*        kunnr    TYPE    vbak-kunnr  , "  客户
*        ernam    TYPE    vbak-ernam  , "  创建人
        erdat          TYPE    vbak-erdat  , "  创建日期
*        matnr    TYPE    vbap-matnr  , "  物料编码
        objnr          TYPE    vbap-objnr  , " 对象编号
        kvgr1          TYPE    string  , "  销售渠道/客户类型
        umziz          TYPE    vbap-umziz  , "转换因子（分子）
        umzin          TYPE    vbap-umzin  ,  "转换因此（分母）
        knumv          TYPE vbak-knumv, "凭证条件号
        zyfkqk         TYPE string, "预付款
        augru          TYPE string, "订货原因
        price2         TYPE vbap-netwr, "不含税金额CNY
        ettyp          TYPE vbep-ettyp, "计划行类别
        bu_sort1       TYPE string, "实际客户的检索项
        zson           TYPE vbap-matnr, "子件
        zsondetail(60) TYPE c, "子件描述
        zmeins         TYPE mara-meins, "子件的单位
        zsum           TYPE vbap-kwmeng, "铝盒装子件销售数量
        partner        TYPE string, "实际客户的业务员
        zsum2          TYPE vbap-kwmeng, "礼合装基本子件数量
        udate          TYPE jcds-udate, "二审日期
        zmeng          TYPE vbap-zmeng, "目标数量
        kpein          TYPE prcd_elements-kpein, "定价单位
        budat          TYPE aufm-budat, "过账日期
      END OF ty_all.
TYPES:BEGIN OF ty_out,"定义结构，用于接收所有的ALV字段数据
        zapszt(10), "  APS状态
        state(10), "  订单状态
        vbeln            TYPE    vbak-vbeln  , "  单据号
        posnr            TYPE    vbap-posnr  , "  行号
        ernam(10), "  制单人
        vkorg            TYPE    vbak-vkorg  , "  销售组织
        auart(15), "  订单类型
        vtweg(25), "  分销渠道
        kvgr1(25), "  销售渠道/客户类型
        werks            TYPE    vbap-werks  , "  工厂
        lgort            TYPE    lips-lgort  , "  库存地点
        vkbur(15), "  二级部门
        saleman(20), "  业务员
        mc_name1         TYPE    but000-mc_name1, "检索项
        mandt            TYPE    vbak-mandt  , "  集团名称
        kunnr(60), "  售达方(客户名称)
        kunn2            TYPE knvp-kunn2, "实际客户的业务员
        kunnr2           TYPE    vbak-kunnr  , "  售达方(编号)
        custname(60), "  实际客户名称
        custname2(60), "  实际客户编号
        kukla            TYPE    kna1-kukla  , "  实际客户等级
        zterm(60), "  付款方式
        zckgj(60), "  出口国家简称
        zckgj2(60), "  出口国家
        bstkd_e          TYPE    vbkd-bstkd_e, "  客户PO
        waerk            TYPE    vbak-waerk  , "  订单币种
        kursk            TYPE    vbkd-kursk  , "  汇率
        matnr            TYPE    vbap-matnr  , "  产品编码
        arktx            TYPE    vbap-arktx  , "  产品名称
        zstdname(60), "  标准名称
        zkhwl            TYPE    vbap-zkhwl  , "  客户产品编码
        zkhwlms          TYPE    vbap-zkhwlms, "  客户产品名称
        pmatn            TYPE    vbap-matnr  , "  定价参考物料编码
        maktx            TYPE    makt-maktx  , "  品名
        zpro(10), "  是否益生菌
        matkl            TYPE    mara-matkl  , "  物料小类编码
        maktxs           TYPE    makt-maktx  , "  物料小类名称
        zcpgg            TYPE    vbap-zcpgg  , "  规格型号
        zbzgg            TYPE    vbap-zbzgg  , "  包装规格
        zpwsx(60), "  批文属性
        zprotype(70)     TYPE    c      , "  产品类型
        zprotype2(70)    TYPE    c      , "  产品类型2
        zappsou(70)      TYPE    c      , "  批文来源
        zappsou2(70)     TYPE    c      , "  批文来源2
        zcotype(60), "  合作类型
        meins(10)        TYPE    c, "  基本计量单位
        number(13)       TYPE   p DECIMALS 3   , "  基本计量数量
        kwmeng           TYPE    vbap-kwmeng , "  销售数量
        zypsl(40)        TYPE    c  , "  样品数量
        vrkme(10)        TYPE    c, "  销售单位
        kbetr            TYPE    vbap-netwr     , "  含税单价
        kwert            TYPE    vbap-netwr      , "  折扣率
        mwsbp2           TYPE    vbap-mwsbp  , "  折扣金额
        kwertdis         TYPE    vbap-kwmeng     , "  税率
        mwsbp            TYPE    vbap-mwsbp  , "  销项税额
        netwr            TYPE    vbap-netwr  , "  不含税金额
        priceall         TYPE    vbap-netwr    , "  价税合计
        zdytk(10)        TYPE    c  , "  短溢条款
        zysjhq           TYPE    vbap-zysjhq , "  客户要求交货日期
        zatpdfrq         TYPE    vbap-zatpdfrq, "  ATP答复日期
        znbjhq           TYPE    vbap-znbjhq , "  实际交货日期
        innum            TYPE    vbap-kwmeng, "  入库量
        innum2           TYPE    vbap-kwmeng, "  入库量
        outnum           TYPE    vbap-kwmeng  , "  出库量
        invoice          TYPE    vbap-kwmeng  , "  开票量
        invoiced         TYPE    vbap-znbjhq   , "  开票日期
        zsfxbz(60) , "  新包装
        zsfxcp(60), "  新产品
        zsfbj(5), "  是否报检
        zphyq(60), "  批号要求
        zzztl            TYPE    vbap-zzztl  , "  最早投料日期
        zzwtl            TYPE    vbap-zzwtl  , "  最晚投料日期
        zzzbz            TYPE    vbap-zzzbz  , "  最早包装时间
        zzwbz            TYPE    vbap-zzwbz  , "  最晚包装时间
        bstkd_e2         TYPE    vbkd-bstkd_e, "  客户合同单号
        posex_e          TYPE    vbkd-posex_e, "  客户合同分录
        note(100), "  生产行项目备注
        zbhcl(60), "  备货策略
        zbomqq           TYPE    vbap-zbomqq , "  BOM欠缺物料
        zblxy            TYPE    vbak-zblxy  , "  是否有备料协议
        zsfxkh           TYPE    vbak-zsfxkh , "  新客户
        pronote(300)     TYPE    c      , "  生产相关文本
        pronote2(300)    TYPE    c      , "  生产相关文本2
        salenot(100), "  销售文本
        voicenot(100), "  发票文本
        invoicenote(100), "  开票文本
        augru(100), "订货原因
        ddpprice         TYPE p DECIMALS 2, "DDP价格
        zyfkqk(20), "预付款
        price2           TYPE vbap-netwr, "不含税金额CNY
        ettyp            TYPE vbep-ettyp, "计划行类别
        bu_sort1(60), "实际客户的检索项
        partner(60), "实际客户的业务员
        zson             TYPE vbap-matnr, "子件
        zsondetail(60)   TYPE c, "子件描述
        zmeins(10)       TYPE c, "子件的单位
        zsum             TYPE vbap-kwmeng, "礼盒装子件销售数量
        zsum2            TYPE vbap-kwmeng, "礼合装基本子件数量
        erdat            TYPE vbak-erdat, "创建日期
        udate            TYPE jcds-udate, "二审日期
        udate1           TYPE jcds-udate, "一审日期

        wadat_ist        TYPE likp-wadat_ist, "首个交货日期
        zmeng            TYPE vbap-zmeng, "目标数量
        chgnr            TYPE jcds-chgnr,
        kpein            TYPE prcd_elements-kpein, "定价单位
        budat            TYPE aufm-budat, "过账日期
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
pvkorg    FOR   vbak-vkorg  OBLIGATORY,"  销售组织
svtweg    FOR   vbak-vtweg  OBLIGATORY,"  分销渠道
sspart    FOR   vbak-spart  ,"  产品组
swerks    FOR   vbap-werks  ,"  工厂
skunnr    FOR   vbak-kunnr  ,"  客户
sernam    FOR   vbak-ernam  ,"  创建人
serdat    FOR   vbak-erdat  ,"  创建日期
smatnr    FOR   vbap-matnr  ,"  物料编码
settyp    FOR   vbep-ettyp  ,"  计划行类别
s_kunn2 FOR knvp-kunn2,"实际业务员
s_udate1 FOR jcds-udate,"一审时间
s_udate FOR jcds-udate."二审时间

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-002.
SELECTION-SCREEN POSITION 23.
PARAMETERS flag_a RADIOBUTTON GROUP rg1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 24(8) TEXT-003 FOR FIELD flag_a.
SELECTION-SCREEN POSITION 44.
PARAMETERS flag_b RADIOBUTTON GROUP rg1.
SELECTION-SCREEN COMMENT 48(8) TEXT-004 FOR FIELD flag_b.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-005.
SELECTION-SCREEN POSITION 23.
PARAMETERS part_a TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 25(6) TEXT-006.
SELECTION-SCREEN POSITION 34.
PARAMETERS part_b TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 36(6) TEXT-007.
SELECTION-SCREEN POSITION 45.
PARAMETERS part_c TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 47(6) TEXT-008.
SELECTION-SCREEN POSITION 56.
PARAMETERS part_d TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 58(6) TEXT-009.
*PARAMETERS part_c TYPE c AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 45(6) TEXT-002.
*PARAMETERS flag_a1 RADIOBUTTON GROUP rg2 DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 24(8) TEXT-003 FOR FIELD flag_a1.
*SELECTION-SCREEN POSITION 44.
*PARAMETERS flag_b1 RADIOBUTTON GROUP rg2.
*SELECTION-SCREEN COMMENT 48(8) TEXT-004 FOR FIELD flag_b1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk.


START-OF-SELECTION.
  PERFORM frm_authority_check.
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
       lt_jcds          TYPE TABLE OF jcds,
       ls_jcds          TYPE jcds,
       lt_vbfa          TYPE TABLE OF vbfa,
       ls_vbfa          TYPE vbfa,
       lt_mska          TYPE TABLE OF mska,
       ls_mara          TYPE mara,
       ls_prcd_elements TYPE prcd_elements,
       ls_mska          TYPE mska,
       lt_ausp          TYPE TABLE OF ausp,
       ls_ausp          TYPE ausp,
       lt_mast          TYPE TABLE OF mast,
       ls_mast          TYPE mast,
       lt_stko          TYPE TABLE OF stko,
       ls_stko          TYPE stko,
       lt_tvakt         TYPE TABLE OF tvakt,
       ls_tvakt         TYPE tvakt,
       lt_tvtwt         TYPE TABLE OF tvtwt,
       ls_tvtwt         TYPE tvtwt,
       lt_tvv1t         TYPE TABLE OF tvv1t,
       ls_tvv1t         TYPE tvv1t,
       lt_dd07v         TYPE TABLE OF dd07v,
       ls_dd07v         TYPE dd07v,
       lt_t005t         TYPE TABLE OF t005t,
       ls_t005t         TYPE t005t,
       lt_tvkbt         TYPE TABLE OF tvkbt,
       ls_tvkbt         TYPE tvkbt,
       ls_kna1          TYPE kna1,
       lt_likp          TYPE TABLE OF likp,
       ls_likp          TYPE likp,
       lt_zco           TYPE TABLE OF ty_zco,
       ls_zco           TYPE ty_zco,
       ls_ettyp         LIKE LINE OF settyp.

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
  zmeng     "目标数量
  vbap~zypsl     "  样品数量
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
  vbak~zckgj     "出口国家
  vbak~zyfkqk    "预付款
  augru     "订货原因
  vbak~erdat "创建日期
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
    AND vbak~erdat IN serdat
  AND vbak~vbtyp <> 'W'.

  "上面获取到的是所有有关VBAP和VBAK的数据

  "将物料编码转类型
  LOOP AT lt_all INTO ls_all.
    ls_all2-matnr90 = ls_all-matnr.
    APPEND ls_all2 TO lt_all2.
  ENDLOOP.
  "定义特征名
  DATA:lv_zappsou(10)  TYPE c,
       lv_zcotype(10)  TYPE c,
       lv_zprotype(10) TYPE c,
       lv_zpro(10)     TYPE c,
       lv_zstdname(10) TYPE c.
  lv_zappsou = 'ZAPPSOU'.
  lv_zcotype = 'ZCOTYPE'.
  lv_zprotype = 'ZPROTYPE'.
  lv_zpro = 'ZPRO'.
  lv_zstdname = 'ZSTDNAME'.
  "使用宏来改变特征名转换前后的值
  DEFINE changeausp.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = &1
    IMPORTING
      output = &2.
  END-OF-DEFINITION.
  changeausp lv_zappsou lv_zappsou.
  changeausp lv_zcotype lv_zcotype.
  changeausp lv_zpro lv_zpro.
  changeausp lv_zprotype lv_zprotype.
  changeausp lv_zstdname lv_zstdname.
  "查询出全部物料的所需要的全部批次属性特征值
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_ausp
  FROM ausp
  FOR ALL ENTRIES IN lt_all2
  WHERE ausp~objek = lt_all2-matnr90
  AND ausp~atinn IN ( lv_zappsou,lv_zcotype,lv_zprotype,lv_zpro,lv_zstdname ).


***********hk 添加最晚入库日期字段**************
  IF lt_all IS NOT INITIAL.
    SELECT
    mblnr,
    zeile,
    mjahr,
    kdauf,
    kdpos,
    matnr,
    bwart,
    budat,
    lgort
    INTO TABLE @DATA(lt_aufma)
    FROM aufm
    FOR ALL ENTRIES IN @lt_all
    WHERE aufm~kdauf = @lt_all-vbeln
      AND aufm~kdpos = @lt_all-posnr
      AND aufm~matnr = @lt_all-matnr
      AND aufm~bwart = '101'
      AND aufm~lgort IN ('1040','2040','3040','1060','2060','3060','1070','2070','3070','1080','2080','3080' ).
*SORT itab BY col1 ASCENDING col2 DESCENDING.
    SORT lt_aufma BY budat DESCENDING kdauf kdpos ASCENDING .
    "下面找到AUFM表中需要剔除掉的数据
    IF lt_aufma IS NOT INITIAL.
      SELECT
      smbln,
      smblp
      INTO TABLE @DATA(lt_mseg)
      FROM mseg
      FOR ALL ENTRIES IN @lt_aufma
      WHERE mseg~smbln = @lt_aufma-mblnr
        AND mseg~smblp = @lt_aufma-zeile.
    ENDIF.


    "添加一审日期
    SELECT
      *
    INTO TABLE @DATA(lt_jcds1)
    FROM jcds
    FOR ALL ENTRIES IN @lt_all
    WHERE jcds~objnr = @lt_all-objnr
      AND jcds~stat = 'E0002'.
  ENDIF.
  LOOP AT lt_aufma INTO DATA(ls_aufma).
    READ TABLE lt_mseg INTO DATA(ls_mseg) WITH KEY smbln = ls_aufma-mblnr
                                                   smblp = ls_aufma-zeile.
    IF sy-subrc = 0.
*      DELETE TABLE lt_aufma FROM ls_aufma.
      DELETE lt_aufma WHERE mblnr = ls_aufma-mblnr AND zeile = ls_aufma-zeile.
    ENDIF.
    CLEAR ls_aufma.
  ENDLOOP.

*****************END *******************



  "将一部分可以直接插入的数据插入到内表中
  LOOP AT lt_all INTO ls_all.

    CLEAR ls_out.
    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zappsou.
    ls_out-zappsou = ls_ausp-atwrt.
    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zcotype.
    ls_out-zcotype = ls_ausp-atwrt.
    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zprotype.
    ls_out-zprotype = ls_ausp-atwrt.
    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zstdname.
    ls_out-zstdname = ls_ausp-atwrt.
    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zpro.
    ls_out-zpro = ls_ausp-atwrt.
    CLEAR ls_ausp.

    CLEAR lt_zco.
    "合作类型取描述
    SELECT
      cawnt~atwtb
      cawn~atinn
      cawn~atzhl
      cawn~atwrt
    INTO CORRESPONDING FIELDS OF TABLE lt_zco
    FROM cawnt
    INNER JOIN cawn
    ON cawn~atinn = cawnt~atinn
    AND cawn~atzhl = cawnt~atzhl
    WHERE cawnt~spras = '1'
    AND cawnt~atinn = lv_zcotype.
    CLEAR ls_zco.
    READ TABLE lt_zco INTO ls_zco WITH KEY atwrt = ls_out-zcotype.
    ls_out-zcotype = ls_zco-atwtb.
*    CLEAR ls_all-augru.
    "订货原因取描述
    IF ls_all-augru IS NOT INITIAL.
      SELECT SINGLE
        bezei
      INTO ls_all-augru
      FROM tvaut
      WHERE tvaut~spras  = '1'
      AND tvaut~augru = ls_all-augru.
    ENDIF.
    "因为TY_OUT的结构包含了TY_ALL的结构，所以直接MOVE
    MOVE-CORRESPONDING ls_all TO ls_out.
    ls_out-kwmeng = ls_all-zmeng + ls_all-kwmeng.
    "计划行类别
    SELECT SINGLE
    ettyp
    INTO ls_out-ettyp
    FROM vbep
    WHERE vbep~vbeln = ls_all-vbeln
    AND vbep~posnr = ls_all-posnr.
    IF settyp IS NOT INITIAL.
      READ TABLE settyp INTO ls_ettyp WITH KEY low = ls_out-ettyp.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
    "一审日期
    READ TABLE lt_jcds1 INTO DATA(ls_jcds1) WITH KEY objnr = ls_all-objnr.
    IF sy-subrc = 0.
      ls_out-udate1 = ls_jcds1-udate.
    ENDIF.
    CLEAR ls_jcds1.
    "售达方编号
    ls_out-kunnr2 = ls_all-kunnr.
    CLEAR ls_jest.
    "订单状态
    SELECT SINGLE
      stat
    INTO CORRESPONDING FIELDS OF ls_jest
    FROM jest
    WHERE jest~objnr = ls_all-objnr
    AND jest~stat LIKE 'E%'
    AND jest~inact = ''.
    IF ls_jest-stat = 'E0001' AND part_a = 'X'.
      ls_out-state = '创建'.
    ELSEIF ls_jest-stat = 'E0002' AND part_b = 'X'.
      ls_out-state = '业务一审'.
    ELSEIF ls_jest-stat = 'E0003' AND part_c = 'X'.
      ls_out-state = '财务二审'.
    ELSEIF ls_jest-stat = '' AND part_d = 'X'.
      ls_out-state = ''.
    ELSE.
      CONTINUE.
    ENDIF.


    IF ls_out-state = '财务二审'.
      "二审日期
      CLEAR lt_jcds.
      SELECT
      udate
      chgnr
      INTO CORRESPONDING FIELDS OF TABLE lt_jcds
      FROM jcds
      WHERE jcds~objnr = ls_all-objnr
      AND jcds~stat = 'E0003'
      ORDER BY chgnr.

      READ TABLE lt_jcds INTO ls_jcds INDEX 1.
      ls_out-udate = ls_jcds-udate.
      CLEAR ls_jcds.
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
*    SELECT SINGLE
*      lgort
*    INTO CORRESPONDING FIELDS OF ls_lips
*    FROM lips
*    WHERE lips~vgbel = ls_all-vbeln
*    AND lips~vgpos = ls_all-posnr.
*    ls_out-lgort = ls_lips-lgort.
*    CLEAR ls_lips.
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
      bu_sort1
    INTO CORRESPONDING FIELDS OF ls_but000
    FROM but000
    WHERE but000~partner = ls_all-kunnr.
    ls_out-mc_name1 = ls_but000-bu_sort1.
    "客户名称
    ls_out-kunnr = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
    "集团名称
    "实际客户名称、实际客户的检索项字段
    SELECT SINGLE
    bu_sort1
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
    ls_out-bu_sort1 = ls_but000-bu_sort1."实际客户的检索项字段
    ls_out-custname = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
    ls_out-custname2 = ls_but000-partner.
    "实际客户对应的业务员
    SELECT SINGLE
      kunn2
      name_org1
    INTO (ls_out-partner,ls_out-kunn2)
    FROM but000
    INNER JOIN knvp
    ON but000~partner = knvp~kunn2
    WHERE knvp~kunnr  = ls_but000-partner
      AND knvp~vkorg = ls_all-vkorg
      AND knvp~vtweg = ls_all-vtweg
      AND knvp~spart = ls_all-spart
    AND knvp~parvw = 'Z3'.

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
    CLEAR ls_vbkd.
    SELECT SINGLE
      posex_e
      bstkd_e
      kursk
      zterm
    INTO CORRESPONDING FIELDS OF ls_vbkd
    FROM vbkd
    WHERE vbkd~posnr = ls_all-posnr
    AND vbkd~vbeln = ls_all-vbeln.
    IF ls_vbkd-posex_e IS INITIAL.
      SELECT SINGLE
      posex_e
      INTO CORRESPONDING FIELDS OF ls_vbkd
      FROM vbkd
      WHERE vbkd~posnr = ''
      AND vbkd~vbeln = ls_all-vbeln.
    ENDIF.
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
    ls_out-posex_e = ls_vbkd-posex_e.
    "不含税金额CNY
    ls_out-price2 = ls_out-netwr * ls_out-kursk."不含税金额CNY



    " 定价参考物料编码
    SELECT SINGLE
      pmatn
    INTO CORRESPONDING FIELDS OF ls_mvke
    FROM mvke
    WHERE mvke~matnr = ls_all-matnr
      AND mvke~vkorg = ls_all-vkorg
      AND mvke~vtweg = ls_all-vtweg.
    ls_out-pmatn = zcl_bc_public=>conv_by_ddic( i_input = ls_mvke-pmatn i_out = 'X' ).
    CLEAR ls_all2-matnr90.
    CLEAR ls_out-zappsou.
    ls_all2-matnr90 = zcl_bc_public=>conv_by_ddic( i_input = ls_out-pmatn ).
    CLEAR ls_out-zappsou.
    IF ls_all2-matnr90 IS NOT INITIAL.
      SELECT SINGLE
      atwrt
      INTO ls_out-zappsou
      FROM  ausp
      WHERE ausp~atinn  = lv_zappsou
      AND ausp~objek = ls_all2-matnr90.
      ls_out-zappsou2 = ls_out-zappsou.
      CLEAR ls_out-zappsou.
    ENDIF.

    "批文来源

    IF ls_out-zappsou2 IS NOT INITIAL.
      SELECT SINGLE
        atwtb
      INTO ls_out-zappsou
      FROM cawnt
      INNER JOIN cawn
      ON cawn~atinn = cawnt~atinn
      AND cawn~atzhl = cawnt~atzhl
      WHERE cawnt~spras = '1'
        AND cawn~atinn = lv_zappsou
        AND cawn~atwrt = ls_out-zappsou2.
    ENDIF.
    "产品类型
    CLEAR ls_out-zprotype.

    IF ls_all2-matnr90 IS NOT INITIAL.
      SELECT SINGLE
      atwrt
      INTO ls_out-zprotype
      FROM  ausp
      WHERE ausp~atinn  = lv_zprotype
      AND ausp~objek = ls_all2-matnr90.
      ls_out-zprotype2 = ls_out-zprotype.
      CLEAR ls_out-zprotype.
    ENDIF.

    IF ls_out-zprotype2 IS NOT INITIAL.
      SELECT SINGLE
      atwtb
      INTO ls_out-zprotype
      FROM cawnt
      INNER JOIN cawn
      ON cawn~atinn = cawnt~atinn
      AND cawn~atzhl = cawnt~atzhl
      WHERE cawnt~spras = '1'
      AND cawn~atinn = lv_zprotype
      AND cawn~atwrt = ls_out-zprotype2.
    ENDIF.




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
    ls_out-matkl = ls_mara-matkl+0(4).
    "物料小类名称
    CLEAR ls_makt.
    SELECT SINGLE
      wgbez
    INTO ls_out-maktxs
    FROM t023t
    WHERE t023t~matkl = ls_out-matkl
    AND t023t~spras = '1'.



    "基本计量数量
    IF ls_all-umzin IS NOT INITIAL.
      ls_out-number = ( ls_all-kwmeng * ( ls_all-umziz / ls_all-umzin ) ) + ( ls_all-zmeng * ( ls_all-umziz / ls_all-umzin ) ).
    ENDIF.
    CLEAR ls_prcd_elements.
    "含税单价
    SELECT SINGLE
      kbetr"含税单价
      kwert"含税金额
      prcd_elements~kpein"定价单位
    INTO CORRESPONDING FIELDS OF ls_prcd_elements
    FROM prcd_elements
    INNER JOIN vbak
    ON vbak~knumv = prcd_elements~knumv
    INNER JOIN vbap
    ON vbap~posnr = prcd_elements~kposn
    WHERE vbak~vbeln = ls_all-vbeln
    AND prcd_elements~kschl IN ( 'ZP00','ZP02' )
    AND prcd_elements~kinak = ''
    AND vbap~posnr = ls_all-posnr.
    ls_out-kbetr = ls_prcd_elements-kbetr.
    "定价单位
    ls_out-kpein = ls_prcd_elements-kpein.
    "折扣金额
    ls_out-mwsbp2 = ls_prcd_elements-kwert - ls_all-netwr - ls_all-mwsbp.
    "折扣率
    IF ls_prcd_elements-kwert IS NOT INITIAL AND ls_all-kwmeng IS NOT INITIAL.
      ls_out-kwert = ls_out-mwsbp2 / ls_prcd_elements-kwert * 100.
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
    CLEAR ls_mska.
    CLEAR lt_mska.
    CLEAR ls_out-innum.
    DATA:lv_value1 TYPE mska-kalab,
         lv_value2 TYPE mska-kains.
    CLEAR lv_value1.
    CLEAR lv_value2.
    SELECT
    kalab
    kains
    kaspe
    kavla
    kavin
    kavsp
    INTO CORRESPONDING FIELDS OF TABLE lt_mska
    FROM mska
    WHERE mska~vbeln = ls_all-vbeln
    AND mska~posnr = ls_all-posnr
    AND mska~matnr = ls_all-matnr
    AND mska~lgort IN ('1040','2040','3040','1070','2070','3070').
    LOOP AT lt_mska INTO ls_mska.
*    ls_out-innum = ls_mska-kalab + ls_mska-kains + ls_mska-kaspe + ls_mska-kavla + ls_mska-kavin + ls_mska-kavsp.
      lv_value1 = lv_value1 + ls_mska-kalab.
      lv_value2 = lv_value2 + ls_mska-kains + ls_mska-kaspe + ls_mska-kavla + ls_mska-kavin + ls_mska-kavsp.
      CLEAR ls_mska.
    ENDLOOP.
    ls_out-innum = lv_value1.
    ls_out-innum2 = lv_value2.
    IF ls_out-number IS NOT INITIAL AND ls_out-kwmeng IS NOT INITIAL.
      ls_out-innum = ls_out-innum / ( ls_out-number / ls_out-kwmeng  ).
      ls_out-innum2 = ls_out-innum2 / ( ls_out-number / ls_out-kwmeng  ).
    ENDIF.
    CLEAR lt_vbfa.
    "出库量
    SELECT
      rfmng
      vbtyp_n
    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
    FROM vbfa
    WHERE vbfa~vbelv = ls_all-vbeln
      AND vbfa~posnv = ls_all-posnr
    AND vbfa~vbtyp_n IN ( 'R','h' ).
    LOOP AT lt_vbfa INTO ls_vbfa.
      IF ls_vbfa-vbtyp_n = 'R'.
        ls_out-outnum = ls_out-outnum + ls_vbfa-rfmng.
      ELSEIF ls_vbfa-vbtyp_n = 'h'.
        ls_out-outnum = ls_out-outnum - ls_vbfa-rfmng.
      ENDIF.
      CLEAR ls_vbfa.
    ENDLOOP.

    IF ls_out-number IS NOT INITIAL AND ls_out-kwmeng IS NOT INITIAL.
      ls_out-outnum = ls_out-outnum /  ( ls_out-number / ls_out-kwmeng  ).
    ENDIF.
    CLEAR ls_vbfa.
    CLEAR lt_vbfa.
    "首个交货日期
    SELECT
      vbeln
    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
    FROM vbfa
    WHERE vbfa~vbelv = ls_all-vbeln
    AND vbfa~posnv = ls_all-posnr
    AND vbfa~vbtyp_n = 'J'
    AND vbfa~rfmng > 0
    ORDER BY vbeln.
    READ TABLE lt_vbfa INTO ls_vbfa INDEX 1.
    CLEAR ls_likp.
    SELECT SINGLE
      wadat_ist
    INTO CORRESPONDING FIELDS OF ls_likp
    FROM likp
    WHERE likp~vbeln = ls_vbfa-vbeln.
    ls_out-wadat_ist = ls_likp-wadat_ist.
    CLEAR lt_vbfa.
    CLEAR ls_vbfa.
    "开票量
    SELECT
    rfmng
    vbtyp_n
    erdat
    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
    FROM vbfa
    WHERE vbfa~vbelv = ls_all-vbeln
    AND vbfa~posnv = ls_all-posnr
    AND vbfa~vbtyp_n IN ( 'M','N','O','S' )
    ORDER BY erdat DESCENDING.
    LOOP AT lt_vbfa INTO ls_vbfa.
*      ls_out-invoice = ls_out-invoice + ls_vbfa-rfmng.
      IF ls_vbfa-vbtyp_n = 'M' OR ls_vbfa-vbtyp_n = 'O'.
        ls_out-invoice = ls_out-invoice + ls_vbfa-rfmng.
      ELSEIF ls_vbfa-vbtyp_n = 'N' OR ls_vbfa-vbtyp_n = 'S'.
        ls_out-invoice = ls_out-invoice - ls_vbfa-rfmng.
      ENDIF.
      CLEAR ls_vbfa.
    ENDLOOP.
    IF ls_out-number IS NOT INITIAL AND ls_out-kwmeng IS NOT INITIAL AND ( ls_out-number / ls_out-kwmeng  ) <> 0.
      ls_out-invoice = ls_out-invoice /  ( ls_out-number / ls_out-kwmeng  ).
    ENDIF.

    CLEAR ls_vbfa.
    "开票日期
    LOOP AT lt_vbfa INTO ls_vbfa.
      ls_out-invoiced = ls_vbfa-erdat.
      CHECK ls_out-invoiced IS NOT INITIAL.
    ENDLOOP.
    "是否报检
*    IF ls_all-zsfbj = 'X'.
*      ls_out-zsfbj = '是'.
*    ELSE.
*      ls_out-zsfbj = '否'.
*    ENDIF.
    ls_out-zsfbj = ls_all-zsfbj.
    "批号要求
*    IF ls_all-zphyq = 'X'.
*      ls_out-zphyq = '是'.
*    ELSE.
*      ls_out-zphyq = '否'.
*    ENDIF.

    "最早投料日期
*    IF ls_all-zzztl = 'X'.
*      ls_out-zzztl = '是'.
*    ELSE.
*      ls_out-zzztl = '否'.
*    ENDIF.
    ls_out-zzwtl = ls_all-zzwtl.

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
    LOOP AT lt_lines INTO ls_lines.
      ls_out-pronote = ls_out-pronote && ls_lines-tdline.
      ls_out-pronote2 = ls_out-pronote+129(128).
      CLEAR ls_lines.
    ENDLOOP.
    CLEAR ls_lines.
    CLEAR  lt_lines.
    "生产行项目备注
    i_name = ls_all-vbeln && ls_all-posnr.
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
    LOOP AT lt_lines INTO ls_lines.
      ls_out-note = ls_out-note && ls_lines-tdline.
      CLEAR ls_lines.
    ENDLOOP.

    CLEAR ls_lines.
    CLEAR  lt_lines.
    CLEAR i_name.
    i_name = ls_all-vbeln.
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
*    READ TABLE lt_lines INTO ls_lines INDEX 1.
*    ls_out-salenot = ls_lines-tdline.
    LOOP AT lt_lines INTO ls_lines.
      ls_out-salenot = ls_out-salenot && ls_lines-tdline.
    ENDLOOP.
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
*    READ TABLE lt_lines INTO ls_lines INDEX 1.
*    ls_out-voicenot = ls_lines-tdline.
    LOOP AT lt_lines INTO ls_lines.
      ls_out-voicenot = ls_out-voicenot && ls_lines-tdline.
    ENDLOOP.
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
*    READ TABLE lt_lines INTO ls_lines INDEX 1.
*    ls_out-invoicenote = ls_lines-tdline.
    LOOP AT  lt_lines INTO ls_lines.
      ls_out-invoicenote = ls_out-invoicenote && ls_lines-tdline.
    ENDLOOP.
    CLEAR ls_lines.
    CLEAR  lt_lines.
    "订单类型取描述
    READ TABLE lt_tvakt INTO ls_tvakt WITH KEY auart = ls_out-auart.
    ls_out-auart = ls_tvakt-bezei.
    CLEAR ls_tvakt.
    "分销渠道取描述
    READ TABLE lt_tvtwt INTO ls_tvtwt WITH KEY vtweg = ls_out-vtweg.
    ls_out-vtweg = ls_tvtwt-vtext.
    CLEAR ls_tvtwt.
    "销售渠道取描述
    READ TABLE lt_tvv1t INTO ls_tvv1t WITH KEY kvgr1 = ls_out-kvgr1.
    ls_out-kvgr1 = ls_tvv1t-bezei.
    CLEAR ls_tvv1t.
    "出口国家取描述
    READ TABLE lt_t005t INTO ls_t005t WITH KEY land1 = ls_out-zckgj.
    ls_out-zckgj2 = ls_t005t-landx.
    CLEAR ls_t005t.
    "二级部门取描述
    READ TABLE lt_tvkbt INTO ls_tvkbt WITH KEY vkbur = ls_out-vkbur.
    ls_out-vkbur = ls_tvkbt-bezei.
    CLEAR ls_tvkbt.

    "是否新产品
    IF ls_all-zsfxcp = 'Y'.
      ls_out-zsfxcp = '是'.
    ELSEIF ls_all-zsfxcp = 'N'.
      ls_out-zsfxcp =  '否'.
    ENDIF.

    "是否有备料协议
    IF ls_all-zblxy = 'Y'.
      ls_out-zblxy = '是'.
    ELSEIF ls_all-zblxy = 'N'.
      ls_out-zblxy = '否'.
    ENDIF.

    "预付款情况zyfkqk
    IF ls_all-zyfkqk = '1'.
      ls_out-zyfkqk = '有'.
    ELSEIF ls_all-zyfkqk = '2'.
      ls_out-zyfkqk = '无'.
    ELSEIF ls_all-zyfkqk = '3'.
      ls_out-zyfkqk = '不需要'.
    ENDIF.
    "新客户
    IF ls_all-zsfxkh = 'Y'.
      ls_out-zsfxkh = '是'.
    ELSEIF ls_all-zsfxkh = 'N'.
      ls_out-zsfxkh = '否'.
    ENDIF.

    "aps装填zapszt
    IF ls_all-zapszt = 'A'.
      ls_out-zapszt = '下达状态'.
    ENDIF.
    CLEAR lt_dd07v.
    SELECT
      ddtext
      valpos
      domname
      domvalue_l
    INTO CORRESPONDING FIELDS OF TABLE lt_dd07v
    FROM dd07v
      WHERE dd07v~ddlanguage = '1'
    AND dd07v~domname IN ( 'ZD_DYTK','ZD_SFXBZ','ZD_XFLAG','ZD_ZDSCQY','ZD_PHYQ','ZD_BHCL' ).
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_BHCL' domvalue_l = ls_out-zbhcl."备货策略
    ls_out-zbhcl = ls_dd07v-ddtext.
    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_SFXBZ' domvalue_l = ls_out-zsfxbz."是否新包装
    ls_out-zsfxbz = ls_dd07v-ddtext.
    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_DYTK' domvalue_l = ls_out-zdytk."短溢条款
    ls_out-zdytk = ls_dd07v-ddtext.
    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_PHYQ' domvalue_l = ls_all-zphyq."批号要求
    ls_out-zphyq = ls_dd07v-ddtext.
    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_XFLAG' domvalue_l = ls_all-zsfbj."批号要求
    ls_out-zsfbj = ls_dd07v-ddtext.
    CLEAR ls_dd07v.






    "DDP价格
    CLEAR ls_prcd_elements.
    SELECT SINGLE
    kbetr
    INTO CORRESPONDING FIELDS OF ls_prcd_elements
    FROM  prcd_elements
    INNER JOIN vbak
    ON prcd_elements~knumv = vbak~knumv
    WHERE prcd_elements~kschl = 'Z100'
    AND prcd_elements~kposn = ls_all-posnr
    AND vbak~vbeln = ls_all-vbeln.
    ls_out-ddpprice =  ls_prcd_elements-kbetr.

    "产品编码
    DATA flag TYPE c. "用于标记物料是否为大M码，如果为大M码的话，这个字段标记为'X‘
    CLEAR ls_mara.
    SELECT SINGLE
      matkl
    INTO CORRESPONDING FIELDS OF ls_mara
    FROM mara
    WHERE mara~matnr = ls_all-matnr.
    CLEAR ls_out-meins.
    CLEAR ls_out-vrkme.
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

    changemeins ls_all-meins ls_out-meins.
    changemeins ls_all-vrkme ls_out-vrkme.


    CLEAR flag.
    IF ls_mara-matkl = '501601'."组合装
      flag = 'X'.
    ENDIF.
    DATA: lt_stb LIKE TABLE OF stpox,
          ls_stb LIKE LINE OF lt_stb.
***********hk 添加最晚过账日期字段*************
    READ TABLE lt_aufma INTO ls_aufma WITH KEY kdauf = ls_out-vbeln
                                               kdpos = ls_out-posnr
                                               matnr = ls_out-matnr.
    IF sy-subrc = 0.
      ls_out-budat = ls_aufma-budat.
    ENDIF.
    CLEAR ls_aufma.

***************END*************************

    IF flag = 'X' AND flag_b = 'X'.
      "获取对应的物料清单
      CLEAR ls_mast.
      SELECT SINGLE
      stlnr
      INTO CORRESPONDING FIELDS OF ls_mast
      FROM mast AS a
      WHERE a~matnr = ls_all-matnr
      AND a~werks = ls_all-werks
      AND a~stlan = '1'
      AND a~stlal = '01'.
      "BOM的基本数量
      CLEAR ls_stko.
      SELECT SINGLE
      bmeng
      INTO CORRESPONDING FIELDS OF ls_stko
      FROM stko AS a
      WHERE a~stlty = 'M'
      AND a~stlal = '01'
      AND a~stlnr = ls_mast-stlnr.







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
        CLEAR ls_mara.
        SELECT SINGLE
          mtart
          meins
        INTO CORRESPONDING FIELDS OF ls_mara
        FROM mara
        WHERE mara~matnr = ls_stb-idnrk.
        IF ls_mara-mtart = 'Z050'.
          ls_out-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_out-matnr i_out = 'X' ).
          ls_out-zson = ls_stb-idnrk."BOM的物料编码
          ls_out-zson = zcl_bc_public=>conv_by_ddic( i_input = ls_out-zson i_out = 'X' ).
          "子件的单位
          CLEAR ls_out-zmeins.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
            EXPORTING
              input          = ls_mara-meins
              language       = sy-langu
            IMPORTING
              long_text      = ls_out-zmeins
*             OUTPUT         =
*             SHORT_TEXT     =
            EXCEPTIONS
              unit_not_found = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
*          ls_out-zmeins = ls_mara-meins.
          "子件的描述
          CLEAR ls_makt.
          SELECT SINGLE
          maktx
          INTO ls_out-zsondetail
          FROM makt
          WHERE makt~matnr = ls_stb-idnrk.

          "礼合装子件销售数量
          ls_out-zsum = ( ls_stb-menge / ls_stko-bmeng ) * ls_out-number.
          "礼合装子件基本数量
          ls_out-zsum2 =  ( ls_stb-menge / ls_stko-bmeng ) * ls_out-kwmeng.

          APPEND ls_out TO lt_out." 输出BOM子件的行数
        ENDIF.
        CLEAR ls_stb.
      ENDLOOP.
      ls_out-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_out-matnr i_out = 'X' ).
      APPEND ls_out TO lt_out.
      CLEAR lt_stb.
*      APPEND ls_out to lt_out.
    ELSEIF flag_a = 'X'.
      ls_out-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_out-matnr i_out = 'X' ).
      APPEND ls_out TO lt_out."输出不含BOM的行数
      CLEAR ls_out.
      CLEAR ls_all.
    ENDIF.
*    APPEND ls_out TO lt_out.
    CLEAR ls_out.
    CLEAR ls_all.
  ENDLOOP.
  SELECT
    *
  FROM @lt_out AS a
  WHERE a~udate IN @s_udate
    AND a~udate1 IN @s_udate1
    AND a~kunn2 IN @s_kunn2
  INTO TABLE @lt_out.
  SORT lt_out BY vbeln posnr.
ENDFORM.



FORM catalog.
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 12."自定义字段的长度
*    fieldcat-ref_tabname = 'BKPF'."调用透明表的数据结构
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-coltext = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.
  fieldcatset 'ERDAT'      '创建日期'    sy-tabix.
  fieldcatset 'STATE'   '订单状态' sy-tabix.
  fieldcatset 'UDATE1'      '一审日期'    sy-tabix.
  fieldcatset 'UDATE'      '二审日期'    sy-tabix.
  fieldcatset 'AUGRU'   '订单原因' sy-tabix.
  fieldcatset 'VBELN'   '单据号Serial Number' sy-tabix.
  fieldcatset 'PRONOTE'   '生产相关文本Comment' sy-tabix.
  fieldcatset 'PRONOTE2'   '生产相关文本2Comment2' sy-tabix.
  fieldcatset 'VKORG'   '销售组织' sy-tabix.
  fieldcatset 'WERKS'   '工厂Factory' sy-tabix.
  fieldcatset 'VTWEG'   '分销渠道Area' sy-tabix.
  fieldcatset 'VKBUR'   '二级部门' sy-tabix.
  fieldcatset 'AUART'   '订单类型' sy-tabix.
  fieldcatset 'MC_NAME1'   '检索项' sy-tabix.
  fieldcatset 'KUNNR'   '售达方(客户名称)Customer' sy-tabix.
  fieldcatset 'KUNNR2'   '售达方(编号)' sy-tabix.
  fieldcatset 'SALEMAN'   '业务员Sales' sy-tabix.
  fieldcatset 'POSNR'   '行号' sy-tabix.
  fieldcatset 'MATNR'   '产品编码Product ID' sy-tabix.
  fieldcatset 'ARKTX'   '产品名称Product Name' sy-tabix.
  fieldcatset 'ZBZGG'   '包装规格Packing Specification' sy-tabix.
  fieldcatset 'ZAPSZT'   'APS状态' sy-tabix.
  fieldcatset 'ETTYP' '计划行类别' sy-tabix.
  fieldcatset 'NUMBER'   '基本计量数量QTY' sy-tabix.
  fieldcatset 'MEINS'   '基本计量单位Unit' sy-tabix.
  fieldcatset 'KWMENG'   '销售数量Marketing QTY' sy-tabix.
  fieldcatset 'VRKME'   '销售单位Marketing Unit' sy-tabix.
  fieldcatset 'INNUM'   '非限制库存' sy-tabix.
  fieldcatset 'INNUM2'   '其它库存' sy-tabix.
  fieldcatset 'OUTNUM'   '出库量' sy-tabix.
  fieldcatset 'INVOICE'   '开票量' sy-tabix.
  fieldcatset 'ZDYTK'   '短溢条款' sy-tabix.
  fieldcatset 'ZYSJHQ'   '原始交货日期' sy-tabix.
  fieldcatset 'ZNBJHQ'   '内部交货日期' sy-tabix.
  fieldcatset 'WADAT_IST'      '首个交货日期'    sy-tabix.
  fieldcatset 'ZATPDFRQ'   'ATP答复日期' sy-tabix.
  fieldcatset 'ZTERM'   '付款方式Terms of Payment' sy-tabix.
  fieldcatset 'WAERK'   '订单币种Currency' sy-tabix.
  fieldcatset 'KURSK'   '汇率Exchange Rate' sy-tabix.
  fieldcatset 'KBETR'   '折前单价Unit price' sy-tabix.
  fieldcatset 'KPEIN'   '定价单位' sy-tabix.
  fieldcatset 'KWERT'   '总折扣率%Discount Rate' sy-tabix.
  fieldcatset 'MWSBP2'   '总折扣金额Discount' sy-tabix.
  fieldcatset 'KWERTDIS'   '税率Tax Rate' sy-tabix.
  fieldcatset 'MWSBP'   '销项税额' sy-tabix.
  fieldcatset 'NETWR'   '不含税金额Amount（Tax excluding）' sy-tabix.
  fieldcatset 'PRICE2'     '不含税金额CNY'   sy-tabix.
  fieldcatset 'PRICEALL'   '价税合计Amount(Tax Including)' sy-tabix.
  fieldcatset 'DDPPRICE' 'DDP价格DDP Price' sy-tabix.
  fieldcatset 'ZYPSL'   '样品数量Sample QTY' sy-tabix.
  fieldcatset 'ZSFXBZ'   '新包装New Package?' sy-tabix.
  fieldcatset 'ZSFXCP'   '新产品New Product' sy-tabix.
  fieldcatset 'ZSFBJ'   '是否报检' sy-tabix.
  fieldcatset 'ZPHYQ'   '批号要求' sy-tabix.
  fieldcatset 'ZZZTL'   '最早投料日期' sy-tabix.
  fieldcatset 'ZZWTL'   '最晚投料日期' sy-tabix.
  fieldcatset 'ZZZBZ'   '最早包装时间' sy-tabix.
  fieldcatset 'ZZWBZ'   '最晚包装时间' sy-tabix.
  fieldcatset 'ZBHCL'   '备货策略' sy-tabix.
  fieldcatset 'ZBOMQQ'   'BOM欠缺物料' sy-tabix.
  fieldcatset 'ZBLXY'   '是否有备料协议' sy-tabix.
  fieldcatset 'ZSFXKH'   '新客户 New Customer?' sy-tabix.
  fieldcatset 'NOTE'   '生产行项目备注' sy-tabix.
  fieldcatset 'SALENOT'   '销售文本' sy-tabix.
  fieldcatset 'VOICENOT'   '发货文本' sy-tabix.
  fieldcatset 'INVOICENOTE' '开票文本' sy-tabix.
  fieldcatset 'ZYFKQK' '预付款Prepayment' sy-tabix.
  fieldcatset 'ERNAM'   '制单人Prepared by' sy-tabix.
  fieldcatset 'ZCKGJ'   '出口国家简称' sy-tabix.
  fieldcatset 'ZCKGJ2'   '出口国家Country' sy-tabix.
  fieldcatset 'KVGR1'   '销售渠道/客户类型Sales Channel' sy-tabix.
  fieldcatset 'BU_SORT1'      '实际客户的检索项'    sy-tabix.
  fieldcatset 'CUSTNAME'   '实际客户名称Actual Customer' sy-tabix.
  fieldcatset 'CUSTNAME2'   '实际客户编号' sy-tabix.
  fieldcatset 'KUKLA'   '实际客户等级Classification' sy-tabix.
  fieldcatset 'PARTNER'      '实际客户的业务员'    sy-tabix.
  fieldcatset 'BSTKD_E'   '客户PO PO' sy-tabix.
  fieldcatset 'POSEX_E'   '客户合同分录' sy-tabix.
  fieldcatset 'ZKHWL'   '客户产品编码Customer Product Code' sy-tabix.
  fieldcatset 'ZKHWLMS'   '客户产品名称Customer Product Name' sy-tabix.
  fieldcatset 'ZSTDNAME'   '标准名称' sy-tabix.
  fieldcatset 'PMATN'   '定价参考物料编码' sy-tabix.
  fieldcatset 'MAKTX'   '品名' sy-tabix.
  fieldcatset 'ZPRO'   '是否益生菌' sy-tabix.
  fieldcatset 'MATKL'   '物料中类编码' sy-tabix.
  fieldcatset 'MAKTXS'   '物料中类名称' sy-tabix.
  fieldcatset 'ZCPGG'   '规格型号Filling Weight' sy-tabix.
  fieldcatset 'ZPROTYPE'   '产品类型' sy-tabix.
  fieldcatset 'ZAPPSOU'   '批文来源' sy-tabix.
  fieldcatset 'ZCOTYPE'   '合作类型' sy-tabix.
  fieldcatset 'BUDAT'   '最晚入库日期' sy-tabix.
*  fieldcatset 'LGORT'   '库存地点' sy-tabix.
*  fieldcatset 'werks'   '公司名称' sy-tabix.
*  fieldcatset 'ZPWSX'   '批文属性' sy-tabix.
*  fieldcatset 'INVOICED'   '开票日期Invoice Date' sy-tabix.
*  fieldcatset 'BSTKD_E2'   '客户合同单号' sy-tabix.
*  fieldcatset 'ZKHWL' '客户物料编码' sy-tabix.
*  fieldcatset 'ZKHWLMS' '客户物料描述' sy-tabix.
  IF flag_b = 'X'.
    fieldcatset   'ZSON'      '子件'    sy-tabix.
    fieldcatset   'ZSONDETAIL'      '子件描述'    sy-tabix.
    fieldcatset   'ZMEINS'      '子件单位'    sy-tabix.
    fieldcatset   'ZSUM'      '礼合装子件销售数量'    sy-tabix.
    fieldcatset   'ZSUM2'      '礼合装子件基本数量'    sy-tabix.
  ENDIF.

  READ TABLE fieldcat INDEX 5."读取报表第三列
  fieldcat-hotspot = 'X'."鼠标热点事件
  fieldcat-key = 'X'."定义为主键（颜色改变）
  MODIFY fieldcat INDEX 5."修改样式

ENDFORM.

FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_USER_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = '销售订单日报表'
*     I_GRID_SETTINGS          =
      is_layout_lvc            = layout
      it_fieldcat_lvc          = fieldcat[]
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
      i_default                = 'X'
      i_save                   = 'A'
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = lt_out
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.


FORM frm_user_status  USING i_it_extab TYPE slis_t_extab.
  SET PF-STATUS '0100' .
ENDFORM.                    " FRM_SET_PF_STATUS

FORM frm_authority_check.
  DATA ls_pvkorg LIKE LINE OF pvkorg.
  DATA ls_svtweg LIKE LINE OF svtweg.
  LOOP AT svtweg INTO ls_svtweg.
*  DATA lv_werks TYPE werks_d.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
     ID 'VKORG' DUMMY
     ID 'VTWEG' FIELD ls_svtweg-low
     ID 'SPART' DUMMY
     ID 'ACTVT' DUMMY.
    IF sy-subrc <> 0.
*     Implement a suitable exception handling here
      MESSAGE '分销渠道没有权限' TYPE 'E'.
      gv_sel_error = 'X'.
    ELSE.
*      MESSAGE '有权限' TYPE 'S'.
      gv_sel_error = ''.
    ENDIF.
    IF gv_sel_error = 'X'.
      EXIT.
    ENDIF.
    CLEAR ls_pvkorg.
  ENDLOOP.
  LOOP AT pvkorg INTO ls_pvkorg.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
    ID 'VKORG' FIELD ls_pvkorg-low
    ID 'VTWEG' DUMMY
    ID 'SPART' DUMMY
    ID 'ACTVT' DUMMY.
    IF sy-subrc <> 0.
*     Implement a suitable exception handling here
      gv_sel_error = 'X'.
      MESSAGE '销售组织没有权限' TYPE 'E' .
    ELSE.
*      MESSAGE '有权限' TYPE 'S'.
      gv_sel_error = ''.
    ENDIF.
    IF gv_sel_error = 'X'.
      EXIT.
    ENDIF.
    CLEAR ls_pvkorg.
  ENDLOOP.

ENDFORM.

*响应鼠标点击时间的子程序
FORM frm_user_command USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
      READ TABLE lt_out INTO ls_out INDEX rs_selfield-tabindex.
      SET PARAMETER ID 'AUN' FIELD ls_out-vbeln.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.