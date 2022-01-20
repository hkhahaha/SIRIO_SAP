*&---------------------------------------------------------------------*
*& 程序名：ZSDR0010
*&作者：Seashell Huang
*&事务代码：ZSD011
*&模块：SD模块
*&创建日期：15.10.2019 10:51:57
*&功能描述：销售订单日报表
*&---------------------------------------------------------------------*
*  Modified Recorder :
*  Date      C#NO        Author      Content
*  --------  ----------  ----------  ---------------
*  20210517  S4DK904793  05168       SD：订单日报汇率问题 20210517
*                                    1、修改VBKD取数逻辑，默认根据订单号和行项目查询，若查询不到，通过订单号查询
*                                    2、汇率取数逻辑：货币判断是否CNY，若不是，取VBKD-KURRF会计核算汇率，若取不到，去VBKD-KURSK汇率，若汇率=1，根据货币，定价日期取TCURR表的UKURS汇率
*                                    3、优化AUSP表，不全表查询
*  20210604  S4DK904838  05168       SD：销售订单日报付款方式修复 20210604
*  20211231  S4DK905471  05168       SD：添加备料单类型 20211231
*&---------------------------------------------------------------------*
REPORT zsdr0010.
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
        vtweg_n        TYPE    string,    "归属分销渠道(因组织架构调整，客户可能更换渠道，但历史订单无法调整)
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
        "最终单据信息
        zysdj          TYPE zsdt0025-zysdj,
        zysdh          TYPE zsdt0025-zysdh,
        zyscj          TYPE zsdt0025-zyscj,
        vbezz          TYPE vbap-vbeln,
        poszz          TYPE vbap-posnr,
        zatzz          TYPE vbap-zatpdfrq,
        zapzz          TYPE vbap-zapszt,
        ettzz          TYPE vbep-ettyp,
        zplp1          TYPE mbew-zplp1, "计划价格1

        "最终单据信息
        werks2         TYPE    vbap-werks  , "  制造工厂
        zbldlx         TYPE    string  , "备料单类型 S4DK905471
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
        vtweg_n(25),"归属分销渠道 (因组织架构调整，客户可能更换渠道，但历史订单无法调整)
        kvgr1(25), "  销售渠道/客户类型
        werks            TYPE    vbap-werks  , "  工厂
        lgort            TYPE    lips-lgort  , "  库存地点
        vkbur(15), "  二级部门
        saleman(20), "  业务员
        salemanno(20),"业务员编号
        mc_name1         TYPE    but000-mc_name1, "检索项
        mandt            TYPE    vbak-mandt  , "  集团名称
        kunnr(60), "  售达方(客户名称)
        kunn2(60), "实际客户的业务员
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
        bstdk_e          TYPE vbkd-bstdk_e, "客户采购日期
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
        rate             TYPE vbap-kwmeng, "销售转换率
        sel(1),
        "最终单据信息
        zysdj            TYPE zsdt0025-zysdj,
        zysdh            TYPE zsdt0025-zysdh,
        vbezz            TYPE vbap-vbeln,
        poszz            TYPE vbap-posnr,
        zatzz            TYPE vbap-zatpdfrq,
        zapzz            TYPE vbap-zapszt,
        ettzz            TYPE vbep-ettyp,
        zleix            TYPE zsdt0022t-zleix,
        zleit            TYPE zsdt0022t-zleit,
        zplp1            TYPE mbew-zplp1, "计划价格1
        "最终单据信息
        werks2           TYPE    vbap-werks  , "  制造工厂
        zbldlx(60), "备料单类型 S4DK905471
        firdat           TYPE zppt0003-erdat, "最早入库日期
      END OF ty_out.

TYPES : BEGIN OF ty_vbapz ,
          vbeln    TYPE vbap-vbeln,
          posnr    TYPE vbap-posnr,
          zatpdfrq TYPE vbap-zatpdfrq,
          zapszt   TYPE vbap-zapszt,
          ettyp    TYPE vbep-ettyp,
          ernam    TYPE vbap-ernam,
        END OF ty_vbapz .

DATA : gs_all TYPE ty_all.
DATA : gs_zsdt0025 TYPE zsdt0025 .
DATA : gt_zsdt0025 TYPE TABLE OF zsdt0025 .
DATA : gs_vbapz TYPE ty_vbapz .
DATA : gt_vbapz TYPE TABLE OF ty_vbapz .
DATA : gs_zsdt0022t TYPE zsdt0022t .
DATA : gt_zsdt0022t TYPE TABLE OF zsdt0022t .

***********  ALV 定义 **********************************************

DATA: lt_out TYPE TABLE OF ty_out, "用于存储ALV内表的数据
      ls_out TYPE ty_out.

"开始选择界面的数据操作
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
sauart    FOR   vbak-auart  ,"  订单类型
svbeln FOR vbap-vbeln,"订单号
sposnr FOR vbap-posnr,"行号
s_znbjhq FOR vbap-znbjhq,
pvkorg    FOR   vbak-vkorg  OBLIGATORY,"  销售组织
svtweg    FOR   vbak-vtweg  OBLIGATORY,"  分销渠道
sspart    FOR   vbak-spart  ,"  产品组
svkbur    FOR   vbak-vkbur  ,"  销售办公室 S4DK905471
swerks    FOR   vbap-werks  ,"  工厂
skunnr    FOR   vbak-kunnr  ,"  客户
sernam    FOR   vbak-ernam  ,"  创建人
serdat    FOR   vbak-erdat  ,"  创建日期
smatnr    FOR   vbap-matnr  ,"  物料编码
settyp    FOR   vbep-ettyp  ,"  计划行类别
*S_ETTZZ   FOR VBEP-ETTYP  ," 最终单据计划行类别
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
       lt_jest          TYPE TABLE OF jest,
       ls_tvaut         TYPE tvaut,
       lt_tvaut         TYPE TABLE OF tvaut,
       ls_adrp          TYPE adrp,
       ls_kna1          TYPE kna1,
       lt_kna1          TYPE TABLE OF kna1,
       ls_knvv          TYPE knvv,
       lt_knvv          TYPE TABLE OF knvv,
       ls_knvp          TYPE knvp,
       lt_knvp          TYPE TABLE OF knvp,
       ls_t052u         TYPE t052u,
       lt_t052u         TYPE TABLE OF t052u,
       ls_vbkd          TYPE vbkd,
       lt_vbkd          TYPE TABLE OF vbkd,
       ls_vbep          TYPE vbep,
       lt_vbep          TYPE TABLE OF vbep,
       ls_vbpa          TYPE vbpa,
       lt_vbpa          TYPE TABLE OF vbpa,
       ls_t023t         TYPE t023t,
       lt_t023t         TYPE TABLE OF t023t,
       ls_makt          TYPE makt,
       lt_makt          TYPE TABLE OF makt,
       ls_mvke          TYPE mvke,
       lt_mvke          TYPE TABLE OF mvke,
       ls_lips          TYPE lips,
       ls_but000        TYPE but000,
       lt_but000        TYPE TABLE OF but000,
       ls_tkukt         TYPE tkukt,
       lt_tkukt         TYPE TABLE OF tkukt,
       lt_jcds          TYPE TABLE OF jcds,
       ls_jcds          TYPE jcds,
       lt_vbfa          TYPE TABLE OF vbfa,
       ls_vbfa          TYPE vbfa,
       lt_vbfa_out      TYPE TABLE OF vbfa,
       ls_vbfa_out      TYPE vbfa,
       lt_vbfa_invoice  TYPE TABLE OF vbfa,
       ls_vbfa_invoice  TYPE vbfa,
       lt_vbfa_jhrq     TYPE TABLE OF vbfa,
       ls_vbfa_jhrq     TYPE vbfa,
       ls_mara          TYPE mara,
       lt_mara          TYPE TABLE OF mara,
       ls_prcd_elements TYPE prcd_elements,
       lt_prcd_elements TYPE TABLE OF prcd_elements,
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

  "获取订单类型描述
  SELECT
    auart,
    bezei
  INTO CORRESPONDING FIELDS OF TABLE @lt_tvakt
  FROM tvakt
  WHERE tvakt~spras = @sy-langu.
  SORT lt_tvakt BY auart.

  "取出所有的分销渠道的描述
  SELECT
    vtweg,
    vtext
  INTO CORRESPONDING FIELDS OF TABLE @lt_tvtwt
  FROM tvtwt
  WHERE tvtwt~spras = @sy-langu.
  SORT lt_tvtwt BY vtweg.

  "获取销售渠道的全部描述
  SELECT
    kvgr1,
    bezei
  INTO CORRESPONDING FIELDS OF TABLE @lt_tvv1t
  FROM tvv1t
  WHERE spras = @sy-langu.
  SORT lt_tvv1t BY kvgr1.

  "获取出口国家的全部描述
  SELECT
    land1,
    landx
  INTO CORRESPONDING FIELDS OF TABLE @lt_t005t
  FROM t005t
  WHERE spras = @sy-langu.
  SORT lt_t005t BY land1.

  "取二级部门的全部描述
  SELECT
    vkbur,
    bezei
  INTO CORRESPONDING FIELDS OF TABLE @lt_tvkbt
  FROM tvkbt
  WHERE spras = @sy-langu.
  SORT lt_tvkbt BY vkbur.

  "首先先查询VBAK
  SELECT
  zapszt    "  APS状态
  vbak~vbeln     "  单据号
  vbap~posnr     "  行号
  vbak~ernam     "  制单人
  vkorg     "  销售组织
  auart     "  订单类型
  vtweg     "  分销渠道
  werks     "  工厂
  vkbur     "  二级部门
  vbak~mandt     "  集团名称
  kunnr     "  售达方
  knumv     "  条件编号
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
  zbldlx    "  备料单类型 S4DK905471
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
  zsdt0025~zysdj
  zsdt0025~zysdh
    zsdt0025~zyscj
  INTO CORRESPONDING FIELDS OF TABLE lt_all
  FROM vbak
  INNER JOIN vbap
  ON vbak~vbeln = vbap~vbeln
  LEFT JOIN zsdt0025   "ADD BY LANJINGCUN 获取多方交易的原始单据 20201214-S
  ON zsdt0025~vbeln = vbap~vbeln
 AND zsdt0025~posnr = vbap~posnr
  WHERE vbap~matnr IN smatnr
    AND vbap~werks IN swerks
    AND vbap~vbeln IN svbeln
    AND vbap~posnr IN sposnr
    AND vbap~znbjhq IN s_znbjhq
    AND vbak~auart IN sauart
    AND vbak~vkorg IN pvkorg
    AND vbak~vkbur IN svkbur
*    AND VBAK~VTWEG IN SVTWEG
    AND vbak~spart IN sspart
    AND vbak~kunnr IN skunnr
    "AND VBAK~ERNAM IN SERNAM
*    AND vbak~vbeln = '0200000829'
    AND vbak~erdat IN serdat
  AND vbak~vbtyp <> 'W'.

  "上面获取到的是所有有关VBAP和VBAK的数据

  IF lt_all IS NOT INITIAL.
    SELECT matnr,bwkey,zplp1 FROM mbew
      INTO TABLE @DATA(lt_mbew)
      FOR ALL ENTRIES IN @lt_all
      WHERE matnr = @lt_all-matnr AND bwkey = @lt_all-werks.
  ENDIF.


  "因组织架构调整，导致客户渠道变更，但历史订单渠道无法更新，因此通过自建表ZSDT0114来更新报表中订单的渠道
****  data: lt_data type TABLE OF zsdt0114,
****        lt_knvv_ TYPE TABLE OF knvv,
****        lv_vtweg type vtweg, "入参
****        r_vtweg type vtweg,   "返参
****        error TYPE string.
****  "调用渠道转换函数，获取转换表数据，用于后续更新渠道使用
****  CALL FUNCTION 'Z_SD_GET_TRANS_DATA'
****    TABLES
****      lt_data       = lt_data
****      lt_knvv       = lt_knvv_
****            .
  "获取实际客户
  SELECT vbeln, kunnr
    INTO CORRESPONDING FIELDS OF TABLE @lt_vbpa
    FROM vbpa
    WHERE vbpa~parvw = 'Z1'
    AND vbpa~posnr = ''.
  SORT lt_vbpa BY vbeln.


  SELECT * FROM zsdt0030 INTO TABLE @DATA(lt_zsdt0030) WHERE ztype = '1'.


  "将物料编码转类型
  LOOP AT lt_all INTO ls_all.
    "S4DK904793-start
    ls_all2-matnr90 = ls_all-matnr.
    APPEND ls_all2 TO lt_all2.
    "S4DK904793-start
    "多方期初的创建人抛转的创建人更新
    IF ls_all-ernam = 'S4_AUTO'.
      ls_all-ernam = ls_all-zyscj.
    ENDIF.

    READ TABLE lt_mbew INTO DATA(ls_mbew) WITH KEY matnr = ls_all-matnr bwkey = ls_all-werks.
    IF sy-subrc = 0.
      ls_all-zplp1 = ls_mbew-zplp1.
    ENDIF.
*  客户渠道变更
    READ TABLE lt_zsdt0030 INTO DATA(ls_zsdt0030) WITH KEY vbeln = ls_all-vbeln.
    IF sy-subrc = 0.
      ls_all-vtweg = ls_zsdt0030-zvtweg.
      ls_all-vtweg_n = ls_zsdt0030-zvtweg.
    ENDIF.
    CLEAR ls_prcd_elements.
    ls_prcd_elements-knumv = ls_all-knumv.
    APPEND ls_prcd_elements TO lt_prcd_elements.


    "获取实际客户
    CLEAR ls_vbpa.
    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_all-vbeln BINARY SEARCH.

    "调用函数更新订单的归属分销渠道字段
****    lv_vtweg = ls_all-vtweg.
****    clear error.
****    CALL FUNCTION 'Z_SD_GET_VTWEG'
****      EXPORTING
****        i_kunnr       = ls_vbpa-kunnr"实际客户
****        i_vkorg       = ls_all-vkorg
****        i_vtweg       = lv_vtweg
****        i_spart       = ls_all-spart
****     IMPORTING
****       R_VTWEG       = r_vtweg
****       ERROR         = error
****      TABLES
****        lt_data       = lt_data
****        lt_knvv       = lt_knvv_
****              .
****    IF error is not INITIAL.
****      "渠道转换失败，显示失败信息
****      ls_all-vtweg_n = error.
****    else.
****      ls_all-vtweg_n = r_vtweg.
****    ENDIF.



    MODIFY lt_all FROM ls_all.
  ENDLOOP.
  DELETE lt_all WHERE ernam NOT IN sernam.
*  DELETE lt_all WHERE vtweg NOT IN svtweg.
  DELETE lt_all WHERE vtweg_n NOT IN svtweg.






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
*04815  SELECT
*04815    *
*04815  INTO CORRESPONDING FIELDS OF TABLE lt_ausp
*04815  FROM ausp
*04815  FOR ALL ENTRIES IN lt_all2
*04815  WHERE ausp~objek = lt_all2-matnr90
*04815  AND ausp~atinn IN ( lv_zappsou,lv_zcotype,lv_zprotype,lv_zpro,lv_zstdname ).
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_ausp
    FROM ausp
    WHERE ausp~atinn IN ( lv_zappsou, lv_zcotype, lv_zprotype, lv_zpro, lv_zstdname ).
  SORT lt_ausp BY objek atinn.



*    汇率 modify by jiguican 20210121
  DATA : lt_tcurr TYPE TABLE OF tcurr,
         ls_tcurr TYPE tcurr,
         gdatu(8) TYPE c.
  SELECT * INTO TABLE lt_tcurr
      FROM tcurr
      WHERE kurst = 'M'
        AND tcurr = 'CNY'.
  SORT lt_tcurr BY fcurr gdatu.


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

  "ADD BY LANJINGCUN 获取多方交易的最终单据信息 20201214-S561
  PERFORM sub_get_zdf TABLES lt_all .
  SELECT *
    INTO TABLE gt_zsdt0022t
    FROM zsdt0022t
    WHERE spras = sy-langu .
  "ADD BY LANJINGCUN 获取多方交易的最终单据信息 20201214-E561


**************************S-2022-01-11 04815 调整代码段***********************************************************************
  DATA: ls_ysdt0001 TYPE ysdt0001,
        lt_ysdt0001 TYPE TABLE OF ysdt0001.

  CLEAR ls_ysdt0001.
  ls_ysdt0001-uuid = zcl_bc_public=>get_guid( ).
  ls_ysdt0001-datum = sy-datum.
  ls_ysdt0001-uzeit = sy-uzeit.









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

  "获取订单原因
  SELECT augru, bezei
      INTO TABLE @lt_tvaut
      FROM tvaut
      WHERE tvaut~spras  = @sy-langu.

  "获取计划行类别
  SELECT vbeln,posnr,ettyp
    INTO CORRESPONDING FIELDS OF TABLE @lt_vbep
    FROM vbep.
  SORT lt_vbep BY vbeln posnr.

  "获取订单状态
  SELECT objnr, stat
    INTO CORRESPONDING FIELDS OF TABLE @lt_jest
    FROM jest
    WHERE jest~stat LIKE 'E%'
    AND jest~inact = ''.
  SORT lt_jest BY objnr.

  "获取二审日期
  SELECT  objnr, udate, chgnr
    INTO CORRESPONDING FIELDS OF TABLE @lt_jcds
    FROM jcds
    WHERE jcds~stat = 'E0003'
    ORDER BY objnr,chgnr.

  "制单人(取描述)
  DATA: BEGIN OF ls_user,
          bname     TYPE usr21-bname,
          name_text TYPE adrp-name_text,
        END OF ls_user,
        lt_user LIKE TABLE OF ls_user.
  SELECT bname, name_text
    INTO TABLE @lt_user
    FROM adrp
    INNER JOIN usr21
    ON adrp~persnumber = usr21~persnumber
  .

  "客户组1
  SELECT kunnr, vkorg, vtweg, spart, kvgr1
    INTO CORRESPONDING FIELDS OF TABLE @lt_knvv
    FROM knvv.
  SORT lt_knvv BY kunnr vkorg vtweg spart.


  "业务员
  SELECT kunnr, vkorg, vtweg, spart, kunn2
    INTO CORRESPONDING FIELDS OF TABLE @lt_knvp
    FROM  knvp
    WHERE knvp~parvw = 'Z3'.
  SORT lt_knvp BY kunnr vkorg vtweg spart.


  "BP名称
  SELECT partner, name_org1, name_org2, name_org3, name_org4, bu_sort1
    INTO CORRESPONDING FIELDS OF TABLE @lt_but000
    FROM but000.
  SORT lt_but000 BY partner.



  "获取客户信息表KNA1
  SELECT kunnr, kukla
    INTO CORRESPONDING FIELDS OF TABLE @lt_kna1
    FROM kna1.
  SORT lt_kna1 BY kunnr.

  "获取客户等级
  SELECT kukla, vtext
    INTO CORRESPONDING FIELDS OF TABLE @lt_tkukt
    FROM tkukt
    WHERE spras = @sy-langu.
  SORT lt_tkukt BY kukla.

  "获取VBKD表，取汇率、PO
  SELECT vbeln, posnr, prsdt, kurrf, kursk, zterm, bstkd_e, posex_e,bstdk_e
    INTO CORRESPONDING FIELDS OF TABLE @lt_vbkd
    FROM vbkd
    FOR ALL ENTRIES IN @lt_all
    WHERE vbeln = @lt_all-vbeln
      AND posnr = @lt_all-posnr..
  SORT lt_vbkd BY vbeln posnr.


  "获取支付条件
  SELECT zterm, text1
    INTO CORRESPONDING FIELDS OF TABLE @lt_t052u
    FROM t052u
    WHERE spras = @sy-langu.
  SORT lt_t052u BY zterm.


  "获取定价参考物料编码
  SELECT vkorg, vtweg, matnr, pmatn
    INTO CORRESPONDING FIELDS OF TABLE @lt_mvke
    FROM mvke.
  SORT lt_mvke BY matnr vkorg vtweg.

  "最早入库日期
  SELECT
    kdauf,
    kdpos,
    erdat
  INTO TABLE @DATA(lt_zppt0003)
  FROM zppt0003
  FOR ALL ENTRIES IN @lt_all
  WHERE kdauf = @lt_all-vbeln
    AND kdpos = @lt_all-posnr.


  "获取批次特性值文本
  DATA: BEGIN OF ls_cawn_t,
          atinn TYPE cawn-atinn,
          atwrt TYPE cawn-atwrt,
          atwtb TYPE cawnt-atwtb,
        END OF ls_cawn_t,
        lt_cawn_t LIKE TABLE OF ls_cawn_t.
  SELECT a~atinn, a~atwrt, t~atwtb
    INTO TABLE @lt_cawn_t
    FROM cawn AS a
    INNER JOIN cawnt AS t ON a~atinn = t~atinn AND a~adzhl = t~adzhl
    WHERE a~atinn IN ( @lv_zappsou, @lv_zcotype, @lv_zprotype, @lv_zpro, @lv_zstdname )
    AND t~spras = @sy-langu.
  SORT lt_cawn_t BY atinn atwrt.

  "获取物料描述
  SELECT matnr, maktx
    INTO CORRESPONDING FIELDS OF TABLE @lt_makt
    FROM makt
    WHERE spras = @sy-langu.
  SORT lt_makt BY matnr.


  "获取域定义描述
  SELECT
    ddtext,
    valpos,
    domname,
    domvalue_l
    INTO CORRESPONDING FIELDS OF TABLE @lt_dd07v
    FROM dd07v
    WHERE dd07v~ddlanguage = @sy-langu
    AND dd07v~domname IN ( 'ZD_DYTK','ZD_SFXBZ','ZD_XFLAG','ZD_ZDSCQY','ZD_PHYQ','ZD_BHCL','ZBLDLX' ).
  SORT lt_dd07v BY domname domvalue_l.


  "获取物料清单
  SELECT matnr, werks, stlnr
      INTO CORRESPONDING FIELDS OF TABLE @lt_mast
      FROM mast AS a
      WHERE a~stlan = '1'
      AND a~stlal = '01'.
  SORT lt_mast BY matnr werks.

  "获取BOM基本数量
  SELECT stlnr, bmeng
      INTO CORRESPONDING FIELDS OF TABLE @lt_stko
      FROM stko AS a
      WHERE a~stlty = 'M'
      AND a~stlal = '01'.
  SORT lt_stko BY stlnr.

  "获取mara表数据
  SELECT matnr, matkl, mtart, meins
    INTO CORRESPONDING FIELDS OF TABLE @lt_mara
    FROM mara.
  SORT lt_mara BY matnr.

  "获取物料组描述
  SELECT matkl, wgbez
    INTO CORRESPONDING FIELDS OF TABLE @lt_t023t
    FROM t023t
    WHERE spras = @sy-langu.



  "获取税率
  IF lt_prcd_elements IS NOT INITIAL.
    SELECT knumv, kposn, kbetr, kwert, kpein
      INTO CORRESPONDING FIELDS OF TABLE @lt_prcd_elements
      FROM prcd_elements
      FOR ALL ENTRIES IN @lt_prcd_elements
      WHERE knumv = @lt_prcd_elements-knumv
      AND kschl = 'MWSI'
      .
    SORT lt_prcd_elements BY knumv kposn.
  ENDIF.

  "获取销售订单库存
  DATA: BEGIN OF ls_mska,
          vbeln  TYPE mska-vbeln,
          posnr  TYPE mska-posnr,
          matnr  TYPE mska-matnr,
          kalab  TYPE mska-kalab,
          kalab1 TYPE mska-kalab,
        END OF ls_mska,
        lt_mska LIKE TABLE OF ls_mska.

  SELECT  vbeln, posnr, matnr, kalab,  ( kains + kaspe + kavla + kavin + kavsp ) AS kalab1
    INTO TABLE @lt_mska
    FROM mska
    WHERE lgort IN ('1040','2040','3040','1070','2070','3070').
  SORT lt_mska BY vbeln posnr matnr.


  "获取出库数量
  IF lt_all IS NOT INITIAL.
    CLEAR lt_vbfa.
    SELECT ruuid, vbelv, posnv, vbtyp_n, rfmng
      INTO CORRESPONDING FIELDS OF TABLE @lt_vbfa
      FROM vbfa
      FOR ALL ENTRIES IN @lt_all
      WHERE vbelv = @lt_all-vbeln
      AND posnv = @lt_all-posnr
      AND vbfa~vbtyp_n IN ( 'R','h' ).

    SELECT vbelv, posnv, SUM( rfmng * (
                   CASE vbtyp_n
                       WHEN 'R' THEN ( 1 )
                       WHEN 'H' THEN ( -1 )
                   END )
                  ) AS rfmng
      FROM @lt_vbfa AS a
      GROUP BY vbelv, posnv
      INTO CORRESPONDING FIELDS OF TABLE @lt_vbfa_out.
    SORT lt_vbfa_out BY vbelv posnv.

  ENDIF.


  "获取首个发货日期，通过VBFA表找出销售订单对应的交货单，按照交货单升序取第一张交货单的过账日期
  IF lt_all IS NOT INITIAL.

    SELECT vbelv, posnv, vbeln
      INTO CORRESPONDING FIELDS OF TABLE @lt_vbfa_jhrq
      FROM vbfa
      FOR ALL ENTRIES IN @lt_all
      WHERE vbelv = @lt_all-vbeln
      AND vbtyp_n = 'J'
      AND rfmng > 0.
    SORT lt_vbfa_jhrq BY vbelv posnv vbeln.

    SELECT vbeln, wadat_ist
      INTO CORRESPONDING FIELDS OF TABLE @lt_likp
      FROM likp.
    SORT lt_likp BY vbeln.

  ENDIF.

  "获取开票数量
  IF ls_all IS NOT INITIAL.
    CLEAR lt_vbfa.
    SELECT vbelv, posnv, vbtyp_n, rfmng
      INTO CORRESPONDING FIELDS OF TABLE @lt_vbfa
      FROM vbfa
      FOR ALL ENTRIES IN @lt_all
      WHERE vbelv = @lt_all-vbeln
      AND posnv = @lt_all-posnr
      AND vbfa~vbtyp_n IN ( 'M','N', 'O', 'S' ).

    SELECT vbelv, posnv, SUM( rfmng * (
                   CASE vbtyp_n
                      WHEN 'M' THEN ( 1 )
                      WHEN 'O' THEN ( 1 )
                      WHEN 'N' THEN ( -1 )
                      WHEN 'S' THEN ( -1 )
                   END )
                  ) AS rfmng
      FROM @lt_vbfa AS a
      GROUP BY vbelv, posnv
      INTO CORRESPONDING FIELDS OF TABLE @lt_vbfa_invoice.
    SORT lt_vbfa_invoice BY vbelv posnv.
  ENDIF.






**************************E-2022-01-11 04815 调整代码段***********************************************************************



  "将一部分可以直接插入的数据插入到内表中
  LOOP AT lt_all INTO ls_all.

    CLEAR ls_out.
    CLEAR ls_ausp.

    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zappsou BINARY SEARCH.
    ls_out-zappsou = ls_ausp-atwrt.
    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zcotype BINARY SEARCH.
    ls_out-zcotype = ls_ausp-atwrt.
    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zprotype BINARY SEARCH.
    ls_out-zprotype = ls_ausp-atwrt.
    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zstdname BINARY SEARCH.
    ls_out-zstdname = ls_ausp-atwrt.
    CLEAR ls_ausp.
    READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all-matnr
                                             atinn = lv_zpro BINARY SEARCH.
    ls_out-zpro = ls_ausp-atwrt.
    CLEAR ls_ausp.

*04815**     CLEAR LT_ZCO.
*04815**    "合作类型取描述
*04815**    SELECT
*04815**      CAWNT~ATWTB
*04815**      CAWN~ATINN
*04815**      CAWN~ATZHL
*04815**      CAWN~ATWRT
*04815**    INTO CORRESPONDING FIELDS OF TABLE LT_ZCO
*04815**    FROM CAWNT
*04815**    INNER JOIN CAWN
*04815**    ON CAWN~ATINN = CAWNT~ATINN
*04815**    AND CAWN~ATZHL = CAWNT~ATZHL
*04815**    WHERE CAWNT~SPRAS = '1'
*04815**    AND CAWNT~ATINN = LV_ZCOTYPE.
    CLEAR ls_zco.
    READ TABLE lt_zco INTO ls_zco WITH KEY atwrt = ls_out-zcotype.
    IF sy-subrc EQ 0.
      ls_out-zcotype = ls_zco-atwtb.
    ENDIF.

*    CLEAR ls_all-augru.
*04815**    "订货原因取描述
*04815**    if ls_all-augru is not initial.
*04815**      select single
*04815**        bezei
*04815**      into ls_all-augru
*04815**      from tvaut
*04815**      where tvaut~spras  = '1'
*04815**      and tvaut~augru = ls_all-augru.
*04815**    endif.
    IF ls_all-augru IS NOT INITIAL.
      CLEAR ls_tvaut.
      READ TABLE lt_tvaut INTO ls_tvaut WITH KEY augru = ls_all-augru.
      IF sy-subrc EQ 0.
        ls_all-augru = ls_tvaut-bezei.
      ENDIF.
    ENDIF.





    "因为TY_OUT的结构包含了TY_ALL的结构，所以直接MOVE
    MOVE-CORRESPONDING ls_all TO ls_out.
    ls_out-kwmeng = ls_all-zmeng + ls_all-kwmeng.

    "ADD BY LANJINGCUN 获取多方交易的最终单据信息 20201214-S599
    ls_out-zysdj = ls_all-zysdj .
    ls_out-zysdh = ls_all-zysdh .
    CLEAR gs_zsdt0025 .
    READ TABLE gt_zsdt0025 INTO gs_zsdt0025 WITH KEY zysdj = ls_out-zysdj
                                                     zysdh = ls_out-zysdh .
    IF sy-subrc = 0 .
      CLEAR gs_vbapz .
      READ TABLE gt_vbapz INTO gs_vbapz WITH  KEY vbeln = gs_zsdt0025-vbeln
                                                  posnr = gs_zsdt0025-posnr .
      ls_out-vbezz = gs_vbapz-vbeln.
      ls_out-poszz = gs_vbapz-posnr.
      ls_out-zatzz = gs_vbapz-zatpdfrq.
      ls_out-zapzz = gs_vbapz-zapszt.
      ls_out-ettzz = gs_vbapz-ettyp.
      ls_out-zleix = gs_zsdt0025-zleix .
      IF ls_out-ernam = 'S4_AUTO'.
        ls_out-ernam = gs_vbapz-ernam.
      ENDIF.

      CLEAR gs_zsdt0022t .
      READ TABLE gt_zsdt0022t INTO gs_zsdt0022t WITH KEY zleix = ls_out-zleix .
      ls_out-zleit = gs_zsdt0022t-zleit .
    ENDIF .

    "ADD BY LANJINGCUN 获取多方交易的最终单据信息 20201214-E599
    "计划行类别

*******************S 04815 2022-01-13****************
*04815    select single
*04815    ettyp
*04815    into ls_out-ettyp
*04815    from vbep
*04815    where vbep~vbeln = ls_all-vbeln
*04815    and vbep~posnr = ls_all-posnr.
    CLEAR ls_vbep.
    READ TABLE lt_vbep INTO ls_vbep WITH KEY vbeln = ls_all-vbeln posnr = ls_all-posnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-ettyp = ls_vbep-ettyp.
    ENDIF.
*******************E 04815 2022-01-13****************

*    IF SETTYP IS NOT INITIAL.
*      READ TABLE SETTYP INTO LS_ETTYP WITH KEY LOW = LS_OUT-ETTYP.
*      IF SY-SUBRC <> 0.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
    CHECK ls_out-ettzz IN settyp OR  ls_out-ettyp IN settyp .
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
*******************S 04815 2022-01-13****************
*    select single
*      stat
*    into corresponding fields of ls_jest
*    from jest
*    where jest~objnr = ls_all-objnr
*    and jest~stat like 'E%'
*    and jest~inact = ''.

    READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_all-objnr BINARY SEARCH.
    IF sy-subrc EQ 0.
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
    ENDIF.
*******************E 04815 2022-01-13****************

    IF ls_out-state = '财务二审'.
      "二审日期
*******************S 04815 2022-01-13****************
      CLEAR ls_jcds.
      READ TABLE lt_jcds INTO ls_jcds WITH KEY objnr = ls_all-objnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_out-udate = ls_jcds-udate.
      ENDIF.
*04815      CLEAR lt_jcds.
*04815      SELECT
*04815      udate
*04815      chgnr
*04815      INTO CORRESPONDING FIELDS OF TABLE lt_jcds
*04815      FROM jcds
*04815      WHERE jcds~objnr = ls_all-objnr
*04815      AND jcds~stat = 'E0003'
*04815      ORDER BY chgnr.
*04815
*04815      READ TABLE lt_jcds INTO ls_jcds INDEX 1.
*04815      ls_out-udate = ls_jcds-udate.
*04815      CLEAR ls_jcds.
*******************E 04815 2022-01-13****************
    ENDIF.

    "制单人(取描述)
*******************S 04815 2022-01-13****************
*04815    SELECT SINGLE
*04815      name_text
*04815    INTO CORRESPONDING FIELDS OF ls_adrp
*04815    FROM adrp
*04815    INNER JOIN usr21
*04815    ON adrp~persnumber = usr21~persnumber
*04815    WHERE usr21~bname = ls_all-ernam.
*04815    ls_out-ernam = ls_adrp-name_text.

    CLEAR ls_user.
    READ TABLE lt_user INTO ls_user WITH KEY bname = ls_all-ernam.
    IF sy-subrc EQ 0.
      ls_out-ernam = ls_user-name_text.
    ENDIF.

*******************E 04815 2022-01-13****************


    " 销售渠道/客户类型 (客户组1)

*******************S 04815 2022-01-13****************
    CLEAR ls_knvv.
    READ TABLE lt_knvv INTO ls_knvv WITH KEY kunnr = ls_all-kunnr vkorg = ls_all-vkorg vtweg = ls_all-vtweg spart = ls_all-spart BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-kvgr1 = ls_knvv-kvgr1.
    ENDIF.


*04815    SELECT SINGLE
*04815      kvgr1
*04815    INTO CORRESPONDING FIELDS OF ls_knvv
*04815    FROM knvv
*04815    WHERE knvv~kunnr = ls_all-kunnr
*04815      AND knvv~vkorg = ls_all-vkorg
*04815      AND knvv~vtweg = ls_all-vtweg
*04815    AND knvv~spart = ls_all-spart.
*04815    ls_out-kvgr1 = ls_knvv-kvgr1.
*04815    CLEAR ls_knvv.
*******************E 04815 2022-01-13****************
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
*    SELECT SINGLE
*      name_org1
*      name_org2
*      name_org3
*      name_org4
*    INTO CORRESPONDING FIELDS OF ls_but000
*    FROM but000
*    INNER JOIN vbpa
*    ON but000~partner = vbpa~kunnr
*    WHERE vbpa~parvw = 'Z3'
*      AND vbpa~vbeln = ls_all-vbeln
*      AND vbpa~posnr = ''.
*    ls_out-saleman = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
*    CLEAR ls_but000.

*******************S 04815 2022-01-13****************
*04815    SELECT SINGLE
*04815      name_org1,
*04815      partner
*04815    FROM  but000
*04815    INNER JOIN knvp
*04815    ON knvp~kunn2 = but000~partner
*04815    WHERE knvp~parvw = 'Z3'
*04815      AND knvp~vkorg = @ls_all-vkorg
*04815      AND knvp~spart = @ls_all-spart
*04815      AND knvp~vtweg = @ls_all-vtweg
*04815      AND knvp~kunnr = @ls_all-kunnr
*04815    INTO (@ls_out-saleman,@ls_out-salemanno).
    CLEAR ls_knvp.
    READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_all-kunnr vkorg = ls_all-vkorg vtweg = ls_all-vtweg spart = ls_all-spart BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_knvp-kunn2 BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_out-salemanno = ls_but000-partner.
        ls_out-saleman = ls_but000-name_org1.
      ENDIF.
    ENDIF.
*******************E 04815 2022-01-13****************

    "检索项
*******************S 04815 2022-01-13****************
*04815    SELECT SINGLE
*04815      mc_name1
*04815      name_org1
*04815      name_org2
*04815      name_org3
*04815      name_org4
*04815      bu_sort1
*04815    INTO CORRESPONDING FIELDS OF ls_but000
*04815    FROM but000
*04815    WHERE but000~partner = ls_all-kunnr.
*04815    ls_out-mc_name1 = ls_but000-bu_sort1.

    CLEAR ls_but000.
    READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_all-kunnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-mc_name1 = ls_but000-bu_sort1.
    ENDIF.

*******************E 04815 2022-01-13****************

    "客户名称
    ls_out-kunnr = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.

    "集团名称
    "实际客户名称、实际客户的检索项字段
*******************S 04815 2022-01-13****************

*04815    SELECT SINGLE
*04815    bu_sort1
*04815    partner
*04815    name_org1
*04815    name_org2
*04815    name_org3
*04815    name_org4
*04815    INTO CORRESPONDING FIELDS OF ls_but000
*04815    FROM but000
*04815    INNER JOIN vbpa
*04815    ON but000~partner = vbpa~kunnr
*04815    WHERE vbpa~parvw = 'Z1'
*04815    AND vbpa~vbeln = ls_all-vbeln
*04815    AND vbpa~posnr = ''.
    CLEAR ls_vbpa.
    READ TABLE lt_vbpa INTO ls_vbpa WITH KEY vbeln = ls_all-vbeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR ls_but000.
      READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_vbpa-kunnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_out-bu_sort1 = ls_but000-bu_sort1."实际客户的检索项字段
        ls_out-custname = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
        ls_out-custname2 = ls_but000-partner.
      ENDIF.
    ENDIF.

    "实际客户对应的业务员
*04815    SELECT SINGLE
*04815      kunn2
*04815      name_org1
*04815    INTO (ls_out-partner,ls_out-kunn2)
*04815    FROM but000
*04815    INNER JOIN knvp
*04815    ON but000~partner = knvp~kunn2
*04815    WHERE knvp~kunnr  = ls_but000-partner
*04815      AND knvp~vkorg = ls_all-vkorg
*04815      AND knvp~vtweg = ls_all-vtweg
*04815      AND knvp~spart = ls_all-spart
*04815    AND knvp~parvw = 'Z3'.

    CLEAR ls_knvp.
    READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_but000-partner vkorg = ls_all-vkorg vtweg = ls_all-vtweg spart = ls_all-spart BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR ls_but000.
      READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_knvp-kunn2 BINARY SEARCH.
      ls_out-partner = ls_but000-partner.
      ls_out-kunn2 = ls_but000-name_org1.
    ENDIF.


*******************E 04815 2022-01-13****************


    "实际客户等级
*******************E 04815 2022-01-13****************
*04815    SELECT SINGLE
*04815      kukla
*04815    INTO CORRESPONDING FIELDS OF ls_kna1
*04815    FROM kna1
*04815    INNER JOIN vbpa
*04815    ON vbpa~kunnr = kna1~kunnr
*04815    WHERE vbpa~parvw = 'Z1'
*04815      AND vbpa~vbeln = ls_all-vbeln
*04815    AND vbpa~posnr = ''.
    CLEAR ls_kna1.
    READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_vbpa-kunnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-kukla = ls_kna1-kukla.
    ENDIF.


*******************E 04815 2022-01-13****************
    IF ls_out-kukla IS NOT INITIAL.
*******************S 04815 2022-01-13****************
*04815      SELECT SINGLE
*04815        vtext
*04815      INTO ls_out-kukla
*04815      FROM tkukt
*04815      WHERE tkukt~spras = '1'
*04815      AND tkukt~kukla = ls_out-kukla.
      CLEAR ls_tkukt.
      READ TABLE lt_tkukt INTO ls_tkukt WITH KEY kukla = ls_out-kukla BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_out-kukla = ls_tkukt-vtext.
      ENDIF.

*******************E 04815 2022-01-13****************
    ENDIF.


    "客户PO、付款方式、汇率
    CLEAR ls_vbkd.
*******************S 04815 2022-01-13****************
*04815    "S4DK904793-start
*04815    SELECT SINGLE
*04815      *
*04815    INTO CORRESPONDING FIELDS OF ls_vbkd
*04815    FROM vbkd
*04815    WHERE vbkd~posnr = ls_all-posnr
*04815    AND vbkd~vbeln = ls_all-vbeln.
*04815
*04815    IF ls_vbkd IS INITIAL.
*04815      SELECT SINGLE
*04815      *
*04815      INTO CORRESPONDING FIELDS OF ls_vbkd
*04815      FROM vbkd
*04815      WHERE vbkd~posnr = ''
*04815      AND vbkd~vbeln = ls_all-vbeln.
*04815    ENDIF.
    READ TABLE lt_vbkd INTO ls_vbkd WITH KEY vbeln = ls_all-vbeln posnr = ls_all-posnr BINARY SEARCH.
    IF sy-subrc NE 0.
      READ TABLE lt_vbkd INTO ls_vbkd WITH KEY vbeln = ls_all-vbeln BINARY SEARCH.
    ENDIF.

    IF ls_vbkd IS NOT INITIAL.
* 汇率 modify by jiguican 20210121

      gdatu = ls_vbkd-prsdt.
      gdatu = '99999999' - gdatu.
      IF ls_out-waerk <> 'CNY'.

        IF ls_vbkd-kurrf IS NOT INITIAL.
          ls_out-kursk = ls_vbkd-kurrf.
        ELSE.
          ls_out-kursk = ls_vbkd-kursk.
        ENDIF.
        IF ls_out-kursk = 1.
          LOOP AT lt_tcurr INTO ls_tcurr WHERE fcurr = ls_out-waerk AND gdatu >= gdatu.
            ls_out-kursk = ls_tcurr-ukurs.
            EXIT.
          ENDLOOP.
        ENDIF.

      ELSE.
        ls_out-kursk = 1.
      ENDIF.
      "S4DK904793-end

      "付款方式
      "add S4DK904838 start
      IF ls_vbkd-zterm IS NOT INITIAL.
*04815        SELECT SINGLE
*04815          text1
*04815        INTO ls_out-zterm
*04815        FROM t052u
*04815        WHERE t052u~spras = '1'
*04815        AND t052u~zterm = ls_vbkd-zterm.
        CLEAR ls_t052u.
        READ TABLE lt_t052u INTO ls_t052u WITH KEY zterm = ls_vbkd-zterm BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_out-zterm = ls_t052u-text1.
        ENDIF.


      ENDIF.

      "add S4DK904838 end

      "客户PO
      ls_out-bstkd_e = ls_vbkd-bstkd_e.
      ls_out-posex_e = ls_vbkd-posex_e.
      "客户采购日期
      ls_out-bstdk_e = ls_vbkd-bstdk_e.
    ENDIF.



*******************E 04815 2022-01-13****************
    "不含税金额CNY
    ls_out-price2 = ls_out-netwr * ls_out-kursk."不含税金额CNY


*******************S 04815 2022-01-13****************
    " 定价参考物料编码
*04815   SELECT SINGLE
*04815     pmatn
*04815   INTO CORRESPONDING FIELDS OF ls_mvke
*04815   FROM mvke
*04815   WHERE mvke~matnr = ls_all-matnr
*04815     AND mvke~vkorg = ls_all-vkorg
*04815     AND mvke~vtweg = ls_all-vtweg.
    CLEAR ls_mvke.
    READ TABLE lt_mvke INTO ls_mvke WITH KEY matnr = ls_all-matnr vkorg = ls_all-vkorg vtweg = ls_all-vtweg BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-pmatn = zcl_bc_public=>conv_by_ddic( i_input = ls_mvke-pmatn i_out = 'X' ).

      "定价参考物料描述
      IF ls_mvke-pmatn IS NOT INITIAL.

        CLEAR ls_makt.
        READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_mvke-pmatn BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_out-maktx = ls_makt-maktx.
        ENDIF.
      ENDIF.

    ENDIF.

*******************E 04815 2022-01-13****************
    CLEAR ls_all2-matnr90.
    CLEAR ls_out-zappsou.
*    ls_all2-matnr90 = zcl_bc_public=>conv_by_ddic( i_input = ls_out-pmatn ).
    CLEAR ls_out-zappsou.
    IF ls_all2-matnr90 IS NOT INITIAL.
*******************S 04815 2022-01-14****************
*04815      SELECT SINGLE
*04815      atwrt
*04815      INTO ls_out-zappsou
*04815      FROM  ausp
*04815      WHERE ausp~atinn  = lv_zappsou
*04815      AND ausp~objek = ls_all2-matnr90.
      "批文来源
      CLEAR ls_ausp.
      READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all2-matnr90 atinn = lv_zappsou BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR ls_cawn_t.
        READ TABLE lt_cawn_t INTO ls_cawn_t WITH KEY atinn = lv_zappsou atwrt = ls_ausp-atwrt BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_out-zappsou = ls_cawn_t-atwtb.
        ENDIF.
      ENDIF.

      "产品类型
      CLEAR ls_ausp.
      READ TABLE lt_ausp INTO ls_ausp WITH KEY objek = ls_all2-matnr90 atinn = lv_zprotype BINARY SEARCH.
      IF sy-subrc EQ 0.
        CLEAR ls_cawn_t.
        READ TABLE lt_cawn_t INTO ls_cawn_t WITH KEY atinn = lv_zprotype atwrt = ls_ausp-atwrt BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_out-zprotype = ls_cawn_t-atwtb.
        ENDIF.
      ENDIF.
    ENDIF.

    "最早入库日期
    READ TABLE lt_zppt0003 INTO DATA(ls_zppt0003) WITH KEY kdauf = ls_all-vbeln
                                                           kdpos = ls_all-posnr.
    IF sy-subrc = 0.
      ls_out-firdat = ls_zppt0003-erdat.
    ENDIF.



*04815   IF ls_out-zappsou IS NOT INITIAL.
*04815     SELECT SINGLE
*04815       atwtb
*04815     INTO ls_out-zappsou
*04815     FROM cawnt
*04815     INNER JOIN cawn
*04815     ON cawn~atinn = cawnt~atinn
*04815     AND cawn~atzhl = cawnt~atzhl
*04815     WHERE cawnt~spras = '1'
*04815       AND cawn~atinn = lv_zappsou
*04815       AND cawn~atwrt = ls_out-zappsou2.
*04815   ENDIF.



    "产品类型
*04815    CLEAR ls_out-zprotype.
*04815
*04815    IF ls_all2-matnr90 IS NOT INITIAL.
*04815      SELECT SINGLE
*04815      atwrt
*04815      INTO ls_out-zprotype
*04815      FROM  ausp
*04815      WHERE ausp~atinn  = lv_zprotype
*04815      AND ausp~objek = ls_all2-matnr90.
*04815      ls_out-zprotype2 = ls_out-zprotype.
*04815      CLEAR ls_out-zprotype.
*04815    ENDIF.
*04815
*04815    IF ls_out-zprotype2 IS NOT INITIAL.
*04815      SELECT SINGLE
*04815      atwtb
*04815      INTO ls_out-zprotype
*04815      FROM cawnt
*04815      INNER JOIN cawn
*04815      ON cawn~atinn = cawnt~atinn
*04815      AND cawn~atzhl = cawnt~atzhl
*04815      WHERE cawnt~spras = '1'
*04815      AND cawn~atinn = lv_zprotype
*04815      AND cawn~atwrt = ls_out-zprotype2.
*04815    ENDIF.

*******************E 04815 2022-01-14****************


    "物料小类编码
    CLEAR ls_mara.
    READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_all-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-matkl = ls_mara-matkl+0(4).
    ENDIF.

*04815    SELECT SINGLE
*04815      matkl
*04815    INTO CORRESPONDING FIELDS OF ls_mara
*04815    FROM mara
*04815    WHERE mara~matnr = ls_all-matnr.


    "物料小类名称
    CLEAR ls_makt.
*04815    SELECT SINGLE
*04815      wgbez
*04815    INTO ls_out-maktxs
*04815    FROM t023t
*04815    WHERE t023t~matkl = ls_out-matkl
*04815    AND t023t~spras = '1'.
    READ TABLE lt_t023t INTO ls_t023t WITH KEY matkl = ls_out-matkl BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-maktxs = ls_t023t-wgbez.
    ENDIF.


    "基本计量数量
    IF ls_all-umzin IS NOT INITIAL.
      ls_out-number = ( ls_all-kwmeng * ( ls_all-umziz / ls_all-umzin ) ) + ( ls_all-zmeng * ( ls_all-umziz / ls_all-umzin ) ).
    ENDIF.

    CLEAR ls_prcd_elements.

    "销售转换率
    ls_out-rate = ls_all-umziz / ls_all-umzin.

    CLEAR ls_prcd_elements.
    SELECT
          SUM( kbetr ) AS kbetr "含税单价
          SUM( kwert ) AS kwert"含税金额
          SUM( prcd_elements~kpein ) AS kpein "定价单位
        INTO CORRESPONDING FIELDS OF ls_prcd_elements
        FROM prcd_elements
        WHERE knumv = ls_all-knumv
        AND kschl IN ( 'ZP00','ZP02','ZP04','Z001','Z002','Z005','Z006' )
        AND kinak = ''
        AND kposn = ls_all-posnr.
    "含税单价
    ls_out-kbetr = ls_prcd_elements-kbetr.

    "定价单位
    ls_out-kpein = ls_prcd_elements-kpein.

    "折扣率
    IF ls_prcd_elements-kwert IS NOT INITIAL.
      "折扣金额
      ls_out-mwsbp2 = ls_prcd_elements-kwert - ls_all-netwr - ls_all-mwsbp.

      "折扣率
      IF ls_all-kwmeng IS NOT INITIAL.
        ls_out-kwert = ls_out-mwsbp2 / ls_prcd_elements-kwert * 100.
      ENDIF.

    ENDIF.

    "税率
    CLEAR ls_prcd_elements.
    READ TABLE lt_prcd_elements INTO ls_prcd_elements WITH KEY knumv = ls_all-knumv kposn = ls_all-posnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-kwertdis = ls_prcd_elements-kbetr .
    ENDIF.

*04815    "含税单价
*04815    SELECT
*04815          SUM( kbetr ) AS kbetr "含税单价
*04815          SUM( kwert ) AS kwert"含税金额
*04815          SUM( prcd_elements~kpein ) AS kpein "定价单位
*04815        INTO CORRESPONDING FIELDS OF ls_prcd_elements
*04815        FROM vbap
*04815    INNER JOIN vbak
*04815    ON vbap~vbeln = vbak~vbeln
*04815    INNER JOIN prcd_elements
*04815        ON vbak~knumv = prcd_elements~knumv AND vbap~posnr = prcd_elements~kposn
*04815        WHERE vbak~vbeln = ls_all-vbeln
*04815        AND prcd_elements~kschl IN ( 'ZP00','ZP02','ZP04','Z001','Z002','Z005','Z006' )"修改定价过程的逻辑 modify by 202210206 纪桂灿
*04815        AND prcd_elements~kinak = ''
*04815        AND vbap~posnr = ls_all-posnr.
*04815    ls_out-kbetr = ls_prcd_elements-kbetr.
*04815    "定价单位
*04815    ls_out-kpein = ls_prcd_elements-kpein.
*04815    "折扣金额
*04815    ls_out-mwsbp2 = ls_prcd_elements-kwert - ls_all-netwr - ls_all-mwsbp.
*04815    "折扣率
*04815    IF ls_prcd_elements-kwert IS NOT INITIAL AND ls_all-kwmeng IS NOT INITIAL.
*04815      ls_out-kwert = ls_out-mwsbp2 / ls_prcd_elements-kwert * 100.
*04815    ENDIF.

*04815    "税率
*04815    CLEAR ls_prcd_elements.
*04815    SELECT SINGLE
*04815      kbetr
*04815    INTO CORRESPONDING FIELDS OF ls_prcd_elements
*04815    FROM prcd_elements
*04815    INNER JOIN vbak
*04815    ON vbak~knumv = prcd_elements~knumv
*04815    WHERE vbak~vbeln = ls_all-vbeln
*04815    AND prcd_elements~kschl = 'MWSI'
*04815    AND prcd_elements~kposn = ls_out-posnr.
*04815    ls_out-kwertdis = ls_prcd_elements-kbetr .

    "价格合计
    ls_out-priceall = ls_all-netwr + ls_all-mwsbp.

    "入库量（当前库存）
    CLEAR ls_mska.
*    CLEAR lt_mska.
    CLEAR ls_out-innum.
*04815    DATA:lv_value1 TYPE mska-kalab,
*04815         lv_value2 TYPE mska-kains.
*04815    CLEAR lv_value1.
*04815    CLEAR lv_value2.
*04815    SELECT
*04815    kalab
*04815    kains
*04815    kaspe
*04815    kavla
*04815    kavin
*04815    kavsp
*04815    INTO CORRESPONDING FIELDS OF TABLE lt_mska
*04815    FROM mska
*04815    WHERE mska~vbeln = ls_all-vbeln
*04815    AND mska~posnr = ls_all-posnr
*04815    AND mska~matnr = ls_all-matnr
*04815    AND mska~lgort IN ('1040','2040','3040','1070','2070','3070').
*04815    LOOP AT lt_mska INTO ls_mska.
*04815*    ls_out-innum = ls_mska-kalab + ls_mska-kains + ls_mska-kaspe + ls_mska-kavla + ls_mska-kavin + ls_mska-kavsp.
*04815      lv_value1 = lv_value1 + ls_mska-kalab.
*04815      lv_value2 = lv_value2 + ls_mska-kains + ls_mska-kaspe + ls_mska-kavla + ls_mska-kavin + ls_mska-kavsp.
*04815      CLEAR ls_mska.
*04815    ENDLOOP.
    CLEAR ls_mska.
    READ TABLE lt_mska INTO ls_mska WITH KEY vbeln = ls_all-vbeln posnr = ls_all-posnr matnr = ls_all-matnr  BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-innum = ls_mska-kalab.
      ls_out-innum2 = ls_mska-kalab1.
    ENDIF.

    IF ls_out-number IS NOT INITIAL AND ls_out-kwmeng IS NOT INITIAL.
      ls_out-innum = ls_out-innum / ( ls_out-number / ls_out-kwmeng  ).
      ls_out-innum2 = ls_out-innum2 / ( ls_out-number / ls_out-kwmeng  ).
    ENDIF.

*04815    CLEAR lt_vbfa.
*04815    "出库量
*04815    SELECT
*04815      rfmng
*04815      vbtyp_n
*04815    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
*04815    FROM vbfa
*04815    WHERE vbfa~vbelv = ls_all-vbeln
*04815      AND vbfa~posnv = ls_all-posnr
*04815    AND vbfa~vbtyp_n IN ( 'R','h' ).
*04815    LOOP AT lt_vbfa INTO ls_vbfa.
*04815      IF ls_vbfa-vbtyp_n = 'R'.
*04815        ls_out-outnum = ls_out-outnum + ls_vbfa-rfmng.
*04815      ELSEIF ls_vbfa-vbtyp_n = 'h'.
*04815        ls_out-outnum = ls_out-outnum - ls_vbfa-rfmng.
*04815      ENDIF.
*04815      CLEAR ls_vbfa.
*04815    ENDLOOP.

    "出库量
    CLEAR ls_vbfa_out.
    READ TABLE lt_vbfa_out INTO ls_vbfa_out WITH KEY vbelv = ls_all-vbeln posnv = ls_all-posnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-outnum = ls_vbfa_out-rfmng.
    ENDIF.


    IF ls_out-number IS NOT INITIAL AND ls_out-kwmeng IS NOT INITIAL.
      ls_out-outnum = ls_out-outnum /  ( ls_out-number / ls_out-kwmeng  ).
    ENDIF.

*04815    CLEAR lt_vbfa.

    "首个交货日期
*04815    SELECT
*04815      vbeln
*04815    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
*04815    FROM vbfa
*04815    WHERE vbfa~vbelv = ls_all-vbeln
*04815    AND vbfa~posnv = ls_all-posnr
*04815    AND vbfa~vbtyp_n = 'J'
*04815    AND vbfa~rfmng > 0
*04815    ORDER BY vbeln.
*04815    READ TABLE lt_vbfa INTO ls_vbfa INDEX 1.
*04815    CLEAR ls_likp.
*04815    SELECT SINGLE
*04815      wadat_ist
*04815    INTO CORRESPONDING FIELDS OF ls_likp
*04815    FROM likp
*04815    WHERE likp~vbeln = ls_vbfa-vbeln.
*04815    ls_out-wadat_ist = ls_likp-wadat_ist.
*04815    CLEAR lt_vbfa.
*04815    CLEAR ls_vbfa.
    CLEAR ls_vbfa_jhrq.
    READ TABLE lt_vbfa_jhrq INTO ls_vbfa_jhrq WITH KEY vbelv = ls_all-vbeln posnv = ls_all-posnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      "找到交货单号，根据交货单号读取likp表取过账日期
      CLEAR ls_likp.
      READ TABLE lt_likp INTO ls_likp WITH KEY vbeln = ls_vbfa_jhrq-vbeln BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_out-wadat_ist = ls_likp-wadat_ist.
      ENDIF.
    ENDIF.


    "开票量
*04815    SELECT
*04815    rfmng
*04815    vbtyp_n
*04815    erdat
*04815    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
*04815    FROM vbfa
*04815    WHERE vbfa~vbelv = ls_all-vbeln
*04815    AND vbfa~posnv = ls_all-posnr
*04815    AND vbfa~vbtyp_n IN ( 'M','N','O','S' )
*04815    ORDER BY erdat DESCENDING.
*04815    LOOP AT lt_vbfa INTO ls_vbfa.
*04815*      ls_out-invoice = ls_out-invoice + ls_vbfa-rfmng.
*04815      IF ls_vbfa-vbtyp_n = 'M' OR ls_vbfa-vbtyp_n = 'O'.
*04815        ls_out-invoice = ls_out-invoice + ls_vbfa-rfmng.
*04815      ELSEIF ls_vbfa-vbtyp_n = 'N' OR ls_vbfa-vbtyp_n = 'S'.
*04815        ls_out-invoice = ls_out-invoice - ls_vbfa-rfmng.
*04815      ENDIF.
*04815      CLEAR ls_vbfa.
*04815    ENDLOOP.
    CLEAR ls_vbfa_invoice.
    READ TABLE lt_vbfa_invoice INTO ls_vbfa_invoice WITH KEY vbelv = ls_all-vbeln posnv = ls_all-posnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-invoice = ls_vbfa_invoice-rfmng.
    ENDIF.

    IF ls_out-number IS NOT INITIAL AND ls_out-kwmeng IS NOT INITIAL AND ( ls_out-number / ls_out-kwmeng  ) <> 0.
      ls_out-invoice = ls_out-invoice /  ( ls_out-number / ls_out-kwmeng  ).
    ENDIF.


*    "开票日期
*    LOOP AT lt_vbfa INTO ls_vbfa.
*      ls_out-invoiced = ls_vbfa-erdat.
*      CHECK ls_out-invoiced IS NOT INITIAL.
*    ENDLOOP.




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
    CLEAR ls_tvakt.
    READ TABLE lt_tvakt INTO ls_tvakt WITH KEY auart = ls_out-auart BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-auart = ls_tvakt-bezei.
    ENDIF.

    "分销渠道取描述
    CLEAR ls_tvtwt.
    READ TABLE lt_tvtwt INTO ls_tvtwt WITH KEY vtweg = ls_out-vtweg BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-vtweg = ls_tvtwt-vtext.
    ENDIF.

    "销售渠道取描述
    CLEAR ls_tvv1t.
    READ TABLE lt_tvv1t INTO ls_tvv1t WITH KEY kvgr1 = ls_out-kvgr1 BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-kvgr1 = ls_tvv1t-bezei.
    ENDIF.

    "出口国家取描述
    CLEAR ls_t005t.
    READ TABLE lt_t005t INTO ls_t005t WITH KEY land1 = ls_out-zckgj BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-zckgj2 = ls_t005t-landx.
    ENDIF.

    "二级部门取描述
    CLEAR ls_tvkbt.
    READ TABLE lt_tvkbt INTO ls_tvkbt WITH KEY vkbur = ls_out-vkbur BINARY SEARCH.
    IF sy-subrc EQ 0.
      ls_out-vkbur = ls_tvkbt-bezei.
    ENDIF.

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
*04815    CLEAR lt_dd07v.
*04815    SELECT
*04815      ddtext
*04815      valpos
*04815      domname
*04815      domvalue_l
*04815    INTO CORRESPONDING FIELDS OF TABLE lt_dd07v
*04815    FROM dd07v
*04815      WHERE dd07v~ddlanguage = '1'
*04815    AND dd07v~domname IN ( 'ZD_DYTK','ZD_SFXBZ','ZD_XFLAG','ZD_ZDSCQY','ZD_PHYQ','ZD_BHCL','ZBLDLX' ). "S4DK905471
    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_BHCL' domvalue_l = ls_out-zbhcl BINARY SEARCH."备货策略
    IF sy-subrc EQ 0.
      ls_out-zbhcl = ls_dd07v-ddtext.
    ENDIF.

    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_SFXBZ' domvalue_l = ls_out-zsfxbz BINARY SEARCH."是否新包装
    IF sy-subrc EQ 0.
      ls_out-zsfxbz = ls_dd07v-ddtext.
    ENDIF.

    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_DYTK' domvalue_l = ls_out-zdytk BINARY SEARCH."短溢条款
    IF sy-subrc EQ 0.
      ls_out-zdytk = ls_dd07v-ddtext.
    ENDIF.

    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_PHYQ' domvalue_l = ls_all-zphyq BINARY SEARCH."批号要求
    IF sy-subrc EQ 0.
      ls_out-zphyq = ls_dd07v-ddtext.
    ENDIF.

    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_XFLAG' domvalue_l = ls_all-zsfbj BINARY SEARCH."批号要求
    IF sy-subrc EQ 0.
      ls_out-zsfbj = ls_dd07v-ddtext.
    ENDIF.

    CLEAR ls_dd07v.
    READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZBLDLX' domvalue_l = ls_all-zbldlx BINARY SEARCH."备料单类型
    IF sy-subrc EQ 0.
      ls_out-zbldlx = ls_dd07v-ddtext.
    ENDIF.


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
*     判断制造工厂
    CASE ls_out-zleix.
      WHEN 'S010' OR 'S030' OR 'S030' OR 'S050' OR 'S070' OR 'S100' OR 'S120' OR 'S130' OR 'S150' OR 'S190' OR 'S210' OR 'S280'.
        ls_out-werks2 = '8100'.
      WHEN :'S020' OR 'S040' OR 'S060' OR 'S080' OR 'S090' OR 'S110' OR 'S140' OR 'S160' OR 'S170' OR 'S180' OR 'S200'.
        ls_out-werks2 = '8200'.
      WHEN 'S220' OR 'S240' OR 'S270'.
        ls_out-werks2 = '8100'.
      WHEN 'S230' OR '250' OR 'S260'.
        ls_out-werks2 = '8200'.
      WHEN OTHERS.
        ls_out-werks2 = ls_out-werks.
    ENDCASE.



    IF flag = 'X' AND flag_b = 'X'.
      "获取对应的物料清单
      CLEAR ls_mast.
      READ TABLE lt_mast INTO ls_mast WITH KEY matnr = ls_all-matnr werks = ls_all-werks BINARY SEARCH.
      IF sy-subrc EQ 0.
        "BOM的基本数量
        CLEAR ls_stko.
        READ TABLE lt_stko INTO ls_stko WITH KEY stlnr = ls_mast-stlnr BINARY SEARCH.

      ENDIF.
*04815      SELECT SINGLE
*04815      stlnr
*04815      INTO CORRESPONDING FIELDS OF ls_mast
*04815      FROM mast AS a
*04815      WHERE a~matnr = ls_all-matnr
*04815      AND a~werks = ls_all-werks
*04815      AND a~stlan = '1'
*04815      AND a~stlal = '01'.


*04815      CLEAR ls_stko.
*04815      SELECT SINGLE
*04815      bmeng
*04815      INTO CORRESPONDING FIELDS OF ls_stko
*04815      FROM stko AS a
*04815      WHERE a~stlty = 'M'
*04815      AND a~stlal = '01'
*04815      AND a~stlnr = ls_mast-stlnr.

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
*04815        SELECT SINGLE
*04815          mtart
*04815          meins
*04815        INTO CORRESPONDING FIELDS OF ls_mara
*04815        FROM mara
*04815        WHERE mara~matnr = ls_stb-idnrk.
        READ TABLE lt_mara INTO ls_mara WITH KEY matnr = ls_stb-idnrk BINARY SEARCH.
        IF sy-subrc EQ 0.
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
*               OUTPUT         =
*               SHORT_TEXT     =
              EXCEPTIONS
                unit_not_found = 1
                OTHERS         = 2.
            IF sy-subrc <> 0.
*   Implement suitable error handling here
            ENDIF.
*            ls_out-zmeins = ls_mara-meins.
            "子件的描述
            CLEAR ls_makt.
*  04815          SELECT SINGLE
*  04815          maktx
*  04815          INTO ls_out-zsondetail
*  04815          FROM makt
*  04815          WHERE makt~matnr = ls_stb-idnrk AND makt~spras = '1'."ADD S4DK904526
            READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_stb-idnrk.
            IF sy-subrc EQ 0.
              ls_out-zsondetail = ls_makt-maktx.
            ENDIF.

            "礼合装子件销售数量
            ls_out-zsum = ( ls_stb-menge / ls_stko-bmeng ) * ls_out-number.
            "礼合装子件基本数量
            ls_out-zsum2 =  ( ls_stb-menge / ls_stko-bmeng ) * ls_out-kwmeng.

            APPEND ls_out TO lt_out." 输出BOM子件的行数
          ENDIF.
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

  DESCRIBE TABLE lt_all LINES ls_ysdt0001-zcount.

  GET TIME.

  ls_ysdt0001-zuzeit = sy-uzeit.
  ls_ysdt0001-zsecond = ls_ysdt0001-zuzeit - ls_ysdt0001-uzeit.
  APPEND ls_ysdt0001 TO lt_ysdt0001.

  MODIFY ysdt0001 FROM TABLE lt_ysdt0001.

  SELECT
    *
  FROM @lt_out AS a
  WHERE a~udate IN @s_udate
    AND a~udate1 IN @s_udate1
    AND a~partner IN @s_kunn2
  INTO TABLE @lt_out.
  SORT lt_out BY vbeln posnr.
ENDFORM.



FORM catalog.
  w_repid = sy-repid.
  CLEAR fieldcat.
  CLEAR sy-tabix.
  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 12."自定义字段的长度
*    fieldcat-ref_tabname = 'BKPF'."调用透明表的数据结构
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-coltext = &2."ALV列名
    fieldcat-col_pos = &3."列位置

     IF &1 = 'VBELN'.
       fieldcat-hotspot = 'X' ."鼠标热点事件
       fieldcat-key = 'X'."定义为主键（颜色改变）
       ELSEIF &1 = 'VBEZZ' OR &1 = 'ZYSDJ' .
      fieldcat-hotspot = 'X' .

     ELSE.
       fieldcat-hotspot = '' ."鼠标热点事件
       fieldcat-key = ''."定义为主键（颜色改变）
    ENDIF .

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
  fieldcatset 'VTWEG'   '订单分销渠道Area' sy-tabix.
  fieldcatset 'VTWEG_N' '归属分销渠道Area' sy-tabix.
  fieldcatset 'VKBUR'   '二级部门' sy-tabix.
  fieldcatset 'AUART'   '订单类型' sy-tabix.
  fieldcatset 'MC_NAME1'   '检索项' sy-tabix.
  fieldcatset 'KUNNR'   '售达方(客户名称)Customer' sy-tabix.
  fieldcatset 'KUNNR2'   '售达方(编号)' sy-tabix.
  fieldcatset 'SALEMAN'   '业务员Sales' sy-tabix.
  fieldcatset 'SALEMANNO'   '业务员编号' sy-tabix.
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
  fieldcatset 'ZBLDLX'   '备料单类型' sy-tabix.                  "S4DK905471
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
  fieldcatset 'PARTNER'      '实际客户的业务员编号'    sy-tabix.
  fieldcatset 'KUNN2'      '实际客户的业务员'    sy-tabix.
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
  fieldcatset 'RATE' '销售转换率' sy-tabix.
  fieldcatset 'ZYSDJ' '原始单据' sy-tabix.
  fieldcatset 'ZYSDH' '原始单据行' sy-tabix.
  fieldcatset 'VBEZZ' '最终单据' sy-tabix.
  fieldcatset 'POSZZ' '最终单据行' sy-tabix.
  fieldcatset 'ZATZZ' '最终ATP答复日期' sy-tabix.
  fieldcatset 'ZAPZZ' '最终APS状态' sy-tabix.
  fieldcatset 'ETTZZ' '最终计划行状态' sy-tabix.
  fieldcatset 'ZLEIX' '多方交易类型' sy-tabix.
  fieldcatset 'ZLEIT' '多方交易描述' sy-tabix.
  fieldcatset 'FIRDAT' '最早入库日期' sy-tabix.

  fieldcatset 'WERKS2' '制造工厂' sy-tabix.

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

  AUTHORITY-CHECK OBJECT 'ZFI_ZPLP1'
 ID 'ZSHOW' FIELD 'X'.
  IF sy-subrc = 0.
    fieldcatset 'ZPLP1' '计划价格1' sy-tabix.
  ENDIF.

  layout-box_fname = 'sel'.

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
      CASE rs_selfield-fieldname .
        WHEN 'VBEZZ' .
          SET PARAMETER ID 'AUN' FIELD ls_out-vbezz.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        WHEN 'ZYSDJ' .
          SET PARAMETER ID 'AUN' FIELD ls_out-zysdj.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        WHEN 'VBELN'.
          SET PARAMETER ID 'AUN' FIELD ls_out-vbeln.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDCASE.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZDF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ALL
*&---------------------------------------------------------------------*
FORM sub_get_zdf  TABLES   pt_all STRUCTURE gs_all .

  DATA : lt_all TYPE TABLE OF ty_all .
  DATA : lt_zsdt0025 TYPE TABLE OF zsdt0025 .

  "获取原始单号
  lt_all[] = pt_all[] .
  SORT lt_all BY zysdh .
  DELETE ADJACENT DUPLICATES FROM lt_all
                   COMPARING zysdj .
  DELETE lt_all WHERE zysdj IS INITIAL .

  CHECK lt_all[] IS NOT INITIAL .

  "根据原始单号获取最终单号
  SELECT *
    INTO TABLE gt_zsdt0025
    FROM zsdt0025
    FOR ALL ENTRIES IN lt_all
    WHERE zdjlx = 'SO'
      AND zysdj = lt_all-zysdj
      AND zjied = '2' .

  CHECK gt_zsdt0025[] IS NOT INITIAL .

  "获取最终单据信息
  lt_zsdt0025[] = gt_zsdt0025[] .
  SORT lt_zsdt0025 BY vbeln .
  DELETE ADJACENT DUPLICATES FROM lt_zsdt0025
                    COMPARING vbeln .


  SELECT
  vbap~vbeln
  vbap~posnr
  vbap~zatpdfrq
  vbap~zapszt
  vbep~ettyp
  vbap~ernam
  INTO TABLE gt_vbapz
  FROM vbap
  LEFT JOIN vbep
  ON vbap~vbeln = vbep~vbeln
 AND vbap~posnr = vbep~posnr
 AND vbep~etenr = '0001'
   FOR ALL ENTRIES IN lt_zsdt0025
  WHERE vbap~vbeln = lt_zsdt0025-vbeln  .

ENDFORM.