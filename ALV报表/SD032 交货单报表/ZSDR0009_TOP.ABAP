*&---------------------------------------------------------------------*
*& 包含               ZSD0009_TOP
*&---------------------------------------------------------------------*
TABLES: likp,vbak,lips,kna1,adrp,vbpa,usr21.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
" 选择屏幕所需要的字段列表
TYPES:BEGIN OF lv_select,
        pvbeln  TYPE    likp-vbeln  , "  交货单
        pvkorg  TYPE    likp-vkorg  , "  销售组织
        pvbeln2 TYPE    vbak-vbeln  , "  销售订单
        pwerks  TYPE    lips-werks  , "  工厂
        puser   TYPE    adrp-name_text  , "  制单人
        pkunnr  TYPE    vbpa-kunnr  , "  业务员
        pcustom TYPE    vbpa-kunnr  , "  客户
        psortl  TYPE    kna1-sortl  , "  检索项
        perdat  TYPE    likp-erdat  , "  创建日期
        pzfhrq  TYPE    likp-zfhrq  , "  发货日期
        pwadat  TYPE    likp-wadat_ist  , "  过账日期
        pmatnr  TYPE    lips-matnr  , "  产品代号
        pwbsta  TYPE    lips-wbsta  , "  交货状态
      END OF lv_select.
"alv界面所需要的字段列表
TYPES:BEGIN OF lv_out,
        vbeln           TYPE    likp-vbeln  , "  交货单
        posnr           TYPE    lips-posnr  , "  行项目
        vgbel           TYPE    lips-vgbel  , "  销售订单
        vgpos           TYPE    lips-vgpos  , "  行项目
        vkorg           TYPE    likp-vkorg  , "  销售组织
        werks           TYPE    lips-werks  , "  工厂
        lgort           TYPE    lips-lgort  , "  库存地址
        erdat           TYPE    likp-erdat  , "  创建日期
        zfhrq           TYPE    likp-lfdat  , "  发货日期
        wadat           TYPE    likp-wadat_ist  , "  过账日期
        matnr           TYPE    lips-matnr  , "  产品代号
        arktx           TYPE    lips-arktx  , "  产品名称
        lfimg           TYPE    lips-lfimg  , "  销售数量
        vrkme           TYPE    lips-vrkme  , "  销售单位
        zjians          TYPE    lips-zjians , "  件数
        zlxsl           TYPE    lips-zlxsl  , "  零箱数量
        zlfimg          TYPE    lips-lfimg  , "  实发销售数量
        meins           TYPE    lips-meins  , "  基本计量单位
        wbsta           TYPE    string  , "  交货单状态
        charg           TYPE    lips-charg  , "  批量流水
        baxssl          TYPE    lips-lfimg , "  基本计量数量
        zfhqd           TYPE    string  , "  是否特殊发货清单
        zshcoa          TYPE    string , "  是否需要随货COA
        zfhsx           TYPE    lips-zfhsx  , "  发货顺序
        zwmszt          TYPE    string , "  是否传输WMS
        zuser           TYPE    but000-name_org1  , "  业务员
        bu_sort1        TYPE but000-bu_sort1, "实际客户的检索项
        zcustom         TYPE    but000-name_org1  , "  客户
        zruserno        TYPE vbpa-kunnr, "实际客户编号
        zruser          TYPE    but000-name_org1  , "  实际客户
        zmakeuser       TYPE    adrp-name_text  , "  制单人
        sortl           TYPE    kna1-sortl  , "  检索项
        kukla           TYPE    kna1-kukla  , "  实际客户等级
        bezei           TYPE    tvv1t-bezei , "  销售渠道/客户类型
        pmatn           TYPE    mvke-pmatn  , "  定价参考物料
        maktx           TYPE    makt-maktx  , "  定价参考物料描述
        zstdname(30)    TYPE    c , "  标准名称
        groes           TYPE    mara-groes  , "  规格型号
        matkl           TYPE    mara-matkl  , "  物料小类编号
        wgbez           TYPE    t023t-wgbez , "  物料小类描述
        zpspe(40)       TYPE    c , "  包装规格
        zsfxcp          TYPE    string , "  新产品
        zypsl           TYPE    vbap-zypsl  , "  样品数量
        zhsdat          TYPE    likp-wadat_ist , "  生产日期
*待定    TYPE    C ,"  保质期
*待定    TYPE    C ,"  失效日期
*        svbeln          TYPE    vbak-vbeln  , "  销售订单号
        bstkd_e         TYPE    vbkd-bstkd_e  , "  客户合同单号
        kpsum           TYPE    vbfa-rfmng , "  开票数量
        ddpprice        TYPE    p DECIMALS 2, "  DDP价格
        perprice        TYPE    vbap-mwsbp , "  含税单价
        disclv(12)      TYPE    c , "  折扣率
        discnum         TYPE    vbap-mwsbp , "  折扣金额
        kbetr           TYPE    p DECIMALS 2 , "  税率
        mwsbp           TYPE    vbap-mwsbp  , "  销项税额
        price           TYPE    vbap-netwr  , "  不含税金额
        price2          TYPE    vbap-netwr  , "  不含税金额
        netwr           TYPE    vbap-netwr , "  价税总计
        waerk           TYPE    vbak-waerk  , "  订单币别
        ernam           TYPE likp-ernam, "制单人
        zckgj           TYPE string, "出口国家
        zckgjt          TYPE string, "出口国家
        zpack_charg(40) TYPE    c , "  打印批号
        zcustomertext   TYPE string, "客户描述
        z004(40)        TYPE    c , "  备注
        vfdat           TYPE mcha-vfdat, "货架到期日
        kursk           TYPE vbkd-kursk, "汇率
        zzqzt           TYPE string, "账期审批状态
        cmgst           TYPE string, "贷方
        sdabw           TYPE string, "特殊处理标
        zkhwl           TYPE string, "客户物料编码
      END OF lv_out.
TYPES:BEGIN OF lv_out2.
        INCLUDE TYPE lv_out.
        TYPES:zson(20)       TYPE c, "子件
        zsondetail(20) TYPE c, "子件描述
        zmeins         TYPE mara-meins, "子件的单位
        zsum(20)       TYPE c, "铝盒装子件销售数量
      END OF lv_out2.
TYPES:BEGIN OF lv_main.

TYPES:
  vbeln_likp  TYPE    likp-vbeln  , "  交货单
  posnr_lips  TYPE    lips-posnr  , "  行项目
  vgbel       TYPE    lips-vgbel  , "  销售订单
  vgpos       TYPE    lips-vgpos  , "  行项目
  vkorg       TYPE    likp-vkorg  , "  销售组织
  werks       TYPE    lips-werks  , "  工厂
  lgort       TYPE    lips-lgort  , "  库存地址
  erdat       TYPE    likp-erdat  , "  创建日期
  zfhrq       TYPE    likp-zfhrq  , "  发货日期
  wadat       TYPE    likp-wadat_ist  , "  过账日期
  matnr       TYPE    lips-matnr  , "  产品代号
  arktx       TYPE    lips-arktx  , "  产品名称
  lfimg       TYPE    lips-lfimg  , "  销售数量
  vrkme       TYPE    lips-vrkme  , "  销售单位
  zjians      TYPE    lips-zjians , "  件数
  zlxsl       TYPE    lips-zlxsl  , "  零箱数量
  zlfimg      TYPE    lips-lfimg  , "  实发销售数量
  meins       TYPE    lips-meins  , "  基本计量单位
  wbsta       TYPE    string  , "  交货单状态
  charg       TYPE    lips-charg  , "  批量流水
  baxssl      TYPE    string  , "  基本计量数量
  zfhqd       TYPE    string  , "  是否特殊发货清单
  zshcoa      TYPE    string , "  是否需要随货COA
  zfhsx       TYPE    lips-zfhsx  , "  发货顺序
  zwmszt      TYPE    string , "  是否传输WMS
  zuser       TYPE    but000-name_org1  , "  业务员
  zcustom     TYPE    but000-name_org1  , "  客户
  zruser      TYPE    but000-name_org1  , "  实际客户
  zmakeuser   TYPE    adrp-name_text  , "  制单人
  sortl       TYPE    kna1-sortl  , "  检索项
  kukla       TYPE    kna1-kukla  , "  实际客户等级
  bezei       TYPE    tvv1t-bezei , "  销售渠道/客户类型
  pmatn       TYPE    mvke-pmatn  , "  定价参考物料
  maktx       TYPE    makt-maktx  , "  定价参考物料描述
  zstdname    TYPE    string  , "  标准名称
  groes       TYPE    mara-groes  , "  规格型号
  zckgjt      TYPE string, "出口国家
  matkl       TYPE    mara-matkl  , "  物料小类编号
  wgbez       TYPE    t023t-wgbez , "  物料小类描述
  zpspe       TYPE    string  , "  包装规格
  zsfxcp      TYPE    string , "  新产品
  zypsl       TYPE    vbap-zypsl  , "  样品数量
  zhsdat      TYPE    string  , "  生产日期
*  待定   TYPE    C ,"  保质期
*  待定   TYPE    C ,"  失效日期
  svbeln      TYPE    vbak-vbeln  , "  销售订单号
  bstkd_e     TYPE    vbkd-bstkd_e  , "  客户合同单号
  kpsum       TYPE    string  , "  开票数量
  ddpprice    TYPE    string  , "  DDP价格
  perprice    TYPE    string  , "  含税单价
  disclv      TYPE    string  , "  折扣率
  discnum     TYPE    string  , "  折扣金额
  kbetr       TYPE    prcd_elements-kbetr , "  税率
  mwsbp       TYPE    vbap-mwsbp  , "  销项税额
  netwr       TYPE    vbap-netwr  , "  不含税金额
  netwr_all   TYPE    string  , "  价税总计
  waerk       TYPE    vbak-waerk  , "  订单币别
  zpack_charg TYPE    string  , "  打印批号
  kunag       TYPE likp-kunag, "客户
  z004        TYPE    string  , "  备注
*上面是从字段里面复制过来的。

  vtweg       TYPE vbak-vtweg,    " 分销渠道VTWEG
  spart       TYPE vbak-spart,  " 产品组SPART
  ernam2      TYPE vbak-ernam, " 创建者
  kunnr       TYPE vbak-kunnr,  " 售达方KUNNR
  knumv       TYPE vbak-knumv, "凭证条件编号

*  下面添加查询所需要的其他字段。
  umvkn       TYPE lips-umvkn, "分子
  umvkz       LIKE lips-umvkz, "分母
  vbeln_vbak  TYPE    vbak-vbeln,   "  销售订单
  kwmeng      TYPE vbap-kwmeng, "销售订单数量
  meins2      TYPE vbap-meins, "销售订单单位
  zernam      TYPE likp-ernam, "制单人
  zckgj       TYPE string, "出口国家
  vfdat       TYPE mcha-vfdat, "货架到期日
  kursk       TYPE vbkd-kursk, "汇率
  zzqzt       TYPE string, "账期审批状态
  cmgst       TYPE string, "贷方
  sdabw       TYPE string, "特殊处理标
  zkhwl       TYPE string, "客户物料编码
  posnr_vbap  TYPE    vbap-posnr.   "  销售订单

TYPES:END OF lv_main.







SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:pvbeln FOR likp-vbeln,"条件输入框，一个for对应一个
pvkorg FOR likp-vkorg OBLIGATORY,"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
svtweg    FOR   vbak-vtweg  OBLIGATORY,"  分销渠道
pvbeln2 FOR vbak-vbeln,
pwerks FOR lips-werks,
puser FOR adrp-name_text,
pkunnr FOR vbpa-kunnr,
pcustom FOR vbpa-kunnr,
psortl FOR kna1-sortl NO-EXTENSION NO INTERVALS,
perdat FOR  likp-erdat,
pzfhrq FOR  likp-lfdat,
pwadat FOR  likp-wadat_ist,
pmatnr FOR  lips-matnr,
pwbsta FOR lips-wbsta NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-002.
SELECTION-SCREEN POSITION 23.
PARAMETERS flag_a RADIOBUTTON GROUP rg1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 24(8) TEXT-003 FOR FIELD flag_a.
SELECTION-SCREEN POSITION 44.
PARAMETERS flag_b RADIOBUTTON GROUP rg1.
SELECTION-SCREEN COMMENT 48(8) TEXT-004 FOR FIELD flag_b.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk.
" 测试选择屏幕的字段的值