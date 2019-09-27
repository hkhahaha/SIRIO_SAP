*&---------------------------------------------------------------------*
*& 包含               ZSD0009_TOP
*&---------------------------------------------------------------------*
TABLES: likp,vbak,lips,kna1,adrp,vbpa.
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
        wbsta           TYPE    lips-wbsta  , "  交货单状态
        charg           TYPE    lips-charg  , "  批量流水
        baxssl          TYPE    lips-lfimg , "  基本计量数量
        zfhqd           TYPE    likp-zfhqd  , "  是否特殊发货清单
        zshcoa          TYPE    likp-zshcoa , "  是否需要随货COA
        zfhsx           TYPE    lips-zfhsx  , "  发货顺序
        zwmszt          TYPE    likp-zwmszt , "  是否传输WMS
        zuser           TYPE    but000-name_org1  , "  业务员
        zcustom         TYPE    but000-name_org1  , "  客户
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
        zsfxcp          TYPE    vbap-zsfxcp , "  新产品
        zypsl           TYPE    vbap-zypsl  , "  样品数量
        zhsdat          TYPE    likp-wadat_ist , "  生产日期
*待定    TYPE    C ,"  保质期
*待定    TYPE    C ,"  失效日期
        svbeln          TYPE    vbak-vbeln  , "  销售订单号
        bstkd           TYPE    vbkd-bstkd  , "  客户合同单号
        kpsum           TYPE    vbfa-rfmng , "  开票数量
        ddpprice        TYPE    prcd_elements-kbetr , "  DDP价格
        perprice        TYPE    vbap-mwsbp , "  含税单价
        disclv(12)      TYPE    c , "  折扣率
        discnum         TYPE    vbap-mwsbp , "  折扣金额
        kbetr           TYPE    prcd_elements-kbetr , "  税率
        mwsbp           TYPE    vbap-mwsbp  , "  销项税额
        price           TYPE    vbap-netwr  , "  不含税金额
        netwr           TYPE    vbap-netwr , "  价税总计
        waerk           TYPE    vbak-waerk  , "  订单币别
        zpack_charg(40) TYPE    c , "  打印批号
        z004(40)        TYPE    c , "  备注
      END OF lv_out.
TYPES:BEGIN OF lv_out2.
        INCLUDE TYPE lv_out.
        TYPES:zson(20) TYPE c,
        zsum(20) TYPE c,
      END OF lv_out2.




SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:pvbeln FOR likp-vbeln,"条件输入框，一个for对应一个，交货单
pvkorg FOR likp-vkorg,"销售组织，for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
pvbeln2 FOR vbak-vbeln,"销售订单
pwerks FOR lips-werks OBLIGATORY,"工厂
puser FOR adrp-name_text,"制单人
pkunnr FOR vbpa-kunnr,"业务员
pcustom FOR vbpa-kunnr,"客户
psortl FOR kna1-sortl NO-EXTENSION NO INTERVALS,"检索项
perdat FOR  likp-erdat,"创建日期
pzfhrq FOR  likp-lfdat,"发货日期 
pwadat FOR  likp-wadat_ist,"过账日期
pmatnr FOR  lips-matnr,"产品代号
pwbsta FOR lips-wbsta NO-EXTENSION NO INTERVALS."交货状态
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-002.
SELECTION-SCREEN POSITION 23.
PARAMETERS a RADIOBUTTON GROUP rg1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 24(8) TEXT-003 FOR FIELD a.
SELECTION-SCREEN POSITION 44.
PARAMETERS b RADIOBUTTON GROUP rg1.
SELECTION-SCREEN COMMENT 48(8) TEXT-004 FOR FIELD b.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk.
" 测试选择屏幕的字段的值