*&---------------------------------------------------------------------*
*& Report ZSDRTEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdrtest.
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
TYPES:BEGIN OF lv_main.
        INCLUDE TYPE lips.
        TYPES: vkorg     TYPE likp-vkorg, "销售组织
        zfhrq     TYPE likp-zfhrq, "发货日期
        wadat_ist TYPE likp-wadat_ist, "过账日期
        zfhqd     TYPE    likp-zfhqd, "是否特殊发货清单
        zshcoa    TYPE    likp-zshcoa, "是否需要随货COA
        zwmszt    TYPE    likp-zwmszt, "是否传输WMS
        zsfxcp    TYPE  vbap-zsfxcp, "新产品
        zypsl     TYPE  vbap-zypsl, "样品数量
        mwsbp     TYPE vbap-mwsbp, "销项税额
        netwr2    TYPE  vbap-netwr, " 不含税金额
*        mwsbp     TYPE vbap-mwsbp,
*        netwr     TYPE vbap-netwr,
        sortl     TYPE kna1-sortl, "检索项
        kukla     TYPE kna1-kukla, "实际客户等级
*        vkorg     TYPE vbak-vkorg,   " 销售机构VKORG
*        vtweg     TYPE vbak-vtweg,    " 分销渠道VTWEG
*        spart     TYPE vbak-spart,  " 产品组SPART
        kunnr     TYPE vbak-kunnr,  " 售达方KUNNR
      END OF lv_main.







SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:pvbeln FOR likp-vbeln,"条件输入框，一个for对应一个
pvkorg FOR likp-vkorg,"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
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


*&---------------------------------------------------------------------*
*& 包含               ZSD0009_MAIN
*&---------------------------------------------------------------------*


" 下面定义的是标准日报表中的字段(内表)
DATA:lt_likp          TYPE TABLE OF likp,
     ls_likp          TYPE likp,
     lt_lips          TYPE TABLE OF lips,
     ls_lips          TYPE lips,
     lt_but000        TYPE TABLE OF but000,
     ls_but000        TYPE but000,
     lt_adrp          TYPE TABLE OF adrp,
     ls_adrp          TYPE adrp,
     lt_kna1          TYPE TABLE OF kna1,
     ls_kna1          TYPE kna1,
     lt_tvv1t         TYPE TABLE OF tvv1t,
     ls_tvv1t         TYPE tvv1t,
     lt_mvke          TYPE TABLE OF mvke,
     ls_mvke          TYPE mvke,
     lt_makt          TYPE TABLE OF makt,
     ls_makt          TYPE makt,
     lt_mara          TYPE TABLE OF mara,
     ls_mara          TYPE mara,
     lt_t023t         TYPE TABLE OF t023t,
     ls_t023t         TYPE t023t,
     lt_vbap          TYPE TABLE OF vbap,
     ls_vbap          TYPE vbap,
     lt_vbak          TYPE TABLE OF vbak,
     ls_vbak          TYPE vbak,
     lt_vbkd          TYPE TABLE OF vbkd,
     ls_vbkd          TYPE vbkd,
     lt_prcd_elements TYPE TABLE OF prcd_elements,
     ls_prcd_elements TYPE prcd_elements,
     lt_vbfa          TYPE TABLE OF vbfa,
     ls_vbfa          TYPE vbfa,
     lt_vbpa          TYPE TABLE OF vbpa,
     ls_vbpa          TYPE vbpa,
     lt_knvv          TYPE TABLE OF knvv,
     ls_knvv          TYPE knvv,
     lt_vbrk          TYPE TABLE OF vbrk,
     ls_vbrk          TYPE vbrk.
" 下面定义的是当报表类型是铝的时候的字段（内表）
DATA: lt_zlv        TYPE TABLE OF zaluminium,
      ls_zlv        TYPE zaluminium,
      lt_out        TYPE TABLE OF lv_out,
      ls_out        TYPE lv_out,
      lt_out2       TYPE TABLE OF lv_out2,
      ls_out2       TYPE lv_out2,
      lt_material   TYPE TABLE OF zmaterial_tx_value,
      ls_material   TYPE zmaterial_tx_value,
      lt_zaluminium TYPE TABLE OF zaluminium,
      ls_zaluminium TYPE zaluminium.
DATA: g_total  TYPE i,
      g_cnt    TYPE i,
      g_index  TYPE i,
      lines    LIKE  TABLE OF tline,
      ls_lines LIKE tline,
      lv_name  LIKE  thead-tdname,
      lt_main  TYPE TABLE OF lv_main,
      ls_main  TYPE lv_main.
*当筛选界面客户为空时，检索项不为空时，根据检索项获取KNA1对应的客户号KNA1-KUNNR

PERFORM getdata.
PERFORM catalog.
PERFORM alvshow.

FORM getdata.
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_main
  FROM lips
  INNER JOIN likp
  ON lips~vbeln = likp~vbeln
  INNER JOIN vbap
  ON vbap~vbeln = lips~vgbel
  AND vbap~posnr = lips~vgpos
  WHERE lips~werks IN pwerks
  AND lips~matnr IN pmatnr
  AND lips~wbsta IN pwbsta
  AND likp~wadat_ist IN pwadat
  AND likp~zfhrq IN pzfhrq
  AND likp~erdat IN perdat
  AND likp~vkorg IN pvkorg
  AND likp~vbeln IN pvbeln
  AND vbap~vbeln IN pvbeln2.
  IF lt_main IS NOT INITIAL.
    "下面对原始数据进行处理，根据前台的筛选条件
    "根据已知的条件查询VBAK里面的数据
    SELECT
      vbak~kunnr
      kna1~sortl
      kna1~kukla
    INTO CORRESPONDING FIELDS OF TABLE lt_main
    FROM vbak
    INNER JOIN kna1
    ON kna1~kunnr = vbak~kunnr
    FOR ALL ENTRIES IN lt_main
    WHERE vbak~vbeln = lt_main-kdauf
      AND kna1~sortl IN psortl.
*    DELETE TABLE lt_main WITH TABLE KEY kunnr = ''.
    "过滤掉数据
    LOOP AT lt_main INTO ls_main.
      IF ls_main-kunnr IS INITIAL.
        DELETE TABLE lt_main FROM ls_main.
      ENDIF.
    ENDLOOP.




    "上面对内表中的数据进行筛选
    "往自定义的内表中插入数据，将来做数据导出
    LOOP AT lt_main INTO ls_main.
      ls_out-baxssl = ( ( ls_main-umvkz / ls_main-umvkn ) * ls_main-lfimg ). "基本计量数量
      ls_out-vbeln = ls_main-vbeln ."  交货单
      ls_out-posnr = ls_main-posnr ."  行项目
      ls_out-vgbel = ls_main-vgbel ."  销售订单
      ls_out-vgpos = ls_main-vgpos ."  行项目
      ls_out-vkorg = ls_main-vkorg ."  销售组织
      ls_out-werks = ls_main-werks ."  工厂
      ls_out-lgort = ls_main-lgort ."  库存地址
      ls_out-erdat = ls_main-erdat ."  创建日期
      ls_out-zfhrq = ls_main-zfhrq ."  发货日期
      ls_out-wadat = ls_main-wadat_ist ."  过账日期
      ls_out-matnr = ls_main-matnr ."  产品代号
      ls_out-arktx = ls_main-arktx ."  产品名称
      ls_out-lfimg = ls_main-lfimg ."  销售数量
      ls_out-vrkme = ls_main-vrkme ."  销售单位
      ls_out-zjians  = ls_main-zjians  ."  件数
      ls_out-zlxsl = ls_main-zlxsl ."  零箱数量
      ls_out-zlfimg  = ls_main-lfimg  ."  实发销售数量
      ls_out-meins = ls_main-meins ."  基本计量单位
      ls_out-wbsta = ls_main-wbsta ."  交货单状态
      ls_out-charg = ls_main-charg ."  批量流水
      ls_out-zfhqd = ls_main-zfhqd ."  是否特殊发货清单
      ls_out-zshcoa  = ls_main-zshcoa  ."  是否需要随货COA
      ls_out-zfhsx = ls_main-zfhsx ."  发货顺序
      ls_out-zwmszt  = ls_main-zwmszt  ."  是否传输WMS
      ls_out-zsfxcp  = ls_main-zsfxcp  ."  新产品
      ls_out-zypsl = ls_main-zypsl ."  样品数量
      ls_out-mwsbp = ls_main-mwsbp."销项税额
      ls_out-netwr = ls_main-netwr2."不含税金额
      ls_out-zsfxcp = ls_main-zsfxcp."新产品
      ls_out-zypsl = ls_main-zypsl."样品数量
    ENDLOOP.
  ENDIF.
ENDFORM.
FORM catalog.
ENDFORM.
FORM alvshow.
ENDFORM.