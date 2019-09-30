*&---------------------------------------------------------------------*
*& Report ZSDRTEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdrtest.
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
*        svbeln          TYPE    vbak-vbeln  , "  销售订单号
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
  wbsta       TYPE    lips-wbsta  , "  交货单状态
  charg       TYPE    lips-charg  , "  批量流水
  baxssl      TYPE    string  , "  基本计量数量
  zfhqd       TYPE    likp-zfhqd  , "  是否特殊发货清单
  zshcoa      TYPE    likp-zshcoa , "  是否需要随货COA
  zfhsx       TYPE    lips-zfhsx  , "  发货顺序
  zwmszt      TYPE    likp-zwmszt , "  是否传输WMS
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
  matkl       TYPE    mara-matkl  , "  物料小类编号
  wgbez       TYPE    t023t-wgbez , "  物料小类描述
  zpspe       TYPE    string  , "  包装规格
  zsfxcp      TYPE    vbap-zsfxcp , "  新产品
  zypsl       TYPE    vbap-zypsl  , "  样品数量
  zhsdat      TYPE    string  , "  生产日期
*  待定   TYPE    C ,"  保质期
*  待定   TYPE    C ,"  失效日期
  svbeln      TYPE    vbak-vbeln  , "  销售订单号
  bstkd       TYPE    vbkd-bstkd  , "  客户合同单号
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
  posnr_vbap  TYPE    vbap-posnr.   "  销售订单

TYPES:END OF lv_main.







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
      lt_main2 TYPE TABLE OF lv_main,
      ls_main  TYPE lv_main.
*当筛选界面客户为空时，检索项不为空时，根据检索项获取KNA1对应的客户号KNA1-KUNNR

PERFORM getdata.
PERFORM catalog.
PERFORM alvshow.

FORM getdata.
*  SELECT
*    likp~vbeln
*  INTO lt_main
*  FROM lips
*  INNER JOIN likp
*  ON lips~vbeln = likp~vbeln
*  INNER JOIN vbap
*  ON lips~vgbel = vbap~vbeln
*  AND lips~vgpos = vbap~posnr
*  WHERE lips~werks IN pwerks
*  AND lips~matnr IN pmatnr
*  AND lips~wbsta IN pwbsta
*  AND likp~wadat_ist IN pwadat
*  AND likp~zfhrq IN pzfhrq
*  AND likp~erdat IN perdat
*  AND likp~vkorg IN pvkorg
*  AND likp~vbeln IN pvbeln
*  AND vbap~vbeln IN pvbeln2.

*  需要解决的问题：
*  VBELN字段的冲突

  SELECT
  likp~vbeln AS vbeln_likp
  lips~posnr AS posnr_lips
  lips~vgbel
  lips~vgpos
  likp~vkorg
  lips~werks
  lips~lgort
  likp~erdat
  likp~zfhrq
  likp~wadat_ist AS wadat
  lips~matnr
  lips~arktx
  lips~lfimg
  lips~vrkme
  lips~zjians
  lips~zlxsl
  lips~lfimg AS zlfimg
  lips~meins
  lips~wbsta
  lips~charg
  likp~zfhqd
  likp~zshcoa
  lips~zfhsx
  likp~zwmszt
  vbap~zsfxcp
  vbap~zypsl
  vbap~mwsbp
  vbap~netwr
  VBAP~POSNR AS posnr_vbap
  INTO CORRESPONDING FIELDS OF TABLE lt_main
  FROM lips
  INNER JOIN likp
  ON lips~vbeln = likp~vbeln
  INNER JOIN vbap
  ON lips~vgbel = vbap~vbeln
  AND lips~vgpos = vbap~posnr
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

*    SELECT
*    vbak~vbeln AS vbeln_vbak,
*    vbak~kunnr,
*    vbak~waerk,
*    vbak~ernam AS ernam2,
*    vbak~knumv
*    INTO TABLE @DATA(lt_main2)
*    FROM vbak
*    FOR ALL ENTRIES IN @lt_main
*    WHERE vbak~vbeln = @lt_main-vgbel.


    SELECT
    vbak~vbeln AS vbeln_vbak,
    vbak~kunnr,
    vbak~waerk,
    kna1~sortl,
    kna1~kukla,
    vbak~ernam AS ernam2,
    vbak~knumv
    INTO TABLE @DATA(lt_main2)
      FROM vbak
      INNER JOIN kna1
      ON kna1~kunnr = vbak~kunnr
      FOR ALL ENTRIES IN @lt_main
      WHERE vbak~vbeln = @lt_main-vgbel
      AND kna1~sortl IN @psortl.

*    SELECT
*      vbak~kunnr
*      kna1~sortl
*      kna1~kukla
*      vbak~waerk
*      vbak~ernam AS ernam2
*      vbak~knumv
*    INTO CORRESPONDING FIELDS OF TABLE lt_main
*    FROM vbak
*    INNER JOIN kna1
*    ON kna1~kunnr = vbak~kunnr
*    FOR ALL ENTRIES IN lt_main
*    WHERE vbak~vbeln = lt_main-vgbel
*      AND kna1~sortl IN psortl.


    SORT lt_main2 BY vbeln_vbak.

*    DELETE TABLE lt_main WITH TABLE KEY kunnr = ''.
    "过滤掉数据
    LOOP AT lt_main INTO ls_main.
      READ TABLE lt_main2 INTO DATA(ls_main2) WITH KEY vbeln_vbak = ls_main-vgbel BINARY SEARCH.
      IF sy-subrc = 0.
        ls_main-kunnr = ls_main2-kunnr.
        ls_main-waerk = ls_main2-waerk.
        ls_main-vbeln_vbak  = ls_main2-vbeln_vbak.
        ls_main-ernam2 = ls_main2-ernam2.
        ls_main-knumv = ls_main2-knumv.
        ls_main-sortl = ls_main2-sortl.
        ls_main-kukla = ls_main2-kukla.
        IF ls_main-kunnr IS INITIAL.
          DELETE TABLE lt_main FROM ls_main.
          CONTINUE.
        ELSE.
          MODIFY lt_main FROM ls_main.
        ENDIF.
      ENDIF.
    ENDLOOP.







    "上面对内表中的数据进行筛选
    "往自定义的内表中插入数据，将来做数据导出
    LOOP AT lt_main INTO ls_main.
      " ZUSER 业务员
      SELECT SINGLE
        name_org1
      INTO ls_out-zuser
      FROM but000
      INNER JOIN vbpa
      ON but000~partner = vbpa~kunnr
      WHERE vbpa~vbeln = ls_main-vbeln_likp
      AND vbpa~posnr = ls_main-posnr_lips
        AND vbpa~parvw = 'Z3'
        AND vbpa~kunnr IN puser.
      "客户
      SELECT SINGLE
      name_org1
      INTO ls_out-zcustom
      FROM but000
      INNER JOIN vbpa
      ON but000~partner = vbpa~kunnr
      WHERE vbpa~vbeln = ls_main-vbeln_likp
      AND vbpa~posnr = ls_main-posnr_lips
        AND vbpa~parvw = 'AG'
        AND vbpa~kunnr IN pcustom.

      "实际客户
      SELECT SINGLE
      name_org1
      INTO ls_out-zruser
      FROM but000
      INNER JOIN vbpa
      ON but000~partner = vbpa~kunnr
      WHERE vbpa~vbeln = ls_main-vbeln_likp
      AND vbpa~posnr = ls_main-posnr_lips
        AND vbpa~parvw = 'Z1'.
      "制单人
      SELECT SINGLE
        name_text
      INTO ls_out-zmakeuser
      FROM adrp
      INNER JOIN usr21
      ON adrp~persnumber = usr21~persnumber
      WHERE adrp~persnumber IN puser
        AND usr21~bname = ls_main-ernam2.
      "销售渠道/客户类型
      SELECT SINGLE
        bezei
      INTO ls_out-bezei
      FROM tvv1t
      INNER JOIN knvv
      ON knvv~kvgr1 = tvv1t~kvgr1
      INNER JOIN vbak
      ON vbak~vkorg = knvv~vkorg
      AND vbak~vtweg = knvv~vtweg
      AND vbak~spart = knvv~spart
      AND vbak~kunnr = knvv~kunnr
      WHERE vbak~vbeln = ls_main-vbeln_vbak.
      "定价参考物料和定价参考物料描述
      SELECT SINGLE
      mvke~pmatn AS pmatn
      makt~maktx AS maktx
      INTO CORRESPONDING FIELDS OF ls_out
      FROM mvke
      INNER JOIN makt
      ON makt~matnr = mvke~pmatn

      WHERE mvke~matnr = ls_main-matnr
        AND mvke~vkorg = ls_main-vkorg
        AND mvke~vtweg = ls_main-vtweg
        AND makt~spras = '1'.

      "标准名称
      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
        EXPORTING
          matnr  = ls_main-matnr
          atnam  = 'ZSTDNAME'
        TABLES
          output = lt_material.
      READ TABLE lt_material INTO ls_material INDEX 1.
      ls_out-zstdname = ls_material-atwrt.
      CLEAR ls_material.
      "包装规格
      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
        EXPORTING
          matnr  = ls_main-matnr
          atnam  = 'ZPSPE'
        TABLES
          output = lt_material.
      READ TABLE lt_material INTO ls_material INDEX 1.
      ls_out-zpspe = ls_material-atwrt.
      CLEAR ls_material.
      "生产日期
      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
        EXPORTING
          matnr  = ls_main-matnr
          atnam  = 'ZHSDAT'
        TABLES
          output = lt_material.
      READ TABLE lt_material INTO ls_material INDEX 1.
      ls_out-zhsdat = ls_material-date_from.
      CLEAR ls_material.
      " 打印批号
      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
        EXPORTING
          matnr  = ls_main-matnr
          atnam  = 'ZPACK_CHARG'
        TABLES
          output = lt_material.
      READ TABLE lt_material INTO ls_material INDEX 1.
      ls_out-zpack_charg = ls_material-atwrt.
      CLEAR ls_material.

      "规格型号、物料小类编号、物料小类描述
      SELECT SINGLE
        groes
        matkl
      INTO CORRESPONDING FIELDS OF ls_mara
      FROM mara
      WHERE mara~matnr = ls_main-matnr.
      MOVE-CORRESPONDING ls_mara TO ls_out.
      IF ls_mara IS NOT INITIAL.
        SELECT SINGLE
          wgbez
        INTO ls_out-wgbez
        FROM t023t
        WHERE t023t~matkl = ls_mara-matkl.
      ENDIF.
      "客户合同单号
      SELECT SINGLE
        bstkd
      INTO ls_out-bstkd
      FROM vbkd
      WHERE vbkd~vbeln = ls_main-vbeln_vbak
      AND vbkd~posnr = ls_main-posnr_vbap.

      "开票数量和DDP价格
      SELECT SINGLE
      kwert
      INTO CORRESPONDING FIELDS OF ls_prcd_elements
      FROM  prcd_elements
      WHERE prcd_elements~knumv = ls_vbak-knumv
      AND prcd_elements~kappl = 'Z100'
      AND prcd_elements~kposn = ls_main-vgpos.
      ls_out-ddpprice = ls_prcd_elements-kwert.
      DATA count TYPE i.
      " 开票数量
      SELECT
      vbelv
      posnv
      INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
      FROM vbfa
      WHERE vbfa~vbtyp_v = 'M'
      AND vbfa~vbeln = ls_main-vgbel
      AND vbfa~posnn = ls_main-vgpos.
      IF lt_vbfa IS NOT INITIAL.
        LOOP AT lt_vbfa INTO ls_vbfa.
          SELECT SINGLE
          vbeln
          fksto
          INTO CORRESPONDING FIELDS OF ls_vbrk
          FROM vbrk
          WHERE vbrk~vbeln = ls_vbfa-vbelv.
          IF ls_vbrk-fksto IS NOT INITIAL.
            DELETE lt_vbfa FROM ls_vbfa-vbelv.
          ENDIF.
        ENDLOOP.
        CLEAR ls_vbfa.
        LOOP AT lt_vbfa INTO ls_vbfa.
          IF ls_vbfa-rfmng IS NOT INITIAL.
            count = count + 1.
          ENDIF.
        ENDLOOP.
      ENDIF.
      ls_out-kpsum = count.

      " 折扣率和折扣金额
      SELECT SINGLE
      kwert
      INTO CORRESPONDING FIELDS OF ls_prcd_elements
      FROM  prcd_elements
      WHERE prcd_elements~knumv = ls_main-knumv
      AND prcd_elements~kappl = 'ZP00'
      OR prcd_elements~kappl = 'ZP02'
      AND prcd_elements~kposn = ls_main-vgpos.
      ls_out-discnum = ls_prcd_elements-kwert - ls_main-mwsbp - ls_main-netwr.
      ls_out-discnum = zcl_bc_public=>decimal_to_string( ls_out-discnum )."负号前置
      IF ls_prcd_elements-kwert IS NOT INITIAL.
        ls_out-disclv = ( 1 - ( ls_vbap-mwsbp + ls_vbap-netwr / ls_prcd_elements-kwert ) ) * 100.
      ENDIF.
      IF ls_out-disclv IS NOT INITIAL.
        ls_out-disclv = ls_out-disclv && '%'.
      ENDIF.

      " 税率
      SELECT SINGLE
      kbetr
      INTO CORRESPONDING FIELDS OF ls_prcd_elements
      FROM  prcd_elements
      WHERE prcd_elements~knumv = ls_main-knumv
      AND prcd_elements~kappl = 'MWSI'
      AND prcd_elements~kposn = ls_main-vgpos.
      ls_out-kbetr = ls_prcd_elements-kbetr.

      lv_name = ls_main-vbeln_likp.
      "备注
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'Z004'
          language                = sy-langu
          name                    = lv_name
          object                  = 'VBBK'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*     IMPORTING
*         HEADER                  =
*         OLD_LINE_COUNTER        =
        TABLES
          lines                   = lines
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
      READ TABLE lines INTO ls_lines INDEX 1.
      ls_out-z004 = ls_lines-tdline.


      "上面是对尚未完善的数据进行添加操作
      "下面是已经处理好的数据进行添加操作
      ls_out-baxssl = ( ls_main-umvkz / ls_main-umvkn ) * ls_main-lfimg." 基本计量数量
      ls_out-waerk = ls_main-waerk."订单币别
      ls_out-netwr = ls_main-mwsbp + ls_main-netwr."价税总计
      IF ls_main-lfimg IS NOT INITIAL.
        ls_out-perprice = ( ls_main-mwsbp + ls_main-netwr ) / ls_main-lfimg." 含税单价
      ENDIF.
      ls_out-sortl = ls_main-sortl."检索项
      ls_out-kukla = ls_main-kukla."实际客户等级
      ls_out-baxssl = ( ( ls_main-umvkz / ls_main-umvkn ) * ls_main-lfimg ). "基本计量数量
      ls_out-vbeln = ls_main-vbeln_likp ."  交货单
      ls_out-posnr = ls_main-posnr_lips ."  行项目
      ls_out-vgbel = ls_main-vgbel ."  销售订单
      ls_out-vgpos = ls_main-vgpos ."  行项目
      ls_out-vkorg = ls_main-vkorg ."  销售组织
      ls_out-werks = ls_main-werks ."  工厂
      ls_out-lgort = ls_main-lgort ."  库存地址
      ls_out-erdat = ls_main-erdat ."  创建日期
      ls_out-zfhrq = ls_main-zfhrq ."  发货日期
      ls_out-wadat = ls_main-wadat ."  过账日期
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
      ls_out-netwr = ls_main-netwr."不含税金额
      APPEND ls_out TO lt_out.
      " 如果制单人字段为空，去掉相对应的输出行
*      IF ls_out-zmakeuser IS INITIAL.
*        DELETE TABLE lt_out FROM ls_out.
*      ENDIF.
*      "如果业务员和客户字段为空的话，去掉相对应的输出行
*      IF ls_out-zcustom IS INITIAL.
*        DELETE TABLE lt_out FROM ls_out.
*      ENDIF.
*      IF ls_out-zuser IS INITIAL AND pkunnr IS NOT INITIAL.
*        DELETE TABLE lt_out FROM ls_out.
*      ENDIF.
*      CLEAR ls_out.
    ENDLOOP.
  ENDIF.
ENDFORM.
FORM catalog.
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.

  fieldcatset   'VBELN'   '交货单'   sy-tabix.
  fieldcatset   'POSNR'   '行项目'   sy-tabix.
  fieldcatset   'VGBEL'   '销售订单'    sy-tabix.
  fieldcatset   'VGPOS'   '行项目'   sy-tabix.
  fieldcatset   'VKORG'   '销售组织'    sy-tabix.
  fieldcatset   'WERKS'   '工厂'    sy-tabix.
  fieldcatset   'LGORT'   '库存地址'    sy-tabix.
  fieldcatset   'ERDAT'   '创建日期'    sy-tabix.
  fieldcatset   'ZFHRQ'   '发货日期'    sy-tabix.
  fieldcatset   'WADAT'   '过账日期'    sy-tabix.
  fieldcatset   'MATNR'   '产品代号'    sy-tabix.
  fieldcatset   'ARKTX'   '产品名称'    sy-tabix.
  fieldcatset   'LFIMG'   '销售数量'    sy-tabix.
  fieldcatset   'VRKME'   '销售单位'    sy-tabix.
  fieldcatset   'ZJIANS'      '件数'    sy-tabix.
  fieldcatset   'ZLXSL'     '零箱数量'    sy-tabix.
  fieldcatset   'ZLFIMG'      '实发销售数量'    sy-tabix.
  fieldcatset   'MEINS'   '基本计量单位'    sy-tabix.
  fieldcatset   'WBSTA'   '交货单状态'   sy-tabix.
  fieldcatset   'CHARG'   '批量流水'    sy-tabix.
  fieldcatset   'BAXSSL'      '基本计量数量'    sy-tabix.
  fieldcatset   'ZFHQD'     '是否特殊发货清单'    sy-tabix.
  fieldcatset   'ZSHCOA'      '是否需要随货'    sy-tabix.
  fieldcatset   'ZFHSX'     '发货顺序'    sy-tabix.
  fieldcatset   'ZWMSZT'      '是否传输WMS'   sy-tabix.
  fieldcatset   'ZUSER'     '业务员'   sy-tabix.
  fieldcatset   'ZCUSTOM'     '客户'    sy-tabix.
  fieldcatset   'ZRUSER'      '实际客户'    sy-tabix.
  fieldcatset   'ZMAKEUSER'     '制单人'   sy-tabix.
  fieldcatset   'SORTL'     '检索项'   sy-tabix.
  fieldcatset   'KUKLA'     '实际客户等级'    sy-tabix.
  fieldcatset   'BEZEI'     '销售渠道/客户类型'   sy-tabix.
  fieldcatset   'PMATN'     '定价参考物料'    sy-tabix.
  fieldcatset   'MAKTX'     '定价参考物料描述'    sy-tabix.
  fieldcatset   'ZSTDNAME'      '标准名称'    sy-tabix.
  fieldcatset   'GROES'     '规格型号'    sy-tabix.
  fieldcatset   'MATKL'     '物料小类编号'    sy-tabix.
  fieldcatset   'WGBEZ'     '物料小类描述'    sy-tabix.
  fieldcatset   'ZPSPE'     '包装规格'    sy-tabix.
  fieldcatset   'ZSFXCP'      '新产品'   sy-tabix.
  fieldcatset   'ZYPSL'     '样品数量'    sy-tabix.
  fieldcatset   'ZHSDAT'      '生产日期'    sy-tabix.
*  fieldcatset    '待定'      保质期   sy-tabix.
*  fieldcatset    '待定'      失效日期    sy-tabix.
  fieldcatset   'SVBELN'      '销售订单号'   sy-tabix.
  fieldcatset   'BSTKD'     '客户合同单号'    sy-tabix.
  fieldcatset   'KPSUM'     '开票数量'    sy-tabix.
  fieldcatset   'DDPPRICE'      'DDP价格'   sy-tabix.
  fieldcatset   'PERPRICE'      '含税单价'    sy-tabix.
  fieldcatset   'DISCLV'      '折扣率'   sy-tabix.
  fieldcatset   'DISCNUM'     '折扣金额'    sy-tabix.
  fieldcatset   'KBETR'     '税率'    sy-tabix.
  fieldcatset   'MWSBP'     '销项税额'    sy-tabix.
  fieldcatset   'NETWR'     '不含税金额'   sy-tabix.
  fieldcatset   'NETWR'     '价税总计'    sy-tabix.
  fieldcatset   'WAERK'     '订单币别'    sy-tabix.
  fieldcatset   'ZPACK_CHARG'     '打印批号'    sy-tabix.
  fieldcatset   'Z004'      '备注'    sy-tabix.
ENDFORM.

FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = w_repid "程序名称
*     I_CALLBACK_PF_STATUS_SET          = 'FRM_SET_PF_STATUS'
*     I_CALLBACK_USER_COMMAND           = 'ALV_USER_COMMAND'"对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
      i_grid_title       = '交货单报表' "标题名
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
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
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