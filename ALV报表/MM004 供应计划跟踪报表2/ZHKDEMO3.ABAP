*&---------------------------------------------------------------------*
*& Report ZHKALV1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhkalv1."报表程序声明
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid, "记录系统当前的程序名
     list     TYPE vrm_values.
TABLES:ekko,zmmwms_pol,mara,lfa1,ekpo,zmmaps_temp,t006a,marc,eket,ekbe.
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf
"定义ALV涉及到的相关字段
DATA:BEGIN OF lv_alv,
       light(4)          TYPE  c , "  状态指示灯
       dlv_planno        TYPE  zmmwms_pol-dlv_planno , "  送货计划号
       dlv_itemno        TYPE  zmmwms_pol-dlv_itemno , "  送货计划行
       ebeln             TYPE  zmmwms_pol-ebeln  , "  采购订单号
       ebelp             TYPE  zmmwms_pol-ebelp  , "  采购订单行
       lifnr             TYPE  ekko-lifnr  , "  供应商编号
       name1_gp          TYPE  lfa1-name1  , "  供应商名称
       pdlv_date         TYPE  zmmwms_pol-pdlv_date  , "  确认送货日期
       arrive_day(8)     TYPE  c , "  预计到货天数
       matnr             TYPE  zmmwms_pol-matnr  , "  物料编码
       maktx             TYPE  makt-maktx  , "  物料描述
       producer          TYPE  zmmwms_pol-producer , "  制造商编号
       producer_nam      TYPE  lfa1-name1  , "  制造商名称
       reqdate           TYPE  zmmwms_pol-pdlv_date  , "  APS需求日期
       pdlv_date_ext     TYPE  zmmwms_pol-pdlv_date  , "  最新调整送货日期
       actual_date       TYPE  zmmwms_pol-pdlv_date  , "  实际到货日期
       meins             TYPE  zmmwms_pol-meins  , "  基本计量单位
       mseht(10)         TYPE  c , "  基本计量单位文本
       apsreq_menge      TYPE  zmmwms_pol-pdlv_menge  , "  APS需求数量
       pdlv_menge        TYPE  zmmwms_pol-pdlv_menge  , "  计划送货数量
       recv_menge        TYPE  zmmwms_pol-pdlv_menge  , "  净收货量
       undlv_mende       TYPE  zmmwms_pol-pdlv_menge  , "  剩余未到货量
       delay(8)          TYPE  c , "  未到货延期天数
       avg_delay(6)      TYPE  c , "  加权延期天数
       count_year(5)     TYPE  c , "  当前交货次数
       po_num(5)         TYPE  c , "  本单到货次数
       min_dlv           TYPE  marc-bstmi , "  最小订购批量
       origin(3)         TYPE  c , "  计划来源
       responsible(20)   TYPE  c , "  采购部门负责人
       zoriadd(100)      TYPE  c , "  原产地
       xprecious(30)     TYPE  c , "  贵重标识
       number            TYPE  zmmwms_pol-pdlv_menge  , "  质检放行数量
       close(3)          TYPE  c , "  关闭标志
       answer(10)        TYPE  c , "  答复状态
       quality_num       TYPE  zmmwms_pol-pdlv_menge  , "  质检状态数量
       noquality_num     TYPE  zmmwms_pol-pdlv_menge  , "  非限制状态数量
       frozen_num        TYPE  zmmwms_pol-pdlv_menge  , "  冻结状态数量
       return_num        TYPE  zmmwms_pol-pdlv_menge  , "  确认退货数量
       delivery_add(100) TYPE  c , "  送货地址
       pay_date          TYPE  zmmwms_pol-pdlv_date  , "  交付日期（版本号）
       or_order          TYPE  zmmwms_pol-pdlv_date  , "  原订单到货时间
       or_order_num      TYPE  zmmwms_pol-pdlv_menge  , "  原订单数量
       charg             TYPE zmatdoc-charg, "批次
       zlwedt            TYPE zmmwms_pol-pdlv_date, "最后一次到货日期
       zlwedt2           TYPE ausp-atflv, "最后一次到货日期
     END OF lv_alv.
"定义结构用于存储采购相关的数据，EKKO和EKPO，引用类型ZMMWMS_POL
DATA:BEGIN OF lv_ekko_po,
       ekorg   TYPE ekko-ekorg, "采购组织
       ekgrp   TYPE ekko-ekgrp, "采购组
       lifnr   TYPE ekko-lifnr, "供应商编号
       matkl   TYPE mara-matkl, "物料组
       ernam   TYPE ekko-ernam, "创建人姓名
       lgort2  TYPE ekpo-lgort, "库存地点
       menge   TYPE ekpo-menge, "采购订单数量
       reqdate TYPE zmmaps_temp-reqdate. "APS需求日期
       INCLUDE      TYPE zmmwms_pol.
     DATA END OF lv_ekko_po.


DATA: lt_zmmwms_pol   TYPE TABLE OF zmmwms_pol,
      ls_zmmwms_pol   TYPE zmmwms_pol,
      lt_ekko_po      LIKE TABLE OF lv_ekko_po,
      ls_ekko_po      LIKE LINE OF lt_ekko_po,
      lt_makt         TYPE TABLE OF makt,
      ls_makt         TYPE makt,
      lt_lfa1         TYPE TABLE OF lfa1,
      ls_lfa1         TYPE lfa1,
      lt_zmmaps_temp  TYPE TABLE OF zmmaps_temp,
      lt_zmmaps_temp2 TYPE TABLE OF zmmaps_temp,
      ls_zmmaps_temp  TYPE zmmaps_temp,
      lt_t006a        TYPE TABLE OF t006a,
      ls_t006a        TYPE t006a,
      lt_marc         TYPE TABLE OF marc,
      ls_marc         TYPE marc,
      lt_zmatdoc      TYPE TABLE OF zmatdoc,
      lt_zmatdoc2     TYPE TABLE OF zmatdoc,
      ls_zmatdoc      TYPE zmatdoc,
      lt_matdoc       TYPE TABLE OF matdoc,
      ls_matdoc       TYPE matdoc,
      lt_alv          LIKE TABLE OF lv_alv,
      ls_alv          LIKE LINE OF lt_alv.

INITIALIZATION.
  SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
  SELECT-OPTIONS:s_ekorg FOR ekko-ekorg,"条件输入框，一个for对应一个
    s_werks FOR zmmwms_pol-werks,"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
    s_ekgrp FOR ekko-ekgrp,
    s_lifnr FOR ekko-lifnr,
    s_ebeln FOR ekko-ebeln,
    s_matkl FOR mara-matkl,
    s_matnr FOR mara-matnr,
    st_code FOR zmmwms_pol-st_code NO INTERVALS,
    pdlv_dat FOR zmmwms_pol-pdlv_date,
    s_ernam FOR ekko-ernam,
    s_planno FOR zmmwms_pol-dlv_planno.
*  xclosed FOR zmmwms_pol-xclosed.
  PARAMETERS: p_closed(1) AS LISTBOX VISIBLE LENGTH 8 DEFAULT '1'.     "关闭标识

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(20) TEXT-005.
  SELECTION-SCREEN POSITION 23.
  PARAMETERS part_a TYPE c AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 25(8) TEXT-006.
  SELECTION-SCREEN POSITION 34.
  PARAMETERS part_b TYPE c AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 36(10) TEXT-007.
  SELECTION-SCREEN POSITION 48.
  PARAMETERS part_c TYPE c AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT 50(10) TEXT-008.
*PARAMETERS part_c TYPE c AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 45(6) TEXT-002.
*PARAMETERS flag_a1 RADIOBUTTON GROUP rg2 DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 24(8) TEXT-003 FOR FIELD flag_a1.
*SELECTION-SCREEN POSITION 44.
*PARAMETERS flag_b1 RADIOBUTTON GROUP rg2.
*SELECTION-SCREEN COMMENT 48(8) TEXT-004 FOR FIELD flag_b1.
  SELECTION-SCREEN END OF LINE.


  SELECTION-SCREEN END OF BLOCK blk.
  PERFORM frm_init_list.

INITIALIZATION.

START-OF-SELECTION.
*调用子程序
  PERFORM frm_init_list.
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
  REFRESH list.

END-OF-SELECTION.
*定义子程序
FORM getdata."查询语句
  w_repid = sy-repid.
  DATA p_closed2(1).
  "获取ZMMWMS_POL表里面涉及到的数据
  IF p_closed IS INITIAL.
    SELECT
    *
    INTO CORRESPONDING FIELDS OF TABLE lt_zmmwms_pol
    FROM zmmwms_pol
    INNER JOIN ekko
    ON ekko~ebeln = zmmwms_pol~ebeln
    INNER JOIN mara
    ON mara~matnr = zmmwms_pol~matnr
    WHERE zmmwms_pol~werks IN s_werks
    AND zmmwms_pol~ebeln IN s_ebeln
    AND zmmwms_pol~matnr IN s_matnr
    AND zmmwms_pol~st_code IN st_code
    AND zmmwms_pol~pdlv_date IN pdlv_dat
    AND zmmwms_pol~dlv_planno IN s_planno
    AND ekko~lifnr IN s_lifnr
    AND mara~matkl IN s_matkl
    AND ekko~ekgrp IN s_ekgrp.
  ELSE."如果前台的关闭标识为空的话，修改关闭标识的查询条件为%
    IF p_closed = '1'.
      p_closed2 = ''.
    ELSEIF p_closed = '2'.
      p_closed2 = 'X'.
    ENDIF.
    SELECT
    *
    INTO CORRESPONDING FIELDS OF TABLE lt_zmmwms_pol
    FROM zmmwms_pol
    INNER JOIN ekko
    ON ekko~ebeln = zmmwms_pol~ebeln
    INNER JOIN mara
    ON mara~matnr = zmmwms_pol~matnr
    WHERE zmmwms_pol~werks IN s_werks
    AND zmmwms_pol~ebeln IN s_ebeln
    AND zmmwms_pol~matnr IN s_matnr
    AND zmmwms_pol~st_code IN st_code
    AND zmmwms_pol~pdlv_date IN pdlv_dat
    AND zmmwms_pol~xclosed = p_closed2
    AND zmmwms_pol~dlv_planno IN s_planno
    AND ekko~lifnr IN s_lifnr
    AND mara~matkl IN s_matkl
    AND ekko~ekgrp IN s_ekgrp.
  ENDIF.

  IF lt_zmmwms_pol IS NOT INITIAL.

    "接下来取采购订单抬头EKKO和采购订单行项目EKPO
    IF p_closed IS INITIAL.
      SELECT
      zmmwms_pol~dlv_planno  " 送料匹配计划号
      zmmwms_pol~dlv_itemno  " 计划行
      zmmwms_pol~origin  " 源头
      zmmwms_pol~ebeln " 采购凭证编号
      zmmwms_pol~ebelp " 采购凭证的项目编号
      zmmwms_pol~guid  " 唯一标识
      zmmwms_pol~uniqueid  " 连接 EBELN 和 EBELP
      zmmwms_pol~duoge " 是否匹配多个临时表数据
      zmmwms_pol~matnr " 物料编号
      zmmwms_pol~pdlv_date " 计划交货日期
      zmmwms_pol~pdlv_date_ext " 调整后计划交货日期
      zmmwms_pol~pdlv_menge  " 计划送货数量
      zmmwms_pol~meins " 基本计量单位
      zmmwms_pol~werks " 工厂
      zmmwms_pol~lgort " 库存地点
      zmmwms_pol~st_code " 特殊标准
      zmmwms_pol~producer  " 生产商
      zmmwms_pol~mfrpn " 制造商零件编号
      zmmwms_pol~uebto " 过量交货限度
      zmmwms_pol~received_quan " 已收数量
      zmmwms_pol~zaps_sts  " 状态
      zmmwms_pol~wms_sts " 下发WMS状态
      zmmwms_pol~xclosed " 关闭标记
      ekko~ekorg "采购组织
      ekko~ekgrp "采购组
      ekko~lifnr "供应商编号
      mara~matkl "物料组
      ekko~ernam "订单创建人姓名
      ekpo~lgort AS lgort2"库存地点
      ekpo~menge "采购订单数量
      INTO CORRESPONDING FIELDS OF TABLE lt_ekko_po
      FROM ekpo
      INNER JOIN ekko
      ON ekko~ebeln = ekpo~ebeln
      INNER JOIN zmmwms_pol
      ON zmmwms_pol~ebeln = ekpo~ebeln
      AND zmmwms_pol~ebelp = ekpo~ebelp
      INNER JOIN mara
      ON zmmwms_pol~matnr = mara~matnr
      FOR ALL ENTRIES IN lt_zmmwms_pol
      WHERE ekpo~ebeln = lt_zmmwms_pol-ebeln
      AND ekpo~ebelp = lt_zmmwms_pol-ebelp
      AND ekko~ekorg IN s_ekorg
      AND ekko~ekgrp IN s_ekgrp
      AND ekko~lifnr IN s_lifnr
      AND mara~matkl IN s_matkl
      AND ekko~ernam IN s_ernam
      AND ekpo~ebeln IN s_ebeln
      AND zmmwms_pol~pdlv_date IN pdlv_dat
      AND zmmwms_pol~dlv_planno IN s_planno.
    ELSE.
      SELECT
      zmmwms_pol~dlv_planno  " 送料匹配计划号
      zmmwms_pol~dlv_itemno  " 计划行
      zmmwms_pol~origin  " 源头
      zmmwms_pol~ebeln " 采购凭证编号
      zmmwms_pol~ebelp " 采购凭证的项目编号
      zmmwms_pol~guid  " 唯一标识
      zmmwms_pol~uniqueid  " 连接 EBELN 和 EBELP
      zmmwms_pol~duoge " 是否匹配多个临时表数据
      zmmwms_pol~matnr " 物料编号
      zmmwms_pol~pdlv_date " 计划交货日期
      zmmwms_pol~pdlv_date_ext " 调整后计划交货日期
      zmmwms_pol~pdlv_menge  " 计划送货数量
      zmmwms_pol~meins " 基本计量单位
      zmmwms_pol~werks " 工厂
      zmmwms_pol~lgort " 库存地点
      zmmwms_pol~st_code " 特殊标准
      zmmwms_pol~producer  " 生产商
      zmmwms_pol~mfrpn " 制造商零件编号
      zmmwms_pol~uebto " 过量交货限度
      zmmwms_pol~received_quan " 已收数量
      zmmwms_pol~zaps_sts  " 状态
      zmmwms_pol~wms_sts " 下发WMS状态
      zmmwms_pol~xclosed " 关闭标记
      ekko~ekorg "采购组织
      ekko~ekgrp "采购组
      ekko~lifnr "供应商编号
      mara~matkl "物料组
      ekko~ernam "订单创建人姓名
      ekpo~lgort AS lgort2"库存地点
      ekpo~menge "采购订单数量
      INTO CORRESPONDING FIELDS OF TABLE lt_ekko_po
      FROM ekpo
      INNER JOIN ekko
      ON ekko~ebeln = ekpo~ebeln
      INNER JOIN zmmwms_pol
      ON zmmwms_pol~ebeln = ekpo~ebeln
      AND zmmwms_pol~ebelp = ekpo~ebelp
      INNER JOIN mara
      ON zmmwms_pol~matnr = mara~matnr
      FOR ALL ENTRIES IN lt_zmmwms_pol
      WHERE ekpo~ebeln = lt_zmmwms_pol-ebeln
      AND ekpo~ebelp = lt_zmmwms_pol-ebelp
      AND ekko~ekorg IN s_ekorg
      AND ekko~ekgrp IN s_ekgrp
      AND ekko~lifnr IN s_lifnr
      AND mara~matkl IN s_matkl
      AND ekko~ernam IN s_ernam
      AND ekpo~ebeln IN s_ebeln
      AND zmmwms_pol~pdlv_date IN pdlv_dat
      AND zmmwms_pol~xclosed = p_closed2
      AND zmmwms_pol~dlv_planno IN s_planno.
    ENDIF.
  ENDIF.


  "获取ZMMAPS_TEMP表中的数据
  SELECT
    uniqueid
    pdlv_date
    reqdate
    FROM zmmaps_temp
    INTO CORRESPONDING FIELDS OF TABLE lt_zmmaps_temp
    FOR ALL ENTRIES IN lt_ekko_po
    WHERE zmmaps_temp~uniqueid EQ lt_ekko_po-uniqueid
      AND zmmaps_temp~pdlv_date EQ lt_ekko_po-pdlv_date.
  SORT lt_zmmaps_temp BY uniqueid pdlv_date reqdate.
  "获取计量单位的文本
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_t006a
  FROM t006a
  FOR ALL ENTRIES IN lt_ekko_po
  WHERE t006a~msehi = lt_ekko_po-meins
    AND t006a~spras = '1'.
  SORT lt_t006a BY msehi.
  "获取MARC的数据
  SELECT
    matnr
    werks
    bstmi
  INTO CORRESPONDING FIELDS OF TABLE lt_marc
  FROM marc
  FOR ALL ENTRIES IN lt_ekko_po
  WHERE marc~matnr = lt_ekko_po-matnr
    AND marc~werks = lt_ekko_po-werks.
  SORT lt_marc BY matnr.
  "获取ZMATDOC表里面的数据，可以获取物料凭证还有年度
  SELECT
    mblnr
    mjahr
    zeile
    dlv_planno
    dlv_itemno
    ebeln
    ebelp
    shkzg
    menge
    charg
  INTO CORRESPONDING FIELDS OF TABLE lt_zmatdoc
  FROM zmatdoc
  FOR ALL ENTRIES IN lt_ekko_po
  WHERE zmatdoc~dlv_planno = lt_ekko_po-dlv_planno
    AND zmatdoc~dlv_itemno = lt_ekko_po-dlv_itemno.
  SORT lt_zmatdoc BY dlv_planno dlv_itemno shkzg.
  "获取MKPF表里面的数据
  SELECT
    bldat,
    dlv_planno,
    dlv_itemno,
    ebeln,
    ebelp
    INTO TABLE @DATA(lt_mkpf)
    FROM mkpf
    INNER JOIN zmatdoc
    ON zmatdoc~mblnr = mkpf~mblnr
    AND zmatdoc~mjahr = mkpf~mjahr
    FOR ALL ENTRIES IN @lt_zmatdoc
    WHERE mkpf~mblnr = @lt_zmatdoc-mblnr
      AND mkpf~mjahr = @lt_zmatdoc-mjahr.
  SORT lt_mkpf BY dlv_planno dlv_itemno.
  LOOP AT lt_zmatdoc INTO ls_zmatdoc.
    COLLECT ls_zmatdoc INTO lt_zmatdoc2.
    CLEAR ls_zmatdoc.
  ENDLOOP.
  "获取全部的APS需求数量相关的字段
  SELECT
    uniqueid,
    pdlv_date,
    aps_menge,
    zmmaps_pl~guid
  INTO TABLE @DATA(lt_zmmaps_pl)
  FROM zmmaps_pl
  INNER JOIN zmmaps_temp
  ON zmmaps_pl~werks = zmmaps_temp~werks
  AND zmmaps_pl~lgort = zmmaps_temp~lgort
  AND zmmaps_pl~version = zmmaps_temp~version
  AND zmmaps_pl~reqdate = zmmaps_temp~reqdate
  AND zmmaps_pl~matnr = zmmaps_temp~matnr
  AND zmmaps_pl~st_code = zmmaps_temp~st_code
  FOR ALL ENTRIES IN @lt_ekko_po
  WHERE zmmaps_temp~uniqueid = @lt_ekko_po-uniqueid
  AND zmmaps_temp~pdlv_date = @lt_ekko_po-pdlv_date.
  DATA: ls_zmmaps_pl  LIKE LINE OF lt_zmmaps_pl,
        lt_zmmaps_pl2 LIKE TABLE OF ls_zmmaps_pl.
*  LOOP AT lt_zmmaps_pl INTO ls_zmmaps_pl.
*    COLLECT ls_zmmaps_pl INTO lt_zmmaps_pl2.
*    CLEAR ls_zmmaps_pl.
*  ENDLOOP.
  IF lt_ekko_po IS NOT INITIAL.
    SELECT
    *
    INTO CORRESPONDING FIELDS OF TABLE lt_zmmaps_temp2
    FROM zmmaps_temp
    FOR ALL ENTRIES IN lt_ekko_po
    WHERE zmmaps_temp~ebeln = lt_ekko_po-ebeln
    AND zmmaps_temp~ebelp = lt_ekko_po-ebelp
    AND zmmaps_temp~dlv_planno = ''.
    SORT lt_zmmaps_temp2 BY ebeln ebelp.
  ENDIF.
  "上面已经取到POL表里面需要的数据到lt_ekko_po里面，现在需要把还未下发WMS的数据但是已经存在TEMP表中的数据放在lt_ekko_po里面
  IF lt_ekko_po IS NOT INITIAL AND s_planno IS INITIAL.
    SELECT
    zmmaps_temp~*,
    ekpo~lgort AS lgort2,
    ekko~ernam
    APPENDING CORRESPONDING FIELDS OF TABLE @lt_ekko_po
    FROM zmmaps_temp
    INNER JOIN ekpo
    ON ekpo~ebeln = zmmaps_temp~ebeln
    AND ekpo~ebelp = zmmaps_temp~ebelp
    INNER JOIN ekko
    ON ekko~ebeln = ekpo~ebeln
    INNER JOIN mara
    ON mara~matnr = zmmaps_temp~matnr
*    LEFT JOIN zmmwms_pol
*    ON zmmwms_pol~ebeln = zmmaps_temp~ebeln
*    AND zmmwms_pol~ebelp = zmmaps_temp~ebelp
*    FOR ALL ENTRIES IN @lt_ekko_po
*    WHERE zmmaps_temp~ebeln = @lt_ekko_po-ebeln
    WHERE zmmaps_temp~ebeln IN @s_ebeln
    AND zmmaps_temp~werks IN @s_werks
    AND zmmaps_temp~matnr IN @s_matnr
    AND zmmaps_temp~st_code IN @st_code
    AND zmmaps_temp~pdlv_date IN @pdlv_dat
    AND zmmaps_temp~dlv_planno = ''
    AND zmmaps_temp~inactive <> 'X'
    AND ekko~lifnr IN @s_lifnr
    AND mara~matkl IN @s_matkl
    AND ekko~ekgrp IN @s_ekgrp.
  ELSEIF lt_ekko_po IS INITIAL AND s_planno IS INITIAL.
    SELECT
    zmmaps_temp~*,
    ekpo~lgort AS lgort2,
    ekko~ernam
    APPENDING CORRESPONDING FIELDS OF TABLE @lt_ekko_po
    FROM zmmaps_temp
    INNER JOIN ekpo
    ON ekpo~ebeln = zmmaps_temp~ebeln
    AND ekpo~ebelp = zmmaps_temp~ebelp
    INNER JOIN ekko
    ON ekko~ebeln = ekpo~ebeln
    INNER JOIN mara
    ON mara~matnr = zmmaps_temp~matnr
    WHERE zmmaps_temp~dlv_planno = ''
      AND zmmaps_temp~dlv_itemno = ''
      AND zmmaps_temp~ebeln IN @s_ebeln
      AND zmmaps_temp~werks IN @s_werks
      AND zmmaps_temp~matnr IN @s_matnr
      AND zmmaps_temp~st_code IN @st_code
      AND zmmaps_temp~pdlv_date IN @pdlv_dat
      AND zmmaps_temp~inactive <> 'X'
      AND zmmaps_temp~released <> 'X'
      AND ekko~lifnr IN @s_lifnr
      AND mara~matkl IN @s_matkl
      AND ekko~ekgrp IN @s_ekgrp.
  ENDIF.
  "关联PL表，获取APS需求数量
  IF lt_ekko_po IS NOT INITIAL.
    SELECT
    *
    INTO CORRESPONDING FIELDS OF TABLE lt_zmmaps_pl2
    FROM zmmaps_pl
    FOR ALL ENTRIES IN lt_ekko_po
    WHERE zmmaps_pl~guid = lt_ekko_po-guid.
    SORT lt_zmmaps_pl2 BY uniqueid pdlv_date.
  ENDIF.


  "接下来从MATK里面取物料描述
  SELECT
  matnr
  maktx
  INTO CORRESPONDING FIELDS OF TABLE lt_makt
  FROM makt
  FOR ALL ENTRIES IN lt_ekko_po
  WHERE makt~matnr EQ lt_ekko_po-matnr
  AND makt~spras EQ '1'.

  "接下来取供应商名称存到内表里面
  SELECT
  lifnr
  name1
  name2
  name3
  name4
  INTO CORRESPONDING FIELDS OF TABLE lt_lfa1
  FROM lfa1
  FOR ALL ENTRIES IN lt_ekko_po
  WHERE lfa1~lifnr EQ lt_ekko_po-lifnr
  OR lfa1~lifnr EQ lt_ekko_po-producer.
  SORT lt_lfa1 BY lifnr.
















  "接下来循环lt_ekko_po表，然后赋值给ALV的结构
  LOOP AT lt_ekko_po INTO ls_ekko_po.
    "物料编码转换前
    MOVE-CORRESPONDING ls_ekko_po TO ls_alv.
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_ekko_po-matnr.
    IF sy-subrc = 0.
      ls_alv-maktx = ls_makt-maktx.
    ENDIF.
    "取供应商名称
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_ekko_po-lifnr BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE ls_lfa1-name1 ls_lfa1-name2 ls_lfa1-name3 ls_lfa1-name4
                  INTO ls_alv-name1_gp SEPARATED BY ' '.
    ENDIF.
    "制造商名称
    CLEAR ls_lfa1.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_ekko_po-producer.
    IF sy-subrc = 0.
      CONCATENATE ls_lfa1-name1 ls_lfa1-name2 ls_lfa1-name3 ls_lfa1-name4
                  INTO ls_alv-producer_nam SEPARATED BY ' '.
    ENDIF.
    "APS需求日期、最新调整送货日期
    IF ls_ekko_po-origin = 'X'."按APS模式取003的日期
      READ TABLE lt_zmmaps_temp INTO ls_zmmaps_temp WITH KEY uniqueid = ls_ekko_po-uniqueid
                                                              pdlv_date = ls_ekko_po-pdlv_date
                                                              BINARY SEARCH.
      IF sy-subrc = 0.
        ls_alv-reqdate = ls_zmmaps_temp-reqdate.
*        ls_alv-pdlv_date_ext = ls_zmmaps_temp-pdlv_date_ext.
      ENDIF.
      ls_alv-pdlv_date_ext = ls_ekko_po-pdlv_date_ext.
    ELSE."按单取MM020的交货日期,没有计划行的话就取TEMP表中的APS的需求日期
      IF ls_ekko_po-dlv_planno IS INITIAL.
        ls_alv-reqdate = ls_ekko_po-reqdate.
        ls_alv-pdlv_date_ext = ''.
      ELSE.
        ls_alv-reqdate = ls_ekko_po-pdlv_date.
        ls_alv-pdlv_date_ext = ls_ekko_po-pdlv_date_ext.
      ENDIF.
    ENDIF.
    CLEAR ls_zmmaps_temp.
    "实际到货日期ACTUAL_DATE
    IF ls_ekko_po-dlv_planno IS NOT INITIAL AND ls_ekko_po-dlv_itemno IS NOT INITIAL.
      READ TABLE lt_mkpf INTO DATA(ls_mkpf) WITH KEY dlv_planno = ls_ekko_po-dlv_planno
                                                     dlv_itemno = ls_ekko_po-dlv_itemno BINARY SEARCH.
      ls_alv-actual_date = ls_mkpf-bldat.
    ENDIF.
    "基本计量单位文本
    READ TABLE lt_t006a INTO ls_t006a WITH KEY msehi = ls_ekko_po-meins BINARY SEARCH.
    IF sy-subrc = 0.
      ls_alv-mseht = ls_t006a-mseht.
    ENDIF.
    "APS需求数量APSREQ_MENGE
    READ TABLE lt_zmmaps_pl2 INTO ls_zmmaps_pl WITH KEY guid =  ls_ekko_po-guid.
    IF sy-subrc = 0.
      ls_alv-apsreq_menge = ls_zmmaps_pl-aps_menge.
    ENDIF.
    IF ls_ekko_po-origin = 'M'."如果是按单的话，取原订单数量
      ls_alv-apsreq_menge = ls_ekko_po-menge.
    ENDIF.
    "净收货量:借方减去贷方
*    READ TABLE lt_zmatdoc INTO ls_zmatdoc WITH KEY dlv_planno = ls_ekko_po-dlv_planno
*                                                   dlv_itemno = ls_ekko_po-dlv_itemno
*                                                   shkzg = 'S'
*                                                   BINARY SEARCH.
    LOOP AT lt_zmatdoc INTO ls_zmatdoc WHERE dlv_planno = ls_ekko_po-dlv_planno AND dlv_itemno = ls_ekko_po-dlv_itemno AND shkzg = 'S'.
      ls_alv-recv_menge = ls_alv-recv_menge + ls_zmatdoc-menge.
      CLEAR ls_zmatdoc.
    ENDLOOP.
    IF sy-subrc = 0."上面查到的是收货的数量
*      ls_alv-recv_menge = ls_zmatdoc-menge.
      ls_alv-charg = ls_zmatdoc-charg.
    ENDIF.
    CLEAR ls_zmatdoc.
*    READ TABLE lt_zmatdoc INTO ls_zmatdoc WITH KEY dlv_planno = ls_ekko_po-dlv_planno
*                                                   dlv_itemno = ls_ekko_po-dlv_itemno
*                                                   shkzg = 'H'
*                                                   BINARY SEARCH.
    LOOP AT lt_zmatdoc INTO ls_zmatdoc WHERE dlv_planno = ls_ekko_po-dlv_planno AND dlv_itemno = ls_ekko_po-dlv_itemno AND shkzg = 'H'.
      ls_alv-recv_menge = ls_alv-recv_menge - ls_zmatdoc-menge.
    ENDLOOP.
    IF sy-subrc = 0."上面查到的是退货数量，需要用收货数量减去退货数量
*      ls_alv-recv_menge = ls_alv-recv_menge - ls_zmatdoc-menge.
      ls_alv-charg = ls_zmatdoc-charg.
    ENDIF.
    CLEAR ls_zmatdoc.
    "净收货量
*    ls_alv-recv_menge = ls_ekko_po-received_quan.
    "剩余未到货量
    ls_alv-undlv_mende = ls_alv-pdlv_menge - ls_alv-recv_menge.
    "未到货延期天数
    "首先取批次分类特性最后一次收货日期
    DATA:lv_atinn TYPE ausp-atinn.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'ZLWEDT'
      IMPORTING
        output = lv_atinn.
    SELECT SINGLE
    atflv
    INTO ls_alv-zlwedt2
    FROM ausp
    INNER JOIN mcha
    ON ausp~objek = mcha~cuobj_bm
    WHERE ausp~atinn = lv_atinn
    AND ausp~klart = '022'
    AND mcha~matnr = ls_ekko_po-matnr
    AND mcha~charg = ls_alv-charg
    AND mcha~werks = ls_ekko_po-werks.
    DATA lv_date(10) TYPE c.
    CALL FUNCTION 'CONVERSION_EXIT_FLTPD_OUTPUT'
      EXPORTING
        input  = ls_alv-zlwedt2
      IMPORTING
        output = lv_date.
    REPLACE '.' INTO lv_date WITH ''.
    REPLACE '.' INTO lv_date WITH ''.
    CONDENSE lv_date NO-GAPS.
    ls_alv-zlwedt = lv_date.
    IF ( ls_alv-recv_menge > 0 AND ls_ekko_po-xclosed = 'X' ) OR ls_alv-recv_menge >= ls_alv-pdlv_menge.
      ls_alv-delay = ls_alv-zlwedt - ls_alv-pdlv_date.
    ELSEIF ls_alv-undlv_mende IS NOT INITIAL AND ls_ekko_po-xclosed = ''.
      ls_alv-delay = sy-datum - ls_alv-pdlv_date.
    ENDIF.
*    ls_ekko_po-xclosed = 'X'.
*    ls_alv-delay = ''.
    "当前交货次数COUNT_YEAR
    SELECT
    COUNT(*)
    INTO @DATA(lv_num1)
    FROM matdoc
    WHERE matdoc~lifnr = @ls_ekko_po-lifnr
      AND matdoc~bwart = '101'.

    SELECT
    COUNT(*)
    INTO @DATA(lv_num2)
    FROM matdoc
    WHERE matdoc~lifnr = @ls_ekko_po-lifnr
      AND matdoc~bwart = '102'.
    ls_alv-count_year = lv_num1 - lv_num2.
    CLEAR:lv_num1,lv_num2.
    "本单到货次数
    "先获取MATDOC表中移动类型101的总次数
    SELECT COUNT( * ) AS number1
    FROM matdoc
    WHERE matdoc~ebeln = @ls_ekko_po-ebeln
      AND matdoc~bwart = '101'
    INTO @DATA(count1).
    "然后获取MATDOC表中移动类型102的总次数
    SELECT COUNT( * ) AS number2
    FROM matdoc
    WHERE matdoc~ebeln = @ls_ekko_po-ebeln
    AND matdoc~bwart = '102'
    INTO @DATA(count2).
    ls_alv-po_num = count1.
    "最小订购批量
    READ TABLE lt_marc INTO ls_marc WITH KEY matnr = ls_ekko_po-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      ls_alv-min_dlv = ls_marc-bstmi.
    ENDIF.
    "计划来源
    "交付日期（版本号）
    IF ls_ekko_po-origin = 'X'.
      ls_alv-origin = 'APS'.
      SELECT SINGLE
        version
      INTO ls_alv-pay_date
      FROM zmmaps_temp
      WHERE zmmaps_temp~dlv_planno = ls_ekko_po-dlv_planno
        AND zmmaps_temp~dlv_itemno = ls_ekko_po-dlv_itemno.
    ELSEIF ls_ekko_po-origin = 'M'.
      ls_alv-origin = '按单'.
      SELECT SINGLE
        bedat
      INTO ls_alv-pay_date
      FROM ekko
      WHERE ekko~ebeln = ls_ekko_po-ebeln.
    ENDIF.
    "采购部门负责人
    CALL FUNCTION 'ZGET_USERNAME'
      EXPORTING
        i_uname = ls_ekko_po-ernam
      IMPORTING
        e_uname = ls_alv-responsible.
    "原产地
    DATA:ls_atinn LIKE zmaterial_tx_value,
         lt_atinn LIKE TABLE OF ls_atinn.
    CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
      EXPORTING
        matnr  = ls_ekko_po-matnr
        atnam  = 'ZORIADD'
      TABLES
        output = lt_atinn.
    READ TABLE lt_atinn INTO ls_atinn INDEX 1.
    IF sy-subrc = 0.
      ls_alv-zoriadd = ls_atinn-atwrt.
    ENDIF.
    CLEAR ls_atinn.
    "贵重标识
    CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
      EXPORTING
        matnr  = ls_ekko_po-matnr
        atnam  = 'ZEXPMAT'
      TABLES
        output = lt_atinn.
    READ TABLE lt_atinn INTO ls_atinn INDEX 1.
    IF sy-subrc = 0.
      ls_alv-xprecious = ls_atinn-atwrt.
    ENDIF.
    CLEAR ls_atinn.
    "质检放行数量
    "1.首先取直接放行数量
    SELECT SINGLE
      menge
    INTO @DATA(lv_number)
    FROM matdoc
    WHERE matdoc~bwart = '321'
      AND matdoc~ebeln = @ls_ekko_po-ebeln
      AND matdoc~ebelp = @ls_ekko_po-ebelp.
    "2获取简介放行数量
    "2.1获取343的数量
    SELECT SINGLE
      charg,
      lgort
    INTO @DATA(lv_matdoc)
    FROM matdoc
    WHERE matdoc~bwart = '101'
      AND matdoc~ebeln = @ls_ekko_po-ebeln
      AND matdoc~ebelp = @ls_ekko_po-ebelp.
    IF lv_matdoc-charg IS NOT INITIAL.
      SELECT
      SUM( menge )
      INTO @DATA(lv_number2)
            FROM matdoc
            WHERE matdoc~charg = @lv_matdoc-charg
            AND matdoc~bwart = '343'.
      SELECT
      SUM( menge )
      INTO @DATA(lv_number3)
            FROM matdoc
            WHERE matdoc~charg = @lv_matdoc-charg
            AND matdoc~bwart = '344'.
      ls_alv-number = lv_number + lv_number2 - lv_number3.
    ELSE.
      CLEAR ls_alv-number.
    ENDIF.



    "2.2获取344的数量
    "关闭标志
    ls_alv-close = ls_ekko_po-xclosed.
    IF p_closed2 IS NOT INITIAL.
      CHECK ls_alv-close EQ p_closed2.
    ENDIF.
    "答复状态
*    READ TABLE lt_zmmaps_temp INTO ls_zmmaps_temp WITH KEY dlv_planno = ls_ekko_po-dlv_planno
*                                                           dlv_itemno = ls_ekko_po-dlv_itemno.
*    IF sy-subrc = 0.
*      IF ls_zmmaps_temp-pdlv_date IS INITIAL.
*        ls_alv-answer = '未答复'.
*      ELSE.
*        ls_alv-answer  = '已答复'.
*      ENDIF.
*    ENDIF.
    IF ls_alv-pdlv_date IS NOT INITIAL AND ls_alv-dlv_planno IS NOT INITIAL.
      ls_alv-answer = '已下传'.
    ELSEIF ls_alv-pdlv_date IS NOT INITIAL AND ls_alv-dlv_planno IS INITIAL.
      ls_alv-answer = '已答复未下传'.
    ELSE.
      ls_alv-answer = '已匹配未答复'.
    ENDIF.
    "如果POL表里面有数据，但是数据还没有下传，去掉垃圾项数据，从TEMP表里面取,答复状态改成已答复未下传
    READ TABLE lt_zmmaps_temp2 INTO ls_zmmaps_temp WITH KEY ebeln = ls_ekko_po-ebeln
                                                            ebelp = ls_ekko_po-ebelp
                                                            reqdate = ls_alv-reqdate.
    IF sy-subrc = 0.
      ls_alv-answer = '已匹配未答复'.
    ENDIF.
    IF ls_alv-pdlv_date IS NOT INITIAL AND ls_alv-dlv_planno IS NOT INITIAL.
      ls_alv-answer = '已下传'.
    ELSEIF ls_alv-pdlv_date IS NOT INITIAL AND ls_alv-dlv_planno IS INITIAL.
      ls_alv-answer = '已答复未下传'.
    ELSE.
      ls_alv-answer = '已匹配未答复'.
    ENDIF.
    IF ls_alv-answer = '已答复未下传' OR ls_alv-answer = '已匹配未答复'.
      "未答复的确认计划送货数量取TEMP表里面的分配数量
      SELECT SINGLE
      assign_menge
      INTO ls_alv-pdlv_menge
      FROM zmmaps_temp
      WHERE zmmaps_temp~ebeln = ls_ekko_po-ebeln
      AND zmmaps_temp~ebelp = ls_ekko_po-ebelp
      AND zmmaps_temp~inactive <> 'X'
      AND zmmaps_temp~released <> 'X'.
      "未答复的制造商和制造商名称
      SELECT SINGLE
        producer
      INTO ls_alv-producer
      FROM zmmwms_pol
      WHERE zmmwms_pol~ebeln = ls_ekko_po-ebeln
        AND zmmwms_pol~ebelp = ls_ekko_po-ebelp.
      "名称
      READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = ls_alv-producer.
      IF sy-subrc = 0.
        CONCATENATE ls_lfa1-name1 ls_lfa1-name2 ls_lfa1-name3 ls_lfa1-name4
        INTO ls_alv-producer_nam SEPARATED BY ' '.
      ENDIF.
      ls_alv-origin = 'APS'.
      "采购部门负责人
      SELECT SINGLE
        ernam
      INTO ls_ekko_po-ernam
      FROM ekko
      WHERE ekko~ebeln = ls_ekko_po-ebeln.
      CALL FUNCTION 'ZGET_USERNAME'
        EXPORTING
          i_uname = ls_ekko_po-ernam
        IMPORTING
          e_uname = ls_alv-responsible.
      "未答复的未到货延期天数是空的
      CLEAR ls_alv-delay.
    ENDIF.
    "加入状态的筛选条件
    IF part_a <> 'X' AND ls_alv-answer = '已下传'.
      CONTINUE.
    ELSEIF part_b <> 'X' AND ls_alv-answer = '已答复未下传'.
      CONTINUE.
    ELSEIF part_c <> 'X' AND ls_alv-answer = '已匹配未答复'.
      CONTINUE.
    ENDIF.
    "质检状态数量
    "非限制状态数量
    "冻结状态数量
    READ TABLE lt_zmatdoc INTO ls_zmatdoc WITH KEY dlv_planno = ls_ekko_po-dlv_planno
                                                   dlv_itemno = ls_ekko_po-dlv_itemno.
    IF sy-subrc = 0.
      ls_alv-quality_num = ls_zmatdoc-menge.
      SELECT
        SUM( clabs )
        SUM( cspem )
      INTO (ls_alv-noquality_num,ls_alv-frozen_num)
      FROM mchb
      WHERE mchb~charg = ls_zmatdoc-charg.
    ENDIF.
    CLEAR ls_zmatdoc.

    "确认退货数量
    IF ls_ekko_po-dlv_planno IS NOT INITIAL AND ls_ekko_po-dlv_itemno IS NOT INITIAL.
      SELECT
      SUM( zrequ )
      INTO ls_alv-return_num
      FROM zmmtreord
      WHERE zmmtreord~dlv_planno = ls_ekko_po-dlv_planno
      AND zmmtreord~dlv_itemno = ls_ekko_po-dlv_itemno
      AND zstat = 'C'
      AND xclosed = ''.
    ENDIF.
    "送货地址
    ls_alv-delivery_add = ls_ekko_po-lgort2.

    "原订单到货时间
    SELECT SINGLE
      eindt
    INTO ls_alv-or_order
    FROM eket
    WHERE eket~ebeln = ls_ekko_po-ebeln
      AND eket~ebelp = ls_ekko_po-ebelp.
    IF ls_ekko_po-origin = 'M'."如果是按单的话，原订单到货时间为订单交货日期
      ls_alv-or_order = ls_ekko_po-pdlv_date.
    ENDIF.
    "原订单数量
    ls_alv-or_order_num = ls_ekko_po-menge.
    IF ls_alv-or_order_num IS INITIAL AND ls_alv-dlv_planno IS INITIAL.
      SELECT SINGLE
      menge
      INTO ls_alv-or_order_num
      FROM ekpo
      WHERE ekpo~ebeln = ls_ekko_po-ebeln
      AND ekpo~ebelp = ls_ekko_po-ebelp.
    ENDIF.
    "覆盖PAY_DATE版本号
    IF ls_alv-pay_date IS INITIAL.
      SELECT SINGLE
        version
      FROM zmmaps_temp
      INTO ls_alv-pay_date
      WHERE zmmaps_temp~dlv_itemno = ls_alv-dlv_itemno
        AND zmmaps_temp~dlv_planno = ls_alv-dlv_planno
        AND zmmaps_temp~ebeln = ls_alv-ebeln
        AND zmmaps_temp~ebelp = ls_alv-ebelp.
    ENDIF.
    "预计到货天数
    IF ls_ekko_po-pdlv_date_ext IS NOT INITIAL AND ls_ekko_po-pdlv_date IS NOT INITIAL AND ls_ekko_po-pdlv_date_ext <> ''.
      ls_alv-arrive_day = ls_ekko_po-pdlv_date_ext - sy-datum.
    ELSE.
      ls_alv-arrive_day = ls_ekko_po-pdlv_date - sy-datum.
    ENDIF.
    IF ls_ekko_po-dlv_planno IS INITIAL.
      ls_alv-arrive_day = ''.
    ENDIF.
    "未答复的话是没有到货日期
    IF ls_alv-answer = '已匹配未答复' OR ls_ekko_po-xclosed = 'X'.
      ls_alv-delay = ''.
    ENDIF.
    "未到货延期天数优化
    IF ls_alv-delay <= 0.
      ls_alv-delay = ''.
    ENDIF.




    "设置状态灯

    IF ls_alv-actual_date > ls_alv-pdlv_date."延期
      ls_alv-light = '@0A@'."红灯
    ELSEIF ls_alv-actual_date <= ls_alv-pdlv_date AND ls_alv-undlv_mende >= 0."黄灯
      ls_alv-light = '@09@'."黄灯
    ELSE.
      ls_alv-light = '@08@'."绿灯
    ENDIF.















    "物料编码转换后
    ls_alv-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_alv-matnr i_out = 'X' ).

    APPEND ls_alv TO lt_alv.
    CLEAR ls_alv.
    CLEAR ls_makt.
    CLEAR: lv_number,lv_matdoc,lv_number2,lv_number3.
  ENDLOOP.

ENDFORM.
FORM frm_init_list.
  "下拉框代码实现
  DATA :list  TYPE vrm_values , "list box的值列表
        value LIKE LINE OF list . "list box的结构
  value-key = '1'.
  value-text = '未关闭'.
  APPEND value TO list .
  value-key = '2'.
  value-text = '已关闭'.
  APPEND value TO list .
  "调用函数展示
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'p_closed'
      values = list.
ENDFORM.

FORM catalog."表头
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'L'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.


  fieldcatset 'LIGHT'         '状态指示灯' sy-tabix.
  fieldcatset 'DLV_PLANNO'     '送货计划号' sy-tabix.
  fieldcatset 'DLV_ITEMNO'     '送货计划行' sy-tabix.
  fieldcatset 'EBELN'         '采购订单号' sy-tabix.
  fieldcatset 'EBELP'         '采购订单行' sy-tabix.
  fieldcatset 'LIFNR'         '供应商编号' sy-tabix.
  fieldcatset 'NAME1_GP'      '供应商名称' sy-tabix.
  fieldcatset 'PDLV_DATE'     '答复送货日期' sy-tabix.
  fieldcatset 'ARRIVE_DAY'    '预计到货天数' sy-tabix.
  fieldcatset 'MATNR'         '物料编号' sy-tabix  .
  fieldcatset 'MAKTX'         '物料描述' sy-tabix  .
  fieldcatset 'PRODUCER'      '制造商编号' sy-tabix  .
  fieldcatset 'PRODUCER_NAM'  '制造商名称' sy-tabix  .
  fieldcatset 'REQDATE'  'APS需求日期' sy-tabix  .
  fieldcatset 'PDLV_DATE_EXT' '最新调整送货日期' sy-tabix  .
  fieldcatset 'ACTUAL_DATE'   '实际到货日期' sy-tabix  .
*  fieldcatset 'MEINS'         '基本计量单位' sy-tabix  .
  fieldcatset 'MSEHT'         '基本计量单位文本' sy-tabix  .
  fieldcatset 'APSREQ_MENGE'  'APS需求数量' sy-tabix  .
  fieldcatset 'PDLV_MENGE'    '确认计划送货数量' sy-tabix  .
  fieldcatset 'RECV_MENGE'    '确认日期净收货量' sy-tabix  .
  fieldcatset 'UNDLV_MENDE'   '剩余未到货量' sy-tabix  .
  fieldcatset 'DELAY'         '未到货延期天数' sy-tabix  .
*  fieldcatset 'AVG_DELAY'     '加权延期 天数' sy-tabix  .
  fieldcatset 'COUNT_YEAR'    '当前交货次数' sy-tabix  .
  fieldcatset 'PO_NUM'        '本单到货次数' sy-tabix  .
  fieldcatset 'MIN_DLV'       '最小订购批量' sy-tabix  .
  fieldcatset 'ORIGIN'        '计划来源' sy-tabix  .
  fieldcatset 'RESPONSIBLE'   '采购部门负责人' sy-tabix  .
  fieldcatset 'ZORIADD'       '原产地' sy-tabix  .
  fieldcatset 'XPRECIOUS'     '贵重标识' sy-tabix  .
  fieldcatset 'NUMBER'        '质检放行数量' sy-tabix  .
  fieldcatset 'CLOSE'         '关闭标志' sy-tabix  .
  fieldcatset 'ANSWER'        '答复状态' sy-tabix  .
  fieldcatset 'QUALITY_NUM'   '质检状态数量' sy-tabix  .
  fieldcatset 'NOQUALITY_NUM' '非限制状态数量' sy-tabix  .
  fieldcatset 'FROZEN_NUM'    '冻结状态 数量' sy-tabix  .
  fieldcatset 'RETURN_NUM'    '确认退货数量' sy-tabix  .
  fieldcatset 'DELIVERY_ADD'  '送货地址' sy-tabix  .
  fieldcatset 'PAY_DATE'      '交付日期(版本号)' sy-tabix  .
  fieldcatset 'OR_ORDER'      '原订单到货时间' sy-tabix  .
  fieldcatset 'OR_ORDER_NUM'  '原订单数量' sy-tabix  .


  READ TABLE fieldcat INDEX 4.
*  fieldcat-hotspot = 'X'.
  fieldcat-emphasize = 'C500'.
  MODIFY fieldcat INDEX 4.



*layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'."斑马线的样式

ENDFORM.
*响应鼠标点击时间的子程序
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
      READ TABLE lbkpf INDEX rs_selfield-tabindex.
      SET PARAMETER ID 'BLN' FIELD lbkpf-belnr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.

FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS '0100'.
ENDFORM.



FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = w_repid "程序名称
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
*     I_CALLBACK_USER_COMMAND  = 'ALV_USER_COMMAND'"对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = '计划订单跟踪' "标题名
*     I_GRID_SETTINGS          =
      is_layout                = layout "程序所定义的layout名称
      it_fieldcat              = fieldcat[] "定义fieldcat数据
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
      i_default                = 'X'
      i_save                   = 'A'
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = lt_alv
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

INCLUDE zhkdemo3_top.