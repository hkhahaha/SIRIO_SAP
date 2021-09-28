*&---------------------------------------------------------------------*
*& Report ZFIR0065
*&---------------------------------------------------------------------*
*&核算项目明细账查询及打印
*&---------------------------------------------------------------------*
REPORT zfir0119.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名

TYPES:BEGIN OF ty_all,
        sel         TYPE c, " 用来标识选择的字段
        style(10),"类型：期初余额、本期明细、本期发生额、本年累计发生额
        rbukrs      TYPE    acdoca-rbukrs , "  公司代码
        racct       TYPE    acdoca-racct  , "  科目号
        txt20       TYPE    skat-txt20  , "  科目描述
        budat       TYPE    acdoca-budat  , "  过账日期
        belnr       TYPE    acdoca-belnr  , "  凭证编号
        sgtxt       TYPE    acdoca-sgtxt  , "  凭证行项目文本
        rcntr       TYPE    acdoca-rcntr  , "  成本中心
        rfarea      TYPE    acdoca-rfarea , "  功能范围
        rfareat(50)     ,"  功能范围描述
        kunnr       TYPE    acdoca-kunnr  , "  客户编码
        kunnrt(70),"  客户描述
        lifnr       TYPE    acdoca-lifnr  , "  供应商编码
        lifnrt(70),"  供应商描述
        hbkid       TYPE    acdoca-hbkid  , "  开户银行
        hktid       TYPE    acdoca-hktid  , "  账户标识
        text1       TYPE    t012t-text1 , "  账户描述
        rwcur       TYPE    acdoca-rwcur  , "  交易货币
        kursf       TYPE    bkpf-kursf  , "  汇率
        wsl         TYPE    acdoca-wsl  , "  交易货币
        hsl         TYPE    acdoca-hsl  , "  本位币
        wsl1        TYPE    acdoca-wsl  , "  借方交易货币
        hsl1        TYPE    acdoca-hsl  , "  借方本位币
        wsl2        TYPE    acdoca-wsl  , "  贷方交易货币
        hsl2        TYPE    acdoca-hsl  , "  贷方本位币,
        wsla        TYPE    acdoca-wsl  , "  贷方交易货币
        hsla        TYPE    acdoca-hsl  , "  贷方本位币,
        wslb        TYPE    acdoca-wsl  , "  贷方交易货币
        hslb        TYPE    acdoca-hsl  , "  贷方本位币,
        drcrk       TYPE acdoca-drcrk, "借贷标识
        type(5),"借贷标识
        gvtyp       TYPE ska1-gvtyp, "损益科目
        fiscyearper TYPE acdoca-fiscyearper, "期间
        bldat       TYPE acdoca-bldat, "凭证日期
        id          TYPE zbc_guid, "唯一标识，用于打印区分数据

      END OF ty_all.

DATA:lt_all      TYPE TABLE OF ty_all,
     ls_all      LIKE LINE OF lt_all,
     lt_alla1    TYPE TABLE OF ty_all,
     lt_alla1a   TYPE TABLE OF ty_all,
     lt_alla1b   TYPE TABLE OF ty_all,
     lt_alla2    TYPE TABLE OF ty_all,
     lt_alla2a   TYPE TABLE OF ty_all,
     lt_alla2b   TYPE TABLE OF ty_all,
     lt_alla3    TYPE TABLE OF ty_all,
     lt_alla4    TYPE TABLE OF ty_all,
     lt_alld1    TYPE TABLE OF ty_all,
     lt_alld1a   TYPE TABLE OF ty_all,
     lt_alld1b   TYPE TABLE OF ty_all,
     lt_alld2    TYPE TABLE OF ty_all,
     lt_alld2a   TYPE TABLE OF ty_all,
     lt_alld2b   TYPE TABLE OF ty_all,
     lt_alld3a   TYPE TABLE OF ty_all,
     lt_alld3b   TYPE TABLE OF ty_all,
     lt_alld4    TYPE TABLE OF ty_all,
     lt_alld4a   TYPE TABLE OF ty_all,
     lt_alld4b   TYPE TABLE OF ty_all,
     lt_all_temp TYPE TABLE OF ty_all,
     lt_detail   TYPE TABLE OF ty_all,
     ls_alla1    LIKE LINE OF lt_alla1,
     ls_alla1a   LIKE LINE OF lt_alla1,
     ls_alla1b   LIKE LINE OF lt_alla1,
     ls_alld1    LIKE LINE OF lt_alla1,
     ls_alld1a   LIKE LINE OF lt_alla1,
     ls_alld1b   LIKE LINE OF lt_alla1,
     ls_alla2    LIKE LINE OF lt_alla2,
     ls_alla2a   LIKE LINE OF lt_alla2,
     ls_alla2b   LIKE LINE OF lt_alla2,
     ls_alla3    LIKE LINE OF lt_alla2,
     ls_alla3a   LIKE LINE OF lt_alla2,
     ls_alla3b   LIKE LINE OF lt_alla2,
     ls_alla4    LIKE LINE OF lt_alla2,
     ls_alla4a   LIKE LINE OF lt_alla2,
     ls_alla4b   LIKE LINE OF lt_alla2,
     ls_alld2    LIKE LINE OF lt_alla2,
     ls_alld2a   LIKE LINE OF lt_alla2,
     ls_alld2b   LIKE LINE OF lt_alla2,
     ls_alld3    LIKE LINE OF lt_alla2,
     ls_alld3a   LIKE LINE OF lt_alla2,
     ls_alld3b   LIKE LINE OF lt_alla2,
     ls_alld4    LIKE LINE OF lt_alla2,
     ls_alld4a   LIKE LINE OF lt_alla2,
     ls_alld4b   LIKE LINE OF lt_alla2.

DATA:lt_out   TYPE TABLE OF ty_all, "ALV输出
     lt_print TYPE TABLE OF ty_all,
     ls_out   LIKE LINE OF lt_out,
     ls_print LIKE LINE OF lt_out,
     ls_out_t LIKE LINE OF lt_out.

DATA lv_gjahr TYPE acdoca-gjahr.
DATA lv_budat TYPE acdoca-budat.
DATA lv_budat2 TYPE acdoca-budat.
DATA lv_id TYPE zbc_guid.
DATA:lv_flag(2).

DATA: fm_name TYPE rs38l_fnam.
DATA:
  lw_output  TYPE ssfcompop,
  lw_control TYPE ssfctrlop.

DATA: ls_zfir0019a_head   LIKE zfir0019a_head,
      lt_zfir0019a_detail LIKE TABLE OF zfir0019a_detail,
      ls_zfir0019a_detail LIKE LINE OF lt_zfir0019a_detail.
"本期发生额
DATA:g_wsl1 TYPE acdoca-wsl, "借方交易货币
     g_hsl1 TYPE acdoca-hsl, "借方本位币
     g_wsl2 TYPE acdoca-wsl, "贷方交易货币
     g_hsl2 TYPE acdoca-hsl. "贷方本位币
TABLES:bkpf,acdoca.

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
PARAMETERS:p_rbukrs LIKE acdoca-rbukrs OBLIGATORY,
           p_year   LIKE acdoca-fiscyearper OBLIGATORY.
*           p_rfarea LIKE acdoca-rfarea.
SELECT-OPTIONS:
           p_rfarea FOR acdoca-rfarea NO INTERVALS.
SELECT-OPTIONS:s_kunnr FOR acdoca-kunnr,
               s_lifnr FOR acdoca-lifnr,
               s_racct FOR acdoca-racct.
SELECTION-SCREEN END OF BLOCK blk.

START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.

FORM getdata.
  "获取年度
  lv_gjahr = p_year+0(4).
  "首先，先获取所需要的科目，作为基底
  "(1)如果科目编码SKA1-SAKNR=1001010000-1012999999,按照科目+开户银行+账号标识汇总数据
  "[1]获得基础数据，包含的是（1）类别下的所有科目、公司代码，不区分借贷方
  SELECT DISTINCT
    rbukrs
    racct
    hbkid
    hktid
    rwcur
  FROM acdoca
  INTO  CORRESPONDING FIELDS OF TABLE lt_alla1
  WHERE racct BETWEEN 1001010000 AND 1012999999
    AND rbukrs = p_rbukrs
    AND fiscyearper < p_year
    AND rfarea IN p_rfarea
    AND kunnr IN s_kunnr
    AND lifnr IN s_lifnr
    AND racct IN s_racct
    AND blart <> ''
  GROUP BY racct hbkid hktid rbukrs rwcur.

  "[2]获得借方的数据，保存在lt_alla1a表中
  SELECT
    rbukrs
    racct
    hbkid
    hktid
    rwcur
    SUM( wsl ) AS wsl
    SUM( hsl ) AS hsl
  FROM acdoca
  INTO  CORRESPONDING FIELDS OF TABLE lt_alla1a
  WHERE racct BETWEEN 1001010000 AND 1012999999
    AND rbukrs = p_rbukrs
    AND fiscyearper < p_year
    AND rfarea IN p_rfarea
    AND kunnr IN s_kunnr
    AND lifnr IN s_lifnr
    AND racct IN s_racct
    AND blart <> ''
    AND drcrk = 'S'
  GROUP BY racct hbkid hktid rbukrs rwcur.

  "本年累计相关，获取（1）类数据的本年累计借方数据
  SELECT
   rbukrs
   racct
   hbkid
   hktid
   rwcur
   SUM( wsl ) AS wsl
   SUM( hsl ) AS hsl
 FROM acdoca
 INTO  CORRESPONDING FIELDS OF TABLE lt_alld1a
 WHERE racct BETWEEN 1001010000 AND 1012999999
   AND rbukrs = p_rbukrs
   AND fiscyearper <= p_year
   AND rfarea IN p_rfarea
   AND kunnr IN s_kunnr
   AND lifnr IN s_lifnr
   AND racct IN s_racct
   AND blart <> ''
   AND drcrk = 'S'
   AND gjahr = lv_gjahr
 GROUP BY racct hbkid hktid rbukrs rwcur.
  "[3]获取贷方数据，保存在LT_ALLA1B表中
  SELECT
    rbukrs
    racct
    hbkid
    hktid
    rwcur
    SUM( wsl ) AS wsl
    SUM( hsl ) AS hsl
  FROM acdoca
  INTO  CORRESPONDING FIELDS OF TABLE lt_alla1b
  WHERE racct BETWEEN 1001010000 AND 1012999999
    AND rbukrs = p_rbukrs
    AND fiscyearper <= p_year
    AND rfarea IN p_rfarea
    AND kunnr IN s_kunnr
    AND lifnr IN s_lifnr
    AND racct IN s_racct
    AND blart <> ''
    AND drcrk = 'H'
  GROUP BY racct hbkid hktid rbukrs rwcur.
  "本年累计相关，获取（1）类数据的本年累计贷方数据
  SELECT
  rbukrs
  racct
  hbkid
  hktid
  rwcur
  SUM( wsl ) AS wsl
  SUM( hsl ) AS hsl
FROM acdoca
INTO  CORRESPONDING FIELDS OF TABLE lt_alld1b
WHERE racct BETWEEN 1001010000 AND 1012999999
  AND rbukrs = p_rbukrs
  AND fiscyearper <= p_year
  AND rfarea IN p_rfarea
  AND kunnr IN s_kunnr
  AND lifnr IN s_lifnr
  AND racct IN s_racct
  AND blart <> ''
  AND drcrk = 'H'
  AND gjahr = lv_gjahr
GROUP BY racct hbkid hktid rbukrs rwcur.

  "(2)如果科目编码SKA1-SAKNR不等于1001010000-1012999999，且SKA1-GVTYP（损益）=X，按照科目+功能范围汇总数据

  SELECT
  rbukrs,
  racct,
  rfarea,
  rwcur,
  SUM( wsl ) AS wsl,
  SUM( hsl ) AS hsl
  FROM acdoca
  LEFT JOIN ska1
  ON ska1~saknr = acdoca~racct
  AND ska1~ktopl = acdoca~ktopl
  WHERE racct NOT BETWEEN 1001010000 AND 1012999999
    AND rbukrs = @p_rbukrs
    AND fiscyearper < @p_year
    AND rfarea IN @p_rfarea
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr
    AND racct IN @s_racct
    AND gvtyp = 'X'
    AND blart <> ''
    AND acdoca~ktopl = '1000'
  GROUP BY racct,rfarea,rbukrs,rwcur
  INTO  CORRESPONDING FIELDS OF TABLE @lt_alla2.
  "[1]本年累计相关，获取(2)类的本年累计借方数据
  SELECT
 rbukrs,
 racct,
 rfarea,
 rwcur,
 SUM( wsl ) AS wsl,
 SUM( hsl ) AS hsl
 FROM acdoca
 LEFT JOIN ska1
 ON ska1~saknr = acdoca~racct
 AND ska1~ktopl = acdoca~ktopl
 WHERE racct NOT BETWEEN 1001010000 AND 1012999999
   AND rbukrs = @p_rbukrs
   AND fiscyearper <= @p_year
   AND rfarea IN @p_rfarea
   AND kunnr IN @s_kunnr
   AND lifnr IN @s_lifnr
   AND racct IN @s_racct
   AND gvtyp = 'X'
   AND blart <> ''
   AND acdoca~ktopl = '1000'
   AND drcrk = 'S'
  AND gjahr = @lv_gjahr
 GROUP BY racct,rfarea,rbukrs,rwcur
 INTO  CORRESPONDING FIELDS OF TABLE @lt_alld2a.

  "[2]本年累计相关，获取(2)类的本年累计贷方数据
  SELECT
  rbukrs,
  racct,
  rfarea,
  rwcur,
  SUM( wsl ) AS wsl,
  SUM( hsl ) AS hsl
  FROM acdoca
  LEFT JOIN ska1
  ON ska1~saknr = acdoca~racct
  AND ska1~ktopl = acdoca~ktopl
  WHERE racct NOT BETWEEN 1001010000 AND 1012999999
    AND rbukrs = @p_rbukrs
    AND fiscyearper <= @p_year
    AND rfarea IN @p_rfarea
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr
    AND racct IN @s_racct
    AND gvtyp = 'X'
    AND blart <> ''
    AND acdoca~ktopl = '1000'
    AND drcrk = 'H'
   AND gjahr = @lv_gjahr
  GROUP BY racct,rfarea,rbukrs,rwcur
  INTO  CORRESPONDING FIELDS OF TABLE @lt_alld2b.

  "(3)如果科目编码SKA1-SAKNR不等于1001010000-1012999999，且SKA1-GVTYP（损益）不等于X，且统御标识SKB1-MITKZ=K/D,按照科目+客户+供应商汇总数据
  SELECT
  rbukrs,
  rwcur,
  racct,
  rfarea,
  SUM( wsl ) AS wsl,
  SUM( hsl ) AS hsl
  FROM acdoca
  LEFT JOIN ska1
  ON ska1~saknr = acdoca~racct
  AND ska1~ktopl = acdoca~ktopl
  INNER JOIN skb1
  ON skb1~saknr = acdoca~racct
  AND skb1~bukrs = acdoca~rbukrs
  WHERE racct NOT BETWEEN 1001010000 AND 1012999999
    AND rbukrs = @p_rbukrs
    AND fiscyearper < @p_year
    AND rfarea IN @p_rfarea
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr
    AND racct IN @s_racct
    AND gvtyp <> 'X'
    AND blart <> ''
    AND mitkz IN ('K','D')
    AND acdoca~ktopl = '1000'
  GROUP BY racct,rfarea,rbukrs,rwcur
  INTO  CORRESPONDING FIELDS OF TABLE @lt_alla3.

  "[1]本年累计相关，获取(3)类的本年累计借方数据
  SELECT
  rbukrs,
  rwcur,
  racct,
  rfarea,
  SUM( wsl ) AS wsl,
  SUM( hsl ) AS hsl
  FROM acdoca
  LEFT JOIN ska1
  ON ska1~saknr = acdoca~racct
  AND ska1~ktopl = acdoca~ktopl
  INNER JOIN skb1
  ON skb1~saknr = acdoca~racct
  AND skb1~bukrs = acdoca~rbukrs
  WHERE racct NOT BETWEEN 1001010000 AND 1012999999
    AND rbukrs = @p_rbukrs
    AND fiscyearper <= @p_year
    AND rfarea IN @p_rfarea
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr
    AND racct IN @s_racct
    AND gvtyp <> 'X'
    AND blart <> ''
    AND drcrk = 'S'
    AND mitkz IN ('K','D')
    AND acdoca~ktopl = '1000'
    AND gjahr = @lv_gjahr
  GROUP BY racct,rfarea,rbukrs,rwcur
  INTO  CORRESPONDING FIELDS OF TABLE @lt_alld3a.

  "[2]本年累计相关，获取(3)类的本年累计贷方数据
  SELECT
  rbukrs,
  rwcur,
  racct,
  rfarea,
  SUM( wsl ) AS wsl,
  SUM( hsl ) AS hsl
  FROM acdoca
  LEFT JOIN ska1
  ON ska1~saknr = acdoca~racct
  AND ska1~ktopl = acdoca~ktopl
  INNER JOIN skb1
  ON skb1~saknr = acdoca~racct
  AND skb1~bukrs = acdoca~rbukrs
  WHERE racct NOT BETWEEN 1001010000 AND 1012999999
    AND rbukrs = @p_rbukrs
    AND fiscyearper <= @p_year
    AND rfarea IN @p_rfarea
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr
    AND racct IN @s_racct
    AND gvtyp <> 'X'
    AND blart <> ''
    AND drcrk = 'H'
    AND mitkz IN ('K','D')
    AND acdoca~ktopl = '1000'
    AND gjahr = @lv_gjahr
  GROUP BY racct,rfarea,rbukrs,rwcur
  INTO  CORRESPONDING FIELDS OF TABLE @lt_alld3b.
  "(4)如果科目编码SKA1-SAKNR不等于1001010000-1012999999，且SKA1-GVTYP（损益）不等于X，且统御标识SKB1-MITKZ不等于K/D,按照科目汇总数据
  SELECT
  rbukrs,
  rwcur,
  racct,
  SUM( wsl ) AS wsl,
  SUM( hsl ) AS hsl
  FROM acdoca
  LEFT JOIN ska1
  ON ska1~saknr = acdoca~racct
  AND ska1~ktopl = acdoca~ktopl
  INNER JOIN skb1
  ON skb1~saknr = acdoca~racct
  AND skb1~bukrs = acdoca~rbukrs
  WHERE racct NOT BETWEEN 1001010000 AND 1012999999
    AND rbukrs = @p_rbukrs
    AND fiscyearper < @p_year
    AND rfarea IN @p_rfarea
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr
    AND racct IN @s_racct
    AND gvtyp <> 'X'
    AND mitkz NOT IN ('K','D')
    AND blart <> ''
    AND acdoca~ktopl = '1000'
  GROUP BY racct,rbukrs,rwcur
  INTO  CORRESPONDING FIELDS OF TABLE @lt_alla4.

  "[1]本年累计相关，获取(4)类的本年累计借方数据
  SELECT
  rbukrs,
  rwcur,
  racct,
  SUM( wsl ) AS wsl,
  SUM( hsl ) AS hsl
  FROM acdoca
  LEFT JOIN ska1
  ON ska1~saknr = acdoca~racct
  AND ska1~ktopl = acdoca~ktopl
  INNER JOIN skb1
  ON skb1~saknr = acdoca~racct
  AND skb1~bukrs = acdoca~rbukrs
  WHERE racct NOT BETWEEN 1001010000 AND 1012999999
    AND rbukrs = @p_rbukrs
    AND fiscyearper <= @p_year
    AND rfarea IN @p_rfarea
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr
    AND racct IN @s_racct
    AND gvtyp <> 'X'
    AND mitkz NOT IN ('K','D')
    AND blart <> ''
    AND drcrk = 'S'
    AND acdoca~ktopl = '1000'
    AND gjahr = @lv_gjahr
  GROUP BY racct,rbukrs,rwcur
  INTO  CORRESPONDING FIELDS OF TABLE @lt_alld4a.

  "[2]本年累计相关，获取(4)类的本年累计贷方数据

  SELECT
  rbukrs,
  rwcur,
  racct,
  SUM( wsl ) AS wsl,
  SUM( hsl ) AS hsl
  FROM acdoca
  LEFT JOIN ska1
  ON ska1~saknr = acdoca~racct
  AND ska1~ktopl = acdoca~ktopl
  INNER JOIN skb1
  ON skb1~saknr = acdoca~racct
  AND skb1~bukrs = acdoca~rbukrs
  WHERE racct NOT BETWEEN 1001010000 AND 1012999999
    AND rbukrs = @p_rbukrs
    AND fiscyearper <= @p_year
    AND rfarea IN @p_rfarea
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr
    AND racct IN @s_racct
    AND gvtyp <> 'X'
    AND mitkz NOT IN ('K','D')
    AND blart <> ''
    AND drcrk = 'H'
    AND acdoca~ktopl = '1000'
    AND gjahr = @lv_gjahr
  GROUP BY racct,rbukrs,rwcur
  INTO  CORRESPONDING FIELDS OF TABLE @lt_alld4b.

  "上面是期初余额的基底数据，接下来首先取出所有相关的全部明细行数据
  SELECT
   rbukrs,
   racct,
   acdoca~budat,
   acdoca~belnr,
   acdoca~bldat,
   sgtxt,
   rcntr,
   rfarea,
   kunnr,
   lifnr,
   hbkid,
   hktid,
   rwcur,
   wsl,
   hsl,
   txt20,
   kursf,
   drcrk,
   fiscyearper
   FROM acdoca
   LEFT JOIN skat
   ON skat~saknr = acdoca~racct
   AND skat~ktopl = acdoca~ktopl
   LEFT JOIN bkpf
   ON bkpf~bukrs = acdoca~rbukrs
   AND bkpf~belnr = acdoca~belnr
   AND bkpf~gjahr = acdoca~gjahr
   WHERE  rbukrs = @p_rbukrs
     AND fiscyearper = @p_year
     AND rfarea IN @p_rfarea
     AND kunnr IN @s_kunnr
     AND lifnr IN @s_lifnr
     AND racct IN @s_racct
     AND acdoca~blart <> ''
     AND spras = '1'
     AND acdoca~ktopl = '1000'
   INTO  CORRESPONDING FIELDS OF TABLE @lt_all.
  SORT lt_all BY racct rbukrs bldat belnr."根据凭证日期、编号进行排序
  "获取其他相关联的数据
  "获取功能范围描述文本
  SELECT
    fkber AS rfarea,
    fkbtx AS rfareat
  INTO TABLE @DATA(lt_tfkbt)
  FROM tfkbt
  WHERE spras = '1'.
  "客户供应商描述
  SELECT
    partner,
    name_org1,
    name_org2,
    name_org3,
    name_org4
  INTO TABLE @DATA(lt_but000)
  FROM but000.
  "账户描述
  SELECT
   bukrs,
   hbkid,
   hktid,
   text1
  FROM t012t
  INTO TABLE @DATA(lt_t012t)
  WHERE spras = '1'.
  "科目描述
  SELECT
    saknr,
    txt20
  FROM skat
  INTO TABLE @DATA(lt_skat)
  WHERE spras = '1'
    AND ktopl = '1000'.



  "本年发生相关

  "(1)类型，外层为基础数据
  LOOP AT lt_alla1 INTO ls_alla1.

    "根据相应条件对内部数据做计算
    "借方
    READ TABLE lt_alla1a INTO ls_alla1a WITH KEY rbukrs = ls_alla1-rbukrs
                                                 racct = ls_alla1-racct
                                                 hbkid = ls_alla1-hbkid
                                                 hktid = ls_alla1-hktid.
    IF sy-subrc = 0.
      ls_alla1-wsl1 = ls_alla1a-wsl.
      ls_alla1-hsl1 = ls_alla1a-hsl.
    ENDIF.
    "贷方
    READ TABLE lt_alla1b INTO ls_alla1b WITH KEY rbukrs = ls_alla1-rbukrs
                                                       racct = ls_alla1-racct
                                                       hbkid = ls_alla1-hbkid
                                                       hktid = ls_alla1-hktid.
    IF sy-subrc = 0.
      ls_alla1-wsl2 = ls_alla1b-wsl.
      ls_alla1-hsl2 = ls_alla1b-hsl.
    ENDIF.

    IF ls_alla1-gvtyp = 'X'.
      ls_alla1-wsl = 0.
      ls_alla1-hsl = 0.
    ENDIF.

    "科目描述
    READ TABLE lt_skat INTO DATA(ls_skat) WITH KEY saknr = ls_alla1-racct.
    IF sy-subrc  = 0.
      ls_alla1-txt20 = ls_skat-txt20.
    ENDIF.
    CLEAR ls_skat.

    ls_alla1-wsl = ls_alla1-wsl1 + ls_alla1-wsl2.
    ls_alla1-hsl = ls_alla1-hsl1 + ls_alla1-hsl2.
    CLEAR:ls_alla1-wsl1,ls_alla1-wsl2,ls_alla1-hsl1,ls_alla1-hsl2.

    IF ls_alla1-wsl > 0.
      ls_alla1-type = '借'.
    ELSEIF ls_alla1-wsl = 0.
      ls_alla1-type = '平'.
    ELSEIF ls_alla1-wsl < 0.
      ls_alla1-type = '贷'.
    ENDIF.
    ls_alla1-hsl = ls_alla1-hsl.
    MOVE-CORRESPONDING ls_alla1 TO ls_out.
    ls_out-style = '期初余额'.

    lv_id = zcl_bc_public=>get_guid( ).
    ls_out-id = lv_id.
    APPEND ls_out TO lt_out.
    MOVE-CORRESPONDING ls_out TO ls_out_t."本期发生额相关

    CLEAR ls_out.

    "开始获取（1）类的本期明细
    LOOP AT lt_all INTO ls_all WHERE rbukrs = ls_alla1-rbukrs AND racct = ls_alla1-racct
                                  AND rwcur = ls_alla1-rwcur AND hbkid = ls_alla1-hbkid
                                  AND hktid = ls_alla1-hktid AND fiscyearper = p_year.

      "科目描述
      READ TABLE lt_skat INTO ls_skat WITH KEY saknr = ls_all-racct.
      IF sy-subrc  = 0.
        ls_all-txt20 = ls_skat-txt20.
      ENDIF.
      CLEAR ls_skat.

      IF ls_all-drcrk = 'S'.
        ls_all-wsl1 = ls_all-wsl.
        ls_all-hsl1 = ls_all-hsl.
      ELSEIF ls_all-drcrk = 'H'.
        ls_all-hsl2 = ls_all-hsl .
        ls_all-wsl2 = ls_all-wsl.
      ENDIF.

      ls_out_t-hsl1 = ls_out_t-hsl1 + ls_all-hsl1.
      ls_out_t-hsl2 = ls_out_t-hsl2 + ls_all-hsl2.
      ls_out_t-wsl1 = ls_out_t-wsl1 + ls_all-wsl1.
      ls_out_t-wsl2 = ls_out_t-wsl2 + ls_all-wsl2.

      ls_out_t-hsl = ls_out_t-hsl + ls_all-hsl1 + ls_all-hsl2.
      ls_out_t-wsl = ls_out_t-wsl + ls_all-wsl1 + ls_all-wsl2.

      ls_all-hsl = ls_out_t-hsl.
      ls_all-wsl = ls_out_t-wsl.

      "获取相关的描述
      IF ls_all-rfarea IS NOT INITIAL.
        READ TABLE lt_tfkbt INTO DATA(ls_tfkbt) WITH KEY rfarea = ls_all-rfarea.
        IF sy-subrc = 0..
          ls_all-rfareat = ls_tfkbt-rfareat.
        ENDIF.
      ENDIF.

      IF ls_all-lifnr IS NOT INITIAL.
        READ TABLE lt_but000 INTO DATA(ls_but000) WITH KEY partner = ls_all-lifnr.
        IF sy-subrc = 0.
          ls_all-lifnrt = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
        ENDIF.
      ENDIF.

      IF ls_all-kunnr IS NOT INITIAL.
        READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_all-lifnr.
        IF sy-subrc = 0.
          ls_all-kunnrt = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
        ENDIF.
      ENDIF.
      IF ls_all-wsl > 0.
        ls_all-type = '借'.
      ELSEIF ls_all-wsl = 0.
        ls_all-type = '平'.
      ELSEIF ls_all-wsl < 0.
        ls_all-type = '贷'.
      ENDIF.
      ls_all-id = lv_id.
      MOVE-CORRESPONDING ls_all TO ls_out.
      APPEND ls_out TO lt_out.
      CLEAR:ls_all,ls_out.
    ENDLOOP.

    "（1）类型的本期发生额
    ls_out_t-style = '本期发生额'.
    ls_out_t-wsl = ls_out_t-wsl1 + ls_out_t-wsl2.
    ls_out_t-hsl = ls_out_t-hsl1 + ls_out_t-hsl2.
    IF ls_out_t-wsl > 0.
      ls_out_t-type = '借'.
    ELSEIF ls_out_t-wsl = 0.
      ls_out_t-type = '平'.
    ELSEIF ls_out_t-wsl < 0.
      ls_out_t-type = '贷'.
    ENDIF.

    ls_out_t-id = lv_id.
    APPEND ls_out_t TO lt_out.
    "（1）类型的本年发生额
    ls_out_t-style = '本年发生额'.
    READ TABLE lt_alld1a INTO ls_alld1a WITH KEY racct = ls_out_t-racct
                                                 rbukrs = ls_out_t-rbukrs
                                                 rwcur = ls_out_t-rwcur
                                                 hbkid = ls_out_t-hbkid
                                                 hktid = ls_out_t-hktid.
    READ TABLE lt_alld1b INTO ls_alld1b WITH KEY racct = ls_out_t-racct
                                               rbukrs = ls_out_t-rbukrs
                                               rwcur = ls_out_t-rwcur
                                               hbkid = ls_out_t-hbkid
                                               hktid = ls_out_t-hktid.
*    IF sy-subrc = 0.
*      ls_alld1b-wsl = ls_alld1b-wsl.
*      ls_alld1b-hsl = ls_alld1b-hsl.
*    ENDIF.
    ls_out_t-wsl1 = ls_alld1a-wsl.
    ls_out_t-wsl2 = ls_alld1b-wsl.
    ls_out_t-hsl1 = ls_alld1a-hsl.
    ls_out_t-hsl2 = ls_alld1b-hsl.
    ls_out_t-wsl = ls_out_t-wsl1 + ls_out_t-wsl2.
    ls_out_t-hsl = ls_out_t-hsl1 + ls_out_t-hsl2.
    ls_out_t-id = lv_id.
    APPEND ls_out_t TO lt_out.
    CLEAR:ls_out_t,lv_id.
  ENDLOOP.


  "（2）类型
  LOOP AT lt_alla2 INTO ls_alla2.
    ls_alla2-style = '期初余额'.
    READ TABLE lt_skat INTO ls_skat WITH KEY saknr = ls_alla2-racct.
    IF sy-subrc = 0.
      ls_alla2-txt20 = ls_skat-txt20.
    ENDIF.
    IF ls_alla2-wsl > 0.
      ls_alla2-type = '借'.
    ELSEIF ls_alla2-wsl = 0.
      ls_alla2-type = '平'.
    ELSEIF ls_alla2-wsl < 0.
      ls_alla2-type = '贷'.
    ENDIF.

    lv_id = zcl_bc_public=>get_guid( ).
    ls_alla2-id = lv_id.
    MOVE-CORRESPONDING ls_alla2 TO ls_out.
    APPEND ls_out TO lt_out.
    MOVE-CORRESPONDING ls_out TO ls_out_t."本期发生额相关
    "根据（2）外层的期初余额来读取内层的本期明细
    LOOP AT lt_all INTO ls_all WHERE rbukrs = ls_alla2-rbukrs AND racct = ls_alla2-racct
                                  AND rwcur = ls_alla2-rwcur AND rfarea = ls_alla2-rfarea.
      "科目描述
      READ TABLE lt_skat INTO ls_skat WITH KEY saknr = ls_all-racct.
      IF sy-subrc = 0.
        ls_all-txt20 = ls_skat-txt20.
      ENDIF.
      CLEAR ls_skat.

      IF ls_all-drcrk = 'S'.
        ls_all-wsl1 = ls_all-wsl.
        ls_all-hsl1 = ls_all-hsl.
      ELSEIF ls_all-drcrk = 'H'.
        ls_all-hsl2 = ls_all-hsl .
        ls_all-wsl2 = ls_all-wsl.
      ENDIF.
      "本期累计用
      ls_out_t-hsl1 = ls_out_t-hsl1 + ls_all-hsl1.
      ls_out_t-hsl2 = ls_out_t-hsl2 + ls_all-hsl2.
      ls_out_t-wsl1 = ls_out_t-wsl1 + ls_all-wsl1.
      ls_out_t-wsl2 = ls_out_t-wsl2 + ls_all-wsl2.

      ls_out_t-hsl = ls_out_t-hsl + ls_all-hsl1 + ls_all-hsl2.
      ls_out_t-wsl = ls_out_t-wsl + ls_all-wsl1 + ls_all-wsl2.

      ls_all-hsl = ls_out_t-hsl.
      ls_all-wsl = ls_out_t-wsl.

      "获取相关的描述
      IF ls_all-rfarea IS NOT INITIAL.
        READ TABLE lt_tfkbt INTO ls_tfkbt WITH KEY rfarea = ls_all-rfarea.
        IF sy-subrc = 0..
          ls_all-rfareat = ls_tfkbt-rfareat.
        ENDIF.
      ENDIF.

      IF ls_all-lifnr IS NOT INITIAL.
        READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_all-lifnr.
        IF sy-subrc = 0.
          ls_all-lifnrt = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
        ENDIF.
      ENDIF.

      IF ls_all-kunnr IS NOT INITIAL.
        READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_all-lifnr.
        IF sy-subrc = 0.
          ls_all-kunnrt = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
        ENDIF.
      ENDIF.

      IF ls_all-wsl > 0.
        ls_all-type = '借'.
      ELSEIF ls_all-wsl = 0.
        ls_all-type = '平'.
      ELSEIF ls_all-wsl < 0.
        ls_all-type = '贷'.
      ENDIF.
      ls_all-id = lv_id.
      MOVE-CORRESPONDING ls_all TO ls_out.
      APPEND ls_out TO lt_out.
      CLEAR:ls_all,ls_out.
    ENDLOOP.
    "（2）类型的本期发生额
    ls_out_t-style = '本期发生额'.
    ls_out_t-wsl = ls_out_t-wsl1 + ls_out_t-wsl2.
    ls_out_t-hsl = ls_out_t-hsl1 + ls_out_t-hsl2.
    IF ls_out_t-wsl > 0.
      ls_out_t-type = '借'.
    ELSEIF ls_out_t-wsl = 0.
      ls_out_t-type = '平'.
    ELSEIF ls_out_t-wsl < 0.
      ls_out_t-type = '贷'.
    ENDIF.

    ls_out_t-id = lv_id.
    APPEND ls_out_t TO lt_out.

    "（2）类型的本年发生额
    ls_out_t-style = '本年发生额'.
    READ TABLE lt_alld2a INTO ls_alld2a WITH KEY racct = ls_out_t-racct
                                                 rbukrs = ls_out_t-rbukrs
                                                 rwcur = ls_out_t-rwcur.
    READ TABLE lt_alld2b INTO ls_alld2b WITH KEY racct = ls_out_t-racct
                                               rbukrs = ls_out_t-rbukrs
                                               rwcur = ls_out_t-rwcur.
*    IF sy-subrc = 0.
*      ls_alld2b-wsl = ls_alld2b-wsl.
*      ls_alld2b-hsl = ls_alld2b-hsl.
*    ENDIF.
    ls_out_t-wsl1 = ls_alld2a-wsl.
    ls_out_t-wsl2 = ls_alld2b-wsl.
    ls_out_t-hsl1 = ls_alld2a-hsl.
    ls_out_t-hsl2 = ls_alld2b-hsl.
    ls_out_t-wsl = ls_out_t-wsl1 + ls_out_t-wsl2.
    ls_out_t-hsl = ls_out_t-hsl1 + ls_out_t-hsl2.
    IF ls_out_t-wsl > 0.
      ls_out_t-type = '借'.
    ELSEIF ls_out_t-wsl = 0.
      ls_out_t-type = '平'.
    ELSEIF ls_out_t-wsl < 0.
      ls_out_t-type = '贷'.
    ENDIF.
    ls_out_t-id = lv_id.
    APPEND ls_out_t TO lt_out.
    CLEAR ls_out_t.

    CLEAR:ls_alla2,ls_out,lv_id..
  ENDLOOP.

  "(3)类型
  LOOP AT lt_alla3 INTO ls_alla3..
    ls_alla3-style = '期初余额'.
    READ TABLE lt_skat INTO ls_skat WITH KEY saknr = ls_alla3-racct.
    IF sy-subrc = 0.
      ls_alla3-txt20 = ls_skat-txt20.
    ENDIF.
    IF ls_alla3-wsl > 0.
      ls_alla3-type = '借'.
    ELSEIF ls_alla3-wsl = 0.
      ls_alla3-type = '平'.
    ELSEIF ls_alla3-wsl < 0.
      ls_alla3-type = '贷'.
    ENDIF.

    lv_id = zcl_bc_public=>get_guid( ).
    ls_alla3-id = lv_id.
    MOVE-CORRESPONDING ls_alla3 TO ls_out.
    APPEND ls_out TO lt_out.
    MOVE-CORRESPONDING ls_out TO ls_out_t."本期发生额相关
    "根据（3）外层的期初余额来读取内层的本期明细
    LOOP AT lt_all INTO ls_all WHERE rbukrs = ls_alla3-rbukrs AND racct = ls_alla3-racct
                                  AND rwcur = ls_alla3-rwcur AND rfarea = ls_alla3-rfarea.
      "科目描述
      READ TABLE lt_skat INTO ls_skat WITH KEY saknr = ls_all-racct.
      IF sy-subrc = 0.
        ls_all-txt20 = ls_skat-txt20.
      ENDIF.
      CLEAR ls_skat.

      IF ls_all-drcrk = 'S'.
        ls_all-wsl1 = ls_all-wsl.
        ls_all-hsl1 = ls_all-hsl.
      ELSEIF ls_all-drcrk = 'H'.
        ls_all-hsl2 = ls_all-hsl .
        ls_all-wsl2 = ls_all-wsl.
      ENDIF.
      "本期累计用
      ls_out_t-hsl1 = ls_out_t-hsl1 + ls_all-hsl1.
      ls_out_t-hsl2 = ls_out_t-hsl2 + ls_all-hsl2.
      ls_out_t-wsl1 = ls_out_t-wsl1 + ls_all-wsl1.
      ls_out_t-wsl2 = ls_out_t-wsl2 + ls_all-wsl2.

      ls_out_t-hsl = ls_out_t-hsl + ls_all-hsl1 + ls_all-hsl2.
      ls_out_t-wsl = ls_out_t-wsl + ls_all-wsl1 + ls_all-wsl2.

      ls_all-hsl = ls_out_t-hsl.
      ls_all-wsl = ls_out_t-wsl.

      "获取相关的描述
      IF ls_all-rfarea IS NOT INITIAL.
        READ TABLE lt_tfkbt INTO ls_tfkbt WITH KEY rfarea = ls_all-rfarea.
        IF sy-subrc = 0..
          ls_all-rfareat = ls_tfkbt-rfareat.
        ENDIF.
      ENDIF.

      IF ls_all-lifnr IS NOT INITIAL.
        READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_all-lifnr.
        IF sy-subrc = 0.
          ls_all-lifnrt = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
        ENDIF.
      ENDIF.

      IF ls_all-kunnr IS NOT INITIAL.
        READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_all-lifnr.
        IF sy-subrc = 0.
          ls_all-kunnrt = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
        ENDIF.
      ENDIF.

      IF ls_all-wsl > 0.
        ls_all-type = '借'.
      ELSEIF ls_all-wsl = 0.
        ls_all-type = '平'.
      ELSEIF ls_all-wsl < 0.
        ls_all-type = '贷'.
      ENDIF.
      ls_all-id = lv_id.
      MOVE-CORRESPONDING ls_all TO ls_out.
      APPEND ls_out TO lt_out.
      CLEAR:ls_all,ls_out.
    ENDLOOP.
    "（3）类型的本期发生额
    ls_out_t-style = '本期发生额'.
    ls_out_t-wsl = ls_out_t-wsl1 + ls_out_t-wsl2.
    ls_out_t-hsl = ls_out_t-hsl1 + ls_out_t-hsl2.
    IF ls_out_t-wsl > 0.
      ls_out_t-type = '借'.
    ELSEIF ls_out_t-wsl = 0.
      ls_out_t-type = '平'.
    ELSEIF ls_out_t-wsl < 0.
      ls_out_t-type = '贷'.
    ENDIF.
    ls_out_t-id = lv_id.
    APPEND ls_out_t TO lt_out.

    "（3）类型的本年发生额
    ls_out_t-style = '本年发生额'.
    READ TABLE lt_alld3a INTO ls_alld3a WITH KEY racct = ls_out_t-racct
                                                 rbukrs = ls_out_t-rbukrs
                                                 rwcur = ls_out_t-rwcur.
    READ TABLE lt_alld3b INTO ls_alld3b WITH KEY racct = ls_out_t-racct
                                               rbukrs = ls_out_t-rbukrs
                                               rwcur = ls_out_t-rwcur.
*    IF sy-subrc = 0.
*      ls_alld3b-wsl = ls_alld3b-wsl.
*      ls_alld3b-hsl = ls_alld3b-hsl.
*    ENDIF.
    ls_out_t-wsl1 = ls_alld3a-wsl.
    ls_out_t-wsl2 = ls_alld3b-wsl.
    ls_out_t-hsl1 = ls_alld3a-hsl.
    ls_out_t-hsl2 = ls_alld3b-hsl.
    ls_out_t-wsl = ls_out_t-wsl1 + ls_out_t-wsl2.
    ls_out_t-hsl = ls_out_t-hsl1 + ls_out_t-hsl2.
    IF ls_out_t-wsl > 0.
      ls_out_t-type = '借'.
    ELSEIF ls_out_t-wsl = 0.
      ls_out_t-type = '平'.
    ELSEIF ls_out_t-wsl < 0.
      ls_out_t-type = '贷'.
    ENDIF.
    ls_out_t-id = lv_id.
    APPEND ls_out_t TO lt_out.
    CLEAR ls_out_t.

    CLEAR:ls_alla3,ls_out..
  ENDLOOP.

  "（4）类型
  LOOP AT lt_alla4 INTO ls_alla4..
    ls_alla4-style = '期初余额'.
    READ TABLE lt_skat INTO ls_skat WITH KEY saknr = ls_alla4-racct.
    IF sy-subrc = 0.
      ls_alla4-txt20 = ls_skat-txt20.
    ENDIF.
    IF ls_alla4-wsl > 0.
      ls_alla4-type = '借'.
    ELSEIF ls_alla4-wsl = 0.
      ls_alla4-type = '平'.
    ELSEIF ls_alla4-wsl < 0.
      ls_alla4-type = '贷'.
    ENDIF.
    lv_id = zcl_bc_public=>get_guid( ).
    ls_alla4-id = lv_id.
    MOVE-CORRESPONDING ls_alla4 TO ls_out.
    APPEND ls_out TO lt_out.
    MOVE-CORRESPONDING ls_out TO ls_out_t."本期发生额相关
    "根据（4）外层的期初余额来读取内层的本期明细
    LOOP AT lt_all INTO ls_all WHERE rbukrs = ls_alla4-rbukrs AND racct = ls_alla4-racct
                                  AND rwcur = ls_alla4-rwcur AND rfarea = ls_alla4-rfarea.
      "科目描述
      READ TABLE lt_skat INTO ls_skat WITH KEY saknr = ls_all-racct.
      IF sy-subrc = 0.
        ls_all-txt20 = ls_skat-txt20.
      ENDIF.
      CLEAR ls_skat.

      IF ls_all-drcrk = 'S'.
        ls_all-wsl1 = ls_all-wsl.
        ls_all-hsl1 = ls_all-hsl.
      ELSEIF ls_all-drcrk = 'H'.
        ls_all-hsl2 = ls_all-hsl .
        ls_all-wsl2 = ls_all-wsl.
      ENDIF.
      "本期累计用
      ls_out_t-hsl1 = ls_out_t-hsl1 + ls_all-hsl1.
      ls_out_t-hsl2 = ls_out_t-hsl2 + ls_all-hsl2.
      ls_out_t-wsl1 = ls_out_t-wsl1 + ls_all-wsl1.
      ls_out_t-wsl2 = ls_out_t-wsl2 + ls_all-wsl2.

      ls_out_t-hsl = ls_out_t-hsl + ls_all-hsl1 + ls_all-hsl2.
      ls_out_t-wsl = ls_out_t-wsl + ls_all-wsl1 + ls_all-wsl2.

      ls_all-hsl = ls_out_t-hsl.
      ls_all-wsl = ls_out_t-wsl.

      "获取相关的描述
      IF ls_all-rfarea IS NOT INITIAL.
        READ TABLE lt_tfkbt INTO ls_tfkbt WITH KEY rfarea = ls_all-rfarea.
        IF sy-subrc = 0..
          ls_all-rfareat = ls_tfkbt-rfareat.
        ENDIF.
      ENDIF.

      IF ls_all-lifnr IS NOT INITIAL.
        READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_all-lifnr.
        IF sy-subrc = 0.
          ls_all-lifnrt = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
        ENDIF.
      ENDIF.

      IF ls_all-kunnr IS NOT INITIAL.
        READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_all-lifnr.
        IF sy-subrc = 0.
          ls_all-kunnrt = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
        ENDIF.
      ENDIF.

      IF ls_all-wsl > 0.
        ls_all-type = '借'.
      ELSEIF ls_all-wsl = 0.
        ls_all-type = '平'.
      ELSEIF ls_all-wsl < 0.
        ls_all-type = '贷'.
      ENDIF.
      ls_all-id = lv_id.
      MOVE-CORRESPONDING ls_all TO ls_out.
      APPEND ls_out TO lt_out.
      CLEAR:ls_all,ls_out.
    ENDLOOP.
    "（4）类型的本期发生额
    ls_out_t-style = '本期发生额'.
    ls_out_t-wsl = ls_out_t-wsl1 + ls_out_t-wsl2.
    ls_out_t-hsl = ls_out_t-hsl1 + ls_out_t-hsl2.
    IF ls_out_t-wsl > 0.
      ls_out_t-type = '借'.
    ELSEIF ls_out_t-wsl = 0.
      ls_out_t-type = '平'.
    ELSEIF ls_out_t-wsl < 0.
      ls_out_t-type = '贷'.
    ENDIF.
    ls_out_t-id = lv_id.
    APPEND ls_out_t TO lt_out.

    "（4）类型的本年发生额
    ls_out_t-style = '本年发生额'.
    READ TABLE lt_alld4a INTO ls_alld4a WITH KEY racct = ls_out_t-racct
                                                 rbukrs = ls_out_t-rbukrs
                                                 rwcur = ls_out_t-rwcur.
    READ TABLE lt_alld4b INTO ls_alld4b WITH KEY racct = ls_out_t-racct
                                               rbukrs = ls_out_t-rbukrs
                                               rwcur = ls_out_t-rwcur.
*    IF sy-subrc = 0.
*      ls_alld4b-wsl = ls_alld4b-wsl.
*      ls_alld4b-hsl = ls_alld4b-hsl.
*    ENDIF.
    ls_out_t-wsl1 = ls_alld4a-wsl.
    ls_out_t-wsl2 = ls_alld4b-wsl.
    ls_out_t-hsl1 = ls_alld4a-hsl.
    ls_out_t-hsl2 = ls_alld4b-hsl.
    ls_out_t-wsl = ls_out_t-wsl1 + ls_out_t-wsl2.
    ls_out_t-hsl = ls_out_t-hsl1 + ls_out_t-hsl2.
    IF ls_out_t-wsl > 0.
      ls_out_t-type = '借'.
    ELSEIF ls_out_t-wsl = 0.
      ls_out_t-type = '平'.
    ELSEIF ls_out_t-wsl < 0.
      ls_out_t-type = '贷'.
    ENDIF.
    ls_out_t-id = lv_id.
    APPEND ls_out_t TO lt_out.
    CLEAR ls_out_t.

    CLEAR:ls_alla4,ls_out,lv_id..
  ENDLOOP.


ENDFORM.

FORM catalog.
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.

  fieldcatset 'ID' 'ID' sy-tabix.
  fieldcatset 'STYLE' ' ' sy-tabix.
  fieldcatset 'RBUKRS' '公司代码' sy-tabix.
  fieldcatset 'RACCT'  '科目号' sy-tabix.
  fieldcatset 'TXT20' '科目描述' sy-tabix.
  fieldcatset 'BUDAT' '过账日期' sy-tabix.
  fieldcatset 'BELNR' '凭证编号' sy-tabix.
  fieldcatset 'SGTXT' '凭证行项目文本' sy-tabix.
  fieldcatset 'RCNTR' '成本中心' sy-tabix.
  fieldcatset 'RFAREA' '功能范围' sy-tabix.
  fieldcatset 'RFAREAT' '功能范围描述' sy-tabix.
  fieldcatset 'KUNNR' '客户编码' sy-tabix.
  fieldcatset 'KUNNRT' '客户描述' sy-tabix.
  fieldcatset 'LIFNR' '供应商编码' sy-tabix.
  fieldcatset 'LIFNRT' '供应商描述' sy-tabix.
  fieldcatset 'HBKID' '开户银行' sy-tabix.
  fieldcatset 'HKTID' '账户标识' sy-tabix.
  fieldcatset 'RWCUR' '交易货币' sy-tabix.
  fieldcatset 'KURSF' '汇率' sy-tabix.
  fieldcatset 'WSL1' '借方交易货币' sy-tabix.
  fieldcatset 'HSL1' '借方本位币' sy-tabix.
  fieldcatset 'WSL2' '贷方交易货币' sy-tabix.
  fieldcatset 'HSL2' '贷方本位币' sy-tabix.
  fieldcatset 'WSL' '交易货币余额' sy-tabix.
  fieldcatset 'HSL' '本位币余额' sy-tabix.
  fieldcatset 'TYPE' '借贷标识' sy-tabix.

  layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'."斑马线的样式
  layout-box_fieldname = 'SEL'.

ENDFORM.

FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = w_repid "程序名称
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     i_grid_title             = '会计凭证' "标题名
*     I_GRID_SETTINGS          =
      is_layout                = layout "程序所定义的layout名称
      it_fieldcat              = fieldcat[] "定义fieldcat数据
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
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
      t_outtab                 = lt_out
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZHKALV1'.
ENDFORM.


FORM frm_user_command USING i_ucomm       TYPE sy-ucomm
      i_wa_selfield TYPE slis_selfield.
  DATA:
    ls_layout TYPE lvc_s_layo,
    lv_grid   TYPE REF TO cl_gui_alv_grid.
  DATA ls_stable TYPE lvc_s_stbl.

  CASE i_ucomm.
    WHEN 'POP'.

      "批量打印
      "获取SMARTFOMRS函数
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = 'ZFIR0119A' "Smart Form名称
        IMPORTING
          fm_name            = fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      lw_control-preview       = 'X'.                 " 打印预览
      lw_control-no_open       = 'X'.                 " 新的假脱机请求
      lw_control-no_close      = 'X'.
      lw_control-no_dialog     = 'X'.                 " 必须设置

      CALL FUNCTION 'SSF_OPEN'
        EXPORTING
          output_options     = lw_output
          control_parameters = lw_control
          user_settings      = ' '
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.


      LOOP AT lt_out INTO ls_out WHERE sel = 'X' AND style = '期初余额'.
        "科目范围
        IF ls_out-txt20 IS NOT INITIAL.
          SEARCH  ls_out-txt20 FOR '-'.
          IF sy-subrc = 0.
            lv_flag = sy-fdpos.
            ls_zfir0019a_head-racct_area = ls_out-racct && ls_out-txt20+0(lv_flag).
          ENDIF.
        ENDIF.
        "核算项目
        ls_zfir0019a_head-hsxm = ls_out-rfarea && ls_out-rfareat.
        "期间
        ls_zfir0019a_head-fiscyearper = p_year+0(4) && '年' && p_year+5(2) && '月'.
        "币别（获取的是公司货币）
        SELECT SINGLE
          rhcur
        INTO ls_zfir0019a_head-rtcur
        FROM acdoca
        WHERE acdoca~racct = ls_out-racct
          AND acdoca~rbukrs = p_rbukrs.
        "获取明细数据
        LOOP AT lt_out INTO ls_print WHERE id = ls_out-id.
          "日期
          IF ls_print-budat IS NOT INITIAL.
            ls_zfir0019a_detail-date = ls_print-budat.
          ENDIF.

          "凭证
          ls_zfir0019a_detail-belnr = ls_print-belnr.
          "摘要
          IF ls_print-style IS NOT INITIAL.
            ls_zfir0019a_detail-content = ls_print-style.
          ELSE.
            ls_zfir0019a_detail-content = ls_print-sgtxt.
          ENDIF.
          "汇率
          IF ls_print-kursf IS NOT INITIAL.
            ls_zfir0019a_detail-kursf = ls_print-kursf.
          ENDIF.

          "借方
          ls_zfir0019a_detail-wsl1 = ls_print-wsl1.
          ls_zfir0019a_detail-hsl1 = ls_print-hsl1.

          "贷方
          ls_zfir0019a_detail-wsl2 = ls_print-wsl2.
          ls_zfir0019a_detail-hsl2 = ls_print-hsl2.

          "余额
          ls_zfir0019a_detail-wsl = ls_print-wsl.
          ls_zfir0019a_detail-hsl = ls_print-hsl.

          ls_zfir0019a_detail-flag = ls_print-type.
          APPEND ls_zfir0019a_detail TO lt_zfir0019a_detail.



          CLEAR:ls_zfir0019a_detail,ls_print.
        ENDLOOP.

        DESCRIBE TABLE lt_zfir0019a_detail LINES DATA(lv_line).
        lv_line = 15 - lv_line MOD 15.
        DO lv_line TIMES.
          APPEND INITIAL LINE TO lt_zfir0019a_detail.
        ENDDO.
        CALL FUNCTION '/1BCDWB/SF00000029'
          EXPORTING
*           ARCHIVE_INDEX      =
*           ARCHIVE_INDEX_TAB  =
*           ARCHIVE_PARAMETERS =
            control_parameters = lw_control
*           MAIL_APPL_OBJ      =
*           MAIL_RECIPIENT     =
*           MAIL_SENDER        =
            output_options     = lw_output
*           USER_SETTINGS      = 'X'
            gs_head            = ls_zfir0019a_head
*           IMPORTING
*           DOCUMENT_OUTPUT_INFO       =
*           JOB_OUTPUT_INFO    =
*           JOB_OUTPUT_OPTIONS =
          TABLES
            gt_detail          = lt_zfir0019a_detail
          EXCEPTIONS
            formatting_error   = 1
            internal_error     = 2
            send_error         = 3
            user_canceled      = 4
            OTHERS             = 5.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CLEAR:lw_control,lw_output.
        CLEAR:ls_out,ls_zfir0019a_head,ls_zfir0019a_detail,lt_zfir0019a_detail.
      ENDLOOP.




      CALL FUNCTION 'SSF_CLOSE'
*      IMPORTING
*        job_output_info  = lw_ssfcrescl
        EXCEPTIONS
          formatting_error = 1
          internal_error   = 2
          send_error       = 3
          OTHERS           = 4.

      CLEAR:lw_control,lw_output,lt_zfir0019a_detail.
  ENDCASE.


ENDFORM.