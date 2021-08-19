*&---------------------------------------------------------------------*
*& Report ZFIR0065
*&---------------------------------------------------------------------*
*&核算项目明细账查询及打印
*&---------------------------------------------------------------------*
REPORT zfir0065.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名

TYPES:BEGIN OF ty_all,
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
        rtcur       TYPE    acdoca-rtcur  , "  交易货币
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
      END OF ty_all.

DATA:lt_all   TYPE TABLE OF ty_all,
     ls_all   LIKE LINE OF lt_all,
     ls_all2  LIKE LINE OF lt_all,
     ls_all4  LIKE LINE OF lt_all,
     ls_all4c LIKE LINE OF lt_all,
     lt_all_t TYPE TABLE OF ty_all, "期初余额缓存表
     lt_all1  TYPE TABLE OF ty_all, "期初余额
     lt_all3a TYPE TABLE OF ty_all, "本期发生额
     lt_all3b TYPE TABLE OF ty_all, "本期发生额
     lt_all4  TYPE TABLE OF ty_all, "本年累计发生额
     lt_all4a TYPE TABLE OF ty_all, "本年累计发生额,借方数据
     lt_all4b TYPE TABLE OF ty_all, "本年累计发生额，贷方数据
     lt_all4c TYPE TABLE OF ty_all, "本年累计发生额，贷方数据
     lt_all2  TYPE TABLE OF ty_all. "本期明细

DATA:lt_out  TYPE TABLE OF ty_all, "ALV输出
     ls_out  LIKE LINE OF lt_out,
     lt_out1 TYPE TABLE OF ty_all,
     lt_out2 TYPE TABLE OF ty_all,
     lt_out3 TYPE TABLE OF ty_all,
     lt_out4 TYPE TABLE OF ty_all.

DATA lv_gjahr TYPE acdoca-gjahr.
DATA lv_budat TYPE acdoca-budat.
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
  "获取全部的相关数据于表ACDOCA中
  "期初余额，支持多个科目进行计算
  SELECT
      rbukrs,
      racct,
      rtcur,
      gvtyp,
      SUM( wsl ) AS wsl,
      SUM( hsl ) AS hsl
    FROM acdoca
    INNER JOIN ska1
    ON ska1~ktopl = acdoca~ktopl
    AND ska1~saknr = acdoca~racct
    WHERE rbukrs = @p_rbukrs
      AND fiscyearper < @p_year
      AND racct IN @s_racct
      AND rfarea IN @p_rfarea
      AND kunnr IN @s_kunnr
      AND lifnr IN @s_lifnr
      AND racct BETWEEN 1001010000 AND 6910999999
    GROUP BY rbukrs,racct,rtcur,gvtyp
    INTO CORRESPONDING FIELDS OF TABLE @lt_all1.
  SORT lt_all1 BY racct rbukrs DESCENDING.
  "汇总相关的数据
  LOOP AT lt_all1 INTO ls_all.
    COLLECT ls_all INTO lt_all_t.
    CLEAR ls_all.
  ENDLOOP.
  "获取相关的科目描述、借贷标识为借方
  SELECT
    *
  FROM skat
  FOR ALL ENTRIES IN @lt_all_t
  WHERE saknr = @lt_all_t-racct
    AND spras = '1'
  INTO TABLE @DATA(lt_skat).
  "上面的期初余额相关数据已经确认无误，下面进行本期明细的数据获取


  "本期明细
  SELECT
    rbukrs    "公司代码
    racct     "科目号
    budat     "过账日期
    belnr
    sgtxt
    rcntr
    rfarea
    kunnr
    lifnr
    hbkid
    hktid
    rtcur
    wsl
    hsl
    drcrk
  FROM acdoca
  INTO CORRESPONDING FIELDS OF TABLE lt_all2
  WHERE rbukrs = p_rbukrs
  AND fiscyearper = p_year
  AND racct IN s_racct
  AND rfarea IN p_rfarea
  AND kunnr IN s_kunnr
  AND lifnr IN s_lifnr
  AND racct BETWEEN 1001010000 AND 6910999999.
  "上面的本期明细相关数据已经确认无误，下面进行本期发生额的数据获取




  "本期发生额,后面处理数据的时候根据本期明细去计算
  "（1）借方，金额正数

  "（2）贷方，金额负数


  "本年累计发生额
  lv_gjahr = p_year+0(4)."会计年度
  lv_budat = p_year+0(4) && p_year+5(2) && '01'.
  "(1)借方数据
  SELECT
      rbukrs,
      racct,
      rtcur,
      gvtyp,
      SUM( wsl ) AS wsla,
      SUM( hsl ) AS hsla
    FROM acdoca
    INNER JOIN ska1
    ON ska1~ktopl = acdoca~ktopl
    AND ska1~saknr = acdoca~racct
    WHERE rbukrs = @p_rbukrs
      AND fiscyearper < @p_year
      AND racct IN @s_racct
      AND rfarea IN @p_rfarea
      AND kunnr IN @s_kunnr
      AND lifnr IN @s_lifnr
      AND drcrk = 'S'
      AND racct BETWEEN 1001010000 AND 6910999999
    GROUP BY rbukrs,racct,rtcur,gvtyp
    INTO CORRESPONDING FIELDS OF TABLE @lt_all4a.

  " (2)贷方数据
  SELECT
      rbukrs,
      racct,
      rtcur,
      gvtyp,
      rfarea,
      SUM( wsl ) AS wslb,
      SUM( hsl ) AS hslb
    FROM acdoca
    INNER JOIN ska1
    ON ska1~ktopl = acdoca~ktopl
    AND ska1~saknr = acdoca~racct
    WHERE rbukrs = @p_rbukrs
      AND fiscyearper < @p_year
      AND racct IN @s_racct
      AND rfarea IN @p_rfarea
      AND kunnr IN @s_kunnr
      AND lifnr IN @s_lifnr
      AND drcrk = 'H'
      AND racct BETWEEN 1001010000 AND 6910999999
    GROUP BY rbukrs,racct,rtcur,gvtyp,rfarea
    INTO CORRESPONDING FIELDS OF TABLE @lt_all4b.
  "总的数据
  SELECT
    rbukrs,
    racct,
    rtcur,
    gvtyp,
    SUM( wsl ) AS wsla,
    SUM( hsl ) AS hsla
  FROM acdoca
  INNER JOIN ska1
  ON ska1~ktopl = acdoca~ktopl
  AND ska1~saknr = acdoca~racct
  WHERE rbukrs = @p_rbukrs
    AND fiscyearper < @p_year
    AND racct IN @s_racct
    AND rfarea IN @p_rfarea
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr
    AND racct BETWEEN 1001010000 AND 6910999999
  GROUP BY rbukrs,racct,rtcur,gvtyp
  INTO CORRESPONDING FIELDS OF TABLE @lt_all4c.

  "(1)+(2)合并
  LOOP AT lt_all4c INTO ls_all.
    READ TABLE lt_all4b INTO ls_all2 WITH KEY racct = ls_all-racct
                                             rbukrs = ls_all-rbukrs
                                             rtcur = ls_all-rtcur.
    IF sy-subrc = 0.
      ls_all-hsl2 = ls_all2-hslb.
      ls_all-wsl2 = ls_all2-wslb.
    ENDIF.
    CLEAR ls_all2.
    READ TABLE lt_all4a INTO ls_all2 WITH KEY racct = ls_all-racct
                                             rbukrs = ls_all-rbukrs
                                             rtcur = ls_all-rtcur.
    IF sy-subrc = 0.
      ls_all-hsl1 = ls_all2-hslb.
      ls_all-wsl1 = ls_all2-wslb.
    ENDIF.
    ls_all-hsl1 = ls_all-hsla.
    ls_all-wsl1 = ls_all-wsla.

*    1.根据选择界面输入的日期，如果该科目为损益科目（SKA1-GVTYP）=X,按照科目+功能范围+货币汇总会计年度GJAHR=选择界面输入会计年度汇总交易货币余额、本位币余额
*    2.根据选择界面输入的日期，如果该科目为损益科目（SKA1-GVTYP）不等于X，按照科目+客户+供应商+开户银行+货币汇总过账日期<选择界面输入日期汇总交易货币余额、本位币余额
    IF ls_all-gvtyp = 'X'.
      SELECT SINGLE
        rbukrs,
        racct,
        rtcur,
        SUM( wsl ) AS wsl,
        SUM( hsl ) AS hsl
      FROM acdoca
      WHERE racct = @ls_all-racct
        AND gjahr = @lv_gjahr
        AND rtcur = @ls_all-rtcur
        AND rfarea = @ls_all-rfarea
      GROUP BY rbukrs,racct,rtcur
      INTO @DATA(ls_tempa).
      ls_all-hsl = ls_tempa-hsl.
      ls_all-wsl = ls_tempa-wsl.
    ELSE.
      SELECT SINGLE
        rbukrs,
        racct,
        rtcur,
        SUM( wsl ) AS wsl,
        SUM( hsl ) AS hsl
      FROM acdoca
      WHERE racct = @ls_all-racct
        AND budat < @lv_budat
        AND rtcur = @ls_all-rtcur
      GROUP BY rbukrs,racct,rtcur
      INTO @ls_tempa.
      ls_all-hsl = ls_tempa-hsl.
      ls_all-wsl = ls_tempa-wsl.
    ENDIF.

    "科目描述
    READ TABLE lt_skat INTO DATA(ls_skat) WITH KEY saknr = ls_all-racct.
    IF sy-subrc = 0.
      ls_all-txt20 = ls_skat-txt20.
    ENDIF.
    IF ls_all-wsl = 0.
      ls_all-type = '平'.
    ELSEIF ls_all-wsl > 0.
      ls_all-type = '借'.
    ELSE.
      ls_all-type = '贷'.
    ENDIF.
    APPEND ls_all TO lt_all4.
    CLEAR:ls_all,ls_all2,ls_tempa.
  ENDLOOP.

  "开始对数据进行处理,以LT_ALL1作为基底，然后循环处理数据
  LOOP AT lt_all1 INTO ls_all.
    "第一行处理的是期初余额
    IF ls_all-gvtyp = 'X'.
      ls_all-hsl = 0.
      ls_all-wsl = 0.
    ENDIF.
    "科目描述
    READ TABLE lt_skat INTO ls_skat WITH KEY saknr = ls_all-racct.
    IF sy-subrc = 0.
      ls_all-txt20 = ls_skat-txt20.
    ENDIF.
    "借贷标识
    IF ls_all-wsl = 0.
      ls_all-type = '平'.
    ELSEIF ls_all-wsl > 0.
      ls_all-type = '借'.
    ELSE.
      ls_all-type = '贷'.
    ENDIF.

    ls_all-style = '期初余额'.
    MOVE-CORRESPONDING ls_all TO ls_out.
    APPEND ls_out TO lt_out.
    CLEAR ls_out.
    "接下来插入本期明细





    "本期发生额



    "本年累计
    READ TABLE lt_all4 INTO ls_all4 WITH KEY rbukrs = ls_all-rbukrs
                                             racct = ls_all-racct
                                             rtcur = ls_all-rtcur.
    CLEAR ls_out.
    MOVE-CORRESPONDING ls_all4 TO ls_out.
    READ TABLE lt_skat INTO ls_skat WITH KEY saknr = ls_all4-racct.
    IF sy-subrc = 0.
      ls_out-txt20 = ls_skat-txt20.
    ENDIF.
    ls_out-style = '本年累计发生额'.
    APPEND ls_out TO lt_out.
    CLEAR ls_out.

    CLEAR ls_all.
  ENDLOOP.








  IF lt_all2 IS NOT INITIAL.
    READ TABLE lt_all1 INTO ls_all INDEX 1.
    IF sy-subrc = 0.
      APPEND ls_all TO lt_all.
    ENDIF.
  ENDIF.
ENDFORM.

FORM catalog.

ENDFORM.

FORM alvshow.

ENDFORM.