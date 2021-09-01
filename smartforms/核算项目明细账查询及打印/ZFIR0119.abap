*&---------------------------------------------------------------------*
*& Report ZFIR0065
*&---------------------------------------------------------------------*
*&核算项目明细账查询及打印
*&---------------------------------------------------------------------*
REPORT ZFIR0119.
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
     ls_all3  LIKE LINE OF lt_all,
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
     ls_out2 LIKE LINE OF lt_out,
     lt_out1 TYPE TABLE OF ty_all,
     lt_out2 TYPE TABLE OF ty_all,
     lt_out3 TYPE TABLE OF ty_all,
     lt_out4 TYPE TABLE OF ty_all.

DATA lv_gjahr TYPE acdoca-gjahr.
DATA lv_budat TYPE acdoca-budat.
DATA lv_budat2 TYPE acdoca-budat.
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
  DATA p_year2 LIKE p_year.
  lv_gjahr = p_year+0(4)."会计年度
  lv_budat = p_year+0(4) && p_year+5(2) && '01'.
  p_year2 = p_year + 1.
  lv_budat2 = p_year+0(4) && p_year2+5(2)  && '01'.
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

    "接下来插入本期明细
    CLEAR:g_wsl1,g_wsl2,g_hsl1,g_hsl2.
    LOOP AT lt_all2 INTO ls_all2 WHERE rbukrs = ls_all-rbukrs
                                   AND racct = ls_all-racct.

      IF ls_all2-drcrk = 'S'."借方
        ls_all2-hsl1 = ls_all2-hsl.
        ls_all2-wsl1 = ls_all2-wsl.
      ELSE."贷方
        ls_all2-wsl2 = ls_all2-wsl.
        ls_all2-hsl2 = ls_all2-hsl.
      ENDIF.
      "借贷标识
      IF ls_all2-wsl = 0.
        ls_all2-type = '平'.
      ELSEIF ls_all2-wsl > 0.
        ls_all2-type = '借'.
      ELSE.
        ls_all2-type = '贷'.
      ENDIF.
      ls_all2-wsl = ls_out-wsl + ls_all2-wsl1 - ls_all2-wsl2."交易货币余额
      ls_all2-hsl = ls_out-hsl + ls_all2-hsl1 - ls_all2-hsl2."本位币余额
      MOVE-CORRESPONDING ls_all2 TO ls_out2.
      APPEND ls_out2 TO lt_out.
      "赋值本期发生额
      g_wsl1 = g_wsl1 + ls_all2-wsl1.
      g_wsl2 = g_wsl2 + ls_all2-wsl2.
      g_hsl1 = g_hsl1 + ls_all2-hsl1.
      g_hsl2 = g_hsl2 + ls_all2-hsl2.
      CLEAR:ls_all2,ls_all2.
    ENDLOOP.
    CLEAR ls_out.



    "本期发生额

    MOVE-CORRESPONDING ls_all TO ls_out.
    ls_out-style = '本期发生额'.
    ls_out-wsl1 = g_wsl1.
    ls_out-wsl2 = g_wsl2.
    ls_out-hsl1 = g_hsl1.
    ls_out-hsl2 = g_hsl2.
    SELECT SINGLE
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
    WHERE rbukrs = @ls_all-rbukrs
      AND racct = @ls_all-racct
      AND budat >= @lv_budat
      AND budat < @lv_budat2
    GROUP BY rbukrs,racct,rtcur,gvtyp
    INTO CORRESPONDING FIELDS OF  @ls_all3.
    ls_out-hsl = ls_all3-hsl.
    ls_out-wsl = ls_all3-wsl.
    CLEAR:ls_out-budat,ls_out-belnr,ls_out-sgtxt.
    "借贷标识
    IF ls_out-wsl = 0.
      ls_out-type = '平'.
    ELSEIF ls_out-wsl > 0.
      ls_out-type = '借'.
    ELSE.
      ls_out-type = '贷'.
    ENDIF.
    APPEND ls_out TO lt_out.


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
  fieldcatset 'RTCUR' '交易货币' sy-tabix.
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

ENDFORM.

FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = w_repid "程序名称
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
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