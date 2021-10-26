*&---------------------------------------------------------------------*
*& Report ZFIR0119B
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0119b.

TABLES: acdoca..
RANGES:s_racct FOR acdoca-racct.
DATA lv_gjahr TYPE acdoca-gjahr.
DATA ls_zfir0019b_head TYPE zfir0019b_head.
DATA ls_zfir0019b_detail TYPE zfir0019b_detail.
DATA lt_zfir0019b_detail TYPE TABLE OF zfir0019b_detail.
DATA:lv_flag(2),
     lv_count(2) TYPE i VALUE '1',
     lv_suma     LIKE zfir0019b_detail-sum1,
     lv_sumb     LIKE zfir0019b_detail-sum1.

DATA: fm_name TYPE rs38l_fnam.
DATA:
  lw_output  TYPE ssfcompop,
  lw_control TYPE ssfctrlop.




SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001.
PARAMETERS:r_01 RADIOBUTTON GROUP rg1 USER-COMMAND crt DEFAULT 'X',
           r_02 RADIOBUTTON GROUP rg1.
SELECTION-SCREEN END OF BLOCK blk.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-002.

PARAMETERS:p_rbukrs LIKE acdoca-rbukrs OBLIGATORY,
           p_year   LIKE acdoca-fiscyearper OBLIGATORY.
SELECT-OPTIONS:
           p_rfarea FOR acdoca-rfarea NO INTERVALS.
SELECT-OPTIONS:s_kunnr FOR acdoca-kunnr,
               s_lifnr FOR acdoca-lifnr.
PARAMETERS:p_racct1(4) MODIF ID m1.
PARAMETERS:p_racct2(6) MODIF ID m2.


SELECTION-SCREEN END OF BLOCK blk1.


AT SELECTION-SCREEN OUTPUT.
*修改屏幕
  PERFORM frm_change_screen.


START-OF-SELECTION.

  PERFORM get_data.



FORM frm_change_screen .
  LOOP AT SCREEN .
    CASE screen-group1.
      WHEN 'M1'.
        IF r_01 = 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN 'M2'.
        IF r_02 = 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .
  "对前台数据做判断，然后补充科目的指定范围
  CASE 'X'.
    WHEN r_01.
      IF p_racct1 IS NOT INITIAL.
        IF p_racct1 NOT BETWEEN '1001' AND '6901'.
          MESSAGE '会计科目范围错误' TYPE 'E'.
        ENDIF.
        s_racct-sign = 'I'.
        s_racct-option = 'BT'.
        s_racct-low = p_racct1 && '000001'.
        s_racct-high = p_racct1 && '999999'.
        APPEND s_racct.
      ELSE.
        s_racct-sign = 'I'.
        s_racct-option = 'BT'.
        s_racct-low = '1001010000'.
        s_racct-high = '6901999999'.
        APPEND s_racct.
      ENDIF.
    WHEN r_02.
      IF p_racct2 IS NOT INITIAL.
        IF p_racct2 NOT BETWEEN '691001' AND '691099'.
          MESSAGE '会计科目范围错误' TYPE 'E'.
        ENDIF.
        s_racct-sign = 'I'.
        s_racct-option = 'BT'.
        s_racct-low = p_racct2 && '0001'.
        s_racct-high = p_racct2 && '9999'.
        APPEND s_racct.
      ELSE.
        s_racct-sign = 'I'.
        s_racct-option = 'BT'.
        s_racct-low = '6910010100'.
        s_racct-high = '6910999999'.
        APPEND s_racct.
      ENDIF.
  ENDCASE.

  "获取基底数据
  SELECT DISTINCT
    racct,
    rfarea,
    rhcur,
    rbukrs,
    fiscyearper,
    kunnr,
    lifnr
  INTO TABLE @DATA(lt_head)
  FROM acdoca
  WHERE racct IN @s_racct
    AND rfarea IN @p_rfarea
    AND rbukrs = @p_rbukrs
    AND kunnr IN @s_kunnr
    AND lifnr IN @s_lifnr.
  SORT lt_head BY racct rfarea rhcur fiscyearper.
  DELETE ADJACENT DUPLICATES FROM lt_head COMPARING racct rfarea rhcur.


  IF lt_head IS NOT INITIAL.
    SELECT
      saknr,
      gvtyp
    FROM ska1
    FOR ALL ENTRIES IN @lt_head
    WHERE saknr = @lt_head-racct
    INTO TABLE @DATA(lt_ska1).

    SELECT
      saknr,
      txt20
    FROM skat
    FOR ALL ENTRIES IN @lt_head
    WHERE spras = '1'
      AND saknr = @lt_head-racct
    INTO TABLE @DATA(lt_skat).

    SELECT
      fkber,
      fkbtx
    FROM tfkbt
    FOR ALL ENTRIES IN @lt_head
    WHERE spras = '1'
      AND fkber = @lt_head-rfarea
   INTO TABLE @DATA(lt_tfkbt).
  ENDIF.
  "供应商


  "客户



  "获取每个区间的数据
  "(1)获取年初数据
  "获取年度
  lv_gjahr = p_year+0(4).
  "获取期初余额
  SELECT
    racct,
    rfarea,
    rhcur,
    rbukrs,
    SUM( hsl ) AS hsl
  INTO TABLE @DATA(lt_alla)
  FROM acdoca
  WHERE racct IN @s_racct
    AND rfarea IN @p_rfarea
    AND rbukrs = @p_rbukrs
    AND gjahr < @lv_gjahr
  GROUP BY racct,rfarea,rhcur,rbukrs
  ORDER BY racct,rfarea,rhcur,rbukrs.


  "获取当年度各个期间的金额累计
  "借方
  SELECT
   racct,
   rfarea,
   rhcur,
   rbukrs,
   poper,
   SUM( hsl ) AS hsl
   INTO TABLE @DATA(lt_allb1)
   FROM acdoca
   WHERE racct IN @s_racct
     AND rfarea IN @p_rfarea
     AND rbukrs = @p_rbukrs
     AND gjahr = @lv_gjahr
     AND drcrk = 'S'
    AND hsl <> 0
     AND poper BETWEEN '001' AND '014'
   GROUP BY racct,rfarea,rhcur,rbukrs,poper
   ORDER BY racct,rfarea,rhcur,rbukrs,poper.

  "贷方
  SELECT
   racct,
   rfarea,
   rhcur,
   rbukrs,
   poper,
   SUM( hsl ) AS hsl
   INTO TABLE @DATA(lt_allb2)
   FROM acdoca
   WHERE racct IN @s_racct
     AND rfarea IN @p_rfarea
     AND rbukrs = @p_rbukrs
     AND gjahr = @lv_gjahr
     AND drcrk = 'H'
     AND hsl <> 0
     AND poper BETWEEN '001' AND '014'
   GROUP BY racct,rfarea,rhcur,rbukrs,poper
   ORDER BY racct,rfarea,rhcur,rbukrs,poper.
  "总数
  SELECT
   racct,
   rfarea,
   rhcur,
   rbukrs,
   poper,
   SUM( hsl ) AS hsl
   INTO TABLE @DATA(lt_allb3)
   FROM acdoca
   WHERE racct IN @s_racct
     AND rfarea IN @p_rfarea
     AND rbukrs = @p_rbukrs
     AND gjahr = @lv_gjahr
     AND poper BETWEEN '001' AND '014'
     AND fiscyearper <= @p_year
     AND hsl <> 0
   GROUP BY racct,rfarea,rhcur,rbukrs,poper
   ORDER BY racct,rfarea,rhcur,rbukrs,poper.

  DATA:ls_allb3 LIKE LINE OF lt_allb3.
  DATA:ls_allb1 LIKE LINE OF lt_allb1.
  DATA:ls_allb2 LIKE LINE OF lt_allb2.


  "处理数据
  DATA lt_allb_temp LIKE  lt_allb1.
  DATA lv_count TYPE acdoca-poper.

  "批量打印
  "获取SMARTFOMRS函数
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZFIR0119B' "Smart Form名称
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





  LOOP AT lt_head INTO DATA(ls_head).
    READ TABLE lt_skat INTO DATA(ls_skat) WITH KEY saknr = ls_head-racct.
    IF sy-subrc = 0.
      SEARCH ls_skat FOR '-'.
      IF sy-subrc = 0.
        lv_flag = sy-fdpos.
        ls_skat-txt20 = ls_skat-txt20+0(lv_flag).
      ENDIF.
      "科目范围
      ls_zfir0019b_head-racct_area = ls_head-racct && ls_skat-txt20.
    ENDIF.
    "核算项目
    READ TABLE lt_tfkbt INTO DATA(ls_tfkbt) WITH KEY fkber = ls_head-rfarea.
    IF sy-subrc = 0.
      ls_zfir0019b_head-hsxm = ls_head-rfarea && ls_tfkbt-fkbtx.
    ENDIF.
    IF ls_head-kunnr IS NOT INITIAL.

    ENDIF.
    "币别
    ls_zfir0019b_head-rtcur = ls_head-rhcur.
    "期间
    ls_zfir0019b_head-fiscyearper = p_year+0(4) && '年' && p_year+5(2) && '月'.

    "明细项
    "第一行为年初余额
    READ TABLE lt_alla INTO DATA(ls_alla) WITH KEY racct = ls_head-racct
                                                   rfarea = ls_head-rfarea
                                                   rbukrs = ls_head-rbukrs
                                                   rhcur = ls_head-rhcur.
    IF sy-subrc = 0.
      ls_zfir0019b_head-wsl = ls_alla-hsl.
      "借贷标识
      IF ls_zfir0019b_head-wsl = 0.
        ls_zfir0019b_head-flag = '平'.
      ELSEIF ls_zfir0019b_head-wsl > 0.
        ls_zfir0019b_head-flag = '借'.
      ELSE.
        ls_zfir0019b_head-flag = '贷'.
      ENDIF.
    ENDIF.




    lv_count = '001'.
    IF lt_allb3 IS NOT INITIAL.
      DO 14 TIMES.
        LOOP AT lt_allb3 INTO ls_allb3 WHERE racct = ls_head-racct
                                          AND rfarea = ls_head-rfarea
                                          AND rbukrs = ls_head-rbukrs
                                          AND rhcur = ls_head-rhcur
                                          AND poper = lv_count.

          "本期合计
          "借方
          READ TABLE lt_allb1 INTO ls_allb1 WITH KEY racct = ls_allb3-racct
                                                     poper = ls_allb3-poper
                                                     rhcur = ls_allb3-rhcur
                                                     rfarea = ls_allb3-rfarea
                                                     rbukrs = ls_allb3-rbukrs.
          IF sy-subrc = 0.
            ls_zfir0019b_detail-wsl1 = ls_allb1-hsl.
          ENDIF.
          "贷方
          READ TABLE lt_allb2 INTO ls_allb2 WITH KEY racct = ls_allb3-racct
                                                     poper = ls_allb3-poper
                                                     rhcur = ls_allb3-rhcur
                                                     rfarea = ls_allb3-rfarea
                                                     rbukrs = ls_allb3-rbukrs.
          IF sy-subrc = 0.
            ls_zfir0019b_detail-hsl1 = ls_allb2-hsl.
          ENDIF.
          ls_zfir0019b_detail-sum1 = ls_allb1-hsl + ls_allb2-hsl.
          "借贷标识
          IF ls_zfir0019b_detail-sum1 = 0.
            ls_zfir0019b_detail-flaga = '平'.
          ELSEIF ls_zfir0019b_detail-sum1 > 0.
            ls_zfir0019b_detail-flaga = '借'.
          ELSE.
            ls_zfir0019b_detail-flaga = '贷'.
          ENDIF.
          "本年累计
          ls_zfir0019b_detail-wsl2 = ls_zfir0019b_detail-wsl2 + ls_zfir0019b_detail-wsl1.
          ls_zfir0019b_detail-hsl2 = ls_zfir0019b_detail-hsl2 + ls_zfir0019b_detail-hsl1.
          ls_zfir0019b_detail-sum2 = ls_zfir0019b_detail-wsl2 + ls_zfir0019b_detail-hsl2.
          "借贷标识
          IF ls_zfir0019b_detail-sum2 = 0.
            ls_zfir0019b_detail-flagb = '平'.
          ELSEIF ls_zfir0019b_detail-sum2 > 0.
            ls_zfir0019b_detail-flagb = '借'.
          ELSE.
            ls_zfir0019b_detail-flagb = '贷'.
          ENDIF.
          APPEND ls_zfir0019b_detail TO lt_zfir0019b_detail.
          CLEAR:ls_zfir0019b_detail-wsl1,ls_zfir0019b_detail-hsl1,ls_zfir0019b_detail-sum1.
          CLEAR:ls_allb1,ls_allb2,ls_allb3.
        ENDLOOP.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO lt_zfir0019b_detail.
        ENDIF.

        lv_count = lv_count + 1.
      ENDDO.
      CLEAR ls_zfir0019b_detail.

    ELSE.
      DO 14 TIMES.
        APPEND INITIAL LINE TO lt_zfir0019b_detail.
      ENDDO.
    ENDIF.







    CALL FUNCTION fm_name
      EXPORTING
*       ARCHIVE_INDEX      =
*       ARCHIVE_INDEX_TAB  =
*       ARCHIVE_PARAMETERS =
        control_parameters = lw_control
*       MAIL_APPL_OBJ      =
*       MAIL_RECIPIENT     =
*       MAIL_SENDER        =
        output_options     = lw_output
*       user_settings      = 'X'
*       gs_line            = '14'
        gs_head            = ls_zfir0019b_head
*      IMPORTING
*       document_output_info = document_output_info
*       job_output_info    = job_output_info
*       job_output_options = job_output_options
      TABLES
        gt_detail          = lt_zfir0019b_detail
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
    CLEAR:lt_zfir0019b_detail,ls_zfir0019b_head,ls_head.
  ENDLOOP.

  CALL FUNCTION 'SSF_CLOSE'
*      IMPORTING
*        job_output_info  = lw_ssfcrescl
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      OTHERS           = 4.

  CLEAR:lw_control,lw_output.


ENDFORM.