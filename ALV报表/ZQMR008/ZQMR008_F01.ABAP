*&---------------------------------------------------------------------*
*& 包含               ZQMR008_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& 包含               ZQMR006_F01
*&---------------------------------------------------------------------*


FORM frm_get_data .
  DATA: lv_obtyp TYPE obtyp.

  " 定义所需的结构体

  DATA: BEGIN OF lt_mseg1 OCCURS 0,
          mblnr TYPE mblnr,
          mjahr TYPE mjahr,
          zeile TYPE mblpo,
          aufnr TYPE aufnr,
          bwart TYPE bwart,
          matnr TYPE matnr,
          groes TYPE groes,
        END OF lt_mseg1,
        ls_mseg1 LIKE LINE OF lt_mseg1.

  DATA: lt_qals  TYPE TABLE OF qals,
        ls_qals  TYPE qals,

        lt_qprs  TYPE TABLE OF qprs,
        ls_qprs  TYPE qprs,


        lt_makt  TYPE TABLE OF makt,
        ls_makt  TYPE makt,

        lt_mara  TYPE TABLE OF mara,
        ls_mara  TYPE mara,

        lt_mseg  TYPE TABLE OF mseg,
        ls_mseg  TYPE mseg,


        lt_qmat  TYPE TABLE OF qmat,
        ls_qmat  TYPE qmat,

        lt_qmtt  TYPE TABLE OF qmtt,
        ls_qmtt  TYPE qmtt,

        lt_plmk  TYPE TABLE OF plmk,
        ls_plmk  TYPE plmk,


        lt_afpo  TYPE TABLE OF afpo,
        ls_afpo  TYPE afpo,

        lt_vbak  TYPE TABLE OF vbak,
        ls_vbak  TYPE vbak,

        lt_qpmk  TYPE TABLE OF qpmk,
        ls_qpmk  TYPE qpmk,

        lt_qamv  TYPE TABLE OF qamv,
        ls_qamv  TYPE qamv,
        lt_qpmt  TYPE TABLE OF qpmt,
        ls_qpmt  TYPE qpmt,


        lt_t001l TYPE TABLE OF t001l,
        ls_t001l TYPE t001l,

        lt_tq11t TYPE TABLE OF tq11t,
        ls_tq11t TYPE tq11t.






  " 批次属性结构定义

  DATA:lv_e_objek TYPE  cuobn,
       lv_e_obtab TYPE  tabelle,
       lv_e_klart TYPE  klassenart,
       lv_e_class TYPE  klasse_d.

  DATA:lv_objectkey      TYPE  bapi1003_key-object,
       lv_objecttable    TYPE  bapi1003_key-objecttable,
       lv_classnum       TYPE  bapi1003_key-classnum,
       lv_classtype      TYPE  bapi1003_key-classtype,
       lv_objectkey_long TYPE  bapi1003_key-object_long.

  DATA: lt_allocvaluesnum  TYPE TABLE OF bapi1003_alloc_values_num,
        lt_allocvalueschar TYPE TABLE OF bapi1003_alloc_values_char,
        lt_allocvaluescurr TYPE TABLE OF bapi1003_alloc_values_curr,
        lt_return          TYPE TABLE OF bapiret2.

  "物料特征字段结构定义
  DATA: ltoutput   TYPE TABLE OF zmaterial_tx_value.

  " 取出 报表所需系统表的字段


  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t001l FROM t001l.


  SELECT
         a~charg
         a~matnr
*         paendterm
         pastrterm
         prueflos
         einhprobe
         gesstichpr
         lagortchrg
         b~mpdau
         plnnr
         plnal
    werk
    a~aufnr
    sellifnr
    a~art
    lagortchrg
    a~kdauf

        FROM qals AS a INNER JOIN qmat AS b ON a~matnr = b~matnr
                                            AND a~art = b~art
*                                            INNER JOIN afpo AS c ON a~aufnr = c~aufnr
        INTO CORRESPONDING FIELDS OF TABLE lt_qals
        WHERE a~matnr IN s_matnr
    AND a~charg IN s_charg
    AND werk IN s_werk
    AND prueflos IN s_flos
    AND a~art IN s_art
    AND lagortchrg IN s_lagort
    AND a~kdauf IN s_kdauf.

  IF lt_qals IS NOT INITIAL.
    SELECT
      phynr
      plos2
      aendt
      menge
      meinh
      prtyp
      INTO CORRESPONDING FIELDS OF TABLE lt_qprs
      FROM qprs
      FOR ALL ENTRIES IN lt_qals
      WHERE plos2 = lt_qals-prueflos AND prtyp = '1'
      AND aendt IN s_aendt.

  ENDIF.

  IF lt_qals IS NOT INITIAL.
    SELECT
      mblnr
      mjahr
      zeile
      aufnr
      bwart
      a~matnr
      b~groes
      INTO CORRESPONDING FIELDS OF TABLE lt_mseg1
      FROM mseg AS a INNER JOIN mara AS b ON a~matnr = b~matnr
      FOR ALL ENTRIES IN lt_qals
      WHERE aufnr = lt_qals-aufnr AND bwart = '261' AND a~matnr = '00000000040*'.

  ENDIF.

  IF lt_qals IS NOT INITIAL.
    SELECT
      matnr
      werks
      mpdau
      INTO CORRESPONDING FIELDS OF TABLE lt_qmat
      FROM qmat
      FOR ALL ENTRIES IN lt_qals
      WHERE matnr = lt_qals-matnr
                 AND art = lt_qals-art.

  ENDIF.



  IF lt_qals IS NOT INITIAL.
    SELECT
      prueflos
      verwmerkm
      mkversion
      pmethode
      pmtversion
      ltextkz
      pmethode

      INTO CORRESPONDING FIELDS OF TABLE lt_qamv
      FROM qamv
      FOR ALL ENTRIES IN lt_qals
      WHERE prueflos = lt_qals-prueflos.

  ENDIF.

  IF lt_qamv IS NOT INITIAL.
    SELECT
      zaehler
      mkmnr
      version
      prfql
      dummy40
      prfql
*      ltextkz

      INTO CORRESPONDING FIELDS OF TABLE lt_qpmk
      FROM qpmk
      FOR ALL ENTRIES IN lt_qamv
      WHERE mkmnr = lt_qamv-verwmerkm
      AND version = lt_qamv-mkversion
      AND prfql IN s_prfql.

  ENDIF.

  IF lt_qpmk IS NOT INITIAL.
    SELECT
      zaehler
      mkmnr
      version
      sprache
      ltextkz
      kurztext


      INTO CORRESPONDING FIELDS OF TABLE lt_qpmt
      FROM qpmt
      FOR ALL ENTRIES IN lt_qpmk
      WHERE zaehler = lt_qpmk-zaehler
      AND mkmnr = lt_qpmk-mkmnr
      AND version = lt_qpmk-version.

  ENDIF.

  IF lt_qpmk IS NOT INITIAL.
    SELECT
      sprsl
      prfql
      pqtxt

      INTO CORRESPONDING FIELDS OF TABLE lt_tq11t
      FROM tq11t
      FOR ALL ENTRIES IN lt_qpmk
      WHERE prfql = lt_qpmk-prfql.

  ENDIF.

  IF lt_qamv IS NOT INITIAL.
    SELECT
      pmtnr
      version
     ltextkz
      kurztext

      INTO CORRESPONDING FIELDS OF TABLE lt_qmtt
      FROM qmtt
      FOR ALL ENTRIES IN lt_qamv
      WHERE pmtnr = lt_qamv-pmethode
      AND version = lt_qamv-pmtversion.

  ENDIF.




  IF lt_qals IS NOT INITIAL.
    SELECT
      kdauf
      aufnr
      INTO CORRESPONDING FIELDS OF TABLE lt_afpo
      FROM afpo
      FOR ALL ENTRIES IN lt_qals
      WHERE aufnr = lt_qals-aufnr
       AND kdauf IN s_kdauf.

  ENDIF.

  IF lt_afpo IS NOT INITIAL.
    SELECT
      kunnr
      vbeln

      INTO CORRESPONDING FIELDS OF TABLE lt_vbak
      FROM vbak
      FOR ALL ENTRIES IN lt_afpo
      WHERE vbeln = lt_afpo-kdauf.

  ENDIF.



  IF lt_qals IS NOT INITIAL.

    SELECT
      verwmerkm
      mkversion
      a~plnnr
      plnal
        FROM plmk AS a INNER JOIN qals AS b ON a~plnnr = b~plnnr AND a~plnkn = b~plnal
      INTO CORRESPONDING FIELDS OF TABLE lt_plmk.

  ENDIF.

  IF lt_qals IS NOT INITIAL.

    SELECT
      a~mblnr
      a~mjahr
      a~zeile
      a~matnr
      a~werks
      a~charg
      a~lifnr
        FROM mseg AS a INNER JOIN qals AS b ON a~charg = b~charg
                                               AND a~matnr = b~matnr
                                               AND a~lifnr = b~sellifnr
      INTO CORRESPONDING FIELDS OF TABLE lt_mseg.

  ENDIF.




  IF lt_qals IS NOT INITIAL.



    "取出MARA物料表所需的字段
    SELECT
      matnr
      groes

           INTO CORRESPONDING FIELDS OF TABLE lt_mara
           FROM mara
           FOR ALL ENTRIES IN lt_qals
           WHERE matnr = lt_qals-matnr.

    IF lt_mara IS NOT INITIAL.

      "取出MAKT物料描述表所需的字段

      SELECT matnr
        maktx
           INTO CORRESPONDING FIELDS OF TABLE lt_makt
          FROM makt
          FOR ALL ENTRIES IN lt_qals
          WHERE matnr = lt_qals-matnr.

    ENDIF.


  ENDIF.


  " LOOP循环取数
  LOOP AT lt_qals INTO ls_qals.

    MOVE-CORRESPONDING ls_qals TO gs_alv.
    gs_alv-fcharg = gs_alv-charg.

    READ TABLE  lt_qprs INTO ls_qprs WITH KEY plos2 = ls_qals-prueflos.  " 将QPRS 表字段赋予
    IF sy-subrc = 0 .
      MOVE-CORRESPONDING ls_qprs TO gs_alv.
    ENDIF.

    READ TABLE  lt_mara INTO ls_mara WITH KEY matnr = ls_qals-matnr.  " 将物料表字段赋予
    IF sy-subrc = 0 .
      MOVE-CORRESPONDING ls_mara TO gs_alv.
    ENDIF.

    READ TABLE  lt_mseg1 INTO ls_mseg1 WITH KEY aufnr = ls_qals-aufnr.  " 将物料表字段赋予
    IF sy-subrc = 0 .
*   MOVE-CORRESPONDING ls_mara TO gs_alv.
      gs_alv-groes = ls_mseg1-groes.
    ENDIF.

    READ TABLE  lt_qmat INTO ls_qmat WITH KEY matnr = ls_qals-matnr.  " 将平均检验期表字段赋予
    IF sy-subrc = 0 .
      gs_alv-mpdau = ls_qmat-mpdau.
    ENDIF.

*    READ TABLE  lt_afpo INTO ls_afpo WITH KEY aufnr = ls_qals-aufnr.  " 将销售订购单号字段赋予
*    IF sy-subrc = 0 .
*      gs_alv-kdauf = ls_afpo-kdauf.
*    ENDIF.

    READ TABLE  lt_vbak INTO ls_vbak WITH KEY vbeln = gs_alv-kdauf.  " 将客户检索码字段赋予
    IF sy-subrc = 0 .
      gs_alv-kunnr = ls_vbak-kunnr.
    ENDIF.

    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = gs_alv-matnr.   "将物料描述表字段赋予
    IF sy-subrc = 0 .

      gs_alv-maktx = ls_makt-maktx.
    ENDIF.

*    LOOP AT lt_qamv INTO ls_qamv. "waymon

    READ TABLE lt_qamv INTO ls_qamv WITH KEY prueflos = gs_alv-prueflos.
*                                              version = gs_alv-mkversion.   "将QAMV 表字段赋予
    IF sy-subrc = 0 .

      MOVE-CORRESPONDING ls_qamv TO gs_alv.
    ENDIF.

    READ TABLE lt_qpmk INTO ls_qpmk WITH KEY mkmnr = gs_alv-verwmerkm
                                           version = gs_alv-mkversion.   "将QPMK表字段赋予
    IF sy-subrc = 0 .

      MOVE-CORRESPONDING ls_qpmk TO gs_alv.

    ENDIF.

    READ TABLE lt_tq11t INTO ls_tq11t WITH KEY prfql = gs_alv-prfql.  "将TQ11T 表字段赋予
    IF sy-subrc = 0 .

      gs_alv-prfql = ls_tq11t-pqtxt.
    ENDIF.

    READ TABLE lt_qpmt INTO ls_qpmt WITH KEY zaehler  = gs_alv-zaehler
                                             mkmnr = gs_alv-mkmnr
                                             version = gs_alv-version
                                             sprache = 'Z'.  "将QPMT 表字段赋予
    IF sy-subrc = 0 .

      gs_alv-ltextkz1 = ls_qpmt-kurztext.
    ENDIF.

    READ TABLE lt_qpmt INTO ls_qpmt WITH KEY zaehler  = gs_alv-zaehler
                                             mkmnr = gs_alv-mkmnr
                                             version = gs_alv-version
                                             sprache = '1'.  "将QPMT 表字段赋予
    IF sy-subrc = 0 .

      gs_alv-ltextkz2 = ls_qpmt-kurztext.
    ENDIF.



    READ TABLE lt_qmtt  INTO ls_qmtt  WITH KEY pmtnr = gs_alv-pmethode
                                               version = gs_alv-pmtversion.   "将QMTT 表字段赋予
    IF sy-subrc = 0 .

      gs_alv-ltextkz3 = ls_qmtt-kurztext.
    ENDIF.

    READ TABLE lt_t001l INTO ls_t001l  WITH KEY lgort = gs_alv-lagortchrg     "将库存地点描述表字段赋予
                                                werks = gs_alv-werk.
    IF sy-subrc = 0 .
      gs_alv-lgobe = ls_t001l-lgobe.
    ENDIF.






    " 获取批次属性和物料特征

    IF gs_alv-matnr IS NOT INITIAL.
      IF gs_alv-charg IS NOT INITIAL.
**          获取物料的批次信息  -- 包装规格 长批号 开箱数 到货日期 生产日期 有效日期
        CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
          EXPORTING
            i_matnr = gs_alv-matnr
            i_charg = gs_alv-charg
            i_werks = gs_alv-werk
          IMPORTING
            e_objek = lv_e_objek
            e_obtab = lv_e_obtab
            e_klart = lv_e_klart
            e_class = lv_e_class.
**          获取批次对象的属性信息
        IF sy-subrc = 0.
          lv_objectkey  =    lv_e_objek.
          lv_objecttable   = lv_e_obtab.
          lv_classnum  = lv_e_class.
          lv_classtype  = lv_e_klart.
          lv_objectkey_long = lv_e_objek.
          CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
            EXPORTING
              objectkey       = lv_objectkey
              objecttable     = lv_objecttable
              classnum        = lv_classnum
              classtype       = lv_classtype
              objectkey_long  = lv_objectkey_long
            TABLES
              allocvaluesnum  = lt_allocvaluesnum
              allocvalueschar = lt_allocvalueschar
              allocvaluescurr = lt_allocvaluescurr
              return          = lt_return.
          IF lt_return IS NOT INITIAL AND lt_return[ 1 ]-type = 'S'.
            DATA lv_date TYPE n LENGTH 8.
***              包装规格
            READ TABLE lt_allocvalueschar WITH KEY charact = 'ZPACK_STA' INTO DATA(ls_allocvalueschar).
            IF sy-subrc = 0.
              gs_alv-zpack_sta = ls_allocvalueschar-value_char. "包装规格
            ENDIF.
***              打印包装批号
            CLEAR ls_allocvalueschar.
            READ TABLE lt_allocvalueschar WITH KEY charact = 'ZPACK_CHARG' INTO ls_allocvalueschar.
            IF sy-subrc = 0.
              gs_alv-zpack_charg = ls_allocvalueschar-value_char.  "打印包装批号
            ENDIF.
***              ZLWEDT 到货日期
            CLEAR ls_allocvalueschar.
            READ TABLE lt_allocvaluesnum  WITH KEY charact = 'ZLWEDT' INTO DATA(ls_allocvaluesnum) .
            IF sy-subrc = 0.
              MOVE ls_allocvaluesnum-value_from TO lv_date.
              gs_alv-zlwedt = lv_date.  "到货日期
            ENDIF.

          ENDIF.
        ENDIF.

*   判断90天内同一供应商批号非首次到货
        DATA: lv_cnt TYPE i.
        LOOP AT lt_mseg INTO ls_mseg WHERE  matnr = gs_alv-matnr     "看下取多少条数据
                                                   AND   werks = gs_alv-werk
                                                  AND    charg = gs_alv-charg
                                                  AND    lifnr = gs_alv-sellifnr.
          lv_cnt = lv_cnt + 1.

        ENDLOOP.

        " 比较日期 长度
        DATA: tday TYPE i.
        DATA: tday2 TYPE d.
        CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
          EXPORTING
            i_date_from = gs_alv-zlwedt
            i_date_to   = sy-datum
          IMPORTING
            e_days      = tday.

        IF tday <= 90 AND lv_cnt >= 2.
          gs_alv-ltextkz4 = '是'.
        ELSE.
          gs_alv-ltextkz4 = '否'.
        ENDIF.

      ENDIF.
    ENDIF.

    " 去除物料前面的零
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_alv-matnr
      IMPORTING
        output = gs_alv-matnr.
*   EXCEPTIONS
*     LENGTH_ERROR       = 1
*     OTHERS             = 2

    APPEND gs_alv TO gt_alv.

*    ENDLOOP. "waymon
    CLEAR gs_alv.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_DIS_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_dis_alv .

  DATA: l_colpos TYPE lvc_s_fcat-col_pos VALUE 0.


  CLEAR gs_layout.

  gs_layout-cwidth_opt  = 'X'.   "设置Grid的字段列宽度自动适应


  CLEAR gt_fieldcat.



  macro_fill_fcat:
    l_colpos 'AENDT'    '接检日期'     '' ' ' ' ' '' '' '',
    l_colpos 'PAENDTERM'    '预计完成日期'     '' ' ' ' ' 'QALS' 'PAENDTERM' 'X',
    l_colpos 'MPDAU'    '平均检验期'     '' ' ' ' ' '' '' '',
    l_colpos 'KDAUF'    '销售订购单号'     '' ' ' ' ' '' '' '',
    l_colpos 'KUNNR'    '客户检索码'     '' ' ' ' ' '' '' '',
    l_colpos 'PRUEFLOS'    '样品编号'     '' ' ' ' ' '' '' '',
    l_colpos 'MATNR'    '物料编码'     '' ' ' ' ' '' '' '',
    l_colpos 'MAKTX'    '物料名称'     '' ' ' ' ' '' '' '',
    l_colpos 'FCHARG'    '成品批号'     '' ' ' ' ' '' '' '',
    l_colpos 'ZPACK_CHARG'    '打印包装批号'     '' ' ' ' ' '' '' '',
    l_colpos 'CHARG'    'SAP批次号'     '' ' ' ' ' '' '' '',
    l_colpos 'GROES'    '规格型号'     '' ' ' ' ' '' '' '',
    l_colpos 'ZPACK_STA'    '包装规格'     '' ' ' ' ' '' '' '',
    l_colpos 'PRFQL'    '检验岗位'     '' ' ' ' ' '' '' '',
    l_colpos 'DUMMY40 '    '检验项目'     '' ' ' ' ' '' '' '',
    l_colpos 'LTEXTKZ1 '    '规定标准'     '' ' ' ' ' '' '' '',
    l_colpos 'LTEXTKZ2'    '内控标准'     '' ' ' ' ' '' '' '',
    l_colpos 'LTEXTKZ3 '    '检验方法'     '' ' ' ' ' '' '' '',
      l_colpos 'LGOBE'    '库存地'     '' ' ' ' ' '' '' '',
    l_colpos 'MENGE '    '样本数量'     '' ' ' ' ' '' '' '',
    l_colpos 'MEINH '    '样本单位'     '' ' ' ' ' '' '' '',
    l_colpos 'LTEXTKZ4 '    '90天内同一供应商批号非首次到货'     '' ' ' ' ' '' '' ''.



  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
*     I_CALLBACK_USER_COMMAND  = 'FRM_USER_COMMAND'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat
*     IT_EVENTS                = I_EVENTS[]
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text  设置alv状态
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.


  SET PF-STATUS 'PF_1000'.

ENDFORM.                    "_SET_PF_STATUS

FORM frm_user_command USING pv_ucomm LIKE sy-ucomm
                             ps_selfield TYPE slis_selfield.
  DATA: lv_grid  TYPE REF TO cl_gui_alv_grid.
  DATA: lv_flag  TYPE c.
  DATA:BEGIN OF ls_mblnr,
         mblnr TYPE mseg-mblnr,
       END OF ls_mblnr.
  DATA lt_mblnr LIKE TABLE OF ls_mblnr.
  DATA num TYPE i.


*&将变更的数据刷新
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lv_grid.
  CALL METHOD lv_grid->check_changed_data.
  ps_selfield-refresh = 'X'.

  TRY.
      DATA(ls_alv) = gt_alv[ ps_selfield-tabindex ].
    CATCH cx_sy_itab_line_not_found.

  ENDTRY.





  CALL METHOD lv_grid->refresh_table_display.
ENDFORM.