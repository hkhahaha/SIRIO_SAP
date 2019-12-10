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

  TYPES:BEGIN OF lv_qals,
          qprs_objnr TYPE qprs-objnr.
          INCLUDE TYPE qals.
        TYPES END OF lv_qals.

  DATA:ls_qals  TYPE lv_qals,
       lt_qals  LIKE TABLE OF ls_qals,

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

       lt_kna1  TYPE TABLE OF kna1,
       ls_kna1  TYPE kna1,

       lt_qpmk  TYPE TABLE OF qpmk,
       ls_qpmk  TYPE qpmk,

       lt_qamv  TYPE TABLE OF qamv,
       ls_qamv  TYPE qamv,

       lt_qamv2 TYPE TABLE OF qamv,
       ls_qamv2 TYPE qamv,

       lt_qpmt  TYPE TABLE OF qpmt,
       ls_qpmt  TYPE qpmt,

       lt_qamr  TYPE TABLE OF qamr,
       ls_qamr  TYPE qamr,


       lt_t001l TYPE TABLE OF t001l,
       ls_t001l TYPE t001l,

       lt_tq11t TYPE TABLE OF tq11t,
       ls_tq11t TYPE tq11t,

       lt_mcha  TYPE TABLE OF mcha,
       ls_mcha  TYPE mcha,
       ls_mcha2 TYPE mcha,

       lt_t006a TYPE TABLE OF t006a,
       ls_t006a TYPE t006a.





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
*  定义状态结构
  DATA: lt_stat LIKE TABLE OF jstat.   "给检验批状态用

  DATA: BEGIN OF lt_obtyp OCCURS 0,
          plos2 TYPE qplos,
          objnr TYPE objnr,
          stat  TYPE j_status,
        END OF lt_obtyp,
        ls_obtyp LIKE LINE OF lt_obtyp.

*给取样状态用
  DATA: lt_stat1 LIKE TABLE OF jstat.

  DATA print TYPE c.

  DATA: lv_name TYPE thead-tdname,
        lv_id   TYPE string.
  " 取出 报表所需系统表的字段

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_t001l FROM t001l.


  SELECT
  a~charg
  a~matnr
  pastrterm
  prueflos
  einhprobe
  gesstichpr
  lagortchrg
    b~phynr
    b~plos2
    b~aendt
    b~menge
    b~meinh
    b~prtyp
  plnnr
  plnal
  werk
  a~aufnr
  sellifnr
  a~art
  lagortchrg
  a~kdauf
  a~objnr
  b~objnr AS qprs_objnr   "20191205 新增

  FROM qals AS a
  INNER JOIN qprs AS b
  ON a~prueflos = b~plos2
  AND b~prtyp = '1'
  INNER JOIN qprn
  ON a~prueflos = qprn~plos
  AND b~pn_nr = qprn~pn_nr     "20191205 新增
  INTO CORRESPONDING FIELDS OF TABLE lt_qals
  WHERE a~matnr IN s_matnr
  AND a~charg IN s_charg
  AND werk = s_werk
  AND prueflos IN s_flos
  AND a~art IN s_art
  AND lagortchrg IN s_lagort
  AND a~kdauf IN s_kdauf
  AND qprn~frgdt IN s_aendt.


  IF lt_qals IS NOT INITIAL.
    SELECT
    phynr
    plos2
    aendt
    menge
    meinh
    prtyp
    objnr
    INTO CORRESPONDING FIELDS OF TABLE lt_qprs
    FROM qprs
    INNER JOIN qprn
    ON qprn~pn_nr = qprs~pn_nr
    FOR ALL ENTRIES IN lt_qals
    WHERE plos2 = lt_qals-prueflos AND prtyp = '1'
    AND qprn~frgdt IN s_aendt.

    LOOP AT lt_qprs INTO ls_qprs. " 取出取样状态,通过函数STATUS_READ，读取OBTYP
      CLEAR ls_obtyp.
      CALL FUNCTION 'STATUS_READ'
        EXPORTING
*         CLIENT      = SY-MANDT
          objnr       = ls_qprs-objnr
          only_active = 'X'
*        IMPORTING
*         obtyp       = ls_obtyp-obtyp
*         STSMA       =
*         STONR       =
        TABLES
          status      = lt_stat1
*      EXCEPTIONS
*         OBJECT_NOT_FOUND       = 1
*         OTHERS      = 2
        .
      IF sy-subrc = 0.
        ls_obtyp-plos2 = ls_qprs-plos2.

        READ TABLE lt_stat1 TRANSPORTING NO FIELDS WITH KEY stat = 'I0076'."20191205 新增
        IF sy-subrc = 0."20191205 新增

          DELETE lt_obtyp WHERE stat NE'I0002'.  "20191205 新增
          DELETE lt_qals WHERE  qprs_objnr = ls_qprs-objnr."20191205 新增

        ELSE."20191205 新增


          READ TABLE lt_stat1 TRANSPORTING NO FIELDS WITH KEY stat = 'I0002'.
          IF sy-subrc = 0.
            ls_obtyp-stat = 'I0002'.

          ENDIF.
          APPEND ls_obtyp TO lt_obtyp.
        ENDIF.

      ENDIF."20191205 新增
    ENDLOOP.

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
    AND  werks = lt_qals-werk
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
    satzstatus
    merknr
    qdynregel "增加动态修改规则字段 20191204
      qmtb_werks "检验方法工厂 20191208
      qpmk_werks "检验特性工厂20191208
    INTO CORRESPONDING FIELDS OF TABLE lt_qamv
    FROM qamv
    FOR ALL ENTRIES IN lt_qals
    WHERE prueflos = lt_qals-prueflos
    AND satzstatus NE '4'.

    SELECT
             qamv~prueflos
             verwmerkm
             mkversion
             pmethode
             pmtversion
             ltextkz
             pmethode
             qamr~satzstatus
             qamv~merknr
             qdynregel "增加动态修改规则字段 20191204
      qmtb_werks"检验方法工厂20191208
      qpmk_werks"检验特性工厂20191208
    APPENDING CORRESPONDING FIELDS OF TABLE lt_qamv
    FROM qamv
    INNER JOIN qamr ON qamv~prueflos = qamr~prueflos
    AND qamv~merknr = qamr~merknr
    FOR ALL ENTRIES IN lt_qals
    WHERE qamv~prueflos = lt_qals-prueflos
    AND qamr~satzstatus NE '4'.

*&---------------------------------------------------------------------*
*&作者：Seashell Huang
*&修改日期：02.12.2019 20:59:05
*&---------------------------------------------------------------------*
*&修改记录：添加工作中心的判断
*&---------------------------------------------------------------------*

*    CLEAR lt_check.
*    SELECT
*    qals~prueflos
*    qals~matnr
*    qals~charg
*    plmk~verwmerkm
*    INTO CORRESPONDING FIELDS OF TABLE lt_check
*      FROM plmk
*      INNER JOIN plpo
*      ON plpo~plnnr = plmk~plnnr
*      AND plpo~plnkn = plmk~plnkn
*      INNER JOIN crhd
*      ON crhd~objid = plpo~arbid
*      INNER JOIN plas
*      ON plas~plnty = plpo~plnty
*      AND plas~plnnr = plpo~plnnr
*      AND plas~plnkn = plpo~plnkn
*      AND plas~zaehl = plpo~zaehl
*      INNER JOIN qals
*      ON qals~plnnr = plas~plnnr
*      AND qals~plnal = plas~plnal
*      FOR ALL ENTRIES IN lt_qamv
*      WHERE plpo~loekz <> 'X'
*      AND plmk~verwmerkm = lt_qamv-verwmerkm
*      AND plpo~plnty = 'Q'
*      AND crhd~arbpl = p_lang
*      AND qals~plnty = 'Q'
*      AND qals~prueflos = lt_qamv-prueflos
*      AND plmk~loekz <> 'X'.

***********02.12.2019 21:00:23结束修改**********


  ENDIF.

  IF lt_qamv IS NOT INITIAL.
    SELECT
    zaehler
    mkmnr
    version
    prfql
    dummy40
    prfql
    masseinhsw
    steuerkz

    INTO CORRESPONDING FIELDS OF TABLE lt_qpmk
    FROM qpmk
    FOR ALL ENTRIES IN lt_qamv
    WHERE mkmnr = lt_qamv-verwmerkm
    AND version = lt_qamv-mkversion
    AND zaehler = lt_qamv-qpmk_werks " 20191208 新增
    AND prfql IN s_prfql.

  ENDIF.

  IF lt_qpmk IS NOT INITIAL.

    SELECT
    msehi
    mseht
    mseh6
msehl
    INTO CORRESPONDING FIELDS OF TABLE lt_t006a
    FROM t006a
    FOR ALL ENTRIES IN lt_qpmk
    WHERE msehi = lt_qpmk-masseinhsw
    AND spras = '1'.

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
    AND zaehler = lt_qpmk-zaehler  " 20191208 新增
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
    werks "20191208 新增
    INTO CORRESPONDING FIELDS OF TABLE lt_qmtt
    FROM qmtt
    FOR ALL ENTRIES IN lt_qamv
    WHERE pmtnr = lt_qamv-pmethode
    AND werks = lt_qamv-qmtb_werks "20191208 新增
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

  IF lt_vbak IS NOT INITIAL.
    SELECT
    kunnr
    sortl

    INTO CORRESPONDING FIELDS OF TABLE lt_kna1
    FROM kna1
    FOR ALL ENTRIES IN lt_vbak
    WHERE kunnr = lt_vbak-kunnr.

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
    mtart

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
*  LOOP AT lt_qamv INTO ls_qamv.
  LOOP AT lt_qals INTO ls_qals.

*判断检验批状态是否有LTCA,有则结束，没有则开始下面操作
    CALL FUNCTION 'STATUS_READ'
      EXPORTING
**       CLIENT                 = SY-MANDT
        objnr       = ls_qals-objnr
        only_active = 'X'
**     IMPORTING
**       OBTYP                  =
**       STSMA                   =
**       STONR                   =
      TABLES
        status      = lt_stat
**     EXCEPTIONS
**       OBJECT_NOT_FOUND        = 1
**       OTHERS                  = 2
      .
    IF sy-subrc = 0.
      READ TABLE lt_stat TRANSPORTING NO FIELDS WITH KEY stat = 'I0224'.
      IF sy-subrc <> 0.
        SELECT
          mkversion
          verwmerkm
          pmethode
          pmtversion
          satzstatus
          qdynregel "增加动态修改规则字段 20191204
          qamv~merknr
          qmtb_werks"检验方法工厂20191209
          qpmk_werks"检验特性工厂20191209
        INTO CORRESPONDING FIELDS OF TABLE lt_qamv2
        FROM qamv
        WHERE qamv~prueflos = ls_qals-prueflos
        AND satzstatus NE '4'.

        SELECT
          mkversion
          verwmerkm
          pmethode
          pmtversion
          qamv~satzstatus
          qdynregel "增加动态修改规则字段 20191204
          qamv~merknr
          qmtb_werks"检验方法工厂20191209
          qpmk_werks"检验特性工厂20191209
      APPENDING CORRESPONDING FIELDS OF TABLE lt_qamv2
      FROM qamv
      INNER JOIN qamr ON qamv~prueflos = qamr~prueflos
      AND qamv~merknr = qamr~merknr
      AND qamv~vorglfnr = qamr~vorglfnr
      WHERE qamv~prueflos = ls_qals-prueflos
      AND qamr~satzstatus = '2'
      AND qamv~satzstatus = '4'.



***********02.12.2019 21:20:42结束修改**********
        LOOP AT lt_qamv2 INTO ls_qamv2.
*&作者：Seashell Huang
*&修改日期：02.12.2019 21:20:27
*&---------------------------------------------------------------------*
*&修改记录：工作中心
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&作者：Seashell Huang
*&修改日期：02.12.2019 20:59:05
*&---------------------------------------------------------------------*
*&修改记录：添加工作中心的判断
*&---------------------------------------------------------------------*
          CLEAR lt_check.
          SELECT
          qals~prueflos
          qals~matnr
          qals~charg
          plmk~verwmerkm
          plmk~pmethode
          INTO CORRESPONDING FIELDS OF TABLE lt_check
          FROM plmk
          INNER JOIN plpo
          ON plpo~plnnr = plmk~plnnr
          AND plpo~plnkn = plmk~plnkn
          INNER JOIN crhd
          ON crhd~objid = plpo~arbid
          INNER JOIN plas
          ON plas~plnty = plpo~plnty
          AND plas~plnnr = plpo~plnnr
          AND plas~plnkn = plpo~plnkn
          AND plas~zaehl = plpo~zaehl
          INNER JOIN qals
          ON qals~plnnr = plas~plnnr
          AND qals~plnal = plas~plnal
          WHERE plpo~loekz <> 'X'
*        AND plmk~verwmerkm = lt_qamv-verwmerkm
          AND plpo~plnty = 'Q'
          AND crhd~arbpl = p_lang
          AND qals~plnty = 'Q'
          AND qals~prueflos = ls_qals-prueflos
          AND plmk~loekz <> 'X'.

          IF lt_check IS INITIAL.
            CLEAR lt_check.
            CONTINUE.
          ELSE.
            READ TABLE lt_check INTO ls_check WITH KEY verwmerkm = ls_qamv2-verwmerkm
                                                       pmethode = ls_qamv2-pmethode.
            IF sy-subrc <> 0.
              CLEAR ls_check.
              CONTINUE.
            ENDIF.
          ENDIF.

          gs_alv-merknr = ls_qamv2-merknr.
***********02.12.2019 21:00:23结束修改**********


          MOVE-CORRESPONDING ls_qals TO gs_alv.
*      gs_alv-charg = gs_alv-charg.

          READ TABLE  lt_qprs INTO ls_qprs WITH KEY plos2 = ls_qals-prueflos.  " 将QPRS 表字段赋予
          IF sy-subrc = 0 .
            MOVE-CORRESPONDING ls_qprs TO gs_alv.
            gs_alv-charg = ls_qals-charg.

            "处理样本单位，获取转换之后的值
            CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
              EXPORTING
                input          = gs_alv-meinh
                language       = sy-langu
              IMPORTING
*               LONG_TEXT      =
                output         = gs_alv-meinh
*               SHORT_TEXT     =
              EXCEPTIONS
                unit_not_found = 1
                OTHERS         = 2.
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

          READ TABLE  lt_vbak INTO ls_vbak WITH KEY vbeln = gs_alv-kdauf.  " 将客户检索码字段赋予,过渡
          IF sy-subrc = 0 .
            gs_alv-kunnr = ls_vbak-kunnr.
          ENDIF.

          READ TABLE  lt_kna1 INTO ls_kna1 WITH KEY kunnr = gs_alv-kunnr.  " 将客户检索码字段赋予,
          IF sy-subrc = 0 .
            gs_alv-sortl = ls_kna1-sortl.
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

          READ TABLE lt_qpmk INTO ls_qpmk WITH KEY mkmnr = ls_qamv-verwmerkm
          version = ls_qamv-mkversion.   "将QPMK表字段赋予
          IF sy-subrc = 0 .

            MOVE-CORRESPONDING ls_qpmk TO gs_alv.
            gs_alv-merknr = ls_qamv2-merknr.
          ENDIF.

          READ TABLE lt_tq11t INTO ls_tq11t WITH KEY prfql = gs_alv-prfql.  "将TQ11T 表字段赋予
          IF sy-subrc = 0 .

            gs_alv-prfql = ls_tq11t-pqtxt.
          ENDIF.

          READ TABLE lt_qpmt INTO ls_qpmt WITH KEY zaehler  = gs_alv-zaehler
          mkmnr = gs_alv-mkmnr
          version = gs_alv-version
          sprache = '1'.  "将QPMT 表字段赋予,规定标准，对应语言ZH
          IF sy-subrc = 0 .

            IF ls_qpmt-ltextkz = 'X'.
              "读取主检验特征长文本

              lv_name = sy-mandt && gs_alv-werk && |{ gs_alv-mkmnr PAD = ' ' WIDTH = 8 }| && gs_alv-version && sy-langu.

              PERFORM frm_read_text USING 'QPMT' 'QPMERKMAL' lv_name sy-langu
                                    CHANGING gs_alv-ltextkz1.
            ELSE.
              "读取短文本
              gs_alv-ltextkz1 = ls_qpmt-kurztext.
            ENDIF.

          ENDIF.
          gs_alv-qdynregel = ls_qamv2-qdynregel.
          READ TABLE lt_qpmt INTO ls_qpmt WITH KEY zaehler  = gs_alv-zaehler
          mkmnr = gs_alv-mkmnr
          version = gs_alv-version
          sprache = 'E'.  "将QPMT 表字段赋予,规定标准(EN ) ，对应语言EN
          IF sy-subrc = 0 .

            IF ls_qpmt-ltextkz = 'X'.
              "读取主检验特征长文本

              lv_name = sy-mandt && gs_alv-werk && |{ gs_alv-mkmnr PAD = ' ' WIDTH = 8 }| && gs_alv-version && sy-langu.

              PERFORM frm_read_text USING 'QPMT' 'QPMERKMAL' lv_name sy-langu
                                    CHANGING gs_alv-ltextkz5.
            ELSE.
              "读取短文本
              gs_alv-ltextkz5 = ls_qpmt-kurztext.
            ENDIF.

          ENDIF.


          READ TABLE lt_qpmt INTO ls_qpmt WITH KEY zaehler  = gs_alv-zaehler
          mkmnr = gs_alv-mkmnr
          version = gs_alv-version
          sprache = 'Z'.  "将QPMT 表字段赋予,内控标准，对应语言Z1
          IF sy-subrc = 0 .

            IF ls_qpmt-ltextkz = 'X'.
              "读取主检验特征长文本

              lv_name = sy-mandt && gs_alv-werk && |{ gs_alv-mkmnr PAD = ' ' WIDTH = 8 }| && gs_alv-version && sy-langu.

              PERFORM frm_read_text USING 'QPMT' 'QPMERKMAL' lv_name sy-langu
                                    CHANGING gs_alv-ltextkz2.
            ELSE.
              "读取短文本
              gs_alv-ltextkz2 = ls_qpmt-kurztext.
            ENDIF.

          ENDIF.



          READ TABLE lt_qmtt  INTO ls_qmtt  WITH KEY pmtnr = gs_alv-pmethode
          version = gs_alv-pmtversion.   "将QMTT 表字段赋予,检验方法
          IF sy-subrc = 0 .

            IF ls_qmtt-ltextkz = 'X'.

              lv_name = sy-mandt && gs_alv-werk && |{ gs_alv-pmethode PAD = ' ' WIDTH = 8 }| && gs_alv-pmtversion && sy-langu.
              "读取检验方法长文本
              PERFORM frm_read_text USING 'QMTT' 'QPMETHODE' lv_name sy-langu
                                  CHANGING gs_alv-ltextkz3.
            ELSE.
              "读取短文本
              gs_alv-ltextkz3 = ls_qmtt-kurztext.
            ENDIF.


          ENDIF.

          READ TABLE lt_t001l INTO ls_t001l  WITH KEY lgort = gs_alv-lagortchrg     "将库存地点描述表字段赋予
          werks = gs_alv-werk.
          IF sy-subrc = 0 .
            gs_alv-lgobe = ls_t001l-lgobe.
          ENDIF.

*          READ TABLE lt_t006a INTO ls_t006a  WITH KEY mseht = gs_alv-masseinhsw.    "将T006A 计量单位字段赋予
*
*          IF sy-subrc = 0 .
*            gs_alv-msehi = ls_t006a-mseht.
*          ENDIF.






          " 获取批次属性和物料特征

          IF gs_alv-matnr IS NOT INITIAL.
            IF gs_alv-charg IS NOT INITIAL.
*&---------------------------------------------------------------------*
*& 程序名：ZQMR008_F01
*&作者：Seashell Huang
*&模块：
*&创建日期：05.11.2019 18:13:23
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：获取供应商批次
*&---------------------------------------------------------------------*
              SELECT SINGLE
                licha
              INTO gs_alv-zlicha
              FROM mcha
              WHERE mcha~matnr = gs_alv-matnr
              AND mcha~charg = gs_alv-charg
              AND mcha~werks = s_werk.

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

                  IF gs_alv-mtart = 'Z010' OR gs_alv-mtart = 'Z020'.

                    READ TABLE lt_allocvalueschar WITH KEY charact = 'ZPACK_STA' INTO DATA(ls_allocvalueschar).
                    IF sy-subrc = 0.
                      gs_alv-zpack_sta = ls_allocvalueschar-value_char. "包装规格
                    ENDIF.

                  ELSEIF gs_alv-mtart = 'Z050'.  " 成品：物料特性001，分类为MARA-MATKL，输出字段ZPSPE的值

                    CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
                      EXPORTING
                        matnr  = gs_alv-matnr
                      TABLES
                        output = ltoutput.
*         " 开始取物料特征  " ZPSPE
                    READ TABLE  ltoutput WITH KEY atnam = 'ZPSPE' INTO DATA(ls_output).

                    IF sy-subrc = 0.
                      gs_alv-zpack_sta = ls_output-atwrt.
                    ENDIF.

                  ENDIF.


*&---------------------------------------------------------------------*
*& 程序名：ZQMR008_F01
*&作者：Seashell Huang
*&模块：
*&创建日期：05.11.2019 15:52:38
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：修改打印包装批号的取数方式,具体内容如下：
*                  字段打印包装批号：
*                  原辅料、包材、半成品：
*                  输出分类022 类ZBATCH_SIRIO，字段ZCHARGL的值
*&
*&---------------------------------------------------------------------*

****              打印包装批号
*                  CLEAR ls_allocvalueschar.
*                  READ TABLE lt_allocvalueschar WITH KEY charact = 'ZPACK_CHARG' INTO ls_allocvalueschar.
*                  IF sy-subrc = 0.
*                    gs_alv-zpack_charg = ls_allocvalueschar-value_char.  "打印包装批号,分类类BATCH_CP
*                  ENDIF.
*
*                  READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_BATCH_DYPC' INTO ls_allocvalueschar.
*                 IF sy-subrc = 0.
*                 gs_alv-zpack_charg = ls_allocvalueschar-value_char.  "打印包装批号，分类类BATCH_CP
*                 ENDIF.

***              打印包装批号

                  "判断物料类型是否是原辅料、包材、半成品
                  IF gs_alv-mtart = 'Z010' OR gs_alv-mtart = 'Z020' OR gs_alv-mtart = 'Z030'.
                    READ TABLE lt_allocvalueschar WITH KEY charact = 'ZCHARGL' INTO ls_allocvalueschar.
                    IF sy-subrc = 0.
                      gs_alv-zpack_charg = ls_allocvalueschar-value_char.  "打印包装批号，分类类BATCH_CP
                    ENDIF.
                  ELSEIF  gs_alv-mtart = 'Z050'.
                    READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_BATCH_DYPC' INTO ls_allocvalueschar.
                    IF sy-subrc = 0.
                      gs_alv-zpack_charg = ls_allocvalueschar-value_char.  "打印包装批号，分类类BATCH_CP
                    ENDIF.
                  ELSE.
                    gs_alv-zpack_charg = ''.
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

*&---------------------------------------------------------------------*
*& 程序名：ZQMR008_F01
*&作者：Seashell Huang
*&模块：
*&创建日期：05.11.2019 16:18:55
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：修改是否首次到货的取数逻辑
*              90天内同一供应商批号是否首次到货：
*              MCHA表中，通过物料+供应商批号,根据创建日期进行排序，
*              IF TODAY-创建日期>90,输出否，IF TODAY-创建日期<=90，
*              第一条为是，其余条目为否
*&---------------------------------------------------------------------*
              "判断供应商批次是否为空的，如果是空的，“是否供应商批次非首次”留空
              IF gs_alv-zlicha IS INITIAL.
                gs_alv-ltextkz4 = ''.
              ELSE.
                "如果供应商批次不为空的话
                "首先先从MCHA取出所有物料、供应商批次对应的数据，按照创建日期进行排序
                SELECT
                ersda
                charg
                INTO CORRESPONDING FIELDS OF TABLE lt_mcha
                FROM mcha
                WHERE mcha~matnr = gs_alv-matnr
                AND mcha~licha = gs_alv-zlicha
                ORDER BY ersda charg.
                "然后检查对应的物料、SAP批次和工厂，然后和上面的内表中的第一条数据进行对比，如果相等的话是首次，否则是非首次
                READ TABLE lt_mcha INTO ls_mcha2 INDEX 1.
                SELECT SINGLE
                ersda"创建日期
                charg
                INTO CORRESPONDING FIELDS OF ls_mcha
                FROM mcha
                WHERE mcha~matnr = gs_alv-matnr
                AND mcha~werks = gs_alv-werk
                AND mcha~charg = gs_alv-charg.
                IF ls_mcha-ersda = ls_mcha2-ersda AND ls_mcha-charg = ls_mcha2-charg AND sy-datum - ls_mcha2-ersda <= 90 .
*                  ls_all-zflag2 = '首次'.
                  gs_alv-ltextkz4 = '是'.

                ELSE.
*                  ls_all-zflag2 = '非首次'.
                  gs_alv-ltextkz4 = '否'.
                ENDIF.
              ENDIF.




**   判断90天内同一供应商批号非首次到货
*              DATA: lv_cnt TYPE i.
*              LOOP AT lt_mseg INTO ls_mseg WHERE  matnr = gs_alv-matnr     "看下取多少条数据
*              AND   werks = gs_alv-werk
*              AND    charg = gs_alv-charg
*              AND    lifnr = gs_alv-sellifnr.
*                lv_cnt = lv_cnt + 1.
*
*              ENDLOOP.
*
*              " 比较日期 长度
*              DATA: tday TYPE i.
*              DATA: tday2 TYPE d.
*              CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
*                EXPORTING
*                  i_date_from = gs_alv-zlwedt
*                  i_date_to   = sy-datum
*                IMPORTING
*                  e_days      = tday.
*
*              IF tday <= 90 AND lv_cnt >= 2.
*                gs_alv-ltextkz4 = '是'.
*              ELSE.
*                gs_alv-ltextkz4 = '否'.
*              ENDIF.

            ENDIF.
          ENDIF.




          "覆盖掉QPMK里面的字段
          CLEAR ls_qpmk.
          SELECT SINGLE
            prfql
            dummy40
            masseinhsw
            steuerkz
          INTO CORRESPONDING FIELDS OF ls_qpmk
          FROM qpmk
          WHERE qpmk~mkmnr = ls_qamv2-verwmerkm
          AND qpmk~version = ls_qamv2-mkversion.

          gs_alv-prfql = ls_qpmk-prfql.
          gs_alv-dummy40 = ls_qpmk-dummy40.
          gs_alv-steuerkz = ls_qpmk-steuerkz.

          "将T006A 计量单位字段赋予
          CLEAR ls_t006a.

          READ TABLE lt_t006a INTO ls_t006a  WITH KEY msehi = ls_qpmk-masseinhsw.

          IF sy-subrc = 0 .
            gs_alv-msehi = ls_t006a-msehl.
          ENDIF.


          "覆盖掉ltextkz3检验方法
          CLEAR ls_qmtt.
*      READ TABLE lt_qmtt INTO ls_qmtt WITH KEY pmtnr = ls_qamv2-pmethode
*      version = ls_qamv2-pmtversion.
          SELECT SINGLE
            kurztext
            ltextkz
            werks
            pmtnr
            version
          INTO CORRESPONDING FIELDS OF ls_qmtt
          FROM qmtt
          WHERE pmtnr = ls_qamv2-pmethode
            AND version = ls_qamv2-pmtversion
            AND werks = ls_qamv2-qmtb_werks. "20191209 新增

          IF ls_qmtt IS NOT INITIAL.

            IF ls_qmtt-ltextkz = 'X'.

              lv_name = sy-mandt && ls_qmtt-werks && |{ ls_qmtt-pmtnr PAD = ' ' WIDTH = 8 }| && gs_alv-version && sy-langu.
              "读取检验方法长文本
              PERFORM frm_read_text USING 'QMTT' 'QPMETHODE' lv_name sy-langu
                                  CHANGING gs_alv-ltextkz3.
            ELSE.
              "读取短文本
              gs_alv-ltextkz3 = ls_qmtt-kurztext.
            ENDIF.

          ENDIF.

          "覆盖掉规定标准ZH
          CLEAR ls_qpmt.
          SELECT SINGLE
            zaehler
            kurztext
            ltextkz
            mkmnr
            version
          INTO CORRESPONDING FIELDS OF ls_qpmt
          FROM qpmt
          WHERE qpmt~sprache = '1'
          AND qpmt~version = ls_qamv2-mkversion
          AND qpmt~mkmnr = ls_qamv2-verwmerkm
          AND qpmt~zaehler = ls_qamv2-qpmk_werks.


          IF ls_qpmt-ltextkz = 'X'.
            "读取主检验特征长文本 20191206

            lv_name = sy-mandt && ls_qpmt-zaehler && |{ ls_qpmt-mkmnr PAD = ' ' WIDTH = 8 }| && ls_qpmt-version && sy-langu.

            PERFORM frm_read_text USING 'QPMT' 'QPMERKMAL' lv_name sy-langu
                                  CHANGING gs_alv-ltextkz1.
          ELSE.
            "读取短文本
            gs_alv-ltextkz1 = ls_qpmt-kurztext.
          ENDIF.


*          gs_alv-ltextkz1 = ls_qpmt-kurztext.


          "覆盖掉内控标准
          CLEAR ls_qpmt.
          SELECT SINGLE
          kurztext
          INTO CORRESPONDING FIELDS OF ls_qpmt
          FROM qpmt
          WHERE qpmt~sprache = 'Z'
          AND qpmt~version = ls_qamv2-mkversion
          AND qpmt~mkmnr = ls_qamv2-verwmerkm
          AND qpmt~zaehler = ls_qamv2-qpmk_werks.

          IF ls_qpmt-ltextkz = 'X'.
            "读取主检验特征长文本 20191206

            lv_name = sy-mandt && ls_qpmt-zaehler && |{ ls_qpmt-mkmnr PAD = ' ' WIDTH = 8 }| && ls_qpmt-version && sy-langu.

            PERFORM frm_read_text USING 'QPMT' 'QPMERKMAL' lv_name sy-langu
                                  CHANGING gs_alv-ltextkz2.
          ELSE.
            "读取短文本
            gs_alv-ltextkz2 = ls_qpmt-kurztext.
          ENDIF.



          "覆盖掉规定标准EN
          CLEAR ls_qpmt.
          SELECT SINGLE
          kurztext
          INTO CORRESPONDING FIELDS OF ls_qpmt
          FROM qpmt
          WHERE qpmt~sprache = 'E'
          AND qpmt~version = ls_qamv2-mkversion
          AND qpmt~mkmnr = ls_qamv2-verwmerkm
          AND qpmt~zaehler = ls_qamv2-qpmk_werks.

          IF ls_qpmt-ltextkz = 'X'.
            "读取主检验特征长文本 20191206

            lv_name = sy-mandt && ls_qpmt-zaehler && |{ ls_qpmt-mkmnr PAD = ' ' WIDTH = 8 }| && ls_qpmt-version && sy-langu.

            PERFORM frm_read_text USING 'QPMT' 'QPMERKMAL' lv_name sy-langu
                                  CHANGING gs_alv-ltextkz5.
          ELSE.
            "读取短文本
            gs_alv-ltextkz5 = ls_qpmt-kurztext.
          ENDIF.





*&---------------------------------------------------------------------*
*&作者：Seashell Huang
*&修改日期：02.12.2019 16:28:03
*&---------------------------------------------------------------------*
*&修改记录：修改接检日期
*&---------------------------------------------------------------------*
          "覆盖接检日期
          CLEAR gs_alv-aendt.
          SELECT SINGLE
            frgdt
          INTO gs_alv-aendt
          FROM qprn
          WHERE qprn~plos = gs_alv-prueflos
            AND qprn~frgna NE '' .    "   20191206取下达人不为空的
*********************修改结束位置02.12.2019 16:31:16*************

*         检验岗位编码转为文本
          READ TABLE lt_tq11t INTO ls_tq11t WITH KEY prfql = gs_alv-prfql.  "将TQ11T 表字段赋予
          IF sy-subrc = 0 .

            gs_alv-prfql = ls_tq11t-pqtxt.

*          ENDIF. 20191207 top

            READ TABLE lt_check INTO ls_check WITH KEY prueflos = gs_alv-prueflos
                                                       charg = gs_alv-charg
                                                       matnr = gs_alv-matnr.
            IF sy-subrc <> 0.
              CLEAR ls_check.
              CONTINUE.
            ENDIF.
*          APPEND gs_alv TO gt_alv.
*
*        CLEAR gs_alv.
*判断是否打印
            DATA(temp) = gs_alv-steuerkz+25(1).

            IF temp = ''.
              gs_alv-print = '1'.
            ELSEIF temp = 'X'.
              gs_alv-print = '2'.
            ELSEIF temp = '*'.
              gs_alv-print = '3'.
            ENDIF.
            " 去除物料前面的零
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = gs_alv-matnr
              IMPORTING
                output = gs_alv-matnr.


            "  判断取样状态 ，取数 20191025 waymon
            READ TABLE lt_obtyp INTO ls_obtyp WITH KEY plos2 = gs_alv-prueflos.

            IF s_lsbox IS NOT INITIAL.
              IF s_lsbox = '10'.

                IF ls_obtyp-stat = 'I0002'.

*          gs_alv-obtyp = ls_obtyp-obtyp.
                  APPEND gs_alv TO gt_alv.

                ENDIF.
              ELSEIF s_lsbox = '20'.

                IF ls_obtyp-stat <> 'I0002'.
*          gs_alv-obtyp = ls_obtyp-obtyp.
                  APPEND gs_alv TO gt_alv.
                ENDIF.

              ENDIF.
            ELSE.
              APPEND gs_alv TO gt_alv.
            ENDIF.

          ENDIF." 20191207 end

          CLEAR gs_alv.       "  判断取样状态 ，取数 20191025 waymon
          CLEAR ls_qamv2.
          CLEAR ls_qamv.
        ENDLOOP.
        SORT gt_alv BY prueflos merknr.
*        DELETE ADJACENT DUPLICATES FROM gt_alv COMPARING ALL FIELDS.
      ENDIF.
    ENDIF.

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
  l_colpos 'PN_NR'    '取样编号'     '' ' ' ' ' '' '' '',
  l_colpos 'AENDT'    '接检日期'     '' ' ' ' ' '' '' '',
  l_colpos 'PAENDTERM'    '预计完成日期'     '' ' ' ' ' 'QALS' 'PAENDTERM' 'X',
  l_colpos 'MPDAU'    '平均检验期'     '' ' ' ' ' '' '' '',
  l_colpos 'KDAUF'    '销售订购单号'     '' ' ' ' ' '' '' '',
  l_colpos 'SORTL'    '客户检索码'     '' ' ' ' ' '' '' '',
  l_colpos 'PRUEFLOS'    '样品编号'     '' ' ' ' ' '' '' '',
  l_colpos 'MATNR'    '物料编码'     '' ' ' ' ' '' '' '',
  l_colpos 'MAKTX'    '物料名称'     '' ' ' ' ' '' '' '',
  l_colpos 'CHARG'    '成品批号'     '' ' ' ' ' '' '' '',
  l_colpos 'ZPACK_CHARG'    '打印包装批号'     '' ' ' ' ' '' '' '',
*  l_colpos 'CHARG'    'SAP批次号'     '' ' ' ' ' '' '' '',
  l_colpos 'GROES'    '规格型号'     '' ' ' ' ' '' '' '',
  l_colpos 'ZPACK_STA'    '包装规格'     '' ' ' ' ' '' '' '',
  l_colpos 'PRFQL'    '检验岗位'     '' ' ' ' ' '' '' '',
  l_colpos 'QDYNREGEL'    '动态修改规则'     '' ' ' ' ' '' '' '',
  l_colpos 'DUMMY40'    '检验项目'     '' ' ' ' ' '' '' '',
  l_colpos 'MSEHI'    '计量单位'     '' ' ' ' ' '' '' '',
  l_colpos 'LTEXTKZ1'    '规定标准'     '' ' ' ' ' '' '' '',
  l_colpos 'LTEXTKZ5'    '规定标准(EN)'     '' ' ' ' ' '' '' '',
  l_colpos 'LTEXTKZ2'    '内控标准'     '' ' ' ' ' '' '' '',
  l_colpos 'LTEXTKZ3'    '检验方法'     '' ' ' ' ' '' '' '',
  l_colpos 'LGOBE'    '库存地'     '' ' ' ' ' '' '' '',
  l_colpos 'MENGE'    '样本数量'     '' ' ' ' ' '' '' '',
  l_colpos 'MEINH'    '样本单位'     '' ' ' ' ' '' '' '',
  l_colpos 'LTEXTKZ4'    '90天内同一供应商批号是否首次到货'     '' ' ' ' ' '' '' '',
  l_colpos 'PRINT'    '是否打印'     '' ' ' ' ' '' '' ''.



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

FORM frm_authority_check.
  AUTHORITY-CHECK OBJECT 'Q_INSPLOT'
*    ID 'QACT_ILOT' FIELD 'xxxxxxxx'
   ID 'WERKS' FIELD s_werk.
*    ID 'QPRUEFART' FIELD 'xxxxxxxx'
*    ID 'QMATAUTH' FIELD 'xxxxxxxx'.
  IF sy-subrc <> 0.
*   Implement a suitable exception handling here
    MESSAGE '权限检查失败' TYPE 'E'.
    gv_sel_error = 'X'.
  ELSE.
    MESSAGE '有权限' TYPE 'S'.
    gv_sel_error = ''.
  ENDIF.

ENDFORM.


FORM frm_read_text USING id TYPE thead-tdid
                         object TYPE thead-tdobject
                         name TYPE thead-tdname
                         langu TYPE thead-tdspras
                   CHANGING ret TYPE char200.
  ret = ''.
  DATA: lt_tline TYPE TABLE OF tline.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = id
      language                = langu
      name                    = name
      object                  = object
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
*   IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc EQ 0.
    LOOP AT lt_tline ASSIGNING FIELD-SYMBOL(<fs_line>).
      ret = ret && <fs_line>-tdline.
    ENDLOOP.
  ENDIF.

ENDFORM.