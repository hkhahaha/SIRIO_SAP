*&---------------------------------------------------------------------*
*& 包含               ZSDR0018_MAIN
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.

END-OF-SELECTION.

FORM getdata.
  DATA:lt_aufm  TYPE TABLE OF aufm,
       ls_aufm  TYPE aufm,
       lt_mseg  TYPE TABLE OF mseg,
       ls_mseg  TYPE mseg,
       lt_vbap  TYPE TABLE OF vbap,
       ls_vbap  TYPE vbap,
       lt_vbep  TYPE TABLE OF vbep,
       ls_vbep  TYPE vbep,
       lt_vbak  TYPE TABLE OF vbak,
       ls_vbak  TYPE vbak,
       lt_alv   TYPE TABLE OF ty_alv,
       ls_alv   TYPE ty_alv,
       lt_vbapk TYPE TABLE OF ty_vbapk,
       ls_vbapk TYPE ty_vbapk,
       lt_vbfa  TYPE TABLE OF vbfa,
       lt_vbfa2 TYPE TABLE OF vbfa,
       ls_vbfa  TYPE vbfa.
  "首先先获取AUFM表里面的数据
  SELECT
    budat
    a~bwart
    a~werks
    a~kdauf
    a~mblnr
    a~zeile
    a~kdauf
    a~kdpos
    b~ettyp
    a~wempf
    a~mblnr
    a~zeile
    INTO CORRESPONDING FIELDS OF TABLE lt_aufm
    FROM  aufm AS a
    INNER JOIN vbep AS b
    ON a~kdpos = b~posnr
    AND a~kdauf = b~vbeln
    INNER JOIN mseg AS c
    ON c~mblnr = a~mblnr
    AND c~zeile = a~zeile
    WHERE a~budat = sy-datum
    AND a~bwart = '101'
    AND a~werks IN s_werks
    AND a~kdauf <> ''
    AND a~budat IN s_budat
    AND b~ettyp NOT LIKE '%ZB%'
    AND c~wempf <> '此单结束'
    AND c~smbln <> a~mblnr
    AND c~smblp <> a~zeile.

  SELECT
   c~kunnr
   c~ernam
   c~vbeln
   d~posnr
   c~vkorg
   c~vtweg
   c~auart
   d~matnr
   d~arktx
   d~kwmeng
   d~zieme
   INTO CORRESPONDING FIELDS OF TABLE lt_vbapk
   FROM vbak AS c
   INNER JOIN vbap AS d
   ON c~vbeln = d~vbeln
   FOR ALL ENTRIES IN lt_aufm
   WHERE c~vbeln = lt_aufm-kdauf
     AND d~posnr = lt_aufm-kdpos.



  SELECT
    c~kunnr
    c~ernam
    c~vbeln
    d~posnr
    c~vkorg
    c~vtweg
    c~auart
    d~matnr
    d~arktx
    d~kwmeng
    d~zieme
    e~ettyp
    APPENDING CORRESPONDING FIELDS OF TABLE lt_vbapk
    FROM vbak AS c
    INNER JOIN vbap AS d
    ON c~vbeln = d~vbeln
    INNER JOIN vbep AS e
    ON e~vbeln = d~posnr
    AND e~vbeln = d~vbeln
    WHERE c~auart = 'ZA*'
    AND c~kunnr IN s_kunnr
    AND c~ernam IN s_ernam
    AND c~vbeln IN s_vbeln
    AND c~vkorg IN s_vkorg
    AND d~kwmeng <= 1
    AND e~ettyp NOT LIKE '%ZB%'.

  SORT lt_vbapk BY vbeln posnr.
  DELETE ADJACENT DUPLICATES FROM lt_vbapk COMPARING vbeln posnr.

  MOVE-CORRESPONDING lt_vbapk TO lt_alv.

  LOOP AT lt_alv INTO ls_alv.
    CLEAR lt_vbfa.
    SELECT
    vbelv
    posnv
    rfmng
    vbtyp_n
    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
    FROM vbfa
    WHERE vbfa~vbelv = ls_alv-vbeln
    AND vbfa~posnv = ls_alv-posnr
    AND vbfa~vbtyp_n IN ( 'R','h' ).
    "获取过账数量
    IF lt_vbfa IS NOT INITIAL.
      LOOP AT lt_vbfa INTO ls_vbfa.
        COLLECT ls_vbfa INTO lt_vbfa2.
        CLEAR ls_vbfa.
      ENDLOOP.
      READ TABLE lt_vbfa2 INTO ls_vbfa WITH KEY vbelv = ls_vbfa-vbelv
                                                posnv = ls_vbfa-posnv
                                                vbtyp_n = 'R'.
      ls_alv-number1 = ls_vbfa-rfmng.
      CLEAR ls_vbfa.
      READ TABLE lt_vbfa2 INTO ls_vbfa WITH KEY vbelv = ls_vbfa-vbelv
                                                posnv = ls_vbfa-posnv
                                                vbtyp_n = 'h'.
      ls_alv-number1 = ls_alv-number1 - ls_vbfa-rfmng.
    ENDIF.
    "当前库存数量
    ls_alv-number2  = '222'.
    "已入库数量
    ls_alv-number3 = '333'.
    MODIFY TABLE lt_alv FROM  ls_alv TRANSPORTING number1 number2 number3.
  ENDLOOP.











ENDFORM.

FORM catalog.
ENDFORM.

FORM alvshow.
ENDFORM.