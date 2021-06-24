*&---------------------------------------------------------------------*
*& Report ZSDR0022
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0022.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:vbak,zsdt0019,but000,vbpa,knvp,makt,vbap,zsdt0025.
TYPES:BEGIN OF ty_alv,
        objnr TYPE vbak-objnr, "对象编号
        matnr TYPE vbap-matnr,
        umvkz TYPE vbap-umvkz,
        umvkn TYPE vbap-umvkn,
        knumv TYPE vbak-knumv,
        mwsbp TYPE vbap-mwsbp,
        ernam TYPE vbak-ernam,
        objek TYPE ausp-objek.
        INCLUDE TYPE zsdt0019b.
      TYPES:END OF ty_alv.
TYPES:BEGIN OF ty_val,
        objek TYPE ausp-objek,
      END OF ty_val.

DATA:ls_zsdt0019   TYPE zsdt0019,
     lt_zsdt0019   TYPE TABLE OF zsdt0019, "自定义表1
     lt_zsdt00192  TYPE TABLE OF zsdt0019, "自定义表1
     lt_zsdt0019b  TYPE TABLE OF zsdt0019b,
*     lt_zsdt0019b1 TYPE TABLE OF zsdt0019b,
     lt_zsdt0019b2 TYPE TABLE OF zsdt0019b,
     ls_zsdt0019b  TYPE zsdt0019b,
     lt_knvp       TYPE TABLE OF knvp,
     ls_knvp       TYPE knvp,
     lt_alv        TYPE TABLE OF ty_alv,
     ls_alv        TYPE ty_alv,
     lt_jest       TYPE TABLE OF jest,
     ls_jest       TYPE jest,
     lt_jcds       TYPE TABLE OF jcds,
     ls_jcds       TYPE jcds,
     lt_but000     TYPE TABLE OF but000,
     ls_but000     TYPE but000,
*     lt_vbpa      TYPE TABLE OF vbpa,
*     ls_vbpa      TYPE vbpa,
     lt_tvakt      TYPE TABLE OF tvakt,
     ls_tvakt      TYPE tvakt,
     lt_val        TYPE TABLE OF ty_val,
     ls_val        TYPE ty_val.

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_auart FOR vbak-auart,"订单类型
               s_erdat FOR vbak-erdat,"创建日期"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
               s_udate FOR vbak-erdat,"二审日期
               s_vbeln FOR vbak-vbeln,
               s_posnr FOR vbap-posnr,
               s_partn FOR zsdt0019-partner."人员职位

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-002.
PARAMETERS: p_flag AS CHECKBOX."是否写入后台表
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk.



START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
*定义子程序
FORM getdata."查询语句
  DATA:ls_s_udate LIKE LINE OF s_udate,
       lv_zpspe   TYPE ausp-atinn.
  "将特性的名称转换成为内码值
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'ZPSPE'
    IMPORTING
      output = lv_zpspe.

*  "如果发送人员职位字段为空的话，从knvp表中查全部
*  IF s_job IS NOT INITIAL.
*    "首先，先查询所有的业务员
*    SELECT
*    *
*    INTO CORRESPONDING FIELDS OF TABLE @lt_zsdt0019
*    FROM zsdt0019
*    WHERE zsdt0019~job IN @s_job
*    AND zsdt0019~active = 'X'.
*    "然后筛选出LT_ZSDT0019里面的区域经理的账号到lt_zsdt_00192
*    SELECT
*    *
*    FROM @lt_zsdt0019 AS a
*    WHERE a~job = '02'
*    INTO CORRESPONDING FIELDS OF TABLE @lt_zsdt00192.
*    "然后重新把区域经理所属的大区重新查询一边，然后append到内表lt_zsdt0019里面
*    IF lt_zsdt00192 IS NOT INITIAL.
*      SELECT
*      *
*      APPENDING CORRESPONDING FIELDS OF TABLE lt_zsdt0019
*      FROM zsdt0019
*      FOR ALL ENTRIES IN lt_zsdt00192
*      WHERE zsdt0019~zdomain = lt_zsdt00192-zdomain
*      AND zsdt0019~active = 'X'.
*    ENDIF.
*    "然后对数据进行筛选，去掉重复的数据
*    SORT lt_zsdt0019 BY partner.
*    DELETE ADJACENT DUPLICATES FROM lt_zsdt0019 COMPARING ALL FIELDS.
*    "上面已经完成了对自定表1的数据获取的操作，接下来根据查到的业务员到VBAK表中获取相应的数据
*  ELSE.
*    "根据上面取到的业务员，到KUNN2取得所需要的字段数据
*    IF lt_zsdt0019 IS NOT INITIAL AND s_job IS NOT INITIAL.
*      SELECT
*      kunnr
*      vkorg
*      vtweg
*      spart
*      parvw
*      kunn2
*      INTO CORRESPONDING FIELDS OF TABLE lt_knvp
*      FROM knvp
*      FOR ALL ENTRIES IN lt_zsdt0019
*      WHERE knvp~kunn2  = lt_zsdt0019-partner
*      AND knvp~parvw = 'Z3'.
*    ELSEIF s_job IS INITIAL.
*      SELECT
*      kunnr
*      vkorg
*      vtweg
*      spart
*      parvw
*      kunn2
*      INTO CORRESPONDING FIELDS OF TABLE lt_knvp
*      FROM knvp
*      WHERE knvp~parvw = 'Z3'.
*    ELSE.
*      EXIT.
*    ENDIF.
*  ENDIF.




  SELECT
    auart
    vbak~erdat
    vbak~objnr
    vbak~erdat
    vbak~kunnr
    vbak~vkorg
    vbak~vtweg
    vbak~spart
    vbak~vbeln
    vbap~posnr
    vbak~vbeln
    vbap~matwa
    vbap~posnr
    vbap~matnr
    vbap~kwmeng AS kwmeng2
    vbap~werks
    vbap~umvkz
    vbap~umvkn
    vbap~meins
    vbap~vrkme
    vbap~znbjhq
    vbap~zatpdfrq
    vbap~netwr
    vbap~mwsbp
    vbak~ernam
    vbak~knumv
    werks
    knvp~kunn2
    vbap~zysjhq
  INTO CORRESPONDING FIELDS OF TABLE lt_alv
  FROM vbap
  INNER JOIN vbak
  ON vbap~vbeln = vbak~vbeln
  INNER JOIN knvp
  ON knvp~kunnr = vbak~kunnr
  AND knvp~vkorg = vbak~vkorg
  AND knvp~vtweg = vbak~vtweg
  AND knvp~spart = vbak~spart
  WHERE vbak~auart IN s_auart
    AND vbak~erdat IN s_erdat
    AND vbak~vbeln IN s_vbeln
    AND vbap~posnr IN s_posnr
    AND knvp~parvw IN ('Z3','')
    AND knvp~kunn2 IN s_partn
    AND vbap~kwmeng <> '0.01'.


  "下面查询的是来自于其他表的数据
  IF lt_alv IS NOT INITIAL.
    "最晚入库日期相关的字段
    SELECT
    mblnr,
    zeile,
    mjahr,
    kdauf,
    kdpos,
    matnr,
    bwart,
    budat,
    lgort
    INTO TABLE @DATA(lt_aufma)
    FROM aufm
    FOR ALL ENTRIES IN @lt_alv
    WHERE aufm~kdauf = @lt_alv-vbeln
    AND aufm~kdpos = @lt_alv-posnr
    AND aufm~matnr = @lt_alv-matnr
    AND aufm~bwart = '101'
    AND aufm~lgort IN ('1040','2040','3040','1060','2060','3060','1070','2070','3070','1080','2080','3080' ).
*SORT itab BY col1 ASCENDING col2 DESCENDING.
    SORT lt_aufma BY budat DESCENDING kdauf kdpos ASCENDING .


    "VBFA表，用于计算开票量
    SELECT
    vbelv AS vbeln,
    posnv AS posnr,
    rfmng,
    vbtyp_n,
    erdat,
    ruuid
    INTO TABLE @DATA(lt_vbfa)
    FROM vbfa
    FOR ALL ENTRIES IN @lt_alv
    WHERE vbfa~vbelv = @lt_alv-vbeln
    AND vbfa~posnv = @lt_alv-posnr
    AND vbfa~vbtyp_n IN ( 'M','N','O','S' ).
    SORT lt_vbfa BY vbeln posnr ASCENDING erdat DESCENDING.
    "VBKD表
    SELECT
      vbeln,
      posnr,
      kursk
    FROM vbkd
    FOR ALL ENTRIES IN @lt_alv
    WHERE vbkd~vbeln = @lt_alv-vbeln
      AND vbkd~posnr = ''
    INTO TABLE @DATA(lt_vbkd).
    "Jest表
    SELECT
      objnr
      stat
      inact
      stat
    INTO CORRESPONDING FIELDS OF TABLE lt_jest
    FROM jest
    FOR ALL ENTRIES IN lt_alv
    WHERE jest~stat LIKE 'E%'
      AND jest~inact = ''
      AND jest~objnr = lt_alv-objnr.
    "JCDS表
    SELECT
      objnr,
      stat,
      udate,
      chgnr
    FROM jcds
    INTO CORRESPONDING FIELDS OF TABLE @lt_jcds
    FOR ALL ENTRIES IN @lt_alv
    WHERE jcds~stat = 'E0003'
      AND jcds~objnr = @lt_alv-objnr.
    SORT lt_jcds BY objnr stat chgnr.
    "BUT000
    SELECT
      partner
      bu_sort1
      name_org1
    INTO CORRESPONDING FIELDS OF TABLE lt_but000
    FROM but000
    FOR ALL ENTRIES IN lt_alv
    WHERE but000~partner = lt_alv-kunnr.

    SELECT
    partner
    bu_sort1
    name_org1
    APPENDING CORRESPONDING FIELDS OF TABLE lt_but000
    FROM but000
    FOR ALL ENTRIES IN lt_alv
    WHERE but000~partner = lt_alv-kunn2.
    "VBPA表
    SELECT
      vbeln,
      posnr,
      parvw,
      kunnr,
      partner,
      name_org1,
      name_org2,
      name_org3,
      name_org4
    INTO TABLE @DATA(lt_vbpa)
    FROM vbpa
    INNER JOIN but000
    ON but000~partner = vbpa~kunnr
    FOR ALL ENTRIES IN @lt_alv
    WHERE vbpa~parvw = 'Z1'
      AND vbpa~posnr = ''
      AND vbpa~vbeln = @lt_alv-vbeln.




    "TVAKT表
    SELECT
      *
    INTO CORRESPONDING FIELDS OF TABLE lt_tvakt
    FROM tvakt
    FOR ALL ENTRIES IN lt_alv
    WHERE tvakt~spras = '1'
      AND tvakt~auart = lt_alv-auart.

    "T052U表
    SELECT
      t052u~text1,
      vbeln,
      t052u~zterm,
      t052u~ztagg,
      posnr
    INTO TABLE @DATA(lt_t052u)
    FROM t052u
    INNER JOIN vbkd
    ON vbkd~zterm = t052u~zterm
    FOR ALL ENTRIES IN @lt_alv
    WHERE t052u~spras = '1'
      AND vbkd~vbeln = @lt_alv-vbeln.

    "MAKT
    SELECT
      matnr,
      maktx
    INTO TABLE @DATA(lt_makt)
    FROM makt
    FOR ALL ENTRIES IN @lt_alv
    WHERE makt~spras = '1'
      AND makt~matnr = @lt_alv-matnr.

    "MARA表
    SELECT
      matnr,
      groes,
      matkl
    INTO TABLE @DATA(lt_mara)
    FROM mara
    FOR ALL ENTRIES IN @lt_alv
    WHERE mara~matnr = @lt_alv-matnr.

    "对物料号进行处理
    LOOP AT lt_alv INTO ls_alv.
      ls_alv-objek = ls_alv-matnr.
      MODIFY lt_alv FROM ls_alv.
      CLEAR ls_alv.
    ENDLOOP.
    "上面处理物料号之后，查询对应的全部AUSP的数据
    SELECT
      objek,
      atwrt,
      atinn
    INTO TABLE @DATA(lt_ausp)
    FROM ausp
    FOR ALL ENTRIES IN @lt_alv
    WHERE ausp~objek = @lt_alv-objek
      AND ausp~atinn = @lv_zpspe.
    "获取文本表的数据
    SELECT
      *
    INTO TABLE @DATA(lt_t006a)
    FROM t006a
    WHERE t006a~spras = '1'.
    "PRCD_ELEMENTS表
    SELECT
      knumv,
      kschl,
      kinak,
      kbetr,
      kposn,
      stunr,
      zaehk,
      kpein
    INTO TABLE @DATA(lt_prcd_elements)
    FROM prcd_elements
    FOR ALL ENTRIES IN @lt_alv
    WHERE prcd_elements~knumv = @lt_alv-knumv
      AND kschl IN ('ZP00','ZP02')
      AND kinak = ''.
    "VBEP表
    SELECT
      vbeln,
      posnr,
      ettyp,
      umvkz,
      umvkn,
      etenr
    INTO TABLE @DATA(lt_vbep)
    FROM vbep
    FOR ALL ENTRIES IN @lt_alv
    WHERE vbep~vbeln = @lt_alv-vbeln
      AND vbep~posnr = @lt_alv-posnr.
    "VBFA表
    SELECT
      vbelv,
      posnv,
      vbtyp_n,
      rfmng,
      vbeln,
      posnn,
      ruuid
    INTO TABLE @DATA(lt_vbfa_all)
    FROM vbfa
    FOR ALL ENTRIES IN @lt_alv
    WHERE vbfa~vbelv = @lt_alv-vbeln
      AND vbfa~posnv = @lt_alv-posnr
      AND vbfa~vbtyp_n IN ( 'h','R' ).
    "保存类型为R的
    SELECT
    vbelv,
    posnv,
    vbtyp_n,
    SUM( rfmng ) AS rfmeng
    FROM @lt_vbfa_all AS a
    GROUP BY vbelv,posnv,vbtyp_n
    INTO TABLE @DATA(lt_vbfa_r).
    "保存类型为h的
    SELECT
    vbelv,
    posnv,
    vbtyp_n,
    SUM( rfmng ) AS rfmeng
    FROM @lt_vbfa_all AS a
    GROUP BY vbelv,posnv,vbtyp_n
    INTO TABLE @DATA(lt_vbfa_h).
    "ADRP表
    SELECT
      bname,
      usr21~persnumber,
      name_text,
      date_from,
      nation
    INTO TABLE @DATA(lt_adrp)
    FROM adrp
    INNER JOIN usr21
    ON adrp~persnumber = usr21~persnumber
    FOR ALL ENTRIES IN @lt_alv
    WHERE usr21~bname = @lt_alv-ernam.
    "MSKA表
    SELECT
      vbeln,
      posnr,
      matnr,
      werks,
      lgort,
      kalab,
      kains,
      charg,
      sobkz
    INTO TABLE @DATA(lt_mska)
    FROM mska
    FOR ALL ENTRIES IN @lt_alv
    WHERE mska~vbeln = @lt_alv-vbeln
      AND mska~posnr = @lt_alv-posnr
      AND mska~werks = @lt_alv-werks
      AND mska~lgort IN ('1040','2040','3040','1070','2070','3070').

    "T023T表
    SELECT
      matkl,
      wgbez
    INTO TABLE @DATA(lt_t023t)
    FROM t023t.






  ENDIF.

  "继续添加BUT000
  IF lt_knvp IS NOT INITIAL.
    SELECT
    partner
    bu_sort1
    name_org1
    APPENDING CORRESPONDING FIELDS OF TABLE lt_but000
    FROM but000
    FOR ALL ENTRIES IN lt_knvp
    WHERE but000~partner = lt_knvp-kunn2.
  ENDIF.




  DATA id(30) TYPE c VALUE 1.
  "下面开始循环读取数据
  LOOP AT lt_alv INTO ls_alv.
    MOVE-CORRESPONDING ls_alv TO ls_zsdt0019b.
    "开票量
    LOOP AT lt_vbfa INTO DATA(ls_vbfa) WHERE vbeln = ls_alv-vbeln AND posnr = ls_alv-posnr.
*      ls_alv-invoice = ls_alv-invoice + ls_vbfa-rfmng.
      IF ls_vbfa-vbtyp_n = 'M' OR ls_vbfa-vbtyp_n = 'O'.
        ls_alv-invoice = ls_alv-invoice + ls_vbfa-rfmng.
      ELSEIF ls_vbfa-vbtyp_n = 'N' OR ls_vbfa-vbtyp_n = 'S'.
        ls_alv-invoice = ls_alv-invoice - ls_vbfa-rfmng.
      ENDIF.
      CLEAR ls_vbfa.
    ENDLOOP.

    "最晚过账日期
    READ TABLE lt_aufma INTO DATA(ls_aufma) WITH KEY kdauf = ls_alv-vbeln
                                                     kdpos = ls_alv-posnr
                                                     matnr = ls_alv-matnr.
    IF sy-subrc = 0.
      ls_alv-budat = ls_aufma-budat.
    ENDIF.
    CLEAR ls_aufma.
    "订单状态
    READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_alv-objnr.
    IF sy-subrc = 0.
      ls_alv-stat = ls_jest-stat.
    ENDIF.
    CLEAR ls_jest.
    "订单状态描述、
    IF ls_alv-stat = 'E0002'.
      ls_alv-statdetail = '业务一审'.
    ELSEIF ls_alv-stat = 'E0003'.
      ls_alv-statdetail = '业务二审'.
    ENDIF.
    "二审日期
    IF ls_alv-stat = 'E0003'.
      READ TABLE lt_jcds INTO ls_jcds WITH KEY objnr = ls_alv-objnr.
      IF sy-subrc = 0.
        ls_alv-udate = ls_jcds-udate.
      ENDIF.
    ENDIF.
    CLEAR ls_jcds.
*    "要查二审，但是二审差不到，这些数据不要
*    IF ls_alv-udate IS not INITIAL AND s_udate IS NOT INITIAL.
*      CONTINUE.
*    ENDIF.
    "判断选择屏幕中的二审日期是否为空，如果为空的话，证明是查全部，如果不为空的话，证明只查询二审
*    IF s_udate IS NOT INITIAL.
*      READ TABLE s_udate INTO ls_s_udate WITH KEY low = ls_alv-udate.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*    CLEAR ls_s_udate.
    "检索项
    READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_alv-kunnr.
    IF sy-subrc = 0.
      ls_alv-bu_sort1 = ls_but000-bu_sort1.
    ENDIF.
    "实际客户
    CLEAR ls_but000.
    READ TABLE lt_vbpa INTO DATA(ls_vbpa) WITH KEY vbeln = ls_alv-vbeln.
    IF sy-subrc = 0.
      ls_alv-kunnr2 = ls_vbpa-kunnr.
      "实际客户的中文名称
      ls_alv-kunnr_nam = ls_vbpa-name_org1 && ls_vbpa-name_org2 && ls_vbpa-name_org3 && ls_vbpa-name_org4.
    ENDIF.
    CLEAR ls_vbpa.
    "实际业务员
    SELECT SINGLE
      kunn2
      name_org1
    INTO (ls_alv-salemanno,ls_alv-saleman)
    FROM but000
    INNER JOIN knvp
    ON but000~partner = knvp~kunn2
    WHERE knvp~kunnr = ls_alv-kunnr2
      AND knvp~vkorg = ls_alv-vkorg
      AND knvp~vtweg = ls_alv-vtweg
      AND knvp~spart = ls_alv-spart
      AND knvp~parvw = 'Z3'.
    "业务员
*    IF s_job IS NOT INITIAL.
*      READ TABLE lt_knvp INTO ls_knvp WITH KEY kunnr = ls_alv-kunnr
*                                               vkorg = ls_alv-vkorg
*                                               vtweg = ls_alv-vtweg
*                                               spart = ls_alv-spart.
*      IF sy-subrc = 0.
*        ls_alv-kunn2 = ls_alv-kunn2.
*      ELSE.
*        CONTINUE.
*      ENDIF.
*      CLEAR ls_knvp.
*    ENDIF.
    "售达方描述
    READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_alv-kunnr.
    IF sy-subrc = 0.
      ls_alv-kunnr_det = ls_but000-name_org1.
    ENDIF.
    CLEAR ls_but000.
    "业务员名称
    READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_alv-kunn2.
    IF sy-subrc = 0.
      ls_alv-name_org1 = ls_but000-name_org1.
    ENDIF.
    CLEAR ls_but000.
    "订单类型描述
    READ TABLE lt_tvakt INTO ls_tvakt WITH KEY auart = ls_alv-auart.
    IF sy-subrc = 0.
      ls_alv-bezei = ls_tvakt-bezei.
    ENDIF.
    CLEAR ls_tvakt.
    "付款方式
    READ TABLE lt_t052u INTO DATA(ls_t052u) WITH KEY vbeln = ls_alv-vbeln.
    IF sy-subrc = 0.
      ls_alv-text1 = ls_t052u-text1.
    ENDIF.
    CLEAR ls_t052u.
    "产品名称
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_alv-matnr.
    IF sy-subrc = 0.
      ls_alv-maktx = ls_makt-maktx.
    ENDIF.
    CLEAR ls_makt.
    "规格型号
    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_alv-matnr.
    IF sy-subrc = 0.
      ls_alv-groes = ls_mara-groes.
      DATA(lv_matkl) = ls_mara-matkl+0(4).
    ENDIF.
    CLEAR ls_mara.
    "包装规格
    READ TABLE lt_ausp INTO DATA(ls_ausp) WITH KEY objek = ls_alv-objek.
    IF sy-subrc = 0.
      ls_alv-atwrt = ls_ausp-atwrt.
    ENDIF.
    CLEAR ls_ausp.
    "基本计量数量
    IF ls_alv-umvkn IS NOT INITIAL.
      ls_alv-kwmeng = ls_alv-kwmeng2 * ls_alv-umvkz / ls_alv-umvkn.
    ENDIF.

    "开票量重新计算
    IF ls_alv-kwmeng IS NOT INITIAL AND ls_alv-kwmeng2 IS NOT INITIAL AND ( ls_alv-kwmeng / ls_alv-kwmeng2  ) <> 0.
      ls_alv-invoice = ls_alv-invoice /  ( ls_alv-kwmeng / ls_alv-kwmeng2  ).
    ENDIF.
    "基本计量单位、销售单位
    READ TABLE lt_t006a INTO DATA(ls_t006a) WITH KEY msehi = ls_alv-meins.
    IF sy-subrc = 0.
      ls_alv-meins = ls_t006a-msehl.
    ENDIF.
    CLEAR ls_t006a.
    READ TABLE lt_t006a INTO ls_t006a WITH KEY msehi = ls_alv-vrkme.
    IF sy-subrc = 0.
      ls_alv-vrkme = ls_t006a-msehl.
    ENDIF.
    CLEAR ls_t006a.
    "不含税金额
    READ TABLE lt_vbkd INTO DATA(ls_vbkd) WITH KEY vbeln = ls_alv-vbeln
                                                   posnr = ''.
    IF sy-subrc = 0.
      ls_alv-netwr2 = ls_alv-netwr * ls_vbkd-kursk.
    ENDIF.
    "价税合计
    ls_alv-netwr = ls_alv-netwr + ls_alv-mwsbp.
    "折前单价
    READ TABLE lt_prcd_elements INTO DATA(ls_prcd_elements) WITH KEY knumv = ls_alv-knumv
                                                                     kposn = ls_alv-posnr.
    IF sy-subrc = 0.
      ls_alv-kbetr = ls_prcd_elements-kbetr / ls_prcd_elements-kpein.
    ENDIF.
    CLEAR ls_prcd_elements.
    "计划行类型
    READ TABLE lt_vbep INTO DATA(ls_vbep) WITH KEY vbeln = ls_alv-vbeln
                                                   posnr = ls_alv-posnr.
    IF sy-subrc = 0.
      ls_alv-ettyp = ls_vbep-ettyp.
    ENDIF.
    CLEAR ls_vbep.
    "出库量
    READ TABLE lt_vbfa_r INTO DATA(ls_vbfa_r) WITH KEY vbelv = ls_alv-vbeln
                                                       posnv = ls_alv-posnr
                                                       vbtyp_n = 'R'.
    IF sy-subrc = 0.
      READ TABLE lt_vbfa_h INTO DATA(ls_vbfa_h) WITH KEY vbelv = ls_alv-vbeln
                                                         posnv = ls_alv-posnr
                                                         vbtyp_n = 'h'.
      ls_alv-rfmng = ls_vbfa_r-rfmeng - ls_vbfa_h-rfmeng.
    ENDIF.
    IF ls_alv-kwmeng IS NOT INITIAL AND ls_alv-kwmeng2 IS NOT INITIAL AND ( ls_alv-kwmeng / ls_alv-kwmeng2  ) <> 0.
      ls_alv-rfmng = ls_alv-rfmng /  ( ls_alv-kwmeng / ls_alv-kwmeng2  ).
    ENDIF.
    CLEAR:ls_vbfa_r,ls_vbfa_h.
    "制单人
    READ TABLE lt_adrp INTO DATA(ls_adrp) WITH KEY bname = ls_alv-ernam.
    IF sy-subrc = 0.
      ls_alv-name_text = ls_adrp-name_text.
    ENDIF.
    CLEAR ls_adrp.
    "非限制库存
    CLEAR:ls_alv-kalab,ls_alv-kains.
    LOOP AT lt_mska INTO DATA(ls_mska) WHERE matnr = ls_alv-matnr AND posnr = ls_alv-posnr AND vbeln = ls_alv-vbeln AND werks = ls_alv-werks.
      ls_alv-kalab = ls_alv-kalab + ls_mska-kalab.
      ls_alv-kains = ls_alv-kains + ls_mska-kains.
      CLEAR ls_mska.
    ENDLOOP.
*    READ TABLE lt_mska INTO ls_mska WITH KEY matnr = ls_alv-matnr
*                                                   posnr = ls_alv-posnr
*                                                   vbeln = ls_alv-vbeln
*                                                   werks = ls_alv-werks.
    IF ls_alv-umvkz <> 0.
      ls_alv-kalab = ls_alv-kalab / ls_alv-umvkz * ls_alv-umvkn.
      ls_alv-kains = ls_alv-kains / ls_alv-umvkz * ls_alv-umvkn.
    ELSE.
      ls_alv-kalab = ls_alv-kalab.
      ls_alv-kains = ls_alv-kains.
    ENDIF.
    CLEAR ls_mska.

    IF lv_matkl IS NOT INITIAL.
      READ TABLE lt_t023t INTO DATA(ls_t023t) WITH KEY matkl = lv_matkl.
      IF sy-subrc = 0.
        ls_alv-wgbez = ls_t023t-wgbez.
      ENDIF.
      CLEAR ls_t023t.
    ENDIF.


    ls_alv-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_alv-matnr i_out = 'X' ).
    ls_alv-matwa = zcl_bc_public=>conv_by_ddic( i_input = ls_alv-matwa i_out = 'X' ).
    MOVE-CORRESPONDING ls_alv TO ls_zsdt0019b.
    ls_zsdt0019b-id = id.
    APPEND ls_zsdt0019b TO lt_zsdt0019b.
    CLEAR:ls_alv,ls_zsdt0019b.
    CLEAR ls_vbep.
    id = id + 1.
  ENDLOOP.
  IF s_partn IS NOT INITIAL.
    LOOP AT lt_alv INTO ls_alv.
      READ TABLE s_partn INTO DATA(lv_part) WITH KEY low = ls_alv-kunn2.
      IF sy-subrc <> 0.
        DELETE lt_alv WHERE kunn2 = lv_part-low.
        DELETE lt_zsdt0019b WHERE kunn2 = lv_part-low.
      ENDIF.
      CLEAR ls_alv.
    ENDLOOP.
  ENDIF.
  SELECT
    *
  FROM @lt_alv AS a
  WHERE a~udate IN @s_udate
  INTO TABLE @lt_alv.

  SORT lt_alv BY vbeln posnr.

  SELECT
    *
  FROM @lt_zsdt0019b AS a
  WHERE a~udate IN @s_udate
  INTO TABLE @lt_zsdt0019b.


  DELETE lt_zsdt0019b WHERE udate IS INITIAL.
  "Seasehll Huang 2021.3 23 新增多方交易类型
*取数逻辑：
*上面实际上已经取到了多方交易的涉及的全部单据
*多方交易的原始单据为“Z1”类型，判断是否多方交易的方式是
*将销售订单反查ZSDT0025数据表里面的数据，如果存在的话那么可以证明是多方交易的类型
*可以通过“多方交易节点”字段判断销售订单的数据是不是最终单据或者是原始单据
*报表需要的是原始单据的数据，但是一部分数据在最终单据里面，所以要把数据填充回原始单据，然后将Z1类型改成最终单据的类型
  "(1)捞出全部单据，保存在lt_zsdt0019b1表里面，并且为原始单据
*  SELECT
*    a~*
*  FROM @lt_zsdt0019b AS a
*  INNER JOIN zsdt0025
*  ON a~vbeln = zsdt0025~vbeln
*  AND a~posnr = zsdt0025~posnr
*  WHERE zsdt0025~zjied = ''
*  AND   zsdt0025~zysdj = a~vbeln
*  INTO TABLE @DATA(lt_zsdt0019b1).
  "（2）捞出全部多方交易的数据，并且为最终单据，保存在LT_ZSDT0019B2表中
  SELECT
    a~*
  FROM @lt_zsdt0019b AS a
  INNER JOIN zsdt0025
  ON a~vbeln = zsdt0025~vbeln
  AND a~posnr = zsdt0025~posnr
  INTO TABLE @DATA(lt_zsdt0019b2).

  SELECT
    *
  INTO TABLE @DATA(lt_zsdt0025)
  FROM zsdt0025
  FOR ALL ENTRIES IN @lt_zsdt0019b
  WHERE zsdt0025~zysdj = @lt_zsdt0019b-vbeln.


  "（3）循环读取LT_ZSDT0019B1表里面的数据，将最终单据所需要的数据覆盖原始单据
  DATA:ls_zsdt0019b1 TYPE zsdt0019b,
       ls_zsdt0019b2 TYPE zsdt0019b.
  LOOP AT lt_zsdt0019b INTO ls_zsdt0019b.
    LOOP AT lt_zsdt0025 INTO DATA(ls_zsdt0025) WHERE zysdj = ls_zsdt0019b-vbeln AND zjied = '2'.
      READ TABLE lt_zsdt0019b2 INTO ls_zsdt0019b2 WITH KEY vbeln = ls_zsdt0025-vbeln posnr = ls_zsdt0025-posnr.
      IF sy-subrc = 0.
        ls_zsdt0019b-ettyp = ls_zsdt0019b2-ettyp.
        ls_zsdt0019b-werks = ls_zsdt0019b2-werks.
        MODIFY lt_zsdt0019b FROM ls_zsdt0019b.
      ENDIF.
      CLEAR ls_zsdt0019b2.
      CLEAR ls_zsdt0025.
    ENDLOOP.
    CLEAR ls_zsdt0019b.
  ENDLOOP.


  DELETE lt_zsdt0019b WHERE ettyp = 'Z1'.
  DELETE lt_zsdt0019b WHERE vtweg = '00'.
  DELETE lt_zsdt0019b WHERE ettyp = 'ZB' AND kains IS INITIAL AND kalab IS INITIAL.
  SORT lt_zsdt0019b BY ettyp DESCENDING udate ASCENDING.






  "如果标识为需要写入后台表的话，就先清空自定义表里面的数据
  IF p_flag = 'X'.
    DELETE FROM zsdt0019b.
    INSERT zsdt0019b FROM TABLE lt_zsdt0019b.
  ENDIF.

ENDFORM.

FORM catalog."表头
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


*  fieldcatset 'BUKRS' '公司代码' sy-tabix.
  fieldcatset   'STAT'   '订单状态'   sy-tabix.
  fieldcatset   'STATDETAIL'   '订单状态描述'   sy-tabix.
  fieldcatset   'UDATE'   '二审日期'   sy-tabix.
  fieldcatset   'ERDAT'   '创建日期'   sy-tabix.
  fieldcatset   'KUNNR'   '售达方'   sy-tabix.
  fieldcatset   'VKORG'   '销售组织'   sy-tabix.
  fieldcatset   'VTWEG'   '分销渠道'   sy-tabix.
  fieldcatset   'SPART'   '产品组'   sy-tabix.
  fieldcatset   'BU_SORT1'   '检索项'   sy-tabix.
  fieldcatset   'KUNNR2'   '实际客户'   sy-tabix.
  fieldcatset   'KUNN2'   '业务员'   sy-tabix.
  fieldcatset   'NAME_ORG1'   '业务员名称'   sy-tabix.
  fieldcatset   'SALEMANNO'   '实际业务员编号'   sy-tabix.
  fieldcatset   'SALEMAN'   '实际业务员'   sy-tabix.

  fieldcatset   'AUART'   '订单类型'   sy-tabix.
  fieldcatset   'BEZEI'   '订单类型描述'   sy-tabix.
  fieldcatset   'VBELN'   '单据号'   sy-tabix.
  fieldcatset   'TEXT1'   '付款方式'   sy-tabix.
  fieldcatset   'MATWA'   '产品编号'   sy-tabix.
  fieldcatset   'POSNR'   '行号'   sy-tabix.
  fieldcatset   'MAKTX'   '产品名称'   sy-tabix.
  fieldcatset   'GROES'   '规格型号'   sy-tabix.
  fieldcatset   'ATWRT'   '包装规格'   sy-tabix.
  fieldcatset   'KWMENG'   '基本计量数量'   sy-tabix.
  fieldcatset   'MEINS'   '基本计量单位'   sy-tabix.
  fieldcatset   'KWMENG2'   '销售数量'   sy-tabix.
  fieldcatset   'VRKME'   '销售单位'   sy-tabix.
  fieldcatset   'KBETR'   '折前单价'   sy-tabix.
  fieldcatset   'NETWR'   '价税合计'   sy-tabix.
  fieldcatset   'ETTYP'   '计划行类型'   sy-tabix.
  fieldcatset   'ZNBJHQ'   '内部交货日期'   sy-tabix.
  fieldcatset   'ZATPDFRQ'   'ATP答复日期'   sy-tabix.
  fieldcatset   'RFMNG'   '出库量'   sy-tabix.
  fieldcatset   'NAME_TEXT'   '制单人'   sy-tabix.
  fieldcatset   'KALAB'   '非限制库存'   sy-tabix.
  fieldcatset   'WGBEZ'   '物料中类名称'   sy-tabix.
  fieldcatset   'NETWR2'   '不含税金额'   sy-tabix.
  fieldcatset   'ZYSJHQ'   '原始交货日期'   sy-tabix.
  fieldcatset   'KUNNR_DET'   '售达方描述'   sy-tabix.
  fieldcatset   'KUNNR_NAM'   '实际客户名称'   sy-tabix.
  fieldcatset   'KAINS'   '质检库存'   sy-tabix.
  fieldcatset   'WERKS'   '工厂'   sy-tabix.
  fieldcatset   'INVOICE'   '开票量'   sy-tabix.
  fieldcatset   'BUDAT'   '最晚入库日期'   sy-tabix.



  READ TABLE fieldcat INDEX 3."读取报表第三列
  fieldcat-key = 'X'."定义为主键（颜色改变）
  MODIFY fieldcat INDEX 3."修改样式

  READ TABLE fieldcat INDEX 4.
  fieldcat-emphasize = 'C500'.
  MODIFY fieldcat INDEX 4.

  READ TABLE fieldcat INDEX 1.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 1.

  READ TABLE fieldcat INDEX 2.
  fieldcat-emphasize = 'C300'.
  MODIFY fieldcat INDEX 2.

  READ TABLE fieldcat INDEX 5.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 5.

  READ TABLE fieldcat INDEX 6.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 6.

  READ TABLE fieldcat INDEX 7.
  fieldcat-emphasize = 'C100'.
  MODIFY fieldcat INDEX 7.

  READ TABLE fieldcat INDEX 8.
  fieldcat-emphasize = 'C200'.
  MODIFY fieldcat INDEX 8.
  READ TABLE fieldcat INDEX 9.
  MODIFY fieldcat INDEX 9.

*layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'."斑马线的样式

ENDFORM.
*响应鼠标点击时间的子程序
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
*      READ TABLE lbkpf INDEX rs_selfield-tabindex.
*      SET PARAMETER ID 'BLN' FIELD lbkpf-belnr.
*      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.

FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZHKALV1'.
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
      t_outtab                 = lt_zsdt0019b
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.