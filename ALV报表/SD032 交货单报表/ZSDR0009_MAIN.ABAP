
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
     lt_mast          TYPE TABLE OF mast,
     ls_mast          TYPE mast,
     lt_stko          TYPE TABLE OF stko,
     ls_stko          TYPE stko,
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
     lt_dd07v         TYPE TABLE OF dd07v,
     ls_dd07v         TYPE dd07v,
     lt_vbrk          TYPE TABLE OF vbrk,
     ls_vbrk          TYPE vbrk.
DATA gv_sel_error TYPE flag.
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
PERFORM frm_authority_check.
CHECK gv_sel_error IS INITIAL."权限检查标识
PERFORM getdata.
PERFORM catalog USING flag_b.
PERFORM alvshow USING flag_b.

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
  lips~umvkz
  lips~umvkn
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
  vbap~posnr AS posnr_vbap
  vbap~kwmeng
  likp~ernam AS zernam
  likp~kunag
  vbap~meins AS meins2
  likp~zzqzt
  likp~cmgst
  likp~sdabw
  vbap~zkhwl

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
  AND vbap~vbeln IN pvbeln2
  AND likp~ernam IN puser
  AND likp~kunag IN pcustom.

  "客户
  SELECT
  partner
  name_org1
  name_org2
  name_org3
  name_org4
  INTO CORRESPONDING FIELDS OF TABLE lt_but000
  FROM but000
  FOR ALL ENTRIES IN lt_main
  WHERE but000~partner = lt_main-kunag.

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
    vbak~knumv,
    vbak~zckgj,
    vbak~vtweg
    INTO TABLE @DATA(lt_main2)
          FROM vbak
          INNER JOIN kna1
          ON kna1~kunnr = vbak~kunnr
          FOR ALL ENTRIES IN @lt_main
          WHERE vbak~vbeln = @lt_main-vgbel
          AND kna1~sortl IN @psortl
          AND vbak~vtweg IN @svtweg
          AND vbak~vkorg IN @pvkorg.

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
        ls_main-vtweg = ls_main2-vtweg.
        ls_main-zckgj = ls_main2-zckgj.
        ls_main-vtweg = ls_main2-vtweg.
        "出口构架取描述
        SELECT SINGLE
          landx
        INTO ls_main-zckgjt
        FROM t005t
        WHERE t005t~land1 = ls_main-zckgj
        AND t005t~spras = '1'.
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
      AND vbpa~posnr = ''
      AND vbpa~parvw = 'Z3'.
      "客户
*      SELECT SINGLE
*      name_org1
*      INTO ls_out-zcustom
*      FROM but000
*      INNER JOIN vbpa
*      ON but000~partner = vbpa~kunnr
*      WHERE vbpa~vbeln = ls_main-vbeln_likp
*      AND vbpa~posnr = ''
*        AND vbpa~parvw = 'AG'
*        AND vbpa~kunnr IN pcustom.
      READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_main-kunag.
      IF sy-subrc = 0.
        ls_out-zcustomertext = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
      ENDIF.
      ls_out-vtweg = ls_main-vtweg.
      "贷方
      ls_out-cmgst = ls_main-cmgst.
      ls_out-zcustom = ls_main-kunag.
      "出口国家
      ls_out-zckgj = ls_main-zckgj.
      ls_out-zckgjt = ls_main-zckgjt.
      "实际客户
      SELECT SINGLE
      name_org1
      bu_sort1
      INTO (ls_out-zruser,ls_out-bu_sort1)
      FROM but000
      INNER JOIN vbpa
      ON but000~partner = vbpa~kunnr
      WHERE vbpa~vbeln = ls_main-vbeln_likp
      AND vbpa~posnr = ''
      AND vbpa~parvw = 'Z1'.
      "实际客户编号
      SELECT SINGLE
      kunnr
      INTO ls_out-zruserno
      FROM vbpa
      WHERE vbpa~vbeln = ls_main-vbeln_likp
      AND vbpa~posnr = ''
      AND vbpa~parvw = 'Z1'.
*      "制单人
*      SELECT SINGLE
*        name_text
*      INTO ls_out-zmakeuser
*      FROM adrp
*      INNER JOIN usr21
*      ON adrp~persnumber = usr21~persnumber
*      WHERE adrp~persnumber IN puser
*        AND usr21~bname = ls_main-ernam2.
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
*      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
*        EXPORTING
*          matnr  = ls_main-matnr
*          atnam  = '0000000006'
*        TABLES
*          output = lt_material.
*      READ TABLE lt_material INTO ls_material INDEX 1.
      SELECT SINGLE
        hsdat
        vfdat
        INTO (ls_out-zhsdat,ls_out-vfdat)
        FROM mcha
        WHERE mcha~matnr = ls_main-matnr
        AND mcha~charg = ls_main-charg
        AND mcha~werks = ls_main-werks.
*      ls_out-zhsdat = ls_material-date_from.
      CLEAR ls_material.
      " 打印批号
      DATA:lv_atinn TYPE ausp-atinn.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'Z_BATCH_DYPC'
        IMPORTING
          output = lv_atinn.

      SELECT SINGLE
        atwrt
        INTO ls_out-zpack_charg
        FROM ausp
        INNER JOIN mcha
        ON ausp~objek = mcha~cuobj_bm
        WHERE ausp~atinn = lv_atinn
        AND ausp~klart = '022'
        AND mcha~matnr = ls_main-matnr
        AND mcha~charg = ls_main-charg
        AND mcha~werks = ls_main-werks.



*      CALL FUNCTION 'Z_FI_GET_MATERIAL_VALUE'
*        EXPORTING
*          matnr  = ls_main-matnr
*          atnam  = 'Z_BATCH_DYPC'
*        TABLES
*          output = lt_material.
*      READ TABLE lt_material INTO ls_material INDEX 1.
*      ls_out-zpack_charg = ls_material-atwrt.
*      CLEAR ls_material.




      "规格型号、物料小类编号、物料小类描述
      SELECT SINGLE
      groes
      matkl
      INTO CORRESPONDING FIELDS OF ls_mara
      FROM mara
      WHERE mara~matnr = ls_main-matnr.
      MOVE-CORRESPONDING ls_mara TO ls_out.
      ls_out-matkl = ls_out-matkl+0(4)."物料小类编码取前4位
      IF ls_mara IS NOT INITIAL.
        SELECT SINGLE
        wgbez
        INTO ls_out-wgbez
        FROM t023t
        WHERE t023t~matkl = ls_out-matkl.
      ENDIF.
      "客户合同单号
      SELECT SINGLE
      bstkd_e
      kursk
      INTO (ls_out-bstkd_e,ls_out-kursk)
      FROM vbkd
      WHERE vbkd~vbeln = ls_main-vbeln_vbak
      AND vbkd~posnr = ls_main-posnr_vbap.
      IF ls_out-bstkd_e IS INITIAL.
        SELECT SINGLE
        bstkd_e
        INTO ls_out-bstkd_e
        FROM vbkd
        WHERE vbkd~vbeln = ls_main-vbeln_vbak.
      ENDIF.
      IF ls_out-kursk IS INITIAL.
        SELECT SINGLE
        kursk
        INTO ls_out-kursk
        FROM vbkd
        WHERE vbkd~vbeln = ls_main-vbeln_vbak.
      ENDIF.
      CLEAR ls_prcd_elements-kbetr.
      "DDP价格
      SELECT SINGLE
      kbetr
      INTO CORRESPONDING FIELDS OF ls_prcd_elements
      FROM  prcd_elements
      INNER JOIN vbak
      ON prcd_elements~knumv = vbak~knumv
      WHERE prcd_elements~kschl = 'Z100'
      AND prcd_elements~kposn = ls_main-posnr_vbap
      AND vbak~vbeln = ls_main-vbeln_vbak.
      ls_out-ddpprice =  ls_prcd_elements-kbetr.
      DATA count TYPE i.
      " 开票数量
*      SELECT
*      vbelv
*      posnv
*      INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
*      FROM vbfa
*      WHERE vbfa~vbtyp_v = 'M'
*      AND vbfa~vbeln = ls_main-vgbel
*      AND vbfa~posnn = ls_main-vgpos.
*      IF lt_vbfa IS NOT INITIAL.
*        LOOP AT lt_vbfa INTO ls_vbfa.
*          SELECT SINGLE
*          vbeln
*          fksto
*          INTO CORRESPONDING FIELDS OF ls_vbrk
*          FROM vbrk
*          WHERE vbrk~vbeln = ls_vbfa-vbelv.
*          IF ls_vbrk-fksto IS NOT INITIAL.
*            DELETE lt_vbfa FROM ls_vbfa-vbelv.
*          ENDIF.
*        ENDLOOP.
*        CLEAR ls_vbfa.
*        LOOP AT lt_vbfa INTO ls_vbfa.
*          IF ls_vbfa-rfmng IS NOT INITIAL.
*            count = count + 1.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
      CLEAR ls_out-kpsum.
      CLEAR lt_vbfa.
      CLEAR ls_vbfa.
      CLEAR count.
      SELECT
      a~vbelv
      a~posnv
      a~vbtyp_n
      a~vbeln
      a~posnn
      a~rfmng
      INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
      FROM vbfa AS a
      INNER JOIN vbrk AS b
      ON a~vbeln = b~vbeln
      WHERE a~vbtyp_n IN ('M','O') AND b~sfakn  = '' AND b~fksto = ''
      AND a~vbelv = ls_main-vbeln_likp AND a~posnv = ls_main-posnr_lips.
      LOOP AT lt_vbfa INTO ls_vbfa.
        count = count + ls_vbfa-rfmng.
      ENDLOOP.
      ls_out-kpsum = ( ls_main-umvkn / ls_main-umvkz ) * count.
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
      CLEAR ls_prcd_elements-kbetr.
      SELECT SINGLE
      kbetr
      INTO CORRESPONDING FIELDS OF ls_prcd_elements
      FROM  prcd_elements
      WHERE prcd_elements~knumv = ls_main-knumv
      AND prcd_elements~kschl = 'MWSI'
      AND prcd_elements~kposn = ls_main-vgpos.
      ls_out-kbetr = ls_prcd_elements-kbetr.

      lv_name = ls_main-vbeln_likp.
      "备注
      CLEAR lines.
      CLEAR ls_out-z004.
      CLEAR ls_lines-tdline.
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
*      READ TABLE lines INTO ls_lines INDEX 1.
*      ls_out-z004 = ls_lines-tdline.
      LOOP AT lines INTO ls_lines.
        ls_out-z004 = ls_out-z004 && ls_lines-tdline.
      ENDLOOP.

      "制单人
      CLEAR ls_adrp.
      SELECT SINGLE
      name_text
      INTO CORRESPONDING FIELDS OF ls_adrp
      FROM adrp
      INNER JOIN usr21
      ON adrp~persnumber = usr21~persnumber
      WHERE usr21~bname = ls_main-zernam.

      "实际客户等级转成描述
      SELECT SINGLE
        vtext
      INTO ls_main-kukla
      FROM tkukt
      WHERE tkukt~spras = '1'
      AND tkukt~kukla = ls_main-kukla.

      "贷方
*      SELECT SINGLE
*        cmgst
*      into ls_out-cmgst
*      FROM VBUK
*      WHERE VBUK~vbeln = LS_MAIN-vbeln_vbak.


      "上面是对尚未完善的数据进行添加操作
      "下面是已经处理好的数据进行添加操作
      ls_out-baxssl = ( ls_main-umvkz / ls_main-umvkn ) * ls_main-lfimg." 基本计量数量
      ls_out-waerk = ls_main-waerk."订单币别
      ls_out-netwr = ls_main-mwsbp + ls_main-netwr."价税总计
      IF ls_main-kwmeng IS NOT INITIAL.
        ls_out-perprice = ( ls_main-mwsbp + ls_main-netwr ) / ls_main-kwmeng." 含税单价
      ENDIF.
      ls_out-sortl = ls_main-sortl."检索项
      ls_out-ernam = ls_adrp-name_text."制单人
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
      ls_out-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_main-matnr i_out = 'X' )."产品代号
*      ls_out-matnr = ls_main-matnr ."  产品代号
      ls_out-arktx = ls_main-arktx ."  产品名称
      ls_out-lfimg = ls_main-lfimg ."  销售数量
      ls_out-vrkme = ls_main-vrkme ."  销售单位
      IF svtweg IS NOT INITIAL AND ls_out-vtweg IS INITIAL.
        CLEAR ls_out.
        CONTINUE.
      ENDIF.
      IF pvkorg IS NOT INITIAL AND ls_out-vkorg IS INITIAL.
        CLEAR ls_out.
        CONTINUE.
      ENDIF.
      "分销渠道取描述
      SELECT SINGLE
        vtext
        INTO ls_out-vtweg2
        FROM tvtwt
        WHERE tvtwt~spras = '1'
        AND   tvtwt~vtweg = ls_out-vtweg.


      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = ls_main-vrkme
          language       = sy-langu
        IMPORTING
          long_text      = ls_out-vrkme
*         OUTPUT         =
*         SHORT_TEXT     =
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      ls_out-zjians  = ls_main-zjians  ."  件数
      ls_out-zlxsl = ls_main-zlxsl ."  零箱数量
      ls_out-zlfimg  = ls_main-lfimg  ."  实发销售数量
      ls_out-meins = ls_main-meins ."  基本计量单位
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = ls_main-meins
          language       = sy-langu
        IMPORTING
          long_text      = ls_out-meins
*         OUTPUT         =
*         SHORT_TEXT     =
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      ls_out-zkhwl = ls_main-zkhwl."客户物料编码
      SELECT SINGLE
        bezei
      INTO ls_out-sdabw
      FROM tvsakt
      WHERE tvsakt~spras = '1'
        AND tvsakt~sdabw = ls_main-sdabw.
*      ls_out-sdabw = ls_main-sdabw.

      ls_out-zzqzt = ls_main-zzqzt."账期审批状态
      ls_out-wbsta = ls_main-wbsta ."  交货单状态
      ls_out-charg = ls_main-charg ."  批量流水
      ls_out-zfhqd = ls_main-zfhqd ."  是否特殊发货清单
      ls_out-zshcoa  = ls_main-zshcoa  ."  是否需要随货COA
      ls_out-zfhsx = ls_main-zfhsx ."  发货顺序
      ls_out-zwmszt  = ls_main-zwmszt  ."  是否传输WMS
      ls_out-zsfxcp  = ls_main-zsfxcp  ."  新产品
      ls_out-zypsl = ls_main-zypsl ."  样品数量
      IF ls_main-kwmeng IS NOT INITIAL.
        ls_out-mwsbp = ls_main-mwsbp / ls_main-kwmeng * ls_out-lfimg."销项税额
*      ls_out-netwr = ls_main-netwr."不含税金额

        ls_out-price = ls_main-netwr / ls_main-kwmeng * ls_out-lfimg. "不含税金额
        ls_out-price2 = ls_out-price * ls_out-kursk."不含税金额CNY
      ENDIF.
      ls_out-netwr = ls_out-price + ls_out-mwsbp."价税总计
      ls_out-discnum = ls_out-perprice * ls_out-lfimg - ls_out-netwr."折扣金额
      ls_out-pmatn = zcl_bc_public=>conv_by_ddic( i_input = ls_out-pmatn i_out = 'X' ).

      "修改部分字段取域的描述
      SELECT
        domname
        valpos
        ddtext
        domvalue_l
      INTO CORRESPONDING FIELDS OF TABLE lt_dd07v
      FROM dd07v
      WHERE dd07v~ddlanguage = '1'
        AND dd07v~domname IN ('STATV','ZD_XFLAG','ZD_WMSZT','ZD_ZQZT','CMGST').
      READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'STATV'
                                                 domvalue_l = ls_out-wbsta.
      ls_out-wbsta = ls_dd07v-ddtext.
      CLEAR ls_dd07v.
      READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_XFLAG'
                                                 domvalue_l = ls_out-zfhqd.
      ls_out-zfhqd = ls_dd07v-ddtext.
      CLEAR ls_dd07v.
      READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_XFLAG'
                                                 domvalue_l = ls_out-zshcoa.
      ls_out-zshcoa = ls_dd07v-ddtext.
      CLEAR ls_dd07v.
      READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_XFLAG'
                                                 domvalue_l = ls_out-zsfxcp.
      ls_out-zsfxcp = ls_dd07v-ddtext.
      CLEAR ls_dd07v.
      READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_WMSZT'
                                                 domvalue_l = ls_out-zwmszt.
      ls_out-zwmszt = ls_dd07v-ddtext.
      CLEAR ls_dd07v.
      READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'ZD_ZQZT'
                                                 domvalue_l = ls_out-zzqzt.
      ls_out-zzqzt = ls_dd07v-ddtext.
      CLEAR ls_dd07v.
      READ TABLE lt_dd07v INTO ls_dd07v WITH KEY domname = 'CMGST'
                                                 domvalue_l = ls_out-cmgst.
      ls_out-cmgst = ls_dd07v-ddtext.
      CLEAR ls_dd07v.


*&---------------------------------------------------------------------*
*& 程序名：ZSDRTEST
*&作者：Seashell Huang
*&模块：
*&创建日期：01.11.2019 13:42:34
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：修改销售订单数量和销售订单单位的取值
*&
*&---------------------------------------------------------------------*
      ls_out-lfimg = ls_main-kwmeng."销售订单数量
*      ls_out-vrkme = ls_main-meins2."销售订单数量单位
*      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*        EXPORTING
*          input          = ls_main-meins2
*          language       = sy-langu
*        IMPORTING
*          long_text      = ls_out-vrkme
**         OUTPUT         =
**         SHORT_TEXT     =
*        EXCEPTIONS
*          unit_not_found = 1
*          OTHERS         = 2.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.

      IF ls_out-zlfimg IS NOT INITIAL.
        " 当是铝盒装的时候：
        IF flag_b = 'X'.
*&---------------------------------------------------------------------*
*&作者：Seashell Huang
*&修改日期：25.10.2019 14:38:41
*&---------------------------------------------------------------------*
*&修改记录：
*&添加物料组判断类型固定值501601
*&---------------------------------------------------------------------*
          DATA flag TYPE mara-matkl. "判断物料号是否市礼合装物料
          SELECT SINGLE
          matkl
          INTO flag
          FROM mara
          WHERE mara~matnr = ls_main-matnr.
          IF flag = '501601'."如果等于501601的话就是礼合装


*&---------------------------------------------------------------------*
*&作者：Seashell Huang
*&修改日期：23.10.2019 11:41:34
*&---------------------------------------------------------------------*
*&修改记录：修改礼合装的取数逻辑
*&
*&---------------------------------------------------------------------*
            "获取对应的物料清单
            SELECT SINGLE
            stlnr
            INTO CORRESPONDING FIELDS OF ls_mast
            FROM mast AS a
            WHERE a~matnr = ls_main-matnr
            AND a~werks = ls_main-werks
            AND a~stlan = '1'
            AND a~stlal = '01'.
            "BOM的基本数量
            SELECT SINGLE
            bmeng
            INTO CORRESPONDING FIELDS OF ls_stko
            FROM stko AS a
            WHERE a~stlty = 'M'
            AND a~stlal = '01'
            AND a~stlnr = ls_mast-stlnr.
            DATA lt_stb  LIKE TABLE OF stpox.
            DATA ls_stb  LIKE stpox.
            "获取BOM子件以及数量
            CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
              EXPORTING
*               FTREL                 = ' '
*               ALEKZ                 = ' '
*               ALTVO                 = ' '
*               AUFSW                 = ' '
*               AUMGB                 = ' '
*               AUMNG                 = 0
*               AUSKZ                 = ' '
*               AMIND                 = ' '
*               BAGRP                 = ' '
*               BEIKZ                 = ' '
*               BESSL                 = ' '
*               BGIXO                 = ' '
*               BREMS                 = ' '
                capid                 = 'PP01'
*               CHLST                 = ' '
*               COSPR                 = ' '
*               CUOBJ                 = 000000000000000
*               CUOVS                 = 0
*               CUOLS                 = ' '
                datuv                 = sy-datum
*               DELNL                 = ' '
*               DRLDT                 = ' '
*               EHNDL                 = ' '
*               EMENG                 = 0
*               ERSKZ                 = ' '
*               ERSSL                 = ' '
*               FBSTP                 = ' '
*               KNFBA                 = ' '
*               KSBVO                 = ' '
*               MBWLS                 = ' '
                mktls                 = 'X'
*               MDMPS                 = ' '
                mehrs                 = 'X'
*               MKMAT                 = ' '
*               MMAPS                 = ' '
*               SALWW                 = ' '
*               SPLWW                 = ' '
*               MMORY                 = ' '
                mtnrv                 = ls_main-matnr
*               NLINK                 = ' '
*               POSTP                 = ' '
*               RNDKZ                 = ' '
*               RVREL                 = ' '
*               SANFR                 = ' '
*               SANIN                 = ' '
*               SANKA                 = ' '
*               SANKO                 = ' '
*               SANVS                 = ' '
*               SCHGT                 = ' '
*               STKKZ                 = ' '
*               STLAL                 = ' '
*               STLAN                 = ' '
*               STPST                 = 0
                svwvo                 = 'X'
                werks                 = ls_main-werks
*               NORVL                 = ' '
*               MDNOT                 = ' '
*               PANOT                 = ' '
*               QVERW                 = ' '
*               VERID                 = ' '
                vrsvo                 = 'X'
*               SGT_SCAT              =
*               SGT_REL               =
*               CALLER_APP            =
*               BOM_VERSN             =
*           IMPORTING
*               TOPMAT                =
*               DSTST                 =
              TABLES
                stb                   = lt_stb
*               MATCAT                =
              EXCEPTIONS
                alt_not_found         = 1
                call_invalid          = 2
                material_not_found    = 3
                missing_authorization = 4
                no_bom_found          = 5
                no_plant_data         = 6
                no_suitable_bom_found = 7
                conversion_error      = 8
                OTHERS                = 9.
            IF sy-subrc <> 0.
* Implement suitable error handling here
            ENDIF.
            LOOP AT lt_stb INTO ls_stb.
*&---------------------------------------------------------------------*
*& 程序名：ZSDRTEST
*&作者：Seashell Huang
*&模块：
*&创建日期：01.11.2019 13:54:15
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：添加子件物料编码的判断，判断其是否是成品
*&
*&---------------------------------------------------------------------*
              CLEAR ls_mara.
              SELECT SINGLE
              mtart
              meins
              INTO CORRESPONDING FIELDS OF ls_mara
              FROM mara
              WHERE mara~matnr = ls_stb-idnrk.
              IF ls_mara-mtart = 'Z050'.
                "子件
                CLEAR ls_makt.
                SELECT SINGLE
                maktx
                INTO ls_out2-zsondetail
                FROM makt
                WHERE makt~matnr = ls_stb-idnrk.
                ls_stb-idnrk = zcl_bc_public=>conv_by_ddic( i_input = ls_stb-idnrk i_out = 'X' ).
                ls_out2-zson = ls_stb-idnrk.
                ls_out2-zmeins = ls_mara-meins.
                CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
                  EXPORTING
                    input          = ls_mara-meins
                    language       = sy-langu
                  IMPORTING
                    long_text      = ls_out2-zmeins
*                   OUTPUT         =
*                   SHORT_TEXT     =
                  EXCEPTIONS
                    unit_not_found = 1
                    OTHERS         = 2.
                IF sy-subrc <> 0.
* Implement suitable error handling here
                ENDIF.

                "礼合装子件销售数量
                ls_out2-zsum = ( ls_stb-menge / ls_stko-bmeng ) * ls_out-baxssl.

                MOVE-CORRESPONDING ls_out TO ls_out2.
                APPEND ls_out2 TO lt_out2.
                CLEAR ls_out2.
              ENDIF.
              CLEAR ls_mara.
            ENDLOOP.
            IF lt_stb IS INITIAL.
              MOVE-CORRESPONDING ls_out TO ls_out2.
              APPEND ls_out2 TO lt_out2.
              CLEAR ls_out2.
            ENDIF.
*            IF ls_out2-vbeln IS NOT INITIAL.
*              APPEND ls_out2 TO lt_out2.
*            ENDIF.
          ELSE.
            IF ls_out-vbeln IS NOT INITIAL.
              APPEND ls_out TO lt_out.
              CLEAR ls_out.
            ENDIF.






*          SELECT
*          zmatnr
*          zson
*          INTO CORRESPONDING FIELDS OF TABLE lt_zaluminium
*          FROM zaluminium
*          WHERE zaluminium~zmatnr = ls_out-matnr
*          AND zaluminium~zdatefrom <= ls_out-wadat
*          AND zaluminium~zdateend >= ls_out-wadat.
*          " 当是铝盒装的时候，把数据存在lt_out2里面
*          LOOP AT lt_zaluminium INTO ls_zaluminium.
*            ls_out2-zson = ls_zaluminium-zson.
*            ls_out2-zsum = ls_zaluminium-zson * ls_out-lfimg.
*            MOVE-CORRESPONDING ls_out TO ls_out2.
*          ENDLOOP.
*          MOVE-CORRESPONDING ls_out TO ls_out2.
*          IF ls_out2-vbeln IS NOT INITIAL.
*            APPEND ls_out2 TO lt_out2.
*          ENDIF.
*        ELSE.
*          IF ls_out-vbeln IS NOT INITIAL.
*            APPEND ls_out TO lt_out.
*          ENDIF.
          ENDIF.
        ENDIF.
        IF ls_out-vbeln IS NOT INITIAL.
          APPEND ls_out TO lt_out.
          CLEAR ls_out.
        ENDIF.

      ENDIF.

*      " 如果制单人字段为空，去掉相对应的输出行
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
      CLEAR ls_main.
      CLEAR ls_out.
    ENDLOOP.

  ENDIF.
ENDFORM.
FORM catalog USING flag_b.
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
  fieldcatset   'ERDAT'   '创建日期Date'    sy-tabix.
  fieldcatset   'ZFHRQ'   '发货日期'    sy-tabix.
  fieldcatset   'ZZQZT'      '账期审批状态'    sy-tabix.
  fieldcatset   'CMGST'      '贷方'    sy-tabix.
  fieldcatset   'WBSTA'   '交货单状态'   sy-tabix.
  fieldcatset   'WADAT'   '过账日期'    sy-tabix.
  fieldcatset   'VKORG'   '销售组织'    sy-tabix.
  fieldcatset   'VTWEG'   '销售渠道'    sy-tabix.
  fieldcatset   'VTWEG2'   '销售渠道描述'    sy-tabix.
  fieldcatset   'WERKS'   '工厂Factory'    sy-tabix.
  fieldcatset   'ERNAM'     '制单人Prepared by'   sy-tabix.
  fieldcatset   'VBELN'   '交货单Serial Number'   sy-tabix.
  fieldcatset   'POSNR'   '行项目'   sy-tabix.
  fieldcatset   'MATNR'   '产品代号Product ID'    sy-tabix.
  fieldcatset   'ARKTX'   '产品名称Product Name'    sy-tabix.
  fieldcatset   'ZPSPE'     '包装规格Packing Specification'    sy-tabix.
  fieldcatset   'LGORT'   '库存地址'    sy-tabix.
  fieldcatset   'ZLFIMG'      '发货数量Actual Marketing QTY'    sy-tabix.
  fieldcatset   'ZJIANS'      '件数Number of Packages'    sy-tabix.
  fieldcatset   'ZLXSL'     '零箱数量'    sy-tabix.
  fieldcatset   'BAXSSL'      '基本计量数量QTY'    sy-tabix.
  fieldcatset   'MEINS'   '基本计量单位Unit'    sy-tabix.
  fieldcatset   'CHARG'   '批量流水Batch Serial Number'    sy-tabix.
  fieldcatset   'ZPACK_CHARG'     '打印批号Bactch Number'    sy-tabix.
  fieldcatset   'ZHSDAT'      '生产日期MFG Date'    sy-tabix.
  fieldcatset   'VFDAT'      '货架寿命到期日'    sy-tabix.
  fieldcatset   'ZFHQD'     '是否特殊发货清单'    sy-tabix.
  fieldcatset   'ZSHCOA'      '是否需要随货COA'    sy-tabix.
  fieldcatset   'ZFHSX'     '发货顺序'    sy-tabix.
  fieldcatset   'ZWMSZT'      '是否传输WMS'   sy-tabix.
  fieldcatset   'SDABW'      '特殊处理标'    sy-tabix.
  fieldcatset   'Z004'      '备注Comment'    sy-tabix.
  fieldcatset   'ZCUSTOM'     '客户编码'    sy-tabix.
  fieldcatset   'ZCUSTOMERTEXT'     '客户描述Customer'    sy-tabix.
  fieldcatset   'SORTL'     '检索项'   sy-tabix.
  fieldcatset   'ZUSER'     '业务员Sales'   sy-tabix.
  fieldcatset   'ZRUSERNO'      '实际客户编号'    sy-tabix.
  fieldcatset   'ZRUSER'      '实际客户Actual Customer'    sy-tabix.
  fieldcatset   'BU_SORT1'      '实际客户的检索项'    sy-tabix.
  fieldcatset   'KUKLA'     '实际客户等级Classification'    sy-tabix.
  fieldcatset   'BEZEI'     '销售渠道/客户类型Sales Channel'   sy-tabix.
  fieldcatset   'VGBEL'   '销售订单Order Number'    sy-tabix.
  fieldcatset   'BSTKD_E'     '客户合同单号PO Number'    sy-tabix.
  fieldcatset   'ZCKGJ'      '出口国家简称'    sy-tabix.
  fieldcatset   'ZCKGJT'      '出口国家Country'    sy-tabix.
  fieldcatset   'VGPOS'   '行项目'   sy-tabix.
  fieldcatset   'VRKME'   '销售单位Marketing Unit'    sy-tabix.
  fieldcatset   'LFIMG'   '销售订单数量Marketing QTY'    sy-tabix.
  fieldcatset   'KPSUM'     '开票数量Invoice QTY'    sy-tabix.
  fieldcatset   'ZSFXCP'      '新产品New Product'   sy-tabix.
  fieldcatset   'ZYPSL'     '样品数量Sample QTY'    sy-tabix.
  fieldcatset   'KURSK'      '汇率'    sy-tabix.
  fieldcatset   'DDPPRICE'      'DDP价格DDP Price'   sy-tabix.
  fieldcatset   'PERPRICE'      '折后单价Unit price'    sy-tabix.
  fieldcatset   'DISCLV'      '折扣率Discount Rate'   sy-tabix.
  fieldcatset   'DISCNUM'     '折扣金额Discount'    sy-tabix.
  fieldcatset   'KBETR'     '税率(%)Tax Rate'    sy-tabix.
  fieldcatset   'MWSBP'     '销项税额'    sy-tabix.
  fieldcatset   'PRICE'     '不含税金额Amount（Tax excluding）'   sy-tabix.
  fieldcatset   'PRICE2'     '不含税金额CNY Amount（Tax excluding）'   sy-tabix.
  fieldcatset   'NETWR'     '价税总计Amount(Tax Including)'    sy-tabix.
  fieldcatset   'WAERK'     '订单币别Order Currency'    sy-tabix.
  fieldcatset   'PMATN'     '定价参考物料'    sy-tabix.
  fieldcatset   'MAKTX'     '定价参考物料描述'    sy-tabix.
  fieldcatset   'ZSTDNAME'      '标准名称'    sy-tabix.
  fieldcatset   'ZKHWL'      '客户物料编码'    sy-tabix.
  fieldcatset   'GROES'     '规格型号Filling Weight'    sy-tabix.
  fieldcatset   'MATKL'     '物料中类编号'    sy-tabix.
  fieldcatset   'WGBEZ'     '物料中类描述'    sy-tabix.
*  fieldcatset    '待定'      保质期   sy-tabix.
*  fieldcatset    '待定'      失效日期    sy-tabix.
*  fieldcatset   'SVBELN'      '销售订单号'   sy-tabix.

  IF flag_b = 'X'.
    fieldcatset   'ZSON'      '子件'    sy-tabix.
    fieldcatset   'ZSONDETAIL'      '子件描述'    sy-tabix.
    fieldcatset   'ZMEINS'      '子件单位'    sy-tabix.
    fieldcatset   'ZSUM'      '礼合装子件销售数量'    sy-tabix.

  ENDIF.
ENDFORM.

FORM alvshow USING flag_b.
  IF flag_b = 'X'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER = ' '
*       I_BUFFER_ACTIVE    = ' '
        i_callback_program = w_repid "程序名称
*       I_CALLBACK_PF_STATUS_SET          = 'FRM_SET_PF_STATUS'
*       I_CALLBACK_USER_COMMAND           = 'ALV_USER_COMMAND'"对ALV操作的时候触发所定义的子程序
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = '交货单报表' "标题名
*       I_GRID_SETTINGS    =
        is_layout          = layout "程序所定义的layout名称
        it_fieldcat        = fieldcat[] "定义fieldcat数据
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
*       IT_SORT            =
*       IT_FILTER          =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
*       I_SAVE             = ' '
*       IS_VARIANT         =
*       IT_EVENTS          =
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  = 0
*       I_HTML_HEIGHT_END  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        t_outtab           = lt_out2
      EXCEPTIONS "下面都是默认的
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER = ' '
*       I_BUFFER_ACTIVE    = ' '
        i_callback_program = w_repid "程序名称
*       I_CALLBACK_PF_STATUS_SET          = 'FRM_SET_PF_STATUS'
*       I_CALLBACK_USER_COMMAND           = 'ALV_USER_COMMAND'"对ALV操作的时候触发所定义的子程序
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = '交货单报表' "标题名
*       I_GRID_SETTINGS    =
        is_layout          = layout "程序所定义的layout名称
        it_fieldcat        = fieldcat[] "定义fieldcat数据
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
*       IT_SORT            =
*       IT_FILTER          =
*       IS_SEL_HIDE        =
        i_default          = 'X'
        i_save             = 'A'
*       IS_VARIANT         =
*       IT_EVENTS          =
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  = 0
*       I_HTML_HEIGHT_END  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        t_outtab           = lt_out
      EXCEPTIONS "下面都是默认的
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.

FORM frm_authority_check.
  DATA ls_pvkorg LIKE LINE OF pvkorg.
  DATA ls_svtweg LIKE LINE OF svtweg.
  LOOP AT svtweg INTO ls_svtweg.
*  DATA lv_werks TYPE werks_d.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
    ID 'VKORG' DUMMY
    ID 'VTWEG' FIELD ls_svtweg-low
    ID 'SPART' DUMMY
    ID 'ACTVT' DUMMY.
    IF sy-subrc <> 0.
*     Implement a suitable exception handling here
      MESSAGE '分销渠道无权限' TYPE 'E'.
      gv_sel_error = 'X'.
    ELSE.
*      MESSAGE '有权限' TYPE 'S'.
      gv_sel_error = ''.
    ENDIF.
    IF gv_sel_error = 'X'.
      EXIT.
    ENDIF.
    CLEAR ls_pvkorg.
  ENDLOOP.
  LOOP AT pvkorg INTO ls_pvkorg.
    AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
    ID 'VKORG' FIELD ls_pvkorg-low
    ID 'VTWEG' DUMMY
    ID 'SPART' DUMMY
    ID 'ACTVT' DUMMY.
    IF sy-subrc <> 0.
*     Implement a suitable exception handling here
      MESSAGE '销售组织无权限' TYPE 'E'.
      gv_sel_error = 'X'.
    ELSE.
*      MESSAGE '有权限' TYPE 'S'.
      gv_sel_error = ''.
    ENDIF.
    IF gv_sel_error = 'X'.
      EXIT.
    ENDIF.
    CLEAR ls_pvkorg.
  ENDLOOP.

ENDFORM.