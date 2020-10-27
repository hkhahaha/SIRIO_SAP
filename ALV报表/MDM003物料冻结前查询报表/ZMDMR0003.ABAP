*&---------------------------------------------------------------------*
*& Report ZMDMR0003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmdmr0003.

TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:bkpf,mara.
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf

TYPES:BEGIN OF ty_out,
        matnr_main        TYPE mara-matnr, "检索物料号
        matnr_bom         TYPE mara-matnr, "展BOM物料号
        maktx             TYPE makt-maktx, "物料描述
        detail(250),"基本信息
        lgort_detail(100),"库存信息
        po_detail(100),"采购信息
        sales_detail(100),"销售信息
      END OF ty_out.
TYPES:BEGIN OF ty_all,
        matnr_top         TYPE mara-matnr,  "物料号
        matnr             TYPE mara-matnr,  "物料号
        mtart             TYPE mara-mtart,  "物料类型
        maktx             TYPE makt-maktx,  "物料长描述
        werks             TYPE marc-werks,  "工厂
        mmsta             TYPE marc-mmsta,  "特定工厂的物料状态
        mstdv             TYPE mara-mstdv,  "有效启用期
        mstav             TYPE mara-mstav,  "指定分销链状态
        lgort             TYPE lips-lgort,  "库存地点
        lgort_num         TYPE mska-kains,  "库存总数量
        ebeln             TYPE ekpo-ebeln,  "采购订单单据编号
        aedat             TYPE ekko-aedat,  "采购最后下单
        lifnr             TYPE ekko-lifnr,  "采购订单供应商
        ekko_noadd_num    TYPE ekpo-menge,  "采购订单未付数量
        ekko_noend_num    TYPE ekbe-menge,  "采购订单未结算数量
        vbeln             TYPE vbak-vbeln,  "销售订单编号
        erdat             TYPE vbak-erdat,  "销售订单最后下单日期
        kunnr             TYPE vbak-kunnr,  "销售订单客户
        sale_noadd_num    TYPE vbep-bmeng, "销售订单未交付数量
        sale_noend_num    TYPE lips-lfimg, "销售订单未结算数量
        po_detail(100),"采购信息
        lgort_detail(100),"库存信息
        sales_detail(100),"销售信息
        unendnumber(12),"未结算数量
        vtweg             TYPE vbak-vtweg, "分销渠道
        bmeng             TYPE vbep-bmeng, "数量
      END OF ty_all.
DATA:lt_out  TYPE TABLE OF ty_out,
     ls_out  LIKE LINE OF lt_out,
     lt_all  TYPE TABLE OF ty_all,
     lt_all2 TYPE TABLE OF ty_all,
     ls_all  LIKE LINE OF lt_all,
     lt_temp TYPE TABLE OF ty_all,
     ls_temp LIKE LINE OF lt_temp.
DATA:lt_mara_top TYPE TABLE OF mara,
     ls_mara_top LIKE LINE OF lt_mara_top.
DATA: it_wultb   LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb2  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb3  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb4  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb5  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb6  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_wultb7  LIKE stpov OCCURS 0 WITH HEADER LINE,
      it_equict  LIKE cscequi OCCURS 0 WITH HEADER LINE,
      it_kncat   LIKE cscknd OCCURS 0 WITH HEADER LINE,
      it_matcat  LIKE cscmat OCCURS 0 WITH HEADER LINE,
      it_stdct   LIKE cscstd OCCURS 0 WITH HEADER LINE,
      it_tplca   LIKE csctpl OCCURS 0 WITH HEADER LINE,
      it_bom     LIKE bom_header_api01 OCCURS 0 WITH HEADER LINE,
      it_bom_rev LIKE bom_header_api01 OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_matnr FOR mara-matnr."条件输入框，一个for对应一个
SELECTION-SCREEN END OF BLOCK blk.

START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
*定义子程序
FORM getdata."查询语句
  "查询顶层物料
  SELECT
    matnr,
    mtart,
    mstdv,
    mstav
  FROM mara
  INTO TABLE @DATA(lt_top)
  UP TO 200 ROWS
  WHERE mara~matnr IN @s_matnr.
  "遍历检索物料的工厂
  SELECT
    matnr,
    werks
  INTO TABLE @DATA(lt_top_werks)
  FROM marc
  FOR ALL ENTRIES IN @lt_top
  WHERE marc~matnr = @lt_top-matnr.
  "遍历顶层物料，开始反查BOM
  CLEAR:lt_all,ls_all,lt_out,ls_out.
  LOOP AT lt_top_werks INTO DATA(ls_top_werks).
    CLEAR:it_wultb,it_wultb[].
    CALL FUNCTION 'CS_WHERE_USED_MAT'
      EXPORTING
        datub                      = '20000101'
        datuv                      = sy-datum
        matnr                      = ls_top_werks-matnr
        werks                      = ls_top_werks-werks
      TABLES
        wultb                      = it_wultb
        equicat                    = it_equict
        kndcat                     = it_kncat
        matcat                     = it_matcat
        stdcat                     = it_stdct
        tplcat                     = it_tplca
      EXCEPTIONS
        call_invalid               = 1
        material_not_found         = 2
        no_where_used_rec_found    = 3
        no_where_used_rec_selected = 4
        no_where_used_rec_valid    = 5
        OTHERS                     = 6.
    IF it_wultb IS INITIAL.
      "第0层
      CLEAR ls_all.
      ls_all-matnr = ls_top_werks-matnr.
      ls_all-werks = ls_top_werks-werks.
      ls_all-matnr_top = ls_top_werks-matnr.
      APPEND ls_all TO lt_all.
    ELSE.
      "第1层
      LOOP AT it_wultb.

        CLEAR ls_all.
        ls_all-matnr_top = ls_top_werks-matnr.
        ls_all-matnr = it_wultb-matnr.
        ls_all-werks = it_wultb-werks.
        APPEND ls_all TO lt_all.

        CLEAR:it_wultb2,it_wultb2[].
        CALL FUNCTION 'CS_WHERE_USED_MAT'
          EXPORTING
            datub                      = sy-datum
            datuv                      = sy-datum
            matnr                      = it_wultb-matnr
            werks                      = it_wultb-werks
          TABLES
            wultb                      = it_wultb2
            equicat                    = it_equict
            kndcat                     = it_kncat
            matcat                     = it_matcat
            stdcat                     = it_stdct
            tplcat                     = it_tplca
          EXCEPTIONS
            call_invalid               = 1
            material_not_found         = 2
            no_where_used_rec_found    = 3
            no_where_used_rec_selected = 4
            no_where_used_rec_valid    = 5
            OTHERS                     = 6.
        IF it_wultb2 IS INITIAL.
*          CLEAR ls_all.
*          ls_all-matnr_top = ls_top_werks-matnr.
*          ls_all-matnr = it_wultb-matnr.
*          ls_all-werks = it_wultb-werks.
*          APPEND ls_all TO lt_all.
        ELSE.
          "第二层
          LOOP AT it_wultb2.

            CLEAR ls_all.
            ls_all-matnr_top = ls_top_werks-matnr.
            ls_all-matnr = it_wultb2-matnr.
            ls_all-werks = it_wultb2-werks.
            APPEND ls_all TO lt_all.

            CLEAR:it_wultb3,it_wultb3[].
            CALL FUNCTION 'CS_WHERE_USED_MAT'
              EXPORTING
                datub                      = sy-datum
                datuv                      = sy-datum
                matnr                      = it_wultb2-matnr
                werks                      = it_wultb2-werks
              TABLES
                wultb                      = it_wultb3
                equicat                    = it_equict
                kndcat                     = it_kncat
                matcat                     = it_matcat
                stdcat                     = it_stdct
                tplcat                     = it_tplca
              EXCEPTIONS
                call_invalid               = 1
                material_not_found         = 2
                no_where_used_rec_found    = 3
                no_where_used_rec_selected = 4
                no_where_used_rec_valid    = 5
                OTHERS                     = 6.
            IF it_wultb3 IS INITIAL.

*              CLEAR ls_all.
*              ls_all-matnr_top = ls_top_werks-matnr.
*              ls_all-matnr = it_wultb2-matnr.
*              ls_all-werks = it_wultb2-werks.
*              APPEND ls_all TO lt_all.
            ELSE.
              LOOP AT it_wultb3.

                CLEAR ls_all.
                ls_all-matnr_top  = ls_top_werks-matnr.
                ls_all-matnr = it_wultb3-matnr.
                ls_all-werks = it_wultb3-werks.

                APPEND ls_all TO lt_all.
                CLEAR:it_wultb4,it_wultb4[].
                CALL FUNCTION 'CS_WHERE_USED_MAT'
                  EXPORTING
                    datub                      = sy-datum
                    datuv                      = sy-datum
                    matnr                      = it_wultb3-matnr
                    werks                      = it_wultb3-werks
                  TABLES
                    wultb                      = it_wultb4
                    equicat                    = it_equict
                    kndcat                     = it_kncat
                    matcat                     = it_matcat
                    stdcat                     = it_stdct
                    tplcat                     = it_tplca
                  EXCEPTIONS
                    call_invalid               = 1
                    material_not_found         = 2
                    no_where_used_rec_found    = 3
                    no_where_used_rec_selected = 4
                    no_where_used_rec_valid    = 5
                    OTHERS                     = 6.
                IF it_wultb4 IS INITIAL.
*                  CLEAR ls_all.
*                  ls_all-matnr_top  = ls_top_werks-matnr.
*                  ls_all-matnr = it_wultb3-matnr.
*                  ls_all-werks = it_wultb3-werks.
*                  APPEND ls_all TO lt_all.
                ELSE.
                  LOOP AT it_wultb4.

                    CLEAR ls_all.
                    ls_all-matnr_top = ls_top_werks-matnr.
                    ls_all-matnr = it_wultb4-matnr.
                    ls_all-werks = it_wultb4-werks.
                    APPEND ls_all TO lt_all.


                    CLEAR:it_wultb5,it_wultb5[].
                    CALL FUNCTION 'CS_WHERE_USED_MAT'
                      EXPORTING
                        datub                      = sy-datum
                        datuv                      = sy-datum
                        matnr                      = it_wultb4-matnr
                        werks                      = it_wultb4-werks
                      TABLES
                        wultb                      = it_wultb5
                        equicat                    = it_equict
                        kndcat                     = it_kncat
                        matcat                     = it_matcat
                        stdcat                     = it_stdct
                        tplcat                     = it_tplca
                      EXCEPTIONS
                        call_invalid               = 1
                        material_not_found         = 2
                        no_where_used_rec_found    = 3
                        no_where_used_rec_selected = 4
                        no_where_used_rec_valid    = 5
                        OTHERS                     = 6.
                    IF it_wultb5 IS INITIAL.
*                      CLEAR ls_all.
*                      ls_all-matnr_top = ls_top_werks-matnr.
*                      ls_all-matnr = it_wultb4-matnr.
*                      ls_all-werks = it_wultb4-werks.
*                      APPEND ls_all TO lt_all.
                    ELSE.
                      LOOP AT it_wultb5.

                        CLEAR ls_all.
                        ls_all-matnr_top = ls_top_werks-matnr.
                        ls_all-matnr = it_wultb5-matnr.
                        ls_all-werks = it_wultb5-werks.
                        APPEND ls_all TO lt_all.

                        CLEAR:it_wultb6,it_wultb6[].
                        CALL FUNCTION 'CS_WHERE_USED_MAT'
                          EXPORTING
                            datub                      = sy-datum
                            datuv                      = sy-datum
                            matnr                      = it_wultb5-matnr
                            werks                      = it_wultb5-werks
                          TABLES
                            wultb                      = it_wultb6
                            equicat                    = it_equict
                            kndcat                     = it_kncat
                            matcat                     = it_matcat
                            stdcat                     = it_stdct
                            tplcat                     = it_tplca
                          EXCEPTIONS
                            call_invalid               = 1
                            material_not_found         = 2
                            no_where_used_rec_found    = 3
                            no_where_used_rec_selected = 4
                            no_where_used_rec_valid    = 5
                            OTHERS                     = 6.
                        IF it_wultb6 IS INITIAL.
*                          CLEAR ls_all.
*                          ls_all-matnr_top = ls_top_werks-matnr.
*                          ls_all-matnr = it_wultb5-matnr.
*                          ls_all-werks = it_wultb5-werks.
*                          APPEND ls_all TO lt_all.
                        ELSE.
                          LOOP AT it_wultb6.

                            CLEAR ls_all.
                            ls_all-matnr_top = ls_top_werks-matnr.
                            ls_all-matnr = it_wultb6-matnr.
                            ls_all-werks = it_wultb6-werks.
                            APPEND ls_all TO lt_all.

                            CLEAR:it_wultb7,it_wultb7[].
                            CALL FUNCTION 'CS_WHERE_USED_MAT'
                              EXPORTING
                                datub                      = sy-datum
                                datuv                      = sy-datum
                                matnr                      = it_wultb6-matnr
                                werks                      = it_wultb6-werks
                              TABLES
                                wultb                      = it_wultb7
                                equicat                    = it_equict
                                kndcat                     = it_kncat
                                matcat                     = it_matcat
                                stdcat                     = it_stdct
                                tplcat                     = it_tplca
                              EXCEPTIONS
                                call_invalid               = 1
                                material_not_found         = 2
                                no_where_used_rec_found    = 3
                                no_where_used_rec_selected = 4
                                no_where_used_rec_valid    = 5
                                OTHERS                     = 6.

                          ENDLOOP.
                          IF it_wultb7 IS INITIAL.

*                            CLEAR ls_all.
*                            ls_all-matnr_top = ls_top_werks-matnr.
*                            ls_all-matnr = it_wultb6-matnr.
*                            ls_all-werks = it_wultb6-werks.
*                            APPEND ls_all TO lt_all.

                          ELSE.

*                            MESSAGE 'BOM层数过多，请联系开发人员' TYPE 'E'.

                          ENDIF.

                        ENDIF.
                      ENDLOOP.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.


  "上面反查BON之后，下面查询其他表所需要的数据
  "获取物料描述
  SELECT
    *
  INTO TABLE @DATA(lt_makt)
  FROM makt
  FOR ALL ENTRIES IN @lt_all
  WHERE makt~spras = '1'
    AND makt~matnr = @lt_all-matnr.
  "获取特定工厂的物料状态
  SELECT
    matnr,
    werks,
    mmsta
  INTO TABLE @DATA(lt_marc)
  FROM marc
  FOR ALL ENTRIES IN @lt_all
  WHERE marc~matnr = @lt_all-matnr
    AND marc~werks = @lt_all-werks.
  "获取有效启用期
  SELECT
    matnr,
    mstdv,
    mstav
  INTO TABLE @DATA(lt_mara)
  FROM mara
  FOR ALL ENTRIES IN @lt_all
  WHERE mara~matnr = @lt_all-matnr.
  "获取库存地点、数量
  SELECT
    mard~matnr,
    mard~lgort,
    SUM( labst ) AS labst,
    SUM( insme ) AS insme,
    SUM( speme ) AS speme
  FROM mard
  INTO TABLE @DATA(lt_mard)
  GROUP BY mard~matnr,mard~lgort.
  SELECT
    matnr,
    lgort,
    SUM( slabs ) AS slabs,
    SUM( sinsm ) AS sinsm,
    SUM( sspem ) AS sspem
  FROM mkol
  INTO TABLE @DATA(lt_mkol)
  GROUP BY matnr,lgort.
  SELECT
    matnr,
    lgort,
    SUM( kalab ) AS labab,
    SUM( kains ) AS kains,
    SUM( kaspe ) AS kaspe
  FROM mska
  INTO TABLE @DATA(lt_mska)
  GROUP BY matnr,lgort.
  "查询采购相关的字段
  SELECT
    ekpo~ebeln,
    ekpo~ebelp,
    ekpo~aedat,
    ekko~lifnr,
    ekpo~matnr,
    ekpo~menge
  INTO TABLE @DATA(lt_ekpo)
  FROM ekpo
  INNER JOIN ekko
  ON ekko~ebeln = ekpo~ebeln
  FOR ALL ENTRIES IN @lt_all
  WHERE ekpo~matnr = @lt_all-matnr.
  SORT lt_ekpo BY ebeln ebelp ASCENDING aedat DESCENDING.
  "已交付
  SELECT
    ebeln,
    ebelp,
    SUM( menge ) AS menge,
    shkzg,
    bewtp
  FROM ekbe
  INTO TABLE @DATA(lt_ekbe)
  WHERE ekbe~shkzg IN ( 'S' , 'H' )
    AND ekbe~bewtp IN ( 'E','Q' )
  GROUP BY ebeln,ebelp,shkzg,bewtp.

  "查询销售相关的字段
  SELECT
    vbap~matnr,
    vbap~vbeln,
    vbap~posnr,
    vbak~erdat,
    vbak~vtweg,
    kunnr,
    lfgsa
  FROM vbap
  INNER JOIN vbak
  ON vbap~vbeln = vbak~vbeln
  INTO TABLE @DATA(lt_vbap)
  FOR ALL ENTRIES IN @lt_all
  WHERE  vbap~matnr = @lt_all-matnr.
  SORT lt_vbap BY matnr vbeln posnr ASCENDING erdat DESCENDING.
  "对LT_VBAP表的数据进行处理，仅保留物料号、销售订单号、销售订单行的信息
  DATA lt_vbap2 LIKE lt_vbap.
  APPEND LINES OF lt_vbap TO lt_vbap2.
  SORT lt_vbap2 BY vbeln posnr matnr.
  DELETE ADJACENT DUPLICATES FROM lt_vbap2 COMPARING vbeln posnr matnr.
  "获取销售订单的计划行数据
  SELECT
    vbeln,
    posnr,
    SUM( bmeng ) AS bmeng,
    SUM( dlvqty_bu ) AS dlvqty_bu
  FROM vbep
  INTO TABLE @DATA(lt_vbep)
  GROUP BY vbeln,posnr.
  "销售订单未结算数量所需要的表
  SELECT
    lfimg,
    fkstk,
    lips~vbeln,
    posnr,
    matnr,
    vgbel,
    vgpos
  INTO TABLE @DATA(lt_lips)
  FROM lips
  INNER JOIN likp
  ON likp~vbeln = lips~vbeln
  FOR ALL ENTRIES IN @lt_all
  WHERE lips~matnr = @lt_all-matnr.
  "VBAP
  SELECT
    vbeln,
    posnr,
    kwmeng
  INTO TABLE @DATA(lt_vbap_lips)
  FROM vbap
  FOR ALL ENTRIES IN @lt_lips
  WHERE vbeln = @lt_lips-vgbel
    AND posnr = @lt_lips-vgpos.
  "VBRP
  SELECT
    fkimg,
    vgbel,
    vgpos
  INTO TABLE @DATA(lt_vbrp)
  FROM vbrp
  INNER JOIN vbrk
  ON vbrk~vbeln = vbrp~vbeln
  FOR ALL ENTRIES IN @lt_lips
  WHERE  vbrp~vgbel = @lt_lips-vbeln
    AND vbrp~vgpos = @lt_lips-posnr
    AND vbrk~fksto <> 'X'.

  SORT lt_all BY matnr_top matnr werks.
  DELETE ADJACENT DUPLICATES FROM lt_all COMPARING matnr_top matnr werks.

  "上面通过反查BOM之后，下面对数据进行处理并存到lt_all2内表中
  CLEAR ls_all.
  LOOP AT lt_all INTO ls_all.
    "物料描述
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_all-matnr.
    IF sy-subrc = 0.
      ls_all-maktx = ls_makt-maktx.
    ENDIF.
    "特定工厂的物料状态
    READ TABLE lt_marc INTO DATA(ls_marc) WITH KEY matnr = ls_all-matnr
                                                   werks = ls_all-werks.
    IF sy-subrc = 0.
      ls_all-mmsta = ls_marc-mmsta.
    ENDIF.
    "有效启用期、指定分销链状态
    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr =  ls_all-matnr.
    IF sy-subrc = 0.
      ls_all-mstdv = ls_mara-mstdv.
      ls_all-mstav = ls_mara-mstav.
    ENDIF.
    "库存信息列
    CLEAR ls_all-lgort_detail.
    LOOP AT lt_mard INTO DATA(ls_mard) WHERE matnr = ls_all-matnr.
      DATA(lv_number1) = ls_mard-labst + ls_mard-insme + ls_mard-speme.
      LOOP AT lt_mkol INTO DATA(ls_mkol) WHERE matnr = ls_all-matnr
                                           AND lgort = ls_mard-lgort.
        lv_number1 = ls_mkol-slabs + ls_mkol-sinsm + ls_mkol-sspem.
      ENDLOOP.
      LOOP AT lt_mska INTO DATA(ls_mska) WHERE matnr = ls_all-matnr
                                           AND lgort = ls_mard-lgort.
        lv_number1 = ls_mska-kains + ls_mska-kaspe + ls_mska-labab.
      ENDLOOP.
      CLEAR ls_mard.
      IF ls_mard-lgort IS NOT INITIAL OR lv_number1 IS NOT INITIAL.
        ls_all-lgort_detail = ls_all-lgort_detail && '库存地点：' && ls_mard-lgort && '数量：' && lv_number1 && ';'.
      ENDIF.
    ENDLOOP.
    "采购信息列
    CLEAR ls_all-po_detail.
    READ TABLE lt_ekpo INTO DATA(ls_ekpo) WITH KEY matnr = ls_all-matnr.
    IF sy-subrc = 0.
      "未交付数量
      READ TABLE lt_ekbe INTO DATA(ls_ekbe) WITH KEY ebeln = ls_ekpo-ebeln
                                                     ebelp = ls_ekpo-ebelp
                                                     shkzg = 'S'
                                                     bewtp = 'E'.
      IF sy-subrc = 0.
        DATA(lv_number3) = ls_ekbe-menge.
      ENDIF.
      READ TABLE lt_ekbe INTO ls_ekbe WITH KEY ebeln = ls_ekpo-ebeln
                                               ebelp = ls_ekpo-ebelp
                                               shkzg = 'H'
                                               bewtp = 'E'.
      IF sy-subrc = 0.
        DATA(lv_number4) = ls_ekbe-menge.
      ENDIF.
      DATA(lv_number5) = ls_ekpo-menge - lv_number3 + lv_number4.
      "未结算数量
      "已交付数lv_number3 - lv_number4 .
      "已开票数LV_NUMBER6 - LV_NUMBER7 .
      READ TABLE lt_ekbe INTO ls_ekbe WITH KEY ebeln = ls_ekpo-ebeln
                                               ebelp = ls_ekpo-ebelp
                                               shkzg = 'S'
                                               bewtp = 'Q'.
      IF sy-subrc = 0.
        DATA(lv_number6) = ls_ekbe-menge.
      ENDIF.
      READ TABLE lt_ekbe INTO ls_ekbe WITH KEY ebeln = ls_ekpo-ebeln
                                               ebelp = ls_ekpo-ebelp
                                               shkzg = 'H'
                                               bewtp = 'Q'.
      IF sy-subrc = 0.
        DATA(lv_number7) = ls_ekbe-menge.
      ENDIF.
      DATA(lv_number8) = lv_number3 - lv_number4 - ( lv_number6 - lv_number7 ).


      ls_all-po_detail = '单据编号：' && ls_ekpo-ebeln && '最晚下单日期：' && ls_ekpo-aedat
       && '供应商：' && ls_ekpo-lifnr && '未交付数量：' && lv_number5 && '未结算数量' && lv_number8.
    ENDIF.

    "销售信息列
    CLEAR: ls_all-sales_detail,ls_all-bmeng.
    LOOP AT lt_vbap2 INTO DATA(ls_vbap2) WHERE matnr = ls_all-matnr.
      ls_all-kunnr = ls_vbap2-kunnr.
      ls_all-erdat = ls_vbap2-erdat.
      ls_all-vbeln = ls_vbap2-vbeln.
      "销售订单未交付数量LS_ALL-BMENG
      "计划行已确认数：
      READ TABLE lt_vbap INTO DATA(ls_vbap) WITH KEY matnr = ls_all-matnr
                                                     vbeln = ls_vbap2-vbeln
                                                     posnr = ls_vbap2-posnr
                                                     lfgsa = 'A'.
      IF sy-subrc = 0.
        "分销渠道
        ls_all-vtweg = ls_vbap-vtweg.
        READ TABLE lt_vbep INTO DATA(ls_vbep) WITH KEY vbeln = ls_vbap-vbeln
                                                       posnr = ls_vbap-posnr.
        ls_all-bmeng = ls_all-bmeng + ls_vbep-bmeng.
      ENDIF.

      READ TABLE lt_vbap INTO ls_vbap WITH KEY matnr = ls_all-matnr
                                               vbeln = ls_vbap2-vbeln
                                               posnr = ls_vbap2-posnr
                                               lfgsa = 'B'.
      IF sy-subrc = 0.
        "分销渠道
        ls_all-vtweg = ls_vbap-vtweg.
        READ TABLE lt_vbep INTO ls_vbep WITH KEY vbeln = ls_vbap-vbeln
                                                 posnr = ls_vbap-posnr.
        ls_all-bmeng = ls_all-bmeng + ls_vbep-bmeng.
      ENDIF.
      "已交付数
      READ TABLE lt_vbap INTO ls_vbap WITH KEY matnr = ls_all-matnr
                                               vbeln = ls_vbap2-vbeln
                                               posnr = ls_vbap2-posnr
                                               lfgsa = 'A'.
      IF sy-subrc = 0.
        "分销渠道
        ls_all-vtweg = ls_vbap-vtweg.
        READ TABLE lt_vbep INTO ls_vbep WITH KEY vbeln = ls_vbap-vbeln
                                                 posnr = ls_vbap-posnr.
        ls_all-bmeng = ls_all-bmeng - ls_vbep-dlvqty_bu.
      ENDIF.
      READ TABLE lt_vbap INTO ls_vbap WITH KEY matnr = ls_all-matnr
                                               vbeln = ls_vbap2-vbeln
                                               posnr = ls_vbap2-posnr
                                               lfgsa = 'B'.
      IF sy-subrc = 0.
        "分销渠道
        ls_all-vtweg = ls_vbap-vtweg.
        ls_all-bmeng = ls_all-bmeng - ls_vbep-dlvqty_bu.
      ENDIF.
      CLEAR:ls_vbap2.
    ENDLOOP.
    "销售订单未结算数量
    "已发货数量
    "已开票数量
    CLEAR ls_all-unendnumber.
    LOOP AT lt_lips INTO DATA(ls_lips) WHERE matnr = ls_all-matnr.
      IF ls_lips-fkstk = 'A'.
        READ TABLE lt_vbap_lips INTO DATA(ls_vbap_lips) WITH KEY vbeln = ls_lips-vgbel
                                                                 posnr = ls_lips-vgpos.
        ls_all-unendnumber = ls_all-unendnumber + ls_vbap_lips-kwmeng.
        CLEAR ls_vbap_lips.
      ELSEIF ls_lips-fkstk = 'B' OR ls_lips-fkstk = 'C'.
        READ TABLE lt_vbrp INTO DATA(ls_vbrp) WITH KEY vgbel = ls_lips-vbeln
                                                       vgpos = ls_lips-posnr.
        ls_all-unendnumber = ls_all-unendnumber - ls_vbrp-fkimg.
        CLEAR ls_vbrp.
      ENDIF.
      CLEAR ls_lips.
    ENDLOOP.
    "销售信息
    ls_all-sales_detail = '单据编号：' && ls_all-vbeln && '最晚下单日期：' && ls_all-erdat && '客户：' && ls_all-kunnr &&
    '未交付数量：' && ls_all-bmeng && '未结算数量：' && ls_all-vtweg.

    APPEND ls_all TO lt_all2.
    CLEAR ls_all.
  ENDLOOP.
ENDFORM.







FORM catalog."表头
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-ref_tabname = 'BKPF'."调用透明表的数据结构
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.


  fieldcatset 'MATNR_TOP' '检索物料' sy-tabix.
  fieldcatset 'MATNR' '物料编码' sy-tabix.
  fieldcatset 'MAKTX' '物料描述' sy-tabix.
  fieldcatset 'WERKS' '工厂' sy-tabix.
  fieldcatset 'MMSTA' '特定工厂的物料状态' sy-tabix.
  fieldcatset 'MSTDV' '有效启用期' sy-tabix.
  fieldcatset 'MSTAV' '指定分销链状态' sy-tabix.
  fieldcatset 'LGORT_DETAIL' '库存信息' sy-tabix.
  fieldcatset 'PO_DETAIL' '采购信息' sy-tabix.
  fieldcatset 'SALES_DETAIL' '销售信息' sy-tabix.

*  READ TABLE fieldcat INDEX 3."读取报表第三列
**  fieldcat-hotspot = 'X'."鼠标热点事件
**  fieldcat-key = 'X'."定义为主键（颜色改变）
*  MODIFY fieldcat INDEX 3."修改样式

  layout-zebra = 'X'."斑马线的样式

ENDFORM.
*响应鼠标点击时间的子程序
*FORM alv_user_command USING r_ucomm LIKE sy-ucomm
*      rs_selfield TYPE slis_selfield.
*  CASE r_ucomm.
*    WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
*      READ TABLE lbkpf INDEX rs_selfield-tabindex.
*      SET PARAMETER ID 'BLN' FIELD lbkpf-belnr.
*      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*  ENDCASE.
*
*ENDFORM.

FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZHKALV'.
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
      i_grid_title             = '物料冻结前信息查询报表' "标题名
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
      t_outtab                 = lt_all2
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.