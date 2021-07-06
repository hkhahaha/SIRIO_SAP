*****************************************************************
*  System      :  仙乐二期ERP项目
*  Module      :  FI
*  ProgramID   :  ZFI049
*  Program     :  ZFIR0049
*  Author      :  gaozhongzhao
*  Date        :  18.11.2020 14:00:44
*  Description :  销售运费分摊表
*****************************************************************
*  Modified Recorder :
*  Date       C#NO         Author        Content
*  --------  ----------    ---------     ---------------
*  20210408  S4DK904689    04815         预提过账和差异过账项目文本调整，增加承运商
*****************************************************************
*  System      :  运维
*  Module      :  FI
*  ProgramID   :  ZFI049
*  Program     :  ZFIR0049
*  Author      :  Seashell Huang
*  Date        :  2021.6.25
*  Description :  销售运费分摊表
*****************************************************************
*  Modified Recorder :
*  Date       C#NO         Author        Content
*  --------  ----------    ---------     ---------------
*  20210625  S4DK904898       05025        FI: 销售运费分摊表-20210625
*****************************************************************
REPORT zfir0049 NO STANDARD PAGE HEADING LINE-SIZE 150 .
*&---------------------------------------------------------------------*
* ALV TYPES
*&---------------------------------------------------------------------*
TYPE-POOLS: slis .

DATA: gs_layout     TYPE lvc_s_layo.            "ALV布局工作区
DATA: gs_layout1     TYPE lvc_s_layo.            "ALV布局工作区

DATA : gv_lights_name TYPE lvc_cifnm VALUE 'LIGHT' ."红绿灯

*DATA: GCL_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: gs_fieldcat TYPE lvc_s_fcat,              "字段工作区
      gt_fieldcat TYPE lvc_t_fcat.
DATA: gs_fieldcat1 TYPE lvc_s_fcat,              "字段工作区
      gt_fieldcat1 TYPE lvc_t_fcat.
DEFINE mac01 .
  CLEAR gs_fieldcat .
  gs_fieldcat-fieldname     = &1 .
  gs_fieldcat-scrtext_l     = &2 .
  gs_fieldcat-edit          = &3 .
  gs_fieldcat-key           = &4 .
  gs_fieldcat-ref_field     = &5 .
  gs_fieldcat-ref_table     = &6 .
  gs_fieldcat-no_zero       = &7 .

  APPEND  gs_fieldcat TO  gt_fieldcat .
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
* TYPES
*&---------------------------------------------------------------------*
TYPES : BEGIN OF ty_likp ,
          vbeln TYPE zfit0025-vbeln,  "抬头交货

          bukrs TYPE tvko-bukrs,      "公司代码
          vkorg TYPE zfit0025-vkorg,  "抬头销售组织
          wadat TYPE zfit0025-wadat,  "抬头
          kunnr TYPE zfit0025-kunnr,  "抬头客户
          name1 TYPE zfit0025-name1,  "抬头客户名称
          vtweg TYPE zfit0025-vtweg,  "抬头分销渠道
          ort01 TYPE zfit0025-ort01,  "抬头目的地
          lfimg TYPE zfit0025-lfimg,  "抬头发货数量（件）
          volum TYPE zfit0025-volum,  "抬头体积
          voleh TYPE zfit0025-voleh,  "抬头体积单位
          zcys1 TYPE zfit0025-zcys1,  "抬头承运商
          kbetr TYPE zfit0025-kbetr,  "抬头金额
          waers TYPE zfit0025-waers,  "抬头货币码


        END OF ty_likp .

TYPES : BEGIN OF ty_alv .
          INCLUDE TYPE ty_likp .
          TYPES : selec     TYPE c,
          light(1)  TYPE c,
          mess(100),
          matnr     TYPE lips-matnr,     "行项目的料号
          posnr     TYPE lips-posnr, "交货行项目
          lfimgm    TYPE lips-lfimg, "行项目发货数量（件）
          vgbel     TYPE lips-vgbel, "参考单据的单据编号
          prctr     TYPE lips-prctr, "参考码
          maktx     TYPE makt-maktx,
          volumm    TYPE mara-volum,         "行项目单位体积
          volehm    TYPE mara-voleh,         "行项目体积单位
          zytje     TYPE zfit0025-kbetr,     "预提金额
          zcyje     TYPE zfit0025-kbetr,     "差异金额
          zcyhb     TYPE zfit0025-waers,     "差异币别
          gjahr     TYPE bkpf-gjahr,     "会计年度
          monat     TYPE bkpf-monat,     "会计期间
          zytpz     TYPE zfit0026-zytpz,  "预提凭证
          zsjpz     TYPE zfit0026-zsjpz,  "实际凭证
          zzt01     TYPE zfit0026-zzt01,  "实际状态
          kostl     TYPE zfit0027-kostl,  "成本中心
          zztms     TYPE char20,          "凭证状态描述
          zytrq     TYPE zfit0026-zytrq, "预提创建日期
          zytsj     TYPE zfit0026-zytsj, "预提创建时间
          zytna     TYPE zfit0026-zytna, "预提创建人的姓名
        END OF ty_alv .

TYPES : BEGIN OF ty_kna1 ,
          kunnr TYPE kna1-kunnr, "客户编号
          name1 TYPE kna1-name1, "客户名称
          ort01 TYPE kna1-ort01, "目的地
        END OF ty_kna1 .

TYPES : BEGIN OF ty_mara ,
          matnr TYPE mara-matnr, "物料
          volum TYPE mara-volum, "业务量
          voleh TYPE mara-voleh, "体积/重量单位
        END OF ty_mara.

TYPES : BEGIN OF ty_lips ,

          vbeln  TYPE lips-vbeln, "交货
          matnr  TYPE lips-matnr, "行项目的料号
          posnr  TYPE lips-posnr, "交货行项目
          kcmeng TYPE lips-kcmeng, "发货数量（件）
          vgbel  TYPE lips-vgbel, "参考单据的单据编号
          vtweg  TYPE lips-vtweg,
          umvkn  TYPE lips-umvkn,
          umvkz  TYPE lips-umvkz,
          lfimg  TYPE lips-lfimg,
          prctr  TYPE lips-prctr, "参考码
        END OF ty_lips .

*&---------------------------------------------------------------------*
* GLOBAL DATA
*&---------------------------------------------------------------------*
DATA : gs_likp TYPE ty_likp .
DATA : gs_alv  TYPE ty_alv .
DATA : gs_kna1 TYPE ty_kna1 .
DATA : gs_mara TYPE ty_mara.
DATA : gs_makt TYPE makt.
DATA : gs_lips TYPE ty_lips.
DATA : gs_zfit0026 TYPE zfit0026.
DATA : gs_zfit0027 TYPE zfit0027.


*&---------------------------------------------------------------------*
* GLOBAL INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA : gt_likp TYPE TABLE OF ty_likp .
DATA : gt_alv  TYPE TABLE OF ty_alv .
DATA : gt_alv2  TYPE TABLE OF ty_alv .
DATA : gt_kna1 TYPE TABLE OF ty_kna1 .
DATA : gt_mara TYPE TABLE OF ty_mara .
DATA : gt_makt TYPE TABLE OF makt .
DATA : gt_lips TYPE TABLE OF ty_lips .
DATA : gt_zfit0026 TYPE TABLE OF zfit0026 .
DATA : gt_zfit0027 TYPE TABLE OF zfit0027 .
DATA: lt_zfit0030 TYPE TABLE OF zfit0030,
      ls_zfit0030 LIKE LINE OF lt_zfit0030.



*&---------------------------------------------------------------------*
* GLOBAL VARIABLE
*&---------------------------------------------------------------------*
FIELD-SYMBOLS: <fs_alv> LIKE gs_alv.
DATA: gr_erdat LIKE RANGE OF  lips-erdat.
DATA: gv_volum TYPE lips-volum.  "总体积
DATA: gv_kbetr TYPE zfit0025-kbetr.  "总金额

*&---------------------------------------------------------------------*
* SELECTION-SCREEN
*&---------------------------------------------------------------------*
TABLES : tvko,bkpf,zfit0025,likp.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
PARAMETERS:
  p_bukrs LIKE tvko-bukrs MODIF ID mi2.
SELECT-OPTIONS:
  s_gjahr FOR bkpf-gjahr NO INTERVALS NO-EXTENSION MODIF ID mi2,
  s_monat FOR bkpf-monat  NO-EXTENSION MODIF ID mi2.

PARAMETERS:
  r_x1 RADIOBUTTON GROUP rg1 USER-COMMAND uc1 DEFAULT 'X',
  r_x2 RADIOBUTTON GROUP rg1.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
SELECT-OPTIONS: s_zcys1 FOR zfit0025-zcys1 NO INTERVALS NO-EXTENSION MODIF ID mi1,
                s_wadat FOR likp-wadat_ist NO-EXTENSION MODIF ID mi1."实际发货日期
PARAMETERS:

  p_kbetr LIKE zfit0025-kbetr MODIF ID mi1,
  p_waers LIKE zfit0025-waers MODIF ID mi1.

SELECTION-SCREEN END OF BLOCK b03.
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_screen_attr.  "先择屏幕做控制

AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON  VALUE-REQUEST FOR s_zcys1-low.
  PERFORM frm_request_f4_insmk USING 'S_ZCYS1-LOW'.

START-OF-SELECTION.
  IF p_bukrs IS INITIAL OR
   s_gjahr  IS INITIAL OR
   s_monat  IS INITIAL .
    MESSAGE '必填字段不能为空' TYPE 'S' DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  CASE 'X'.
    WHEN r_x1.

    WHEN r_x2.
      IF
         s_zcys1 IS INITIAL OR
         p_kbetr IS INITIAL OR
         p_waers IS INITIAL .
        MESSAGE '必填字段不能为空' TYPE 'S' DISPLAY LIKE 'E'. EXIT.
      ENDIF.
  ENDCASE.


  PERFORM sub_get_data.             "获取数据
  PERFORM sub_deal_data.            "处理数据

END-OF-SELECTION.

  PERFORM sub_show_alv.


*&---------------------------------------------------------------------*
*& Form SUB_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_data .

  PERFORM sub_get_likp ."
  PERFORM sub_get_lips .
  PERFORM sub_get_kna1 .
  PERFORM sub_get_mara .
  PERFORM sub_get_makt .
  PERFORM sub_get_zfit0026 .
  PERFORM sub_get_zfit0025 .
  PERFORM sub_get_zfit0027 .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_LIKP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&-------------------------------------------------------------------~~*
FORM  sub_get_likp .
  CLEAR gt_likp .
  READ TABLE s_gjahr INTO DATA(ls_gjahr) INDEX 1.

  READ TABLE s_monat INTO DATA(ls_monst) INDEX 1.
  DATA(month_l) = SWITCH #( ls_monst-low
                    WHEN '00' THEN '01'
                    ELSE ls_monst-low
                          ).
  DATA(month_h) = SWITCH #( ls_monst-high
                    WHEN '00' THEN ls_monst-low
                    ELSE ls_monst-high
                     ).
  DATA(date_l) = CONV sy-datum( ls_gjahr-low && month_l && '01' ).
  DATA(date_h) = CONV sy-datum( ls_gjahr-low && month_h && '01' ).
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = date_h
    IMPORTING
      last_day_of_month = date_h
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    PERFORM standard_message.
  ENDIF.

  APPEND VALUE #( sign   = 'I'
                  option = 'BT'
                  low    = date_l
                  high   = date_h
                ) TO gr_erdat.

  CASE 'X'.
    WHEN r_x1.

      SELECT
        tvko~bukrs
        zfit0025~vbeln
        zfit0025~vkorg
        zfit0025~wadat
        zfit0025~kunnr
        zfit0025~name1
        zfit0025~vtweg
        zfit0025~ort01
        zfit0025~lfimg
        zfit0025~volum
        zfit0025~voleh
        zfit0025~zcys1
        zfit0025~kbetr
        zfit0025~waers

        INTO CORRESPONDING FIELDS OF TABLE gt_likp
        FROM tvko
        INNER JOIN zfit0025 ON tvko~vkorg =  zfit0025~vkorg
        WHERE tvko~bukrs = p_bukrs
        AND   wadat IN gr_erdat.


    WHEN r_x2.

      SELECT
        tvko~bukrs
        zfit0025~vbeln
        zfit0025~vkorg
        zfit0025~wadat
        zfit0025~kunnr
        zfit0025~name1
        zfit0025~vtweg
        zfit0025~ort01
        zfit0025~lfimg
        zfit0025~volum
        zfit0025~voleh
        zfit0025~zcys1
        zfit0025~kbetr
        zfit0025~waers

        INTO CORRESPONDING FIELDS OF TABLE gt_likp
        FROM tvko
        INNER JOIN zfit0025 ON tvko~vkorg =  zfit0025~vkorg
        WHERE tvko~bukrs = p_bukrs
        AND   zfit0025~zcys1 IN s_zcys1
        AND   zfit0025~wadat IN s_wadat.

  ENDCASE.

  IF sy-subrc <> 0 .
    MESSAGE '没有符合条件的数据' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
  ENDIF .

  SORT gt_likp BY vbeln ASCENDING .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_LIPS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_lips .
  DATA : lt_likp TYPE TABLE OF ty_likp .
  lt_likp[] = gt_likp[] .
  SORT lt_likp BY vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_likp
                COMPARING vbeln .

  CLEAR gt_lips[] .
  SELECT
    vbeln
    posnr
    kcmeng
    matnr
    vgbel
    vtweg
    umvkn
    umvkz
    lfimg
    prctr
    INTO CORRESPONDING FIELDS OF TABLE gt_lips
    FROM lips
    FOR ALL ENTRIES IN lt_likp
    WHERE vbeln = lt_likp-vbeln
    AND   charg <> ''.

  SORT gt_lips BY vbeln matnr ASCENDING .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_KNA1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_kna1 .
  DATA : lt_likp TYPE TABLE OF ty_likp .
  lt_likp[] = gt_likp[] .
  SORT lt_likp BY kunnr.
  DELETE ADJACENT DUPLICATES FROM lt_likp
                COMPARING kunnr .

  CLEAR gt_kna1[] .
  SELECT
    kunnr
    name1
    ort01
    INTO CORRESPONDING FIELDS OF TABLE gt_kna1
    FROM kna1
    FOR ALL ENTRIES IN lt_likp
    WHERE kunnr = lt_likp-kunnr .

  SORT gt_kna1 BY kunnr ASCENDING .
  DELETE ADJACENT DUPLICATES FROM gt_kna1
                 COMPARING kunnr .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_MARA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_mara .

  DATA : lt_lips TYPE TABLE OF ty_lips .
  IF gt_lips IS NOT INITIAL.
    lt_lips[] = gt_lips[] .
    SORT lt_lips BY matnr .
    DELETE ADJACENT DUPLICATES FROM lt_lips
                  COMPARING matnr.
    CLEAR gt_mara[] .
    SELECT
      matnr
      volum
      voleh
      INTO CORRESPONDING FIELDS OF TABLE gt_mara
      FROM mara
      FOR ALL ENTRIES IN lt_lips
      WHERE matnr = lt_lips-matnr .

    SORT gt_mara BY matnr ASCENDING .
    DELETE ADJACENT DUPLICATES FROM gt_mara
                   COMPARING matnr.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_MAKT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_makt .
  DATA : lt_lips TYPE TABLE OF ty_lips .
  IF gt_lips IS NOT INITIAL.
    lt_lips[] = gt_lips[] .
    SORT lt_lips BY matnr .
    DELETE ADJACENT DUPLICATES FROM lt_lips
                  COMPARING matnr.

    CLEAR gt_makt[] .
    SELECT
      *
      INTO CORRESPONDING FIELDS OF TABLE gt_makt
      FROM makt
      FOR ALL ENTRIES IN lt_lips
      WHERE matnr = lt_lips-matnr
      AND   spras = sy-langu .

    SORT gt_makt BY matnr ASCENDING .
    DELETE ADJACENT DUPLICATES FROM gt_makt
                   COMPARING matnr.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZFIT0026
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_zfit0026 .
  DATA : lt_likp TYPE TABLE OF ty_likp .
  lt_likp[] = gt_likp[] .
  SORT lt_likp BY vbeln.
  DELETE ADJACENT DUPLICATES FROM lt_likp
                COMPARING vbeln .

  CLEAR gt_zfit0026[] .
  SELECT
     *
    INTO CORRESPONDING FIELDS OF TABLE gt_zfit0026
    FROM zfit0026
    FOR ALL ENTRIES IN lt_likp
    WHERE vbeln = lt_likp-vbeln.

  SORT gt_zfit0026 BY vbeln matnr ASCENDING .
  DELETE ADJACENT DUPLICATES FROM gt_zfit0026
                 COMPARING vbeln matnr.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZFIT0025
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_zfit0025 .
  CLEAR gv_volum.
  SELECT SUM( volum ) SUM( kbetr )  FROM zfit0025 INTO (gv_volum,gv_kbetr) WHERE zcys1 IN s_zcys1  AND wadat IN s_wadat .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZFIT0027
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_zfit0027 .
  DATA : lt_likp TYPE TABLE OF ty_likp .
  lt_likp[] = gt_likp[] .
  SORT lt_likp BY vkorg vtweg.
  DELETE ADJACENT DUPLICATES FROM lt_likp
                COMPARING vkorg vtweg.

  CLEAR gt_zfit0027[] .
  SELECT
     *
    INTO CORRESPONDING FIELDS OF TABLE gt_zfit0027
    FROM zfit0027
    FOR ALL ENTRIES IN lt_likp
    WHERE vkorg = lt_likp-vkorg
    AND   vtweg = lt_likp-vtweg.

  SORT gt_zfit0027 BY vkorg vtweg ASCENDING .
  DELETE ADJACENT DUPLICATES FROM gt_zfit0027
                 COMPARING vkorg vtweg.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_DEAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_deal_data .
  DATA:lv_zcyje  TYPE zfit0025-kbetr, "记录差异金额
       lv_lfimgm TYPE lips-lfimg,     "记录发货数量（件
       lv_volumm TYPE lips-volum,     "记录体积/重量'
       lv_zytje  TYPE zfit0026-zytje. "记录预提金额

  DATA:ls_lips TYPE ty_lips.

  READ TABLE gr_erdat INTO DATA(rs_erdat) INDEX 1. "读取会计期间的最后一天
  CLEAR gt_alv .
  CLEAR gs_likp .
  CLEAR: lv_zcyje,lv_zytje.

  p_kbetr = p_kbetr - gv_kbetr.

  LOOP AT gt_likp INTO gs_likp .

    CLEAR gs_alv .
    MOVE-CORRESPONDING gs_likp TO gs_alv .
    gs_alv-bukrs = p_bukrs.

    gs_alv-gjahr = rs_erdat-high+0(4).
    gs_alv-monat = rs_erdat-high+4(2).

    CLEAR gs_kna1 .
    READ TABLE gt_kna1 INTO gs_kna1 WITH  KEY kunnr = gs_alv-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      gs_alv-name1 = gs_kna1-name1 .
      gs_alv-ort01 = gs_kna1-ort01 .
      DATA(lv_vbeln) = gs_likp-vbeln.
    ENDIF.

    CLEAR gs_zfit0027.
    READ TABLE gt_zfit0027 INTO gs_zfit0027 WITH KEY vkorg = gs_alv-vkorg  vtweg = gs_alv-vtweg BINARY SEARCH.
    IF sy-subrc = 0.
      gs_alv-kostl = gs_zfit0027-kostl.
    ENDIF.
    AT LAST.
      LOOP AT gt_lips INTO gs_lips WHERE vbeln = lv_vbeln.
        lv_lfimgm = lv_lfimgm + gs_lips-lfimg .

        CLEAR gs_mara.
        READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_lips-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          lv_volumm = lv_volumm + gs_mara-volum * gs_lips-lfimg."体积/重量' = "VOLUM * 发货数量
          gs_alv-volehm = gs_mara-voleh."单位
        ENDIF.
        "最后一条做减法
        CLEAR ls_lips.
        ls_lips = gs_lips.
        AT END OF matnr.

          gs_alv-matnr  = ls_lips-matnr.
          gs_alv-prctr  = ls_lips-prctr.
          gs_alv-lfimgm = lv_lfimgm.
          gs_alv-volumm = lv_volumm.
          CLEAR gs_makt .
          READ TABLE gt_makt INTO gs_makt WITH  KEY matnr = gs_alv-matnr BINARY SEARCH.
          IF sy-subrc = 0.
            gs_alv-maktx = gs_makt-maktx .
          ENDIF.

          CLEAR gs_zfit0026.
          READ TABLE gt_zfit0026 INTO gs_zfit0026 WITH KEY vbeln = gs_alv-vbeln  matnr = gs_alv-matnr.
          IF sy-subrc = 0.
            gs_alv-zytpz = gs_zfit0026-zytpz.
            gs_alv-zsjpz = gs_zfit0026-zsjpz.
            gs_alv-zzt01 = gs_zfit0026-zzt01.
            gs_alv-zytje = gs_zfit0026-zytje.
            gs_alv-zcyje = gs_zfit0026-zcyje.
            gs_alv-zcyhb = gs_zfit0026-zcyhb.

          ENDIF.
          gs_alv-zztms = SWITCH #( gs_alv-zzt01
               WHEN 0 THEN '未过账'
               WHEN 1 THEN '预提过账'
               WHEN 2 THEN '差异过账'
               WHEN ' ' THEN '未过账'  ).

          IF gs_zfit0026-zzt01 IS INITIAL.
            gs_alv-zcyhb = p_waers.
          ENDIF.
          gs_alv-zcyhb = p_waers.

          IF gs_zfit0026-zzt01 = 0.
            gs_alv-zytje = gs_alv-kbetr * ( gs_alv-volumm /  gs_alv-volum ).  "1)分摊金额=运费录入表金额*分摊表体积/录入表体积
          ENDIF.
          lv_zytje = lv_zytje + gs_alv-zytje. "记录预提金额。
          IF gs_zfit0026-zzt01 <> 2.
            IF r_x2 = 'X'.
              IF p_kbetr <> 0.
                gs_alv-zcyje = p_kbetr * ( gs_alv-volumm /  gv_volum ). "2) 实际差额=实际金额*分摊表体积/录入表体积-预提金额（小数点差异摊入最后一行）

                AT LAST.
*                  IF LV_ZCYJE <> 0.
                  gs_alv-zcyje = p_kbetr - lv_zcyje .  "屏幕数据金额 - 前面差异金额汇总  = 最后一条差异金额
*                  ENDIF.
                ENDAT.
                lv_zcyje = lv_zcyje + gs_alv-zcyje.
              ENDIF.
            ENDIF.
          ENDIF.
*          GS_ALV-LIGHT = 2.
          APPEND gs_alv TO gt_alv .
          CLEAR:lv_lfimgm,
                lv_volumm,
                gs_alv.
        ENDAT.
        CLEAR gs_likp.
      ENDLOOP.

      EXIT.
    ENDAT.


    LOOP AT gt_lips INTO gs_lips WHERE vbeln = gs_likp-vbeln.
      lv_lfimgm = lv_lfimgm + gs_lips-lfimg .

      CLEAR gs_mara.
      READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_lips-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        lv_volumm = lv_volumm + gs_mara-volum * gs_lips-lfimg."体积/重量' = "VOLUM * 发货数量
        gs_alv-volehm = gs_mara-voleh."单位
      ENDIF.
      CLEAR ls_lips.
      ls_lips = gs_lips.
      AT END OF matnr.
        gs_alv-matnr  = ls_lips-matnr.
        gs_alv-prctr  = ls_lips-prctr.
        gs_alv-lfimgm = lv_lfimgm.
        gs_alv-volumm = lv_volumm.
        CLEAR gs_makt .
        READ TABLE gt_makt INTO gs_makt WITH  KEY matnr = gs_alv-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          gs_alv-maktx = gs_makt-maktx .
        ENDIF.

        CLEAR gs_zfit0026.
        READ TABLE gt_zfit0026 INTO gs_zfit0026 WITH KEY vbeln = gs_alv-vbeln  matnr = gs_alv-matnr.
        IF sy-subrc = 0.
          gs_alv-zytpz = gs_zfit0026-zytpz.
          gs_alv-zsjpz = gs_zfit0026-zsjpz.
          gs_alv-zzt01 = gs_zfit0026-zzt01.
          gs_alv-zytje = gs_zfit0026-zytje.
          gs_alv-zcyje = gs_zfit0026-zcyje.
          gs_alv-zcyhb = gs_zfit0026-zcyhb.
        ENDIF.
        gs_alv-zztms = SWITCH #( gs_alv-zzt01
         WHEN 0 THEN '未过账'
         WHEN 1 THEN '预提过账'
         WHEN 2 THEN '差异过账'
         WHEN ' ' THEN '未过账'  ).
        IF gs_zfit0026-zcyhb IS INITIAL.
          gs_alv-zcyhb = p_waers.
        ENDIF.
        gs_alv-zcyhb = p_waers.

        IF gs_zfit0026-zzt01 = 0.
          gs_alv-zytje = gs_alv-kbetr * ( gs_alv-volumm /  gs_alv-volum ).  "1)分摊金额=运费录入表金额*分摊表体积/录入表体积
        ENDIF.
        lv_zytje = lv_zytje + gs_alv-zytje. "记录预提金额。
        IF gs_zfit0026-zzt01 <> 2.
          IF r_x2 = 'X'.
            IF p_kbetr <> 0.
              gs_alv-zcyje = p_kbetr * ( gs_alv-volumm /  gv_volum ). "2) 实际差额=实际金额*分摊表体积/录入表体积-预提金额（小数点差异摊入最后一行）

              lv_zcyje = lv_zcyje + gs_alv-zcyje.
            ENDIF.
          ENDIF.
        ENDIF.
*        GS_ALV-LIGHT = 2.
        APPEND gs_alv TO gt_alv .
        CLEAR:lv_lfimgm,
              lv_volumm,
              gs_alv.
      ENDAT.
      CLEAR gs_likp.
    ENDLOOP.

  ENDLOOP .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_SHOW_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_show_alv .

*-行属性
  CLEAR gs_layout.

  gs_layout-zebra        = 'X' .              " 显示界面成色带交替
  gs_layout-sel_mode     = 'B' .              " 选择模式()
  gs_layout-detailinit   = 'X' .              " 是否可以弹出细节屏幕
  gs_layout-cwidth_opt   = 'X' .              " 优化宽度
  gs_layout-box_fname    = 'SELEC' .            " 指明复选框
  gs_layout-grid_title = 'Flight'(100).
  gs_layout-excp_fname = gv_lights_name.

*-列属性
  CLEAR gt_fieldcat[] .
*  MAC01 : 'MESS' '返回消息文本' '' '' '' '' ''.
  mac01 : 'BUKRS' '公司代码' '' '' '' '' 'X'.
  mac01 : 'GJAHR' '会计年度' '' '' '' '' ''.
  mac01 : 'MONAT' '会计期间' '' '' '' '' 'X'.
  mac01 : 'VKORG' '销售组织' '' '' '' '' 'X'.
  mac01 : 'WADAT' '发货日期' '' '' '' '' ''.
  mac01 : 'VBELN' 'ERP单号' '' '' '' '' 'X'.
  mac01 : 'KUNNR' '客户代码' '' '' '' '' 'X'.
  mac01 : 'NAME1' '客户名称' '' '' '' '' ''.
  mac01 : 'VTWEG' '销售渠道' '' '' '' '' 'X'.
  mac01 : 'ORT01' '目的地' '' '' '' '' ''.
  mac01 : 'MATNR' '物料' '' '' '' '' 'X'.
  mac01 : 'MAKTX' '物料描述' '' '' '' '' ''.
  mac01 : 'LFIMGM' '发货数量（件）' '' '' '' '' ''.
  mac01 : 'VOLUMM' '体积/重量' '' '' '' '' ''.
  mac01 : 'VOLEHM' '体积/重量单位' '' '' '' '' ''.
  mac01 : 'ZCYS1' '承运商' '' '' '' '' ''.
  mac01 : 'ZYTJE' '预提金额' '' '' '' '' ''.
  mac01 : 'WAERS' '货币' '' '' '' '' ''.
  mac01 : 'ZCYJE' '差异金额' '' '' '' '' ''.
  mac01 : 'ZCYHB' '货币' '' '' '' '' ''.
  mac01 : 'ZYTPZ' '预提凭证' '' '' '' '' 'X'.
  mac01 : 'ZSJPZ' '实际凭证' '' '' '' '' 'X'.
  mac01 : 'ZZTMS' '凭证状态' '' '' '' '' ''.

*-ALV函数
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SUB_STATUS'
      i_callback_user_command  = 'SUB_USER_COMMAND'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat
      i_save                   = 'A'
*     IT_EVENTS                = GT_EVENT[]
    TABLES
      t_outtab                 = gt_alv[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  SUB_STATUS
*&---------------------------------------------------------------------*
*       状态栏
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM sub_status USING rt_extab TYPE slis_t_extab .

  CLEAR:   rt_extab.
  REFRESH: rt_extab.
  SET PF-STATUS 'ZFIR0049' EXCLUDING rt_extab .
*  SET TITLEBAR  'ZRQEVAC5001'  .

ENDFORM. " LVC_SET_STATUS
*&---------------------------------------------------------------------*
*&      FORM  SUB_USER_COMMAND
*&---------------------------------------------------------------------*
*       控制按钮
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*

FORM sub_user_command USING p_ucomm TYPE sy-ucomm
                             p_rs_selfield TYPE slis_selfield.

  DATA:
   ls_grid     TYPE REF TO cl_gui_alv_grid.         "GRID定义

* 取得屏幕更新数据
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ls_grid.

  CALL METHOD ls_grid->check_changed_data.

  CASE p_ucomm.
    WHEN '&IC1' .   " 判断用户的动作
      CLEAR gs_alv.
      READ TABLE gt_alv INTO gs_alv INDEX p_rs_selfield-tabindex.

      CASE  p_rs_selfield-fieldname .
        WHEN 'ZYTPZ' .
          SET PARAMETER ID 'BLN' FIELD gs_alv-zytpz.
          SET PARAMETER ID 'BUK' FIELD gs_alv-bukrs.
          SET PARAMETER ID 'GJR' FIELD gs_alv-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'ZSJPZ'.
          SET PARAMETER ID 'BLN' FIELD gs_alv-zsjpz.
          SET PARAMETER ID 'BUK' FIELD gs_alv-bukrs.
          SET PARAMETER ID 'GJR' FIELD gs_alv-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'VBELN'.
          SET PARAMETER ID 'VL' FIELD gs_alv-vbeln.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

      ENDCASE .

    WHEN '&CS'.
      PERFORM frm_call_bapi USING 'X'.  "测试过账
    WHEN 'GZ'.
      PERFORM frm_call_bapi USING ''.   "测试过账
    WHEN '&RYT'.
      PERFORM frm_call_bapi2 USING 'X'.   "预提冲销
    WHEN '&RCY'.
      PERFORM frm_call_bapi2 USING ''.   "差异冲销
  ENDCASE.

*-同时内表中的数据也随之刷新
  p_rs_selfield-col_stable = 'X' .   " 行
  p_rs_selfield-row_stable = 'X' .   " 列
  p_rs_selfield-refresh    = 'X' .   " 当用户在显式界面上对数据进行修改时，

ENDFORM. " LVC_USER_COMMAND

*&---------------------------------------------------------------------*
*& Form SET_SCREEN_ATTR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_screen_attr .
  CASE 'X'.
    WHEN r_x1.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'MI2'.
            IF screen-name NS 'HIGH'.
              screen-required = '2'.
            ENDIF.
          WHEN 'MI1'.
            screen-active = '0'.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.

    WHEN r_x2.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'MI1' OR 'MI2'.
            IF screen-name NS 'HIGH'.
              screen-required = '2'.
            ENDIF.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_BAPI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_call_bapi  USING p_test.
  DATA: ls_zfit0026 TYPE zfit0026,
        lt_zfit0026 TYPE TABLE OF zfit0026.

  DATA lt_errormsg TYPE tsmesg.
  DATA ls_errormsg LIKE LINE OF lt_errormsg.

  DATA: documentheader       TYPE bapiache09,
        ls_accountgl         TYPE bapiacgl09,
        lt_accountgl         TYPE TABLE OF bapiacgl09,
        ls_accountreceivable TYPE bapiacar09,
        lt_accountreceivable TYPE TABLE OF bapiacar09,
        ls_accountpayable    TYPE bapiacap09,
        lt_accountpayable    TYPE TABLE OF bapiacap09,
        ls_extension2        TYPE bapiparex,
        lt_extension2        TYPE TABLE OF bapiparex,
        ls_currencyamount    TYPE bapiaccr09,
        lt_currencyamount    TYPE TABLE OF bapiaccr09,
        es_return            TYPE bapiret2,
        et_return            TYPE TABLE OF bapiret2.


  DATA: ls_exten2 TYPE zfis_acc_document,
        l_type    TYPE bapiache09-obj_type,
        l_key     TYPE bapiache09-obj_key,
        l_sys     TYPE bapiache09-obj_sys,
        l_itemno1 TYPE bapiacgl09-itemno_acc,
        l_itemno2 TYPE bapiacgl09-itemno_acc.

  DATA: lt_alv    TYPE TABLE OF ty_alv.
  DATA: lv_zytje  TYPE zfit0025-kbetr.  "预提过账计算
  DATA: lv_zcyje  TYPE zfit0025-kbetr.  "差异过账计算
  DATA: lv_text   TYPE bapiacgl09-item_text.  "行项目文本                                                                    "add by 04815 S4DK904689


  READ TABLE gt_alv TRANSPORTING NO FIELDS WITH KEY selec = abap_true.
  IF sy-subrc GT 0.
    MESSAGE '未选中有效数据！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*  LT_ALV[] = GT_ALV[].
*  DELETE LT_ALV WHERE SELEC  = SPACE.
*  DELETE ADJACENT DUPLICATES FROM LT_ALV
*               COMPARING VBELN MATNR.
  CLEAR:lt_zfit0026.
  READ TABLE gr_erdat INTO DATA(rs_erdat) INDEX 1. "读取会计期间的最后一天
  CLEAR: documentheader, lt_accountgl[], lt_accountreceivable[], lt_accountpayable[], lt_currencyamount[], lt_extension2[].
  CLEAR:l_itemno1,l_itemno2.

  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE selec  = 'X'.
    CASE 'X'.             "判断如果状态是1才能过2，0才能过1，
      WHEN r_x1.
        IF <fs_alv>-zzt01 <> 0.
          CONTINUE.
        ENDIF.
        IF  <fs_alv>-zytje = 0.
          CONTINUE.
        ENDIF.
      WHEN r_x2.
        IF <fs_alv>-zzt01 <> 1.
          CONTINUE.
        ENDIF.
        IF  <fs_alv>-zcyje = 0.
          CONTINUE.
        ENDIF.
    ENDCASE.
    l_itemno1 =  l_itemno2 + 1.
    l_itemno2 =  l_itemno1 + 1.
    CLEAR:<fs_alv>-mess.

    documentheader-bus_act    = 'RFBU'.
    documentheader-username   = sy-uname.        "系统当前用户
    documentheader-comp_code  = <fs_alv>-bukrs.   "公司代码

    documentheader-doc_date   = rs_erdat-high.   "凭证中的凭证日期
    documentheader-pstng_date = rs_erdat-high.   "凭证中的记帐日期

*    DOCUMENTHEADER-TRANS_DATE = SY-DATUM.        "换算日期

    documentheader-fisc_year  = <fs_alv>-gjahr."会计年度
    documentheader-fis_period = <fs_alv>-monat.   "会计期间

    documentheader-doc_type   = 'SA'.   "凭证类型
*    DOCUMENTHEADER-REF_DOC_NO = <FS_ALV>-XBLNR.   "参考凭证号
    IF r_x1 = 'X'.
      documentheader-header_txt =  '运费预提分摊过账-' && <fs_alv>-zcys1 && <fs_alv>-wadat+0(6).   "凭证抬头文本              "modify by 04815 S4DK904689
      lv_text = '运费预提分摊过账-' && <fs_alv>-zcys1 && '-'.                                                                "add by 04815 S4DK904689
      "预提和实际金额不一样，所以要分开写
      CLEAR lv_zytje.
      IF <fs_alv>-zytje > 0.
        PERFORM frm_call_accountgl  TABLES lt_accountgl  USING l_itemno1 '6910180102' rs_erdat-high lv_text. "总帐科目项     "modify by 04815 S4DK904689
        PERFORM frm_call_accountgl  TABLES lt_accountgl  USING l_itemno2 '2241031900' rs_erdat-high lv_text.                "modify by 04815 S4DK904689

        PERFORM frm_call_currencyamount  TABLES lt_currencyamount  USING l_itemno1 <fs_alv>-zytje.  "货币项目
        lv_zytje = <fs_alv>-zytje * -1.
        PERFORM frm_call_currencyamount  TABLES lt_currencyamount  USING l_itemno2 lv_zytje.

        PERFORM frm_call_exten2  TABLES lt_extension2  USING l_itemno1 '40'.     "增强字段
        PERFORM frm_call_exten2  TABLES lt_extension2  USING l_itemno2 '50'..
      ELSE.
        PERFORM frm_call_accountgl  TABLES lt_accountgl  USING l_itemno1 '6910180102' rs_erdat-high lv_text.                "modify by 04815 S4DK904689
        PERFORM frm_call_accountgl  TABLES lt_accountgl  USING l_itemno2 '2241031900' rs_erdat-high lv_text.                "modify by 04815 S4DK904689

        PERFORM frm_call_currencyamount  TABLES lt_currencyamount  USING l_itemno1 <fs_alv>-zytje.
        lv_zytje = <fs_alv>-zytje * -1.
        PERFORM frm_call_currencyamount  TABLES lt_currencyamount  USING l_itemno2 lv_zytje.

        PERFORM frm_call_exten2  TABLES lt_extension2  USING l_itemno1 '50'.     "增强字段
        PERFORM frm_call_exten2  TABLES lt_extension2  USING l_itemno2 '40'.

      ENDIF.

    ELSE.
      documentheader-header_txt = '实际运费差额过账'&& <fs_alv>-zcys1 && <fs_alv>-wadat.   "凭证抬头文本                      "modify by 04815 S4DK904689
      lv_text = '实际运费差额过账-' && <fs_alv>-zcys1 && '-'.                                                                "add by 04815 S4DK904689
      CLEAR lv_zcyje.
      IF <fs_alv>-zcyje > 0.
        PERFORM frm_call_accountgl  TABLES lt_accountgl  USING l_itemno1 '6910180102' rs_erdat-high lv_text. "总帐科目项     "modify by 04815 S4DK904689
        PERFORM frm_call_accountgl  TABLES lt_accountgl  USING l_itemno2 '2241031900' rs_erdat-high lv_text.                "modify by 04815 S4DK904689

        PERFORM frm_call_currencyamount  TABLES lt_currencyamount  USING l_itemno1 <fs_alv>-zcyje.  "货币项目
        lv_zcyje = <fs_alv>-zcyje * -1.
        PERFORM frm_call_currencyamount  TABLES lt_currencyamount  USING l_itemno2 lv_zcyje.

        PERFORM frm_call_exten2  TABLES lt_extension2  USING l_itemno1 '40'.     "增强字段
        PERFORM frm_call_exten2  TABLES lt_extension2  USING l_itemno2 '50'..
      ELSE.
        PERFORM frm_call_accountgl  TABLES lt_accountgl  USING l_itemno1 '6910180102' rs_erdat-high lv_text.                "modify by 04815 S4DK904689
        PERFORM frm_call_accountgl  TABLES lt_accountgl  USING l_itemno2 '2241031900' rs_erdat-high lv_text.                "modify by 04815 S4DK904689

        PERFORM frm_call_currencyamount  TABLES lt_currencyamount  USING l_itemno1 <fs_alv>-zcyje.
        lv_zcyje = <fs_alv>-zcyje * -1.
        PERFORM frm_call_currencyamount  TABLES lt_currencyamount  USING l_itemno2 lv_zcyje.

        PERFORM frm_call_exten2  TABLES lt_extension2  USING l_itemno1 '50'.     "增强字段
        PERFORM frm_call_exten2  TABLES lt_extension2  USING l_itemno2 '40'.

      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR et_return[].

  IF p_test EQ 'X'.
    IF documentheader IS NOT INITIAL.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader    = documentheader       "录入凭证头
        TABLES
          accountgl         = lt_accountgl         "总帐科目项
          accountreceivable = lt_accountreceivable "客户项目
          accountpayable    = lt_accountpayable    "供应商项目
          currencyamount    = lt_currencyamount    "货币项目
          return            = et_return            "返回值
          extension2        = lt_extension2.       "扩展字段
      LOOP AT et_return INTO es_return WHERE type = 'A' OR type = 'E' .
*      <FS_ALV>-LIGHT = 1.
        ls_errormsg-msgty = es_return-type.
        ls_errormsg-arbgb = es_return-id.
        ls_errormsg-txtnr = es_return-number.
        ls_errormsg-msgv1 = es_return-message.
        APPEND ls_errormsg TO lt_errormsg.
      ENDLOOP.
      IF sy-subrc NE 0.
        MESSAGE s000(oo) WITH '测试成功'.
      ENDIF.
    ELSE.
      MESSAGE s000(oo) WITH '测试成功'.
    ENDIF.

  ELSE.
    IF documentheader IS NOT INITIAL.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = documentheader       "录入凭证头
        IMPORTING
          obj_type          = l_type
          obj_key           = l_key
          obj_sys           = l_sys
        TABLES
          accountgl         = lt_accountgl         "总帐科目项
          accountreceivable = lt_accountreceivable "客户项目
          accountpayable    = lt_accountpayable    "供应商项目
          currencyamount    = lt_currencyamount    "货币项目
          return            = et_return            "返回值
          extension2        = lt_extension2.       "扩展字段

      LOOP AT et_return INTO es_return WHERE type = 'A' OR type = 'E' .
*      <FS_ALV>-LIGHT = 1.
        ls_errormsg-msgty = es_return-type.
        ls_errormsg-arbgb = es_return-id.
        ls_errormsg-txtnr = es_return-number.
        ls_errormsg-msgv1 = es_return-message.
        APPEND ls_errormsg TO lt_errormsg.
      ENDLOOP.

      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CLEAR: ls_zfit0026.
        <fs_alv>-light = 3.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.

      ENDIF.
    ENDIF.
  ENDIF.

  IF lt_errormsg[] IS NOT INITIAL.
    CALL FUNCTION 'FB_MESSAGES_DISPLAY_POPUP'
      EXPORTING
        it_smesg        = lt_errormsg[]
      EXCEPTIONS
        no_messages     = 1
        popup_cancelled = 2
        OTHERS          = 3.
  ELSE.
    IF p_test EQ ''.
      LOOP AT gt_alv ASSIGNING <fs_alv> WHERE selec  = 'X' .
        CASE 'X'.             "判断如果状态是1才能过2，0才能过1，
          WHEN r_x1.
            IF <fs_alv>-zzt01 <> 0.
              <fs_alv>-light = 1.
              <fs_alv>-mess ='请选择未过账的状态'.
              CONTINUE.
            ENDIF.
            <fs_alv>-zztms = '预提过账'.

            <fs_alv>-zytpz = l_key+0(10).
            <fs_alv>-zzt01 = 1.
            ls_zfit0026-zytrq = sy-datum.
            ls_zfit0026-zytsj = sy-uzeit.
            ls_zfit0026-zytna = sy-uname.

          WHEN r_x2.
            IF <fs_alv>-zzt01 <> 1.
              <fs_alv>-light = 1.
              <fs_alv>-mess ='请选择预提过账的状态'.
              CONTINUE.
            ENDIF.
            <fs_alv>-zztms = '差异过账'.
            <fs_alv>-zsjpz = l_key+0(10).
            <fs_alv>-zzt01 = 2.
            ls_zfit0026-zcyrq = sy-datum.
            ls_zfit0026-zcysj = sy-uzeit.
            ls_zfit0026-zcyna = sy-uname.
        ENDCASE.
        <fs_alv>-light = 3.
        MOVE-CORRESPONDING <fs_alv> TO ls_zfit0026 .

        ls_zfit0026-lfimg = <fs_alv>-lfimgm.
        ls_zfit0026-volum = <fs_alv>-volumm.
        ls_zfit0026-voleh = <fs_alv>-volehm.
        ls_zfit0026-zcyhb = <fs_alv>-zcyhb.

        APPEND ls_zfit0026 TO lt_zfit0026.
        CLEAR ls_zfit0026.

      ENDLOOP.

    ENDIF.
  ENDIF.

  IF p_test EQ ''.
    IF lt_zfit0026 IS NOT INITIAL.
      MODIFY zfit0026 FROM TABLE lt_zfit0026.
      COMMIT WORK.
      MESSAGE s000(oo) WITH '审核成功'.
    ELSE.
      MESSAGE '审核失败' TYPE 'S' DISPLAY LIKE 'E' .
    ENDIF.
  ENDIF.


ENDFORM.
**********预提冲销、差异冲销所在的form***************
FORM frm_call_bapi2  USING p_test.
***要先按照顺序，需要差异冲销的就先差异冲销，然后再预提冲销***
  DATA: ls_exten2 TYPE zfis_acc_document,
        l_type    TYPE bapiache09-obj_type,
        l_key     TYPE bapiache09-obj_key,
        l_sys     TYPE bapiache09-obj_sys,
        l_itemno1 TYPE bapiacgl09-itemno_acc,
        l_itemno2 TYPE bapiacgl09-itemno_acc.

  DATA: documentheader       TYPE bapiache09,
        ls_accountgl         TYPE bapiacgl09,
        lt_accountgl         TYPE TABLE OF bapiacgl09,
        ls_accountreceivable TYPE bapiacar09,
        lt_accountreceivable TYPE TABLE OF bapiacar09,
        ls_accountpayable    TYPE bapiacap09,
        lt_accountpayable    TYPE TABLE OF bapiacap09,
        ls_extension2        TYPE bapiparex,
        lt_extension2        TYPE TABLE OF bapiparex,
        ls_currencyamount    TYPE bapiaccr09,
        lt_currencyamount    TYPE TABLE OF bapiaccr09,
        es_return            TYPE bapiret2,
        et_return            TYPE TABLE OF bapiret2,
        reversal             LIKE  bapiacrev,
        bus_act              LIKE  bapiache09-bus_act,
        obj_type             LIKE  bapiacrev-obj_type,
        obj_key              LIKE  bapiacrev-obj_key,
        obj_sys              LIKE  bapiacrev-obj_sys,
        return               LIKE TABLE OF bapiret2,
        ls_return            LIKE bapiret2.

  DATA: ls_zfit0026 TYPE zfit0026,
        lt_zfit0026 TYPE TABLE OF zfit0026.
  DATA: ls_zfit0030 TYPE zfit0030,
        lt_zfit0030 TYPE TABLE OF zfit0030.

  DATA lt_errormsg TYPE tsmesg.
  DATA ls_errormsg LIKE LINE OF lt_errormsg.
  DATA:lt_bkpf LIKE TABLE OF bkpf,
       ls_bkpf LIKE LINE OF lt_bkpf.

  READ TABLE gt_alv TRANSPORTING NO FIELDS WITH KEY selec = abap_true.
  IF sy-subrc GT 0.
    MESSAGE '未选中有效数据！' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR:lt_zfit0026,lt_errormsg.
  READ TABLE gr_erdat INTO DATA(rs_erdat) INDEX 1. "读取会计期间的最后一天
  CLEAR: documentheader, lt_accountgl[], lt_accountreceivable[], lt_accountpayable[], lt_currencyamount[], lt_extension2[].
  CLEAR:l_itemno1,l_itemno2.

  "批量进行冲销的时候，每次只能是同种类型的，如果存在不同种类型的话需要提示报错
  LOOP AT gt_alv ASSIGNING <fs_alv> WHERE selec  = 'X'.
    IF <fs_alv>-zytpz IS INITIAL AND <fs_alv>-zsjpz IS INITIAL.
      MESSAGE '存在未预提的行' TYPE 'E'.
    ENDIF.
    IF p_test = 'X'."预提冲销
      IF <fs_alv>-zsjpz IS NOT INITIAL.
        MESSAGE '存在已差异过账的行，需要先进行差异冲销' TYPE 'E'.
      ENDIF.
    ENDIF.
    IF p_test = '' AND <fs_alv>-zsjpz IS INITIAL.
      MESSAGE '该项目还未差异过账，不允许差异冲销' TYPE 'E'.
    ENDIF.
  ENDLOOP.

  MOVE gt_alv TO gt_alv2.
  DELETE gt_alv2 WHERE selec <> 'X'.
  SORT gt_alv2 BY zytpz zsjpz.
  DELETE ADJACENT DUPLICATES FROM gt_alv2 COMPARING zytpz zsjpz.

  IF p_test = 'X'.
    SELECT
      *
    FROM bkpf
    INTO TABLE lt_bkpf
    FOR ALL ENTRIES IN gt_alv2
    WHERE bukrs = gt_alv2-bukrs
      AND belnr = gt_alv2-zytpz
      AND gjahr = gt_alv2-gjahr.
  ELSE.
    SELECT
    *
  FROM bkpf
  INTO TABLE lt_bkpf
  FOR ALL ENTRIES IN gt_alv2
  WHERE bukrs = gt_alv2-bukrs
    AND belnr = gt_alv2-zsjpz
    AND gjahr = gt_alv2-gjahr.
  ENDIF.

  "支持批量冲销
  LOOP AT gt_alv2 ASSIGNING <fs_alv> WHERE selec  = 'X'.
    IF p_test = 'X'.
      READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = <fs_alv>-bukrs
                                               belnr = <fs_alv>-zytpz
                                               gjahr = <fs_alv>-gjahr.

    ELSE.
      READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = <fs_alv>-bukrs
                                              belnr = <fs_alv>-zsjpz
                                              gjahr = <fs_alv>-gjahr.
    ENDIF.
    CLEAR:<fs_alv>-mess.

*    reversal-obj_type = 'BKPFF'.
    reversal-obj_type = ls_bkpf-awtyp.
*    reversal-obj_key = '010000011881002021'.
    reversal-obj_key = ls_bkpf-awkey.
*    reversal-obj_sys = 'S4DCLNT300'.
    reversal-obj_key_r = ls_bkpf-awkey.
    reversal-obj_key = ls_bkpf-awkey.
    reversal-pstng_date = rs_erdat-high.
    reversal-fis_period = '00'.
    reversal-comp_code = <fs_alv>-bukrs.
    reversal-reason_rev ='03'.
    "进行检查测试
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_CHECK'
      EXPORTING
        reversal = reversal
        bus_act  = 'RFBU'
      TABLES
        return   = return.
    "读取检查的结果
    LOOP AT return INTO ls_return WHERE type = 'A' OR type = 'E'.
      ls_errormsg-msgty = ls_return-type.
      ls_errormsg-arbgb = ls_return-id.
      ls_errormsg-txtnr = ls_return-number.
      ls_errormsg-msgv1 = ls_return-message.
      APPEND ls_errormsg TO lt_errormsg.
    ENDLOOP.
    IF lt_errormsg[] IS NOT INITIAL.
      "弹窗错误消息
      CALL FUNCTION 'FB_MESSAGES_DISPLAY_POPUP'
        EXPORTING
          it_smesg        = lt_errormsg[]
        EXCEPTIONS
          no_messages     = 1
          popup_cancelled = 2
          OTHERS          = 3.
    ELSE.
      "调用真正的BAPI
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
        EXPORTING
          reversal = reversal
          bus_act  = 'RFBU'
        IMPORTING
          obj_type = obj_type
          obj_key  = obj_key
          obj_sys  = obj_sys
        TABLES
          return   = return.
      IF sy-subrc <> 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        "读取检查的结果
        LOOP AT return INTO ls_return WHERE type = 'A' OR type = 'E'.
          ls_errormsg-msgty = ls_return-type.
          ls_errormsg-arbgb = ls_return-id.
          ls_errormsg-txtnr = ls_return-number.
          ls_errormsg-msgv1 = ls_return-message.
          APPEND ls_errormsg TO lt_errormsg.
        ENDLOOP.
        IF lt_errormsg[] IS NOT INITIAL.
          "弹窗错误消息
          CALL FUNCTION 'FB_MESSAGES_DISPLAY_POPUP'
            EXPORTING
              it_smesg        = lt_errormsg[]
            EXCEPTIONS
              no_messages     = 1
              popup_cancelled = 2
              OTHERS          = 3.
        ENDIF.
        MESSAGE 'FAIL' TYPE 'E'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
        MESSAGE 'SUCCESS' TYPE 'S'.
        "如果是差异冲销的话，凭证状态从2更新为1
        IF p_test = ''.
          LOOP AT gt_alv INTO gs_alv WHERE zsjpz = <fs_alv>-zsjpz.
            gs_alv-zsjpz = '1'.
            gs_alv-zztms = '预提过账'.
            CLEAR: gs_alv-zcyje,gs_alv-zcyhb,gs_alv-zsjpz.
            MODIFY gt_alv FROM gs_alv.
            MOVE-CORRESPONDING gs_alv TO ls_zfit0026.
            ls_zfit0026-zzt01 = '1'.
            CLEAR:ls_zfit0026-zcyrq,ls_zfit0026-zcysj,ls_zfit0026-zcyna.
            MODIFY zfit0026 FROM ls_zfit0026.
            COMMIT WORK.
            CLEAR:gs_alv,ls_zfit0026.
          ENDLOOP.
          "写入日志
          IF sy-subrc = 0.
            SELECT SINGLE
              bukrs,
              gjahr,
              monat,
              belnr AS belod,
              awref_rev AS belnr
            FROM bkpf
            INTO CORRESPONDING FIELDS OF @ls_zfit0030
            WHERE awkey = @ls_bkpf-awkey.
            ls_zfit0030-optty = '2'.
            ls_zfit0030-ernam = sy-uname.
            ls_zfit0030-erdat = sy-datum.
            ls_zfit0030-erzet = sy-uzeit.
            INSERT zfit0030 FROM ls_zfit0030.
            COMMIT WORK.
          ENDIF.
        ELSE.
          "如果是预提冲销的话，凭证状态从1更新为0,
          LOOP AT gt_alv INTO gs_alv WHERE zsjpz = <fs_alv>-zsjpz.
            gs_alv-zsjpz = '0'.
            gs_alv-zztms = '未过账'.
            CLEAR: gs_alv-zytpz,gs_alv-zytje.
            MODIFY gt_alv FROM gs_alv.
            MOVE-CORRESPONDING gs_alv TO ls_zfit0026.
            ls_zfit0026-zzt01 = '0'.
            CLEAR:ls_zfit0026-zytrq,ls_zfit0026-zytsj,ls_zfit0026-zytna.
            MODIFY zfit0026 FROM ls_zfit0026.
            COMMIT WORK.
            CLEAR:gs_alv,ls_zfit0026.
          ENDLOOP.
          "写入日志
          IF sy-subrc = 0.
            SELECT SINGLE
              bukrs,
              gjahr,
              monat,
              belnr AS belod,
              awref_rev AS belnr
            FROM bkpf
            INTO CORRESPONDING FIELDS OF @ls_zfit0030
            WHERE awkey = @ls_bkpf-awkey.
            ls_zfit0030-optty = '1'.
            ls_zfit0030-ernam = sy-uname.
            ls_zfit0030-erdat = sy-datum.
            ls_zfit0030-erzet = sy-uzeit.
            INSERT zfit0030 FROM ls_zfit0030.
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR ls_bkpf.
  ENDLOOP.
  CLEAR:et_return[],return.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_REQUEST_F4_INSMK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM frm_request_f4_insmk USING pv_field TYPE help_info-dynprofld.


  DATA: BEGIN OF ls_value_tab,
          zcys1 TYPE zfit0025-zcys1,
        END OF ls_value_tab.
  DATA: lt_value_tab LIKE TABLE OF ls_value_tab.

  SELECT  DISTINCT
       zcys1
       INTO CORRESPONDING FIELDS OF TABLE lt_value_tab
       FROM zfit0025.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZCYS1'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = pv_field
      value_org       = 'S'
    TABLES
      value_tab       = lt_value_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.
FORM standard_message.

  sy-msgty = COND #( WHEN sy-msgty = '' THEN 'E' ELSE sy-msgty ).
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
  sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_ACCOUNTGL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ACCOUNTGL
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_call_accountgl  TABLES   pt_accountgl STRUCTURE  bapiacgl09
                          USING   p_itemno_acc
                                  p_gl_account
                                  p_high
                                  p_text.

  DATA:ls_accountgl         TYPE bapiacgl09.

  ls_accountgl-itemno_acc = p_itemno_acc.           "会计凭证行项目编号
  ls_accountgl-gl_account = p_gl_account.
  ls_accountgl-doc_type   = 'SA'.
  ls_accountgl-comp_code  = <fs_alv>-bukrs.
  ls_accountgl-fis_period = <fs_alv>-monat.
  ls_accountgl-fisc_year  = <fs_alv>-gjahr.
  ls_accountgl-pstng_date = p_high.
  ls_accountgl-material_long = <fs_alv>-matnr.

  DATA(lv_matnr)           = |{ <fs_alv>-matnr ALPHA = OUT }|. "短物料
  ls_accountgl-material    = lv_matnr.
  ls_accountgl-ref_key_1   = <fs_alv>-prctr.
  ls_accountgl-costcenter  = <fs_alv>-kostl.
  "Seashell Huang 241031900科目分录不挂成本中心
  IF p_gl_account = '241031900'.
    ls_accountgl-costcenter = ''.
  ENDIF.
  "END
  ls_accountgl-item_text   = p_text && <fs_alv>-wadat+0(6).
  APPEND ls_accountgl TO pt_accountgl. CLEAR ls_accountgl.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_CURRENCYAMOUNT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_CURRENCYAMOUNT
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_call_currencyamount  TABLES   pt_currencyamount STRUCTURE bapiaccr09
                              USING  p_itemno_acc p_zytje.
  DATA  ls_currencyamount    TYPE bapiaccr09.

  ls_currencyamount-itemno_acc = p_itemno_acc.
  ls_currencyamount-curr_type  = '00'.
  ls_currencyamount-currency = <fs_alv>-waers.
  ls_currencyamount-amt_doccur = p_zytje . "金额
  APPEND ls_currencyamount TO pt_currencyamount. CLEAR  ls_currencyamount.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_CALL_EXTEN2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_EXTENSION2
*&      --> P_
*&---------------------------------------------------------------------*
FORM frm_call_exten2  TABLES pt_extension2 STRUCTURE bapiparex
                       USING p_posnr p_bschl.

  DATA: ls_extension2 TYPE bapiparex,
        ls_exten2     TYPE zfis_acc_document.

  CLEAR ls_exten2.
  ls_exten2-posnr = p_posnr.
  ls_exten2-bschl = p_bschl. "记账码
  ls_extension2-structure  = 'ZFIS_ACC_DOCUMENT'.
  ls_extension2-valuepart1 = ls_exten2.

  APPEND ls_extension2 TO pt_extension2. CLEAR ls_extension2.

ENDFORM.