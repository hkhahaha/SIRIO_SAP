*****************************************************************
*  System      : 仙乐 SAP ERP项目
*  Module      :  MM
*  ProgramID   : ZMM067
*  Program     :  ZMMR0067
*  Author      :  Seashell
*  Date        :  04.06.2021 13:16:42
*  Description :  MM067
*****************************************************************
*  Modified Recorder :
*  Date         C#NO        Author                Content
*  -----------  -------    ------------------     ---------------
*****************************************************************
REPORT zmmr0067
*MESSAGE-ID ZSD01
NO STANDARD PAGE HEADING LINE-SIZE 150 .
*&---------------------------------------------------------------------*
* ALV TYPES
*&---------------------------------------------------------------------*
TYPE-POOLS: slis .

DATA: gs_layout     TYPE lvc_s_layo.            "ALV布局工作区

*DATA: GCL_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.
DATA : gv_lights_name TYPE lvc_cifnm VALUE 'LIGHT' ."红绿灯

DATA: gs_fieldcat TYPE lvc_s_fcat,              "字段工作区
      gt_fieldcat TYPE lvc_t_fcat.

DEFINE mac01 .
  CLEAR gs_fieldcat .
  gs_fieldcat-fieldname     = &1 .
  gs_fieldcat-scrtext_l     = &2 .
  gs_fieldcat-edit          = &3 .
  gs_fieldcat-key           = &4 .
  gs_fieldcat-no_zero       = &5 .

  CASE gs_fieldcat-fieldname  .
  WHEN 'LIFNR' OR 'PRODUCER'.
    gs_fieldcat-ref_field = 'LIFNR' .
    gs_fieldcat-ref_table = 'LFA1' .
    CLEAR gs_fieldcat-no_zero  .
  WHEN 'DLV_PLANNO' .
    gs_fieldcat-ref_field = 'DLV_PLANNO' .
    gs_fieldcat-ref_table = 'ZMMWMS_POL' .
    CLEAR gs_fieldcat-no_zero  .
  WHEN 'MATNR' .
    gs_fieldcat-ref_field = 'MATNR' .
    gs_fieldcat-ref_table = 'MARA' .
    CLEAR gs_fieldcat-no_zero  .
  WHEN 'MFRPN' .
    gs_fieldcat-ref_field = 'MFRPN' .
    gs_fieldcat-ref_table = 'MARA' .
    CLEAR gs_fieldcat-no_zero  .
  WHEN 'ERNAM'  .
    CLEAR gs_fieldcat-no_zero  .
  ENDCASE .

  APPEND  gs_fieldcat TO  gt_fieldcat .
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
* TYPES
*&---------------------------------------------------------------------*
TYPES : BEGIN OF ty_pol .
          INCLUDE TYPE zmmwms_pol .
          TYPES : ekorg TYPE ekko-ekorg , "
          ekgrp TYPE ekko-ekgrp , "批次号
          lifnr TYPE ekko-lifnr,
          ernam TYPE ekko-ernam , "制单人 待确认
          aedat TYPE ekko-aedat,
        END OF ty_pol .

TYPES : BEGIN OF ty_temp .
          INCLUDE TYPE zmmaps_temp .
          TYPES : ekorg TYPE ekko-ekorg , "
          ekgrp TYPE ekko-ekgrp , "批次号
*          LIFNR TYPE EKKO-LIFNR,
          ernam TYPE ekko-ernam , "制单人 待确认
          aedat TYPE ekko-aedat,
        END OF ty_temp .

TYPES : BEGIN OF ty_ebeln ,
          ebeln TYPE ekpo-ebeln,
          ebelp TYPE ekpo-ebelp,
          lifnr TYPE ekko-lifnr,
        END OF ty_ebeln .

TYPES : BEGIN OF ty_ekpo ,
          ebeln TYPE ekpo-ebeln,
          ebelp TYPE ekpo-ebelp,
          txz01 TYPE ekpo-txz01,
          matnr TYPE ekpo-matnr,
          werks TYPE ekpo-werks,
          lgort TYPE ekpo-lgort,
          matkl TYPE ekpo-matkl,
          menge TYPE ekpo-menge,
          meins TYPE ekpo-meins,
          uebto TYPE ekpo-uebto,
          mfrpn TYPE ekpo-mfrpn,
          mfrnr TYPE ekpo-mfrnr,
          etenr TYPE eket-etenr,
          eindt TYPE eket-eindt,
        END OF ty_ekpo .

TYPES : BEGIN OF ty_ekbe ,
          ebeln TYPE ekbe-ebeln,
          ebelp TYPE ekbe-ebelp,
          zekkn TYPE ekbe-zekkn,
          vgabe TYPE ekbe-vgabe,
          gjahr TYPE ekbe-gjahr,
          belnr TYPE ekbe-belnr,
          buzei TYPE ekbe-buzei,
          bewtp TYPE ekbe-bewtp,
          bwart TYPE ekbe-bwart,
          budat TYPE ekbe-budat,
          menge TYPE ekbe-menge,
          bpmng TYPE ekbe-bpmng,
          shkzg TYPE ekbe-shkzg,
        END OF ty_ekbe .

TYPES : BEGIN OF ty_zmatdoc .
          INCLUDE TYPE zmatdoc .
          TYPES : bldat TYPE mkpf-bldat,
          budat TYPE mkpf-budat,
          cpudt TYPE mkpf-cpudt,
          shkzm TYPE mseg-shkzg,
        END OF ty_zmatdoc .

TYPES : BEGIN OF ty_lfa1 ,
          lifnr TYPE lfa1-lifnr,
          land1 TYPE lfa1-land1,
          name1 TYPE adrc-name1,
          name2 TYPE adrc-name2,
          name3 TYPE adrc-name3,
          name4 TYPE adrc-name4,
        END OF ty_lfa1 .

TYPES : BEGIN OF ty_marc ,
          matnr TYPE marc-matnr,
          werks TYPE marc-werks,
          bstmi TYPE marc-bstmi,
          plifz TYPE marc-plifz,
        END OF ty_marc .

TYPES : BEGIN OF ty_alv ,
          dlv_planno        TYPE zmmwms_pol-dlv_planno,
          dlv_itemno        TYPE zmmwms_pol-dlv_itemno,
          werks             TYPE zmmwms_pol-werks,
          ebeln             TYPE zmmwms_pol-ebeln,
          ebelp             TYPE zmmwms_pol-ebelp,
          lifnr             TYPE ekko-lifnr,
          pdlv_date         TYPE zmmwms_pol-pdlv_date,
          matnr             TYPE zmmwms_pol-matnr,
          producer          TYPE zmmwms_pol-producer,
          pdlv_menge        TYPE zmmwms_pol-pdlv_menge,
          xclosed           TYPE zmmwms_pol-xclosed,
          ernam             TYPE ekko-ernam,
          zslzt             TYPE zslzt,
          zslzm(20)         TYPE c,
          zjhly             TYPE zjhly,
          zjhlm(20)         TYPE c,
          zyjdh             TYPE i,
          recv_menge        TYPE zmatdoc-menge , "净收货量
          po_num            TYPE i , "本单到货次数
          zclosed(1)        TYPE c,
          undlv_mende       TYPE zmatdoc-menge  , "剩余未到货量
          maktx             TYPE ekpo-txz01,
          lgort             TYPE ekpo-lgort,
          menge             TYPE ekpo-menge,
          meins             TYPE ekpo-meins , "单位
          uebto             TYPE ekpo-uebto , "容差率
          eindt             TYPE eket-eindt , "原订单到货时间
          aps_req_date      TYPE zmmaps_temp-reqdate , "APS需求日期
          apsreq_menge      TYPE ekpo-menge , "APS需求数量
          pdlv_date_ext     TYPE zmmaps_temp-pdlv_date_ext , "调整后答复送货日期
          version           TYPE zmmaps_temp-version , "交付日期（版本号）
          budat             TYPE mkpf-budat , "实际到货日期
          zdkss             TYPE ekpo-menge , "最大可收货数量
          delay             TYPE i , ""未到货延期天数
          zpoys             TYPE ekpo-menge , "PO已收
          zrequ             TYPE ekpo-menge , "已退数量
          zzsdb             TYPE zzsdb , "指示灯
          zzsdm(20)         TYPE c, "指示灯
          zzsdx             TYPE zzsdx , "指示灯信息
          zzsdt(20)         TYPE c , "指示灯信息
          zname(20)         TYPE c , "制单人
          made_in_china(20) TYPE c , "原产地
          xprecious(20)     TYPE c , "贵重标识
          bstmi             TYPE marc-bstmi,
          note              TYPE zmmwms_poh-note,
          name1             TYPE lfa1-name1 , "供应商
          name2             TYPE lfa1-name1 , "制造商
          zzhsh             TYPE sy-datum , "最后收货日期（计算用）
          zwysh             TYPE sy-datum , "唯一的答复收货日期
          light(1)          TYPE c,
          selec(1)          TYPE c,
          st_code           TYPE zmmwms_pol-st_code,
          ekgrp             TYPE ekko-ekgrp,
          eknam             TYPE t024-eknam,
          mfrpn             TYPE ekpo-mfrpn,
          zyqts             TYPE i,
          zzhsx             TYPE sy-datum , "最后收货日期（显示用）
          aedat             TYPE ekko-aedat, "单据日期
          delaydate(6)      TYPE c, "迟到天数
          plifz             TYPE marc-plifz, "采购周期
          polevel(5)        TYPE c, "订单级别
          remark            TYPE zmmaps_temp-zbeiz, "备注
          z_date            TYPE mkpf-budat, "用于指示灯计算，代表最终答复的日期
        END OF ty_alv .

TYPES : BEGIN OF ty_znb9,
          ebeln   TYPE ekko-ebeln,
          bsart   TYPE ekko-bsart , "
          ekorg   TYPE ekko-ekorg , "
          ekgrp   TYPE ekko-ekgrp , "批次号
          lifnr   TYPE ekko-lifnr,
          ernam   TYPE ekko-ernam , "制单人 待确认
*          LOEKZ   TYPE EKKO-LOEKZ,
          memory  TYPE ekko-memory,
          ebelp   TYPE ekpo-ebelp,
          txz01   TYPE ekpo-txz01,
          matnr   TYPE ekpo-matnr,
          werks   TYPE ekpo-werks,
          lgort   TYPE ekpo-lgort,
          matkl   TYPE ekpo-matkl,
          menge   TYPE ekpo-menge,
          meins   TYPE ekpo-meins,
          uebto   TYPE ekpo-uebto,
          mfrpn   TYPE ekpo-mfrpn,
          mfrnr   TYPE ekpo-mfrnr,
          st_code TYPE ekpo-st_code,
          aedat   TYPE ekko-aedat,
        END OF ty_znb9 .

*&---------------------------------------------------------------------*
* GLOBAL DATA
*&---------------------------------------------------------------------*
DATA : gs_pol TYPE ty_pol .
DATA : gs_temp TYPE ty_temp .
DATA : gs_ebeln TYPE ty_ebeln .
DATA : gs_ekpo TYPE ty_ekpo .
DATA : gs_ekbe TYPE ty_ekbe .
DATA : gs_zmatdoc TYPE ty_zmatdoc .
DATA : gs_zmmtreord TYPE zmmtreord .
DATA : gs_poh TYPE zmmwms_poh .
DATA : gs_lfa1 TYPE ty_lfa1 .
DATA : gs_templ TYPE zmmaps_temp ."APS需求日期 获取使用
DATA : gs_t006a TYPE t006a .
DATA : gs_pll TYPE zmmaps_pl .
DATA : gs_pl TYPE zmmaps_pl .
DATA : gs_marc TYPE ty_marc .
DATA : gs_dd07t TYPE dd07t .
DATA : gs_alv TYPE ty_alv .
DATA : gs_t024 TYPE t024 .
DATA : gs_znb9 TYPE ty_znb9 .
DATA : gs_makt TYPE makt .

*&---------------------------------------------------------------------*
* GLOBAL INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA : gt_pol TYPE TABLE OF ty_pol .
DATA : gt_temp TYPE TABLE OF ty_temp .
RANGES : r_zgbbs FOR zmms0002-zgbbs .
DATA : gt_ebeln TYPE TABLE OF ty_ebeln .
DATA : gt_ekpo TYPE TABLE OF ty_ekpo .
DATA : gt_ekbe TYPE TABLE OF ty_ekbe .
DATA : gt_zmatdoc TYPE TABLE OF ty_zmatdoc .
DATA : gt_zmmtreord TYPE TABLE OF zmmtreord .
DATA : gt_poh TYPE TABLE OF zmmwms_poh .
DATA : gt_lfa1 TYPE TABLE OF ty_lfa1 .
DATA : gt_templ TYPE TABLE OF zmmaps_temp ."APS需求日期 获取使用
DATA : gt_t006a TYPE TABLE OF t006a .
DATA : gt_pll TYPE TABLE OF zmmaps_pl .
DATA : gt_pl TYPE TABLE OF zmmaps_pl .
DATA : gt_marc TYPE TABLE OF ty_marc .
DATA : gt_dd07t TYPE TABLE OF dd07t .
DATA : gt_alv TYPE TABLE OF ty_alv .
DATA : gt_t024 TYPE TABLE OF t024 .
DATA : gt_znb9 TYPE TABLE OF ty_znb9 .
DATA : gt_makt TYPE TABLE OF makt .

*&---------------------------------------------------------------------*
* GLOBAL VARIABLE
*&---------------------------------------------------------------------*
DATA:sumbatch(4)         TYPE i,
     latebatch(4)        TYPE i,
     intimenobatch(4)    TYPE i,
     intimerate_tmp(5)   TYPE p DECIMALS 2,
     intimerate(7)   ,
     intimerate2_tmp(5)  TYPE p DECIMALS 2,
     intimerate2(7)  ,
     intime_close_six(4) TYPE i.
DATA:i_alv_top_of_page TYPE slis_t_listheader.
*&---------------------------------------------------------------------*
* SELECTION-SCREEN
*&---------------------------------------------------------------------*
TABLES : ekko ,ekpo,zmmaps_temp,zmmwms_pol,zmms0002.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : s_ekorg FOR ekko-ekorg ."
SELECT-OPTIONS : s_werks FOR ekpo-werks ."物料号
SELECT-OPTIONS : s_ekgrp FOR ekko-ekgrp ."批次号
SELECT-OPTIONS : s_versi FOR zmmaps_temp-version ."订单开始时间
PARAMETERS : p_zjhly TYPE zjhly AS LISTBOX VISIBLE LENGTH 20 ."计划来源
SELECT-OPTIONS : s_code FOR zmmaps_temp-st_code .
SELECT-OPTIONS : s_lifnr FOR ekko-lifnr .
SELECT-OPTIONS : s_ebeln FOR ekko-ebeln .
SELECT-OPTIONS : s_ebelp FOR ekpo-ebelp .
SELECT-OPTIONS : s_matnr FOR ekpo-matnr .
SELECT-OPTIONS : s_mfrpn FOR ekpo-mfrpn .
SELECT-OPTIONS : s_ernam FOR ekko-ernam ."制单人 待确认
SELECT-OPTIONS : s_planno FOR zmmwms_pol-dlv_planno .
SELECT-OPTIONS : s_itemno FOR zmmwms_pol-dlv_itemno .
SELECT-OPTIONS : s_zslzt FOR zmms0002-zslzt  ."状态
PARAMETERS : p_zgbbs TYPE zgbbs AS LISTBOX VISIBLE LENGTH 20 DEFAULT ''."关闭标识
SELECT-OPTIONS : s_date FOR zmmwms_pol-pdlv_date .
PARAMETERS : p_days TYPE i  ."预计到货天数
PARAMETERS : p_zzsdb TYPE zzsdb AS LISTBOX VISIBLE LENGTH 20 ."指示灯
PARAMETERS : p_zzsdx TYPE zzsdx AS LISTBOX VISIBLE LENGTH 20 ."指示灯信息

SELECTION-SCREEN END OF BLOCK a1.

INITIALIZATION.


AT SELECTION-SCREEN.

START-OF-SELECTION.

  PERFORM sub_check_auth .
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
*  PERFORM SUB_GET_ZGBBS ."获取关闭标识选择条件
  PERFORM sub_get_zmmwms_pol .
  PERFORM sub_get_zmmaps_temp .
  PERFORM sub_get_znb9 .
  IF gt_pol[] IS INITIAL
  AND gt_temp[] IS INITIAL
  AND gt_znb9[] IS INITIAL  .
    MESSAGE '没有符合条件的数据' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
  ENDIF .
  PERFORM sub_get_ebeln ."收集统计到的采购订单
  PERFORM sub_get_ekpo .
  PERFORM sub_get_ekbe .
  PERFORM sub_get_zmatdoc .
  PERFORM sub_get_zmmtreord .
  PERFORM sub_get_poh .
  PERFORM sub_get_lfa1 .
  PERFORM sub_get_templ ."pol 对应aps temp
  PERFORM sub_get_pll ."POL 对应aps pl
  PERFORM sub_get_pl ."temp 对应aps pl
*  PERFORM SUB_GET_T006A .
  PERFORM sub_get_marc ."
  PERFORM sub_get_dd07l .
  PERFORM sub_get_t024 .
  PERFORM sub_get_kakt .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZGBBS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_zgbbs .
  CLEAR r_zgbbs .
  CLEAR r_zgbbs[] .
  CASE p_zgbbs .
    WHEN '1' ."未关闭
      r_zgbbs-sign = 'I' .
      r_zgbbs-option = 'EQ' .
      r_zgbbs-low = space .
      APPEND r_zgbbs .
    WHEN '2' .
      r_zgbbs-sign = 'I' .
      r_zgbbs-option = 'EQ' .
      r_zgbbs-low = 'X' .
      APPEND r_zgbbs .
  ENDCASE .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZMMWMS_POL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_zmmwms_pol .

  DATA : lv_where(400) TYPE c .
  IF s_mfrpn[] IS INITIAL .
    lv_where = 'EKKO~EBELN IN @S_EBELN'  .
  ELSE .
    lv_where = 'EXISTS ( SELECT * FROM EKPO WHERE EBELN = POL~EBELN AND EBELP = POL~EBELP AND MFRPN IN @S_MFRPN ) ' .
  ENDIF .

  CLEAR gt_pol[] .
  IF s_versi[] IS INITIAL . "版本号
    SELECT pol~* ,
           ekko~ekorg,
           ekko~ekgrp,
           ekko~lifnr,
           ekko~ernam,
           ekko~aedat
      INTO TABLE @gt_pol
      FROM zmmwms_pol AS pol
      INNER JOIN ekko
      ON ekko~ebeln = pol~ebeln
      WHERE pol~dlv_planno IN @s_planno
        AND pol~dlv_itemno IN @s_itemno
        AND pol~ebeln  IN @s_ebeln
        AND pol~ebelp  IN @s_ebelp
        AND pol~matnr  IN @s_matnr
*        AND POL~PDLV_DATE IN @S_DATE
        AND pol~werks IN @s_werks
        AND pol~st_code IN @s_code
*        AND POL~XCLOSED IN @R_ZGBBS
        AND ekko~bsart <> 'ZRT'
        AND ekko~ekorg IN @s_ekorg
        AND ekko~ekgrp IN @s_ekgrp
        AND ekko~lifnr IN @s_lifnr
        AND ekko~ernam IN @s_ernam
        AND (lv_where).
  ELSE .

    SELECT pol~* ,
           ekko~ekorg,
           ekko~ekgrp,
           ekko~lifnr,
           ekko~ernam,
           ekko~aedat
      INTO TABLE @gt_pol
      FROM zmmwms_pol AS pol
      INNER JOIN ekko
      ON ekko~ebeln = pol~ebeln
      WHERE pol~dlv_planno IN @s_planno
        AND pol~dlv_itemno IN @s_itemno
        AND pol~ebeln  IN @s_ebeln
        AND pol~ebelp  IN @s_ebelp
        AND pol~matnr  IN @s_matnr
*        AND POL~PDLV_DATE IN @S_DATE
        AND pol~werks IN @s_werks
        AND pol~st_code IN @s_code
*        AND POL~XCLOSED IN @R_ZGBBS
        AND ekko~bsart <> 'ZRT'
        AND ekko~ekorg IN @s_ekorg
        AND ekko~ekgrp IN @s_ekgrp
        AND ekko~lifnr IN @s_lifnr
        AND ekko~ernam IN @s_ernam
        AND EXISTS ( SELECT *
                        FROM zmmaps_temp
                        WHERE version IN @s_versi
                          AND inactive = ''
                          AND dlv_planno = pol~dlv_planno
                          AND dlv_itemno = pol~dlv_itemno )
      AND (lv_where) .
*       AND EXISTS ( SELECT *
*                       FROM EKPO
*                       WHERE EBELN = POL~EBELN
*                         AND EBELP = POL~EBELP
*                         AND MFRPN IN @S_MFRPN )
    .

  ENDIF .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZMMAPS_TEMP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_zmmaps_temp .

*SELECT-OPTIONS : S_DATE FOR ZMMWMS_POL-PDLV_DATE .计划收货日期
  CHECK s_date[] IS INITIAL  .

  DATA : lv_where(400) TYPE c .
  IF s_mfrpn[] IS INITIAL .
    lv_where = 'EKKO~EBELN IN @S_EBELN'  .
  ELSE .
    lv_where = 'EXISTS ( SELECT * FROM EKPO WHERE EBELN = TEMP~EBELN AND EBELP = TEMP~EBELP AND MFRPN IN @S_MFRPN ) ' .
  ENDIF .

  CLEAR gt_temp[] .
  SELECT temp~* ,
         ekko~ekorg,
         ekko~ekgrp,
*         EKKO~LIFNR,
         ekko~ernam,
         ekko~aedat
    INTO TABLE @gt_temp
    FROM zmmaps_temp AS temp
    INNER JOIN ekko
    ON ekko~ebeln = temp~ebeln
    WHERE temp~werks IN @s_werks
      AND temp~version IN @s_versi
    AND temp~matnr  IN @s_matnr
    AND temp~st_code IN @s_code
      AND temp~ebeln  IN @s_ebeln
      AND temp~ebelp  IN @s_ebelp
*     AND TEMP~PDLV_DATE IN @S_DATE
      AND temp~inactive IN @r_zgbbs
      AND temp~dlv_planno IN @s_planno
      AND temp~dlv_itemno IN @s_itemno
      AND temp~dlv_planno = ''
    AND ekko~bsart <> 'ZRT'
        AND ekko~ekorg IN @s_ekorg
        AND ekko~ekgrp IN @s_ekgrp
        AND ekko~lifnr IN @s_lifnr
        AND ekko~ernam IN @s_ernam
    AND (lv_where).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZNB9
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_znb9 .

*SELECT-OPTIONS : S_DATE FOR ZMMWMS_POL-PDLV_DATE .计划收货日期
  CHECK s_date[] IS INITIAL  .
  CHECK s_planno[] IS INITIAL  .
  CHECK s_itemno[] IS INITIAL  .

  CLEAR gt_znb9[] .
  SELECT
  ekko~ebeln "
  ekko~bsart "
  ekko~ekorg "
  ekko~ekgrp "批次号
  ekko~lifnr
  ekko~ernam "制单人 待确认
  ekko~memory
  ekpo~ebelp
  ekpo~txz01
  ekpo~matnr
  ekpo~werks
  ekpo~lgort
  ekpo~matkl
  ekpo~menge
  ekpo~meins
  ekpo~uebto
  ekpo~mfrpn
  ekpo~mfrnr
  ekpo~st_code
  ekko~aedat
   INTO TABLE gt_znb9
   FROM ekko
   INNER JOIN ekpo
   ON ekko~ebeln = ekpo~ebeln
   WHERE ekko~bsart = 'ZNB9'
     AND ekko~ebeln IN s_ebeln
     AND ekko~ekorg IN s_ekorg
     AND ekko~ekgrp IN s_ekgrp
     AND ekko~lifnr IN s_lifnr
     AND ekko~ernam IN s_ernam
     AND ekko~memory = ''   "采购订单未完成
     AND ekpo~ebelp IN s_ebelp
     AND ekpo~matnr IN s_matnr
     AND ekpo~werks IN s_werks
     AND ekpo~st_code IN s_code
     AND ekpo~mfrpn IN s_mfrpn
     AND ekpo~ematn    <> ''
     AND ekpo~loekz = ''
     AND ekpo~abskz = '' "拒绝标识
     AND ekpo~elikz = ''  "“交货已完成”标识
     AND NOT EXISTS ( SELECT *
                         FROM zmmwms_pol
                         WHERE ebeln = ekko~ebeln
                           AND ebelp = ekpo~ebelp
                           AND xclosed = ''
    )
    .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_EBELN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_ebeln .
  DATA : lt_ebeln TYPE TABLE OF ty_ebeln .

  CLEAR gt_ebeln[] .
  MOVE-CORRESPONDING gt_pol[] TO gt_ebeln[] .
  CLEAR lt_ebeln[] .
  MOVE-CORRESPONDING gt_temp[] TO lt_ebeln[] .
  APPEND LINES OF lt_ebeln[] TO gt_ebeln[] .
  CLEAR lt_ebeln[] .
  MOVE-CORRESPONDING gt_znb9[] TO lt_ebeln[] .
  APPEND LINES OF lt_ebeln[] TO gt_ebeln[] .
  SORT gt_ebeln BY ebeln ebelp .
  DELETE ADJACENT DUPLICATES FROM gt_ebeln
                   COMPARING ebeln ebelp .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_EKPO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_ekpo .

  CHECK gt_ebeln[] IS NOT INITIAL .

  CLEAR gt_ekpo[] .
  SELECT
  ekpo~ebeln
  ekpo~ebelp
  ekpo~txz01
  ekpo~matnr
  ekpo~werks
  ekpo~lgort
  ekpo~matkl
  ekpo~menge
  ekpo~meins
  ekpo~uebto
  ekpo~mfrpn
  ekpo~mfrnr
  eket~etenr
  eket~eindt
  INTO TABLE gt_ekpo
  FROM ekpo
  LEFT JOIN eket
  ON eket~ebeln = ekpo~ebeln
 AND eket~ebelp = ekpo~ebelp
 AND eket~etenr = '0001'
  FOR ALL ENTRIES IN gt_ebeln
  WHERE ekpo~ebeln = gt_ebeln-ebeln
    AND ekpo~ebelp = gt_ebeln-ebelp .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_EKBE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_ekbe .
  CHECK gt_ebeln[] IS NOT INITIAL .

  CLEAR gt_ekbe[] .
  SELECT
ebeln
ebelp
zekkn
vgabe
gjahr
belnr
buzei
bewtp
bwart
budat
menge
bpmng
shkzg
INTO TABLE gt_ekbe
FROM ekbe
FOR ALL ENTRIES IN gt_ebeln
WHERE ebeln = gt_ebeln-ebeln
  AND ebelp = gt_ebeln-ebelp
  AND bwart <> ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZMATDOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_zmatdoc .
  CHECK gt_ebeln[] IS NOT INITIAL .

  CLEAR gt_zmatdoc[] .
  SELECT zmatdoc~* ,
        mkpf~bldat,
        mkpf~budat,
        mkpf~cpudt,
        mseg~shkzg AS shkzm
    INTO TABLE @gt_zmatdoc
    FROM zmatdoc
    INNER JOIN mkpf
    ON mkpf~mblnr = zmatdoc~mblnr
   AND mkpf~mjahr = zmatdoc~mjahr
   INNER JOIN mseg
   ON mseg~mblnr = zmatdoc~mblnr
   AND mseg~mjahr = zmatdoc~mjahr
   AND mseg~zeile = zmatdoc~zeile
    FOR ALL ENTRIES IN @gt_ebeln
WHERE zmatdoc~ebeln = @gt_ebeln-ebeln
  AND zmatdoc~ebelp = @gt_ebeln-ebelp .



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_ZMMTREORD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_zmmtreord .
  CHECK gt_ebeln[] IS NOT INITIAL .

  CLEAR gt_zmmtreord[] .
  SELECT *
    INTO TABLE gt_zmmtreord
    FROM zmmtreord
    FOR ALL ENTRIES IN gt_ebeln
WHERE ebeln = gt_ebeln-ebeln
  AND ebelp = gt_ebeln-ebelp .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_POH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_poh .
  CLEAR gt_poh .

  DATA : lt_pol TYPE TABLE OF ty_pol .

  CLEAR lt_pol[] .
  lt_pol[] = gt_pol[] .

  SORT lt_pol BY dlv_planno .
  DELETE ADJACENT DUPLICATES FROM lt_pol
                      COMPARING dlv_planno .

  CHECK lt_pol[] IS NOT INITIAL .

  SELECT *
    INTO TABLE gt_poh
    FROM zmmwms_poh
    FOR ALL ENTRIES IN lt_pol
    WHERE dlv_planno = lt_pol-dlv_planno .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_LFA1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_lfa1 .

  CLEAR gt_lfa1[] .

  DATA : lt_ebeln TYPE TABLE OF ty_ebeln .
  CLEAR lt_ebeln[] .
  lt_ebeln[] = gt_ebeln[] .
  SORT lt_ebeln BY lifnr .
  DELETE ADJACENT DUPLICATES FROM lt_ebeln
                     COMPARING lifnr .
  DELETE lt_ebeln WHERE lifnr = '' .

  CHECK lt_ebeln[] IS NOT INITIAL .

  SELECT
lfa1~lifnr
lfa1~land1
adrc~name1
adrc~name2
adrc~name3
adrc~name4
 INTO TABLE gt_lfa1
  FROM lfa1
  INNER JOIN adrc
 ON adrc~addrnumber = lfa1~adrnr
 FOR ALL ENTRIES IN lt_ebeln
 WHERE lifnr = lt_ebeln-lifnr .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_TEMPL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_templ .
  CLEAR gt_templ[] .

  DATA : lt_pol TYPE TABLE OF ty_pol .
  CLEAR lt_pol[] .
  lt_pol[] = gt_pol[] .

  DELETE lt_pol WHERE origin <> 'X' .

  SORT lt_pol BY guid .
  DELETE ADJACENT DUPLICATES FROM lt_pol
      COMPARING guid .

  CHECK lt_pol[] IS NOT INITIAL .
  SELECT *
    INTO TABLE gt_templ
    FROM zmmaps_temp
    FOR ALL ENTRIES IN lt_pol
    WHERE guid = lt_pol-guid .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_PLL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_pll .

  CLEAR gt_pll[] .
  DATA : lt_pol TYPE TABLE OF ty_pol .
  CLEAR lt_pol[] .
  lt_pol[] = gt_pol[] .

  DELETE lt_pol WHERE origin <> 'X' .

  SORT lt_pol BY guid .
  DELETE ADJACENT DUPLICATES FROM lt_pol
      COMPARING guid .

  CHECK lt_pol[] IS NOT INITIAL .
  SELECT *
    INTO TABLE gt_pll
    FROM zmmaps_pl
    FOR ALL ENTRIES IN lt_pol
    WHERE guid = lt_pol-guid .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_PL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_pl .

  CLEAR gt_pl[] .
  DATA : lt_temp TYPE TABLE OF ty_temp .
  CLEAR lt_temp[] .
  lt_temp[] = gt_temp[] .

  SORT lt_temp BY guid .
  DELETE ADJACENT DUPLICATES FROM lt_temp
      COMPARING guid .

  CHECK lt_temp[] IS NOT INITIAL .
  SELECT *
    INTO TABLE gt_pl
    FROM zmmaps_pl
    FOR ALL ENTRIES IN lt_temp
    WHERE guid = lt_temp-guid .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_T006A
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_t006a .

  CLEAR gt_t006a[] .

  DATA : lt_ekpo TYPE TABLE OF ty_ekpo  .
  CLEAR lt_ekpo[] .
  lt_ekpo[] = gt_ekpo[] .

  SORT lt_ekpo BY meins .
  DELETE ADJACENT DUPLICATES FROM lt_ekpo
                  COMPARING meins .

  SELECT *
    INTO TABLE gt_t006a
    FROM t006a
    FOR ALL ENTRIES IN lt_ekpo
    WHERE spras = sy-langu
      AND msehi = lt_ekpo-meins .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_MARC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_marc .

  CLEAR gt_marc[] .

  DATA : lt_ekpo TYPE TABLE OF ty_ekpo .
  CLEAR lt_ekpo[] .
  lt_ekpo[] = gt_ekpo[] .

  SORT lt_ekpo BY matnr werks.
  DELETE ADJACENT DUPLICATES FROM lt_ekpo
                  COMPARING matnr werks.

  CHECK lt_ekpo[] IS NOT INITIAL .

  SELECT
    matnr
    werks
    bstmi
    plifz
    INTO TABLE gt_marc
    FROM marc
    FOR ALL ENTRIES IN lt_ekpo
    WHERE matnr = lt_ekpo-matnr
      AND werks = lt_ekpo-werks .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_DD07L
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_dd07l .

  CLEAR gt_dd07t[] .

  SELECT *
    INTO TABLE gt_dd07t
    FROM dd07t
    WHERE domname IN ('ZZSDB','ZJHLY','ZSLZT','ZGBBS','ZZSDB','ZZSDX')
      AND ddlanguage = sy-langu .

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

  PERFORM sub_deal_data_pol .
  PERFORM sub_deal_data_temp .
  PERFORM sub_deal_data_znb9 .
  SORT gt_alv BY ebeln ebelp .
  IF gt_alv[] IS INITIAL .
    MESSAGE '没有符合条件的数据' TYPE 'S' DISPLAY LIKE 'E' .
    LEAVE LIST-PROCESSING .
  ENDIF .
  PERFORM sub_build_header.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_DEAL_DATA_POL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_deal_data_pol .

  DATA : lv_lines TYPE i .
  DATA : ls_alv TYPE ty_alv .

  SORT gt_zmatdoc BY dlv_planno  dlv_itemno  shkzg DESCENDING budat ASCENDING .
  SORT gt_ekpo BY ebeln ebelp .
  SORT gt_templ BY guid .
  SORT gt_pll BY guid .
  SORT gt_poh BY dlv_planno .
  SORT gt_marc BY matnr werks .
  SORT gt_lfa1 BY lifnr .
  SORT gt_makt BY matnr .
  CLEAR gs_pol .
  LOOP AT gt_pol INTO gs_pol .

    CLEAR gs_alv .
    IF gs_pol-wms_sts <> 'I' .
      gs_alv-dlv_planno = gs_pol-dlv_planno .
      gs_alv-dlv_itemno = gs_pol-dlv_itemno .
      gs_alv-pdlv_date = gs_pol-pdlv_date  .
    ENDIF .
    gs_alv-pdlv_menge = gs_pol-pdlv_menge   .
    gs_alv-werks = gs_pol-werks .
    gs_alv-ebeln = gs_pol-ebeln .
    gs_alv-ebelp = gs_pol-ebelp .
    gs_alv-lifnr = gs_pol-lifnr  .
    gs_alv-matnr = gs_pol-matnr   .
    gs_alv-producer = gs_pol-producer   .
    gs_alv-xclosed  = gs_pol-xclosed   .
    gs_alv-ernam  = gs_pol-ernam   .
    gs_alv-ekgrp  = gs_pol-ekgrp   .
    gs_alv-st_code  = gs_pol-st_code   .
    gs_alv-pdlv_date_ext = gs_pol-pdlv_date_ext ."调整后答复送货日期
    gs_alv-aedat = gs_pol-aedat.
    CLEAR gs_ekpo .
    READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_alv-ebeln
                                             ebelp = gs_alv-ebelp
                                             BINARY SEARCH .
*    GS_ALV-MAKTX = GS_EKPO-TXZ01   .
    gs_alv-lgort = gs_ekpo-lgort   .
    gs_alv-menge = gs_ekpo-menge   .
    gs_alv-meins = gs_ekpo-meins ."单位
    gs_alv-mfrpn  = gs_ekpo-mfrpn   .
*    CLEAR gs_t006a .
*    READ TABLE gt_t006a INTO gs_t006a WITH  KEY MSEHI = GS_ALV-MEINS .
    gs_alv-uebto = gs_ekpo-uebto ."容差率
    gs_alv-eindt = gs_ekpo-eindt ."原订单到货时间

    READ TABLE gt_marc INTO gs_marc WITH KEY matnr = gs_ekpo-matnr.
    gs_alv-plifz = gs_marc-plifz.
    IF gs_pol-origin = 'X' .
      CLEAR gs_templ .
      READ TABLE gt_templ INTO gs_templ WITH KEY guid = gs_pol-guid BINARY SEARCH  .
      gs_alv-aps_req_date = gs_templ-reqdate ."APS需求日期
*      GS_ALV-PDLV_DATE_EXT = GS_TEMPL-PDLV_DATE_EXT ."调整后答复送货日期
      gs_alv-version = gs_templ-version ."交付日期（版本号）
      CLEAR gs_pll .
      READ TABLE gt_pll INTO gs_pll WITH KEY guid = gs_pol-guid BINARY SEARCH
                                              .
      gs_alv-apsreq_menge = gs_pll-aps_menge ."APS需求数量
    ELSEIF gs_pol-origin = 'M' AND gs_pol-wms_sts <> 'I' .
      gs_alv-aps_req_date = gs_ekpo-eindt ."APS需求日期
      gs_alv-apsreq_menge = gs_ekpo-menge ."APS需求数量
    ELSEIF gs_pol-wms_sts = 'I' .
      gs_alv-aps_req_date  = gs_templ-pdlv_date .
      gs_alv-apsreq_menge = gs_pol-pdlv_menge .
    ENDIF .
    IF gs_pol-pdlv_date_ext IS INITIAL   "唯一的收货答复日期
      OR gs_pol-pdlv_date_ext = '' .
      gs_alv-zwysh = gs_pol-pdlv_date .
    ELSE .
      gs_alv-zwysh = gs_pol-pdlv_date_ext .
    ENDIF .

*SELECT-OPTIONS : S_DATE FOR ZMMWMS_POL-PDLV_DATE .计划收货日期
    CHECK gs_alv-zwysh IN s_date .

*SELECT-OPTIONS : S_ZSLZT FOR ZMMS0002-ZSLZT  ."状态  **************\
    IF gs_pol-wms_sts = 'I' .
      gs_alv-zslzt = '2' ."待确认
    ELSE .
      gs_alv-zslzt = '1' ."已下传
    ENDIF .
    CHECK gs_alv-zslzt IN s_zslzt .

*PARAMETERS : P_ZJHLY TYPE ZJHLY AS LISTBOX VISIBLE LENGTH 20 ."计划来源******
    CLEAR gs_poh .
    READ TABLE gt_poh INTO gs_poh WITH KEY dlv_planno = gs_alv-dlv_planno BINARY SEARCH .
    gs_alv-note = gs_poh-note .
    IF gs_pol-origin = 'X' .
      gs_alv-zjhly = '1' ."按APS
    ELSEIF gs_pol-origin = 'M' .
      IF gs_poh-note CS '退货补货' .
        gs_alv-zjhly = '2' ."退货补货
      ELSEIF gs_poh-note CS '冲销补入' .
        gs_alv-zjhly = '4' ."冲销补货
      ELSE .
        gs_alv-zjhly = '3' ."按单
      ENDIF .
    ENDIF .
    IF p_zjhly <> '' .
      CHECK gs_alv-zjhly = p_zjhly  .
    ENDIF .


    CLEAR lv_lines .
    CLEAR gs_zmatdoc .
    LOOP AT gt_zmatdoc INTO gs_zmatdoc WHERE dlv_planno = gs_alv-dlv_planno
                                         AND dlv_itemno = gs_alv-dlv_itemno .

      CASE gs_zmatdoc-shkzg ."ZMATDOC上的借贷标识
        WHEN 'S' .
          lv_lines = lv_lines + 1 .
*          GS_ALV-RECV_MENGE = GS_ALV-RECV_MENGE + GS_ZMATDOC-MENGE ."净收货量
          IF lv_lines = 1 .
            gs_alv-budat = gs_zmatdoc-budat ."实际到货日期
          ENDIF .
          gs_alv-zzhsh = gs_zmatdoc-cpudt ."最后收货日期 已经按照升序排列 每次运行到最后获取到最大 计算用
          gs_alv-zzhsx = gs_zmatdoc-cpudt ."最后收货日期 已经按照升序排列 每次运行到最后获取到最大 显示用
        WHEN OTHERS ."'H' .
*          GS_ALV-RECV_MENGE = GS_ALV-RECV_MENGE - GS_ZMATDOC-MENGE ."净收货量
          gs_alv-zrequ = gs_alv-zrequ + gs_zmatdoc-menge ."已退数量
      ENDCASE .

      CASE gs_zmatdoc-shkzm ."MATDOC上的借贷标识
        WHEN 'S' .
          gs_alv-recv_menge = gs_alv-recv_menge + gs_zmatdoc-menge ."净收货量
        WHEN OTHERS ."'H' .
*          GS_ALV-RECV_MENGE = GS_ALV-RECV_MENGE - GS_ZMATDOC-MENGE ."净收货量
      ENDCASE .

    ENDLOOP .
    IF gs_alv-zzhsh IS INITIAL .
      gs_alv-zzhsh = sy-datum .
    ENDIF .
    IF gs_pol-wms_sts = 'I' .
      CLEAR gs_alv-budat .
      CLEAR gs_alv-zzhsh .
      CLEAR  gs_alv-recv_menge .
    ENDIF .

    gs_alv-po_num = lv_lines ."本单到货次数
    IF gs_pol-wms_sts <> 'I' .
      IF gs_alv-xclosed <> ''
        OR gs_alv-pdlv_menge <= gs_alv-recv_menge .
        gs_alv-zclosed = 'X' .
      ENDIF .
    ELSE ."为 I时不考虑数量
      IF gs_alv-xclosed <> ''.
        gs_alv-zclosed = 'X' .
      ENDIF .
    ENDIF .
    IF gs_alv-zclosed = 'X' .
      gs_alv-zslzt = '4' ."已关闭
      CHECK gs_alv-zslzt IN s_zslzt .
    ENDIF .
    CASE p_zgbbs .
      WHEN '1' .
        CHECK gs_alv-zclosed = '' .
      WHEN '2' .
        CHECK gs_alv-zclosed = 'X' .
      WHEN OTHERS .
    ENDCASE .

    IF gs_alv-zclosed = 'X' .
      gs_alv-zslzt = '4' ."已下传
      CHECK gs_alv-zslzt IN s_zslzt .
    ENDIF .

*    如果状态为关闭，且收货数量为0，则为空
    IF gs_alv-zslzt = '4'
    AND gs_alv-recv_menge = 0 .
      CLEAR gs_alv-pdlv_menge  .
      CLEAR gs_alv-pdlv_date  .
    ENDIF .

*PARAMETERS : P_DAYS TYPE I  ."预计到货天数***********************
    IF gs_pol-wms_sts <> 'I' .
      gs_alv-zyjdh = gs_alv-zwysh  - sy-datum .
    ENDIF .
    IF  gs_alv-recv_menge >= gs_alv-pdlv_menge."8.  预计到货天数，当行项目已收数量≥答复数量时，显示为空
      gs_alv-zyjdh = 0 .
    ENDIF .
    "关闭
    IF gs_alv-zclosed = 'X' .
      CLEAR  gs_alv-zyjdh .
    ENDIF .
    IF gs_alv-zyjdh < 0 .
      gs_alv-zyjdh = 0 .
    ENDIF .
    IF p_days <> 0 .
      CHECK gs_alv-zyjdh <= p_days .
    ENDIF .


    "读取ZCLOSED若为空，此值显示0，若不为空，答复送货数量-净收货量
    IF gs_alv-zclosed = ''
      AND gs_pol-wms_sts <> 'I'.
      gs_alv-undlv_mende = gs_alv-pdlv_menge - gs_alv-recv_menge .
    ENDIF .

    "最大可收货数量
    gs_alv-zdkss = gs_alv-pdlv_menge * ( 1 + gs_alv-uebto / 100 ) .


*    "已退数量
*    CLEAR GS_ZMMTREORD .
*    LOOP AT GT_ZMMTREORD INTO GS_ZMMTREORD WHERE DLV_PLANNO = GS_ALV-DLV_PLANNO
*                                              AND DLV_PLANNO = GS_ALV-DLV_PLANNO  .
*      GS_ALV-ZREQU = GS_ALV-ZREQU + GS_ZMMTREORD-ZREQU .
*    ENDLOOP .

*PARAMETERS : P_ZZSDB TYPE ZZSDB AS LISTBOX VISIBLE LENGTH 20 ."指示灯  *************
*PARAMETERS : P_ZZSDX TYPE ZZSDX AS LISTBOX VISIBLE LENGTH 20 ."指示灯信息**************

    PERFORM sub_get_light USING gs_alv gs_pol-wms_sts ."
    IF p_zzsdb <> '' .
      CHECK gs_alv-zzsdb = p_zzsdb .
    ENDIF .
    IF p_zzsdx <> '' .
      CHECK gs_alv-zzsdx = p_zzsdx .
    ENDIF .


    "未到货延期天数
    IF gs_alv-zclosed = '' AND  gs_pol-wms_sts <> 'I' .
      gs_alv-delay = sy-datum  - gs_alv-zwysh ."GS_POL-PDLV_DATE_EXT  .
    ENDIF .
    IF gs_pol-wms_sts = 'I' .
      gs_alv-delay = 0 .
    ENDIF .
    IF gs_alv-light = '2' .
      gs_alv-delay = 0 ."黄灯的，都是0
    ENDIF .
    IF gs_alv-delay < 0 .
      gs_alv-delay = 0 .
    ENDIF .

    "迟到天数
    IF  gs_alv-pdlv_date_ext = ''.
      CLEAR gs_alv-pdlv_date_ext.
    ENDIF.
    IF gs_alv-pdlv_date_ext IS NOT INITIAL.
      gs_alv-delaydate = gs_alv-pdlv_date_ext - gs_alv-budat.
    ELSE.
      gs_alv-delaydate = gs_alv-pdlv_date - gs_alv-budat.
    ENDIF.
    IF gs_alv-pdlv_date_ext IS INITIAL AND gs_alv-pdlv_date IS INITIAL OR gs_alv-budat IS INITIAL.
      CLEAR gs_alv-delaydate.
    ENDIF.

    "订单级别
    IF gs_alv-pdlv_date_ext IS NOT INITIAL.
      IF ( gs_alv-pdlv_date_ext - gs_alv-aedat ) >= gs_alv-plifz.
        gs_alv-polevel = '正常订单'.
      ELSE.
        gs_alv-polevel = '紧急订单'.
      ENDIF.
    ELSE.
      IF ( gs_alv-pdlv_date - gs_alv-aedat ) >= gs_alv-plifz.
        gs_alv-polevel = '正常订单'.
      ELSE.
        gs_alv-polevel = '紧急订单'.
      ENDIF.
    ENDIF.

    PERFORM sub_edi_public ."pol temp 公共处理部分


    APPEND gs_alv TO gt_alv .

  ENDLOOP .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_CLASS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_ALV
*&---------------------------------------------------------------------*
FORM sub_get_class  USING    ps_alv TYPE ty_alv .

  "批次特性通用返回类型表 num型 char型 curr型
  DATA: lt_num    TYPE TABLE OF bapi1003_alloc_values_num,
        lt_char   TYPE TABLE OF bapi1003_alloc_values_char,
        lt_curr   TYPE TABLE OF bapi1003_alloc_values_curr,
*        LT_LIST   TYPE TABLE OF BAPI1003_ALLOC_LIST,
        lt_list   TYPE TABLE OF bapi1003_alloc_list,
        lt_return TYPE TABLE OF bapiret2.
  DATA : ls_char   TYPE bapi1003_alloc_values_char .

  DATA : lv_object      LIKE  bapi1003_key-object,
         lv_objecttable LIKE  bapi1003_key-objecttable VALUE 'MARA',
         lv_classtype   LIKE  bapi1003_key-classtype   VALUE '001',
         ls_list        LIKE  bapi1003_alloc_list,
         lv_classnum    LIKE  bapi1003_key-classnum.

  CLEAR :lt_num, lt_char,lt_curr,lt_return,lv_object.
  lv_object = ps_alv-matnr.

  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp   = lv_object
      objecttable_imp = lv_objecttable
      classtype_imp   = lv_classtype
*     READ_VALUATIONS =
      keydate         = sy-datum
      language        = sy-langu
*     OBJECTKEY_IMP_LONG       =
    TABLES
      alloclist       = lt_list
      return          = lt_return.

  IF lt_list  IS NOT INITIAL.
    CLEAR: lt_return,ls_list,lv_classnum.
    READ TABLE lt_list INTO ls_list WITH KEY classtype = lv_classtype.
    lv_classnum  =  ls_list-classnum.
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_object
        objecttable     = lv_objecttable
        classnum        = lv_classnum
        classtype       = lv_classtype
*       KEYDATE         = SY-DATUM
*       LANGUAGE        = SY-LANGU
      TABLES
        allocvaluesnum  = lt_num
        allocvalueschar = lt_char
        allocvaluescurr = lt_curr
        return          = lt_return.

    READ TABLE lt_char INTO ls_char WITH KEY charact = 'ZORIADD'."原产地
    IF sy-subrc = 0.
      ps_alv-made_in_china =  ls_char-value_char.
    ENDIF.

    READ TABLE lt_char INTO ls_char WITH KEY charact = 'ZEXPMAT'."贵重标识
    IF sy-subrc = 0.
      ps_alv-xprecious =  ls_char-value_char.
    ENDIF.

    CLEAR:ls_list, ls_char.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_LIGHT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_ALV
*&---------------------------------------------------------------------*
FORM
  sub_get_light  USING    ps_alv TYPE ty_alv
                             p_wms  .

  IF ps_alv-pdlv_date_ext  = '' OR ps_alv-pdlv_date_ext IS INITIAL.
    ps_alv-z_date = ps_alv-pdlv_date.
  ELSE.
    ps_alv-z_date = ps_alv-pdlv_date_ext.
  ENDIF.
  IF p_wms = 'I' .
    "同其他
    IF ps_alv-light = '' .
      ps_alv-light = '' .
      ps_alv-zzsdx = '5' ."
    ENDIF .
  ELSE .
**如果最后收货日期大于答复日期（含调整日期），且已收货数量小于答复数量，则指示灯信息为“逾期未齐备到货”，
**类型为“红灯”
*    IF PS_ALV-ZZHSH > PS_ALV-ZWYSH
*      AND PS_ALV-RECV_MENGE < PS_ALV-PDLV_MENGE .
*      PS_ALV-LIGHT = '1' .
*      PS_ALV-ZZSDX = '1' ."逾期未齐备到货
*    ENDIF .
*
**如果最后收货日期大于答复日期（含调整日期），且已收货数量大于等于答复数量，则指示灯信息为“逾期齐备到货”，
**类型为“红灯”
*    IF   PS_ALV-ZZHSH > PS_ALV-ZWYSH
*      AND  PS_ALV-RECV_MENGE >= PS_ALV-PDLV_MENGE.
*      PS_ALV-LIGHT = '1' .
*      PS_ALV-ZZSDX = '2' ."逾期齐备到货
*    ENDIF .
*
**如果最后收货日期等于答复日期（含调整日期）且最后收货日期小于系统日期且已收货数量大于0且已收货数量小于
**答复数量，则指示灯信息为“逾期未齐备到货” ，类型为“红灯”
*    IF   PS_ALV-ZZHSH = PS_ALV-ZWYSH
*      AND PS_ALV-ZZHSH < SY-DATUM
*      AND PS_ALV-RECV_MENGE > 0
*      AND  PS_ALV-RECV_MENGE < PS_ALV-PDLV_MENGE .
*      PS_ALV-LIGHT = '1' .
*      PS_ALV-ZZSDX = '1' ."逾期未齐备到货
*    ENDIF .
*
**如果答复日期（含调整日期）等于系统日期且已收货数量等于0，则指示灯信息为“当天未到货”，类型为“黄灯”
*    IF    GS_ALV-ZWYSH = SY-DATUM
*      AND PS_ALV-RECV_MENGE = 0 .
**    AND PS_ALV-PDLV_MENGE > PS_ALV-RECV_MENGE .
*      PS_ALV-LIGHT = '2' .
*      PS_ALV-ZZSDX = '3' ."当天未到货
*    ENDIF .
*
**如果最后收货日期等于答复日期（含调整日期）且最后收货日期等于系统日期且已收货数量大
**于0且已收货数量小于答复数量，则类型为“黄灯”
*    IF   PS_ALV-ZZHSH = GS_ALV-ZWYSH
*      AND PS_ALV-ZZHSH = SY-DATUM
*      AND PS_ALV-RECV_MENGE > 0
*      AND  PS_ALV-RECV_MENGE < PS_ALV-PDLV_MENGE .
*      PS_ALV-LIGHT = '2' .
*      PS_ALV-ZZSDX = '3' ."当天未到货
*    ENDIF .
*
**11.  收货数量<答复数量且系统日期>答复日期的 ，也应该显示红灯，逾期未齐备到货，
**即提前到货但剩下的都会迟到或缺料了。
*    IF PS_ALV-RECV_MENGE <  PS_ALV-PDLV_MENGE
*    AND SY-DATUM > GS_ALV-ZWYSH .
*      PS_ALV-LIGHT = '1' .
*      PS_ALV-ZZSDX = '1' ."逾期未齐备到货
*    ENDIF .
*
*
*    "除以上情况之外
*    IF  PS_ALV-LIGHT = '' .
*
**如果已收货数量大于答复数量，则指示灯信息为“及时齐备到货”，则类型为“绿灯”，
*      IF PS_ALV-RECV_MENGE >= PS_ALV-PDLV_MENGE .
*        PS_ALV-LIGHT = '3' .
*        PS_ALV-ZZSDX = '4' ."
*      ELSE ."其他，则指示灯信息为“预计到货”，则类型为“无颜色”，不亮灯
*        PS_ALV-LIGHT = '' .
*        PS_ALV-ZZSDX = '5' ."
*      ENDIF .
*
*    ENDIF .

*如果最后收货日期大于答复日期（如有调整日期以调整日期为准），且
*已收货数量小于答复数量，则指示灯信息为“逾期未齐备到货”，
*类型为“红灯”，显示红灯'@0A@'icon_led_red；
    IF ps_alv-budat > ps_alv-z_date
      AND ps_alv-recv_menge < ps_alv-pdlv_menge .
      ps_alv-light = '1' .
      ps_alv-zzsdx = '1' ."逾期未齐备到货
    ENDIF .

*如果系统日期大于答复日期（如有调整日期以调整日期为准），且已收货数量小于答复数量，
*则指示灯信息为“逾期未齐备到货”，
*且未关闭
*类型为“红灯”，显示红灯'@0A@'icon_led_red；
*    IF sy-datum > ps_alv-zwysh
*     AND ps_alv-recv_menge < ps_alv-pdlv_menge
*     AND ps_alv-zslzt <> '4' .
*      ps_alv-light = '1' .
*      ps_alv-zzsdx = '1' ."逾期未齐备到货
*    ENDIF .

*如果最后收货日期大于答复日期（如有调整日期以调整日期为准），
*且（已收货数量大于等于答复数量或状态为关闭），则指示灯信息为“逾期齐备到货”，
*类型为“红灯”，显示红灯'@0A@'icon_led_red；
    IF ps_alv-budat > ps_alv-z_date
      AND ( ps_alv-recv_menge >= ps_alv-pdlv_menge
      OR  ps_alv-zslzt = '4' ).
      ps_alv-light = '1' .
      ps_alv-zzsdx = '2' ."逾期齐备到货
    ENDIF .

*如果最后收货日期等于答复日期（如有调整日期以调整日期为准）
*且最后收货日期小于系统日期且已收货数量大于0且已收货数量
*小于答复数量，则指示灯信息为“逾期未齐备到货” ，
*类型为“红灯”，显示红灯'@0A@'icon_led_red；
*    IF ps_alv-budat = ps_alv-z_date
*    AND ps_alv-zwysh < sy-datum
*      AND ps_alv-recv_menge > 0
*     AND ps_alv-recv_menge < ps_alv-pdlv_menge .
*      ps_alv-light = '2' .
*      ps_alv-zzsdx = '7' ."逾期未齐备到货
*    ENDIF .

*如果第一次收货日期小于等于答复日期（如有调整日期以调整日期为准）
*且已收货数量大于0且已收货数量小于答复数量，则指示灯信息为“及时未齐备到货”
*，类型为“黄灯”，显示红灯'@0A@'icon_led_red；
    IF ps_alv-budat <= ps_alv-z_date
      AND ps_alv-recv_menge > 0
     AND ps_alv-recv_menge < ps_alv-pdlv_menge .
      ps_alv-light = '2' .
      ps_alv-zzsdx = '7' ."及时未齐备到货
    ENDIF .

*如果答复日期（如有调整日期以调整日期为准）等于系统日期且状
*态为未关闭，则指示灯信息为“当天未到货”，类型为“黄灯”，显示为黄灯
    IF ps_alv-z_date = sy-datum
    AND ps_alv-zslzt <> '4' .
      ps_alv-light = ' ' .
      ps_alv-zzsdx = '3' ."当天未到货
    ENDIF .

*如果最后收货日期等于答复日期（如有调整日期以调整日期为准）
*且最后收货日期等于系统日期且且状态为未关闭
*（已收货数量大于0）
*，则指示灯信息为
*“当天应到未齐备”，则类型为“黄灯”,显示为黄灯'@09@'icon_led_yellow；
*    IF ps_alv-z_date = ps_alv-budat
*    AND ps_alv-z_date = sy-datum
*    AND ps_alv-zslzt = '4'.
*      ps_alv-light = '' .
*      ps_alv-zzsdx = '6' ."当天未到货
*    ENDIF .

*除以上情况之外，如果（已收货数量大于答复数量或已关闭），
*则指示灯信息为“及时齐备到货”，则类型为“绿灯”，显示绿灯'@08@' icon_led_green；
    IF ps_alv-light = ''
     AND ( ps_alv-recv_menge >= ps_alv-pdlv_menge ).
*      OR ps_alv-zslzt = '4' ) .
      ps_alv-light = '3' .
      ps_alv-zzsdx = '4' ."及时齐备到货
    ENDIF .

    "其他
    IF ps_alv-light = '' .
      ps_alv-light = '' .
      ps_alv-zzsdx = '5' ."
    ENDIF .

  ENDIF .

*计划来源为冲销补货，且已关闭，则类型为“绿灯”，类型为“及时齐备到货”
  IF ps_alv-zjhly = '4' "冲销补货
    AND ps_alv-zslzt = '4' .
    ps_alv-light = '3' .
    ps_alv-zzsdx = '4' ."
  ENDIF .

*如果收货数量为0且状态为已关闭，，则类型为“无颜色”，不亮灯，类型为“预计到货”
  IF ps_alv-recv_menge = 0
 AND gs_alv-zslzt = '4' .
    ps_alv-light = '' .
    ps_alv-zzsdx = '5' ."
  ENDIF .



********************************8
**黄  当天未到货 1.判断是否满足黄灯  未关闭且答复日期=系统日期且行收货数量=0最后一次收货日期不等于系统日期
*  IF GS_ALV-ZSLZT <> '4'
*  AND GS_ALV-ZWYSH = SY-DATUM
*  AND GS_ALV-ZZHSH <> SY-DATUM .
*    PS_ALV-LIGHT = '2' .
*    PS_ALV-ZZSDX = '1' ."
*  ENDIF .
** 当天应到未齐备   未关闭且答复日期=系统日期=最后一次收货日期且行收货数量<行答复数量
*  IF PS_ALV-ZSLZT <> '4'
*  AND PS_ALV-ZWYSH = SY-DATUM
*  AND PS_ALV-ZZHSH = SY-DATUM
*  AND PS_ALV-RECV_MENGE < PS_ALV-PDLV_MENGE .
*    PS_ALV-LIGHT = '2' .
*    PS_ALV-ZZSDX = '6' ."
*  ENDIF .
*
**红 逾期未齐备到货 2.不满足黄灯的情况下，判断是否满足红灯  未关闭且答复日期<系统日期且行收货数量=0
**      答复日期<最后一次收货日期且行收货数量<行答复数量
**  逾期齐备到货    已关闭且答复日期<最后一次收货日期且行收货数量≥行答复数量
*  IF PS_ALV-LIGHT <> '2' .
*    IF PS_ALV-ZSLZT <> '4'
*    AND PS_ALV-ZWYSH < SY-DATUM
*    AND  PS_ALV-RECV_MENGE = 0 .
*      PS_ALV-LIGHT = '1' .
*      PS_ALV-ZZSDX = '1' ."
*    ENDIF .
*
*    IF PS_ALV-ZSLZT <> '4'
*    AND PS_ALV-ZWYSH < GS_ALV-ZZHSH
*    AND  PS_ALV-RECV_MENGE < PS_ALV-PDLV_MENGE  .
*      PS_ALV-LIGHT = '1' .
*      PS_ALV-ZZSDX = '1' ."
*    ENDIF .
*
*    IF PS_ALV-ZSLZT = '4'
*    AND PS_ALV-ZWYSH < GS_ALV-ZZHSH
*    AND  PS_ALV-RECV_MENGE >= PS_ALV-PDLV_MENGE  .
*      PS_ALV-LIGHT = '1' .
*      PS_ALV-ZZSDX = '2' ."
*    ENDIF .
*
*  ENDIF .
*
**绿  及时齐备到货  3.不满足黄灯且不满足红灯的情况下，判断是否满足绿灯  已关闭且答复日期≥最后一次收货日期且行收货数量＞0
*  IF PS_ALV-LIGHT <> '2'
*    AND PS_ALV-LIGHT <> '1'  .
*
*    IF PS_ALV-ZSLZT = '4'
*      AND PS_ALV-ZWYSH >= GS_ALV-ZZHSH
*      AND PS_ALV-RECV_MENGE > 0 .
*
*      PS_ALV-LIGHT = '3' .
*      PS_ALV-ZZSDX = '4' ."
*
*    ENDIF .
*
*  ENDIF .
*
**无  预计到货  4.不满足红灯、黄灯、绿灯的其他情况  答复日期>系统日期且行收货数量=0
**     答复日期>最后一次收货日期且行收货数量<行答复数量
**     收货数量＝0,已关闭
*  IF PS_ALV-LIGHT = '' .
*    IF PS_ALV-ZWYSH > SY-DATUM
*      AND PS_ALV-RECV_MENGE > 0 .
*      PS_ALV-ZZSDX = '5' ."
*    ENDIF .
*
*    IF PS_ALV-ZWYSH > GS_ALV-ZZHSH
*      AND PS_ALV-RECV_MENGE < PS_ALV-PDLV_MENGE .
*      PS_ALV-ZZSDX = '5' ."
*    ENDIF .
*
*    IF PS_ALV-RECV_MENGE = 0
*      AND PS_ALV-ZSLZT = '4' .
*      PS_ALV-ZZSDX = '5' ."
*    ENDIF .
*
*  ENDIF .


  CASE ps_alv-light .
    WHEN '1' .
      ps_alv-zzsdb = 'A' .
    WHEN '2' .
      ps_alv-zzsdb = 'B' .
    WHEN '3' .
      ps_alv-zzsdb = 'C' .
    WHEN OTHERS  .
      ps_alv-zzsdb = 'D' .
  ENDCASE .



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_EDI_DDO7T
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> GS_ALV_ZZSDB
*&      --> GS_ALV_ZZSDM
*&---------------------------------------------------------------------*
FORM sub_edi_ddo7t  USING   p_type
                             p_zzsdb
                             p_zzsdm.

  CLEAR gs_dd07t .
  READ TABLE gt_dd07t INTO gs_dd07t WITH KEY domname = p_type
                                             domvalue_l = p_zzsdb  .
  p_zzsdm = gs_dd07t-ddtext .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_SET_DDO7T
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_ALV
*&---------------------------------------------------------------------*
FORM sub_set_ddo7t  USING ps_alv TYPE ty_alv .

*  SELECT *
*    INTO TABLE GT_DD07T
*    FROM DD07T
*    WHERE DOMNAME IN ('ZZSDB','','','','','')
*      AND DDLANGUAGE = SY-LANGU .
  PERFORM sub_edi_ddo7t USING 'ZZSDB'
                               gs_alv-zzsdb
                               gs_alv-zzsdm .

  PERFORM sub_edi_ddo7t USING 'ZJHLY'
                               gs_alv-zjhly
                               gs_alv-zjhlm .

  PERFORM sub_edi_ddo7t USING 'ZSLZT'
                               gs_alv-zslzt
                               gs_alv-zslzm .

*  PERFORM SUB_EDI_DDO7T USING 'ZGBBS'
*                               GS_ALV-ZGBBS
*                               GS_ALV-ZGBBM .

  PERFORM sub_edi_ddo7t USING 'ZZSDB'
                               gs_alv-zzsdb
                               gs_alv-zzsdm .

  PERFORM sub_edi_ddo7t USING 'ZZSDX'
                               gs_alv-zzsdx
                               gs_alv-zzsdt .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_DEAL_DATA_TEMP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_deal_data_temp .

  DATA : lv_lines TYPE i .
  DATA : ls_alv TYPE ty_alv .

  SORT gt_zmatdoc BY dlv_planno  dlv_itemno  shkzg DESCENDING budat ASCENDING .
  SORT gt_ekpo BY ebeln ebelp .
  SORT gt_pl BY guid .

  SORT gt_marc BY matnr werks .
  SORT gt_lfa1 BY lifnr .
  SORT gt_makt BY matnr .
  CLEAR gs_temp .
  LOOP AT gt_temp INTO gs_temp.

    CLEAR gs_alv .
    gs_alv-werks = gs_temp-werks .
    gs_alv-ebeln = gs_temp-ebeln .
    gs_alv-ebelp = gs_temp-ebelp .
    gs_alv-lifnr = gs_temp-lifnr  .
    gs_alv-pdlv_date = gs_temp-pdlv_date  .
    gs_alv-matnr = gs_temp-matnr   .
*    GS_ALV-PRODUCER = GS_POL-PRODUCER   .
    gs_alv-aps_req_date = gs_temp-reqdate ."APS需求日期
    gs_alv-pdlv_menge = gs_temp-assign_menge   .
    gs_alv-ernam  = gs_temp-ernam   .
    gs_alv-ekgrp  = gs_temp-ekgrp   .
    gs_alv-st_code  = gs_temp-st_code   .
    gs_alv-version = gs_temp-version ."交付日期（版本号）
    gs_alv-aedat = gs_temp-aedat.
    gs_alv-remark = gs_temp-zbeiz.
    READ TABLE gt_marc INTO gs_marc WITH KEY matnr = gs_temp-matnr.
    gs_alv-plifz = gs_marc-plifz.
*SELECT-OPTIONS : S_DATE FOR ZMMWMS_POL-PDLV_DATE .计划收货日期
    CHECK gs_alv-zwysh IN s_date .

*PARAMETERS : P_ZZSDB TYPE ZZSDB AS LISTBOX VISIBLE LENGTH 20 ."指示灯  *************
*PARAMETERS : P_ZZSDX TYPE ZZSDX AS LISTBOX VISIBLE LENGTH 20 ."指示灯信息**************
*    PERFORM SUB_GET_LIGHT USING GS_ALV 'I' ."
    gs_alv-light = '' .
    gs_alv-zzsdx = '5' ."
    IF p_zzsdb <> '' .
      CHECK gs_alv-zzsdb = p_zzsdb .
    ENDIF .
    IF p_zzsdx <> '' .
      CHECK gs_alv-zzsdx = p_zzsdx .
    ENDIF .


*SELECT-OPTIONS : S_ZSLZT FOR ZMMS0002-ZSLZT  ."状态  **************\
    IF gs_temp-pdlv_date IS INITIAL  .
      gs_alv-zslzt = '2' ."待确认
    ELSE .
      gs_alv-zslzt = '3' ."待下传
    ENDIF .

    CHECK gs_alv-zslzt IN s_zslzt .

*PARAMETERS : P_ZJHLY TYPE ZJHLY AS LISTBOX VISIBLE LENGTH 20 ."计划来源*******
    gs_alv-zjhly = '1' ."按APS
    IF p_zjhly <> '' .
      CHECK gs_alv-zjhly = p_zjhly  .
    ENDIF .


    gs_alv-xclosed  = gs_temp-inactive   .
    gs_alv-zclosed = gs_alv-xclosed .
    CASE p_zgbbs .
      WHEN '1' .
        CHECK gs_alv-zclosed = '' .
      WHEN '2' .
        CHECK gs_alv-zclosed = 'X' .
      WHEN OTHERS .
    ENDCASE .

    IF gs_alv-zclosed = 'X' .
      gs_alv-zslzt = '4' ."已下传
      CHECK gs_alv-zslzt IN s_zslzt .
    ENDIF .

*    如果状态为关闭，且收货数量为0，则为空
    IF gs_alv-zslzt = '4'
    AND gs_alv-recv_menge = 0 .
      CLEAR gs_alv-pdlv_menge  .
      CLEAR gs_alv-pdlv_date  .
    ENDIF .

*    "读取ZCLOSED若为空，此值显示0，若不为空，答复送货数量-净收货量
*    IF GS_ALV-ZCLOSED = '' .
*      GS_ALV-UNDLV_MENDE = GS_ALV-PDLV_MENGE - GS_ALV-RECV_MENGE .
*    ENDIF .


    CLEAR gs_ekpo .
    READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_alv-ebeln
                                             ebelp = gs_alv-ebelp BINARY SEARCH.
*    GS_ALV-MAKTX = GS_EKPO-TXZ01   .
    gs_alv-lgort = gs_ekpo-lgort   .
    gs_alv-menge = gs_ekpo-menge   .
    gs_alv-meins = gs_ekpo-meins ."单位
    gs_alv-mfrpn  = gs_ekpo-mfrpn   .
*    CLEAR gs_t006a .
*    READ TABLE gt_t006a INTO gs_t006a WITH  KEY MSEHI = GS_ALV-MEINS .
    gs_alv-uebto = gs_ekpo-uebto ."容差率
    gs_alv-eindt = gs_ekpo-eindt ."原订单到货时间
    gs_alv-producer = gs_ekpo-mfrnr ."制造商


    CLEAR gs_pll .
    READ TABLE gt_pl INTO gs_pl WITH KEY guid = gs_temp-guid BINARY SEARCH.
    gs_alv-apsreq_menge = gs_pl-aps_menge ."APS需求数量

    IF gs_alv-zzhsh IS INITIAL .
      gs_alv-zzhsh = sy-datum .
    ENDIF .

    "最大可收货数量
    gs_alv-zdkss = gs_alv-pdlv_menge * ( 1 + gs_alv-uebto / 100 ) .


    PERFORM sub_edi_public ."pol temp 公共处理部分

    APPEND gs_alv TO gt_alv .

  ENDLOOP .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_DEAL_DATA_ZNB9
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_deal_data_znb9 .

  SORT gt_ekpo BY ebeln ebelp .

  SORT gt_marc BY matnr werks .
  SORT gt_lfa1 BY lifnr .
  SORT gt_makt BY matnr .

  CLEAR gs_znb9 .
  LOOP AT gt_znb9 INTO gs_znb9 .
    CLEAR gs_alv .

    MOVE-CORRESPONDING gs_znb9 TO gs_alv .

    gs_alv-producer = gs_znb9-mfrnr ."制造商
    gs_alv-apsreq_menge = gs_znb9-menge .
    gs_alv-pdlv_menge = gs_znb9-menge .
    gs_alv-aedat = gs_znb9-aedat.
    READ TABLE gt_marc INTO gs_marc WITH KEY matnr = gs_znb9-matnr.
    gs_alv-plifz = gs_marc-plifz.
    "PO已收
    CLEAR gs_ekbe .
    LOOP AT gt_ekbe INTO gs_ekbe WHERE ebeln = gs_alv-ebeln
                                   AND ebelp = gs_alv-ebelp .
      CASE gs_ekbe-shkzg .
        WHEN 'S' .
          gs_alv-zpoys = gs_alv-zpoys + gs_ekbe-menge .
        WHEN 'H' .
          gs_alv-zpoys = gs_alv-zpoys - gs_ekbe-menge .
      ENDCASE .
    ENDLOOP .
    DELETE gt_ekbe WHERE ebeln = gs_alv-ebeln
                                   AND ebelp = gs_alv-ebelp .
    gs_alv-pdlv_menge = gs_alv-pdlv_menge - gs_alv-zpoys .
*PARAMETERS : P_ZZSDB TYPE ZZSDB AS LISTBOX VISIBLE LENGTH 20 ."指示灯  *************
*PARAMETERS : P_ZZSDX TYPE ZZSDX AS LISTBOX VISIBLE LENGTH 20 ."指示灯信息**************
    PERFORM sub_get_light USING gs_alv 'I' ."
*ZNB9：采购订单类型为ZNB9，且已生效（MEMORY = ''），且未关闭，未完成，且不存在通过采购订单号，采购订单行号关联的未关闭的POL表数据，指示灯信息为“预计到货”，类型为“无颜色”，不亮灯
*计划来源为冲销补货，且已关闭，则类型为“绿灯”，类型为“及时齐备到货”
*如果收货数量为0且状态为已关闭，，则类型为“无颜色”，不亮灯，类型为“预计到货”
    gs_alv-light = '' .
    gs_alv-zzsdx = '5' ."
    CASE gs_alv-light .
      WHEN '1' .
        gs_alv-zzsdb = 'A' .
      WHEN '2' .
        gs_alv-zzsdb = 'B' .
      WHEN '3' .
        gs_alv-zzsdb = 'C' .
      WHEN OTHERS  .
        gs_alv-zzsdb = 'D' .
    ENDCASE .

    IF p_zzsdb <> '' .
      CHECK gs_alv-zzsdb = p_zzsdb .
    ENDIF .
    IF p_zzsdx <> '' .
      CHECK gs_alv-zzsdx = p_zzsdx .
    ENDIF .

*    IF GS_ZNB9-LOEKZ <> '' .
*      GS_ALV-ZCLOSED = 'X' .
*    ENDIF .

    "关闭标识
    CASE p_zgbbs .
      WHEN '1' .
        CHECK gs_alv-zclosed = '' .
      WHEN '2' .
        CHECK gs_alv-zclosed = 'X' .
      WHEN OTHERS .
    ENDCASE .

**PARAMETERS : P_DAYS TYPE I  ."预计到货天数***********************
*    IF P_DAYS <> 0 .
*      CHECK GS_ALV-ZYJDH <= P_DAYS .
*    ENDIF .

*SELECT-OPTIONS : S_ZSLZT FOR ZMMS0002-ZSLZT  ."状态  **************\
    IF gs_alv-zclosed = 'X' .
      gs_alv-zslzt = '4' ."已关闭
    ELSE .
      gs_alv-zslzt = '3' ."待下传
    ENDIF .

    CHECK gs_alv-zslzt IN s_zslzt .

*    如果状态为关闭，且收货数量为0，则为空
    IF gs_alv-zslzt = '4'
    AND gs_alv-recv_menge = 0 .
      CLEAR gs_alv-pdlv_menge  .
      CLEAR gs_alv-pdlv_date  .
    ENDIF .

*PARAMETERS : P_ZJHLY TYPE ZJHLY AS LISTBOX VISIBLE LENGTH 20 ."计划来源*******
    gs_alv-zjhly = '3' ."按单
    IF p_zjhly <> '' .
      CHECK gs_alv-zjhly = p_zjhly  .
    ENDIF .


    CLEAR gs_ekpo .
    READ TABLE gt_ekpo INTO gs_ekpo WITH KEY ebeln = gs_alv-ebeln
                                             ebelp = gs_alv-ebelp BINARY SEARCH.
    gs_alv-eindt = gs_ekpo-eindt ."原订单到货时间

    PERFORM sub_edi_public ."pol temp 公共处理部分


    APPEND gs_alv TO gt_alv .
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
  mac01 : 'LIGHT' '指示灯' '' ''  ''.
  mac01 : 'ZZSDT' '指示灯信息' '' ''  ''.
  mac01 : 'WERKS' '工厂' '' ''  ''.
  mac01 : 'ZSLZM' '状态' '' ''  ''.
  mac01 : 'ZJHLM' '计划来源' '' ''  ''.
  mac01 : 'DLV_PLANNO' '送料计划号' '' ''  ''.
  mac01 : 'DLV_ITEMNO' '送料计划行' '' ''  ''.
  mac01 : 'EBELN' '采购订单号' '' ''  ''.
  mac01 : 'EBELP' '采购订单行' '' ''  '' .
  mac01 : 'LIFNR' '供应商编号' '' ''  '' .
  mac01 : 'NAME1' '供应商名称' '' ''  '' .
  mac01 : 'PDLV_DATE' '答复送货日期' '' ''  ''.
  mac01 : 'ZYJDH' '预计到货天数' '' ''  ''.
  mac01 : 'MATNR' '物料编号' '' ''  ''.
  mac01 : 'MAKTX' '物料描述' '' ''  ''.
  mac01 : 'MFRPN' 'MPN物料' '' ''  ''.
  mac01 : 'PRODUCER' '制造商编号' '' ''  ''.
  mac01 : 'NAME2' '制造商名称' '' ''  ''.
*  MAC01 : 'BUDAT' '第一次收货日期' '' '' .
  mac01 : 'APS_REQ_DATE' 'APS需求日期' '' ''  ''.
  mac01 : 'PDLV_DATE_EXT' '调整后答复送货日期' '' ''  ''.
*  MAC01 : 'BUDAT' '实际到货日期' '' '' .
  mac01 : 'BUDAT' '首次收货日期' '' ''  ''.
  mac01 : 'ZZHSX' '最后收货日期' '' ''  ''.
  mac01 : 'MEINS' '基本计量单位' '' ''  ''.
  mac01 : 'APSREQ_MENGE' 'APS需求数量' '' ''  ''.
  mac01 : 'PDLV_MENGE' '答复送货数量' '' ''  ''.
  mac01 : 'UEBTO' '容差率' '' '' '' .
  mac01 : 'RECV_MENGE' '净收货量' '' ''  ''.
  mac01 : 'UNDLV_MENDE' '剩余未到货量' '' '' ''.
  mac01 : 'ZDKSS' '最大可收货数量' '' ''  ''.
  mac01 : 'DELAY' '未到货延期天数' '' '' '' .
  mac01 : 'ZYQTS' '延期天数' '' ''  ''.
  mac01 : 'PO_NUM' '本单到货次数' '' ''  ''.
  mac01 : 'BSTMI' '最小订购批量' '' ''  ''.
  mac01 : 'ERNAM' '制单人' '' ''  ''.
  mac01 : 'ZNAME' '制单人名' '' ''  ''.
*  MAC01 : 'MADE_IN_CHINA' '原产地' '' '' .
*  MAC01 : 'XPRECIOUS' '贵重标识' '' '' .
  mac01 : 'ZCLOSED' '关闭标识' '' ''  ''.
  mac01 : 'ZPOYS' 'PO已收' '' ''  ''.
  mac01 : 'ZREQU' '已退数量' '' ''  ''.
  mac01 : 'LGORT' '送货地址' '' ''  ''.
  mac01 : 'VERSION' '交付日期（版本号）' '' '' '' .
  mac01 : 'EINDT' '原订单到货时间' '' '' ''.
  mac01 : 'MENGE' '原订单数量' '' ''  ''.
  mac01 : 'NOTE' '备注' '' ''  ''.
  mac01 : 'EKGRP' '采购组' '' ''  ''.
  mac01 : 'EKNAM' '采购组描述' '' ''  ''.
  mac01 : 'ST_CODE' '特殊标准' '' ''  ''.
  mac01 : 'AEDAT' '单据日期' '' ''  ''.
  mac01 : 'DELAYDATE' '迟到天数' '' ''  ''.
  mac01 : 'PLIFZ' '采购周期' '' '' '' .
  mac01 : 'POLEVEL' '订单级别' '' ''  ''.
  mac01 : 'REMARK' '备注' '' ''  ''.


*-ALV函数
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SUB_STATUS'
      i_callback_user_command  = 'SUB_USER_COMMAND'
      i_callback_top_of_page   = 'SUB_ALV_TOP_OF_PAGE'
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
  SET PF-STATUS 'ZMMR000401' EXCLUDING rt_extab .
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
**      CALL TRANSACTION 'MB52' .
      READ TABLE gt_alv INTO gs_alv INDEX p_rs_selfield-tabindex .
      IF sy-subrc = 0 .
        CASE  p_rs_selfield-fieldname .
          WHEN 'EBELN' OR 'EBELP'.
            CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
              EXPORTING
                i_ebeln              = gs_alv-ebeln
                i_ebelp              = gs_alv-ebelp
              EXCEPTIONS
                not_found            = 1
                no_authority         = 2
                invalid_call         = 3
                preview_not_possible = 4
                OTHERS               = 5.
        ENDCASE .
      ENDIF .

  ENDCASE.

*-同时内表中的数据也随之刷新
  p_rs_selfield-col_stable = 'X' .   " 行
  p_rs_selfield-row_stable = 'X' .   " 列
  p_rs_selfield-refresh    = 'X' .   " 当用户在显式界面上对数据进行修改时，

ENDFORM. " LVC_USER_COMMAND
*&---------------------------------------------------------------------*
*& Form SUB_CHECK_AUTH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_check_auth .

  "调用外部的form ZCA_AUTHORITY_CHECK
  PERFORM frm_check_m_best_wrk TABLES s_werks.
  PERFORM frm_check_m_best_ekg TABLES s_ekgrp.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_M_BEST_WRK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_WERKS  text
*----------------------------------------------------------------------*
FORM frm_check_m_best_wrk TABLES it_werks.

  SELECT werks
    INTO TABLE @DATA(lt_werks)
    FROM t001w
   WHERE werks IN @it_werks.
  IF lt_werks[] IS INITIAL.
    MESSAGE s001(zauthority) DISPLAY LIKE 'E' WITH '工厂'.
    STOP.
  ENDIF.

  LOOP AT lt_werks INTO DATA(ls_werks).
    AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
             ID 'WERKS' FIELD ls_werks-werks.
    IF sy-subrc <> 0.
      MESSAGE s002(zauthority) DISPLAY LIKE 'E' WITH '工厂' ls_werks-werks.
      STOP.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_M_BEST_EKG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->it_ekgrp  text
*----------------------------------------------------------------------*
FORM frm_check_m_best_ekg TABLES it_ekgrp .

  SELECT ekgrp
    INTO TABLE @DATA(lt_ekgrp)
    FROM t024
   WHERE ekgrp IN @it_ekgrp.
  IF lt_ekgrp[] IS INITIAL.
    MESSAGE s001(zauthority) DISPLAY LIKE 'E' WITH '采购组'.
    STOP.
  ENDIF.

  LOOP AT lt_ekgrp INTO DATA(ls_ekgrp).
    AUTHORITY-CHECK OBJECT 'M_BEST_EKG'
             ID 'EKGRP' FIELD ls_ekgrp-ekgrp.
    IF sy-subrc <> 0.
      MESSAGE s002(zauthority) DISPLAY LIKE 'E' WITH '采购组' ls_ekgrp-ekgrp.
      STOP.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_T024
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_t024 .
  CLEAR gt_t024[] .
  SELECT *
    INTO TABLE gt_t024
    FROM t024 .

  SORT gt_t024 BY ekgrp .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_KAKT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_get_kakt .
  CLEAR gt_makt[] .

  DATA : lt_ekpo TYPE TABLE OF ty_ekpo .
  CLEAR lt_ekpo[] .
  lt_ekpo[] = gt_ekpo[] .

  SORT lt_ekpo BY matnr .
  DELETE ADJACENT DUPLICATES FROM lt_ekpo
                  COMPARING matnr .

  CHECK lt_ekpo[] IS NOT INITIAL .

  SELECT *
    INTO TABLE gt_makt
    FROM makt
    FOR ALL ENTRIES IN lt_ekpo
    WHERE matnr = lt_ekpo-matnr
      AND spras = sy-langu  .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_EDI_PUBLIC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM sub_edi_public .
  DATA : ls_alv TYPE ty_alv .
  SORT gt_alv BY ebeln ebeln .
  "PO已收
  IF gs_alv-zpoys = 0 .
    CLEAR ls_alv .
    READ TABLE gt_alv INTO ls_alv WITH KEY ebeln = gs_alv-ebeln
                                           ebelp = gs_alv-ebelp BINARY SEARCH .
    IF sy-subrc = 0 .
      gs_alv-zpoys = ls_alv-zpoys .
    ELSE .
      CLEAR gs_ekbe .
      LOOP AT gt_ekbe INTO gs_ekbe WHERE ebeln = gs_alv-ebeln
                                     AND ebelp = gs_alv-ebelp .
        CASE gs_ekbe-shkzg .
          WHEN 'S' .
            gs_alv-zpoys = gs_alv-zpoys + gs_ekbe-menge .
          WHEN 'H' .
            gs_alv-zpoys = gs_alv-zpoys - gs_ekbe-menge .
        ENDCASE .
      ENDLOOP .
    ENDIF .
  ENDIF .


  "制单人
  CLEAR ls_alv  .
  SORT gt_alv BY ernam .
  READ TABLE gt_alv INTO ls_alv WITH KEY ernam = gs_alv-ernam BINARY SEARCH .
  IF sy-subrc <> 0 .
    CALL FUNCTION 'ZGET_USERNAME'
      EXPORTING
        i_uname = gs_alv-ernam
      IMPORTING
        e_uname = ls_alv-zname.
  ENDIF .
  gs_alv-zname = ls_alv-zname  .

*  "物料属性
*  CLEAR LS_ALV .
*  SORT GT_ALV BY MATNR .
*  READ TABLE GT_ALV INTO LS_ALV WITH KEY MATNR = GS_ALV-MATNR BINARY SEARCH .
*  IF SY-SUBRC = 0 .
*    GS_ALV-MADE_IN_CHINA =  LS_ALV-MADE_IN_CHINA."原产地
*    GS_ALV-XPRECIOUS     =  LS_ALV-XPRECIOUS    ."贵重标识
*  ELSE .
*    PERFORM SUB_GET_CLASS USING GS_ALV .
*  ENDIF .

  CLEAR gs_marc .
  READ TABLE gt_marc INTO gs_marc WITH KEY matnr = gs_alv-matnr
                                           werks = gs_alv-werks BINARY SEARCH .
  gs_alv-bstmi = gs_marc-bstmi .

  CLEAR gs_lfa1 .
  SORT gt_lfa1 BY lifnr .
  READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_alv-lifnr BINARY SEARCH .
  IF sy-subrc <> 0 .
    SELECT SINGLE
    lfa1~lifnr
    lfa1~land1
    adrc~name1
    adrc~name2
    adrc~name3
    adrc~name4
   INTO gs_lfa1
    FROM lfa1
     INNER JOIN adrc
 ON adrc~addrnumber = lfa1~adrnr
   WHERE lifnr = gs_alv-lifnr.
    APPEND gs_lfa1 TO gt_lfa1 .
  ENDIF .
  gs_alv-name1 = gs_lfa1-name1 && gs_lfa1-name2 && gs_lfa1-name3 && gs_lfa1-name4.

  CLEAR gs_lfa1 .
  SORT gt_lfa1 BY lifnr .
  READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_alv-producer BINARY SEARCH .
  IF sy-subrc <> 0 .
    SELECT SINGLE
    lfa1~lifnr
    lfa1~land1
    adrc~name1
    adrc~name2
    adrc~name3
    adrc~name4
   INTO gs_lfa1
    FROM lfa1
    INNER JOIN adrc
    ON adrc~addrnumber = lfa1~adrnr
   WHERE lifnr = gs_alv-producer .
    APPEND gs_lfa1 TO gt_lfa1 .
  ENDIF .
  gs_alv-name2 = gs_lfa1-name1 && gs_lfa1-name2 && gs_lfa1-name3 && gs_lfa1-name4."制造商

  CLEAR gs_t024 .
  READ TABLE gt_t024 INTO gs_t024 WITH KEY ekgrp = gs_alv-ekgrp .
  gs_alv-eknam = gs_t024-eknam .

  CLEAR gs_makt .
  READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_alv-matnr BINARY SEARCH .
  gs_alv-maktx = gs_makt-maktx .

  "延期天数
  IF gs_alv-zwysh = ''
  OR gs_alv-zwysh = '00000000'
  OR   gs_alv-zslzt = '2' ."待确认.
    gs_alv-zyqts = 0 .
  ELSE .
**  ①  有关闭标识且收货数量=0，不计算，不显示
*  IF GS_ALV-ZCLOSED = 'X'
*  AND GS_ALV-RECV_MENGE = 0 .
*    GS_ALV-ZYQTS = 0 .
*  ENDIF .
*②  有关闭标识且收货数量≠0，计算值=最后收货日期-答复日期
*   （如有调整后日期取调整后日期），＜0的显示空值，≥0的显示计算值；
    IF gs_alv-zclosed = 'X'
   AND gs_alv-recv_menge <> 0 .
      gs_alv-zyqts = gs_alv-zzhsh - gs_alv-zwysh .
      IF gs_alv-zyqts < 0 .
        gs_alv-zyqts = 0 .
      ENDIF .
    ENDIF .
*③  没有关闭标识且收货数量＜答复数量，计算值=系统日期-答复日期
*（如有调整后日期取调整后日期），＜0的显示空值，≥0的显示计算值；
    IF gs_alv-zclosed = ''
   AND gs_alv-recv_menge < gs_alv-pdlv_menge .
      gs_alv-zyqts = sy-datum - gs_alv-zwysh .
      IF gs_alv-zyqts < 0 .
        gs_alv-zyqts = 0 .
      ENDIF .
    ENDIF .
*④  没有关闭标识且收货数量≥答复数量，计算值=最后收货日期-答复日期
*（如有调整后日期取调整后日期），＜0的显示空值，≥0的显示计算值；
    IF gs_alv-zclosed = ''
  AND gs_alv-recv_menge >= gs_alv-pdlv_menge .
      gs_alv-zyqts = gs_alv-zzhsh - gs_alv-zwysh .
      IF gs_alv-zyqts < 0 .
        gs_alv-zyqts = 0 .
      ENDIF .
    ENDIF .
*⑤  未下传/未确认显示为空值
  ENDIF .

  PERFORM sub_set_ddo7t USING gs_alv .
ENDFORM.



FORM sub_build_header .
* Local data declaration
  DATA: l_system     TYPE char10 ,          "System id
        l_r_line     TYPE slis_listheader,  "Hold list header
        l_title(300) TYPE c,                " Title
        lv_num1(4)   TYPE i,
        lv_num2(4)   TYPE i.
  "总到货批次
  DESCRIBE TABLE gt_alv LINES sumbatch.
  LOOP AT gt_alv INTO gs_alv.
    "逾期到货批次
    IF gs_alv-zzsdt = '逾期齐备到货' OR gs_alv-zzsdt = '逾期未齐备到货'.
      latebatch = latebatch + 1.
      "及时未齐备批次
    ELSEIF gs_alv-zzsdt = '及时未齐备到货'.
      intimenobatch = intimenobatch + 1.
      "设置第六类的数据：及时未齐备到货的数据中且状态为关闭的数据；Seashell 20220221
      IF gs_alv-zslzt = 4.
        intime_close_six = intime_close_six + 1.
      ENDIF.
      "end 20220221
    ENDIF.
  ENDLOOP.

  IF sumbatch IS NOT INITIAL.
    "及时到货率
    intimerate_tmp = ( sumbatch - latebatch ) / sumbatch * 100.
    intimerate = intimerate_tmp && '%'.
    "及时齐备到货率
*    intimerate2_tmp = ( sumbatch - latebatch - intimenobatch ) / sumbatch * 100.
    "及时齐备到货率新逻辑：（1-2-3+6）/1得出百分比
    intimerate2_tmp = ( sumbatch - latebatch - intimenobatch + intime_close_six ) / sumbatch * 100.
    intimerate2 = intimerate2_tmp && '%'.
  ENDIF.

  l_r_line-typ  = 'H'.                " Item
  l_r_line-info = '汇总数据'.
  APPEND l_r_line TO i_alv_top_of_page.
  CLEAR: l_r_line.
  l_r_line-typ  = 'S'.                " Item
  l_r_line-key = '总到货批次：'.
  l_r_line-info = sumbatch.
  APPEND l_r_line TO i_alv_top_of_page.
  CLEAR: l_r_line.
  l_r_line-typ  = 'S'.                " Item
  l_r_line-key = '逾期到货批次：'.
  l_r_line-info = latebatch.
  APPEND l_r_line TO i_alv_top_of_page.
  CLEAR: l_r_line.
  l_r_line-typ  = 'S'.                " Item
  l_r_line-key = '及时未齐备批次：'.
  l_r_line-info = intimenobatch.
  APPEND l_r_line TO i_alv_top_of_page.
  CLEAR: l_r_line.
  l_r_line-typ  = 'S'.                " Item
  l_r_line-key = '及时到货率：'.
  l_r_line-info = intimerate.
  APPEND l_r_line TO i_alv_top_of_page.
  CLEAR: l_r_line.
  l_r_line-typ  = 'S'.                " Item
  l_r_line-key = '及时齐备到货率：'.
  l_r_line-info = intimerate2.
  APPEND l_r_line TO i_alv_top_of_page.
  CLEAR: l_r_line.
ENDFORM.                    " sub_build_header


FORM sub_alv_top_of_page.                                   "#EC CALLED
* To write header for the ALV
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = i_alv_top_of_page.

ENDFORM.                    "alv_top_of_page