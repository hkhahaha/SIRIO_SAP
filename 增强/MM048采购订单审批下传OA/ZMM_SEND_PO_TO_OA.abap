FUNCTION zmm_send_po_to_oa.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(EBELN) TYPE  EKKO-EBELN OPTIONAL
*"  TABLES
*"      T_PRCD STRUCTURE  PRCD_ELEMENTS OPTIONAL
*"      T_ITEM STRUCTURE  MEPOITEM OPTIONAL
*"      T_KONP STRUCTURE  KONP OPTIONAL
*"      T_A017 STRUCTURE  A017 OPTIONAL
*"----------------------------------------------------------------------


  DATA:lw_zpurchase_order TYPE zcreate_purchase_order_soap_in ."ZPURCHASE_ORDER.
  DATA:lw_zdt_sap2oa_issue TYPE LINE OF zlist_of_others_tab ."ZDT_SAP2OA_ISSUE_SUB.
  DATA:lt_zdt_sap2oa_issue TYPE TABLE OF zlist_of_others_tab ."ZDT_SAP2OA_ISSUE_SUB.
  DATA:lo_clientproxy       TYPE REF TO  zco_si_sap2oa_issue_out,
       lo_system_fault      TYPE REF TO cx_ai_system_fault,
       lo_ack               TYPE REF TO if_ws_acknowledgment,
       lo_ack_status_simple TYPE prx_ack_status,
       lo_ack_status_detail TYPE prx_ack_status_detail_table,
       lo_ack_request       TYPE prx_ack_request_details,
       lo_async_messaging   TYPE REF TO if_wsprotocol_async_messaging,
       lo_msg_id_protocol   TYPE REF TO if_wsprotocol_message_id,
       lv_msg_id            TYPE sxmsguid,
       lv_error             TYPE c,
       lv_msgty             TYPE sy-msgty,
       lv_msg               TYPE string.
  DATA : lv_marks TYPE c .

  DATA:lv_name1 TYPE lfa1-name1,
       lv_name2 TYPE lfa1-name2,
       lv_name3 TYPE lfa1-name3,
       lv_name4 TYPE lfa1-name4.

  DATA : lv_kposn TYPE prcd_elements-kposn .
  DATA : lv_kawrt TYPE p DECIMALS 2 ."PRCD_ELEMENTS-KAWRT .

  TYPES:BEGIN OF lw_makt,
          matnr TYPE makt-matnr,
          maktx TYPE makt-maktx,
        END OF lw_makt.

  TYPES:BEGIN OF lw_eket,
          ebeln TYPE eket-ebeln,
          ebelp TYPE eket-ebelp,
          eindt TYPE eket-eindt,
          charg TYPE eket-charg,
        END OF lw_eket.

  TYPES:BEGIN OF lw_name_org1,
          matnr     TYPE mara-matnr,
          mfrnr     TYPE mara-mfrnr,
          name_org1 TYPE but000-name_org1,
        END OF lw_name_org1.




  DATA:lt_flines TYPE TABLE OF tline.
  DATA:lt_makt TYPE TABLE OF lw_makt.
  DATA:lt_eket TYPE TABLE OF lw_eket.
  DATA:lt_name_org1 TYPE TABLE OF lw_name_org1.
  DATA:lv_tdname TYPE stxl-tdname.


*&---------------------------------------------------------------------*
*& DEFINE CONSTANTS
*&---------------------------------------------------------------------*
  CONSTANTS: gc_msgty_success      TYPE c VALUE 'S',
             gc_msgty_error        TYPE c VALUE 'E',
             gc_zdwxz_gongsi       TYPE c VALUE '1',
             gc_otype_organization TYPE text02 VALUE 'O',
             gc_inif_id(6)         TYPE c VALUE 'MM048', "接口编号
             gc_src_system(3)      TYPE c VALUE 'SAP',    "源系统
             gc_dest_system(3)     TYPE c VALUE 'OA'.    "目标系统


  CLEAR:lw_zpurchase_order,lw_zdt_sap2oa_issue,lv_name1,lv_name2,
        lv_name3,lv_name4.
  REFRESH:lt_zdt_sap2oa_issue,lt_makt,lt_eket,lt_name_org1.



  DATA: lr_items TYPE purchase_order_items,
        lr_item  TYPE purchase_order_item,
        ls_item  TYPE mepoitem,
        lt_item  TYPE TABLE OF mepoitem,
        ls_head  TYPE mepoheader.

  DATA: BEGIN OF lt_ekpo1 OCCURS 0,
          ebeln  TYPE ebeln,
          ebelp  TYPE ebelp,
          loekz  TYPE loekz,
          loekz1 TYPE loekz,

        END OF lt_ekpo1,
        ls_ekpo1 LIKE LINE OF lt_ekpo1,
        ls_ekpo2 LIKE LINE OF lt_ekpo1.
*&---------------------------------------------------------------------*
* 1实例化proxy
*&---------------------------------------------------------------------*
  TRY.
      CREATE OBJECT lo_clientproxy.
      lo_async_messaging ?= lo_clientproxy->get_protocol( if_wsprotocol=>async_messaging ).
*     ask for transport acknowledgment
      CLEAR lo_ack_request.
      lo_ack_request = if_wsprotocol_async_messaging=>co_transport_acknowledgment.
      lo_async_messaging->set_acknowledgment_requested( lo_ack_request ).
    CATCH cx_ai_system_fault INTO lo_system_fault.
      lv_msg   = lo_system_fault->get_text( ).
      lv_msgty = gc_msgty_error.
      lv_error = abap_true.
  ENDTRY.

*抬头信息
*    LS_HEAD = IM_HEADER->GET_DATA( ).
  "获取最新信息
  WAIT UP TO 2 SECONDS .
  DO  3000 TIMES  .
    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF ls_head
      FROM ekko
      WHERE ebeln = ebeln .
    IF sy-subrc = 0 .
      EXIT .
    ENDIF .
  ENDDO.
***********检查是否下发
  SELECT SINGLE * FROM zmmt0010 INTO @DATA(ls_zmmt0010)
    WHERE bukrs EQ @ls_head-bukrs
    AND   ekgrp EQ @ls_head-ekgrp
    AND   sends NE @space.

  CHECK sy-subrc EQ 0.

***********检查单据是否下发-ADD BY LANJINGCUN 20210111
  SELECT COUNT(*) FROM zmmt0014
    WHERE bsart = ls_head-bsart.
  CHECK sy-subrc <> 0.

************检查金额

  DO  30000 TIMES  .
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_item
      FROM ekpo
      WHERE ebeln = ebeln .
    IF sy-subrc = 0 .
      EXIT .
    ENDIF .
  ENDDO .

***********检查金额
*  DATA LT_PRCD TYPE TABLE OF PRCD_ELEMENTS .
*  DATA LS_PRCD TYPE PRCD_ELEMENTS .
  DATA : lv_jine1 TYPE prcd_elements-kwert .
  DATA : lv_jine2 TYPE prcd_elements-kwert .
  DATA : lv_jine3 TYPE prcd_elements-kwert .
  DATA : lv_jine4 TYPE prcd_elements-kwert .

  DATA : ls_a017 TYPE a017 .
  DATA : lt_a017 TYPE TABLE OF a017 .
  DATA : lt_konp TYPE TABLE OF konp .
  DATA : ls_konp TYPE konp .

  SELECT  *
    INTO TABLE lt_a017
    FROM a017
    FOR ALL ENTRIES IN lt_item
    WHERE kappl = 'M'
      AND kschl = 'ZPB0'
      AND lifnr = ls_head-lifnr
      AND matnr = lt_item-matnr
      AND ekorg = ls_head-ekorg
      AND werks = lt_item-werks
      AND esokz = '2'
      AND datbi >= ls_head-bedat
      AND datab <= ls_head-bedat .
  IF sy-subrc = 0 .

    SELECT *
      INTO TABLE lt_konp
      FROM konp
      FOR ALL ENTRIES IN lt_a017
      WHERE knumh = lt_a017-knumh
        AND kappl = 'M'
        AND kschl = 'ZPB0' .

  ENDIF .

*  CLEAR LS_PRCD .
*  LOOP AT T_PRCD INTO LS_PRCD WHERE KPOSN <> '000000' .
*
*    READ TABLE T_ITEM INTO LS_ITEM WITH KEY EBELP = LS_PRCD-KPOSN
*                                             LOEKZ = '' .
*    CHECK SY-SUBRC = 0 .
*    IF LS_PRCD-KSCHL  = 'ZM01'
*    OR LS_PRCD-KSCHL  = 'ZPB0' .
*      LV_JINE1 = LV_JINE1 + LS_PRCD-KWERT .
*    ENDIF .
*
*  ENDLOOP .
*
*  SELECT *
*    INTO TABLE LT_PRCD
*    FROM PRCD_ELEMENTS
*    WHERE KNUMV = LS_HEAD-KNUMV .
*
*  CLEAR LS_PRCD .
*  LOOP AT LT_PRCD INTO LS_PRCD WHERE KPOSN <> '000000' .
*    READ TABLE LT_ITEM INTO LS_ITEM WITH KEY EBELP = LS_PRCD-KPOSN
*                                             LOEKZ = '' .
*    CHECK SY-SUBRC = 0 .
*    IF LS_PRCD-KSCHL  = 'ZM01'
*    OR LS_PRCD-KSCHL  = 'ZPB0' .
*      LV_JINE2 = LV_JINE2 + LS_PRCD-KWERT .
*    ENDIF .
*
*  ENDLOOP .
  SELECT COUNT(*)
     FROM zmmt0012
     WHERE aedat >= ls_head-aedat ."如果自建表日期大于等于创建日期的 不需要检查金额
  IF sy-subrc <> 0 .

    ""20200828 top waymon 有打上删除标志就下发
    CLEAR lv_marks  .
    LOOP AT t_item INTO DATA(ts_item) WHERE loekz = ''.

      READ TABLE lt_item INTO ls_item WITH KEY  ebeln = ts_item-ebeln
                                                ebelp = ts_item-ebelp
                                                 loekz = 'L'.

      IF sy-subrc = 0 .
        lv_marks = 'X' .
        EXIT.
      ENDIF .                                               "20200828

    ENDLOOP .                                               "20200828
    CLEAR ls_item  .
    ""20200828 end waymon 有打上删除标志就下发

    IF ls_head-frgke EQ 'I'"审批状态I的都需要下传
       OR lv_marks  <> ''."或者删除的时候需要下传
    ELSE .
      CLEAR ls_item  .

      LOOP AT t_item INTO ls_item WHERE loekz = '' .
        IF ls_head-bsart = 'ZJS' .
          CLEAR ls_a017 .
          READ TABLE t_a017 INTO ls_a017 WITH KEY matnr = ls_item-matnr .
          CLEAR ls_konp .
          READ TABLE t_konp INTO ls_konp WITH  KEY knumh = ls_a017-knumh .
          IF sy-subrc = 0 .
            lv_jine1 = lv_jine1 + ls_item-menge * ls_konp-kbetr / ls_konp-kpein  * ls_head-wkurs .
          ENDIF .
        ELSE .
          lv_jine1 = lv_jine1 +  ls_item-kzwi1 * ls_head-wkurs .
          lv_jine3 = lv_jine3 + ls_item-effwr * ls_head-wkurs .
        ENDIF .

      ENDLOOP .

      CLEAR ls_item  .

      LOOP AT lt_item INTO ls_item WHERE loekz = '' .
        IF ls_head-bsart = 'ZJS' .
          CLEAR ls_a017 .
          READ TABLE lt_a017 INTO ls_a017 WITH KEY matnr = ls_item-matnr .
          CLEAR ls_konp .
          READ TABLE lt_konp INTO ls_konp WITH  KEY knumh = ls_a017-knumh .
          IF sy-subrc = 0 .
            lv_jine2 = lv_jine2 + ls_item-menge * ls_konp-kbetr / ls_konp-kpein * ls_head-wkurs .
          ENDIF .
        ELSE .
          lv_jine2 = lv_jine2 +  ls_item-kzwi1 * ls_head-wkurs .
          lv_jine4 = lv_jine4 + ls_item-effwr  * ls_head-wkurs .
        ENDIF .
      ENDLOOP .

*如果含税总额或者不含税总额有增加 , 就下发OA重审；
*含税总金额10W以上(包括10W)变为10W以下,就下发OA重审；否则就不用重新下发OA。

      IF t_item[] IS NOT INITIAL.
*        AND LV_JINE2 <> LV_JINE1 .
        IF lv_jine1 + lv_jine3 = lv_jine2 + lv_jine4 .
          EXIT .
        ENDIF .

        IF lv_jine2 > lv_jine1
           OR lv_jine4 > lv_jine3  .
        ELSE .
          IF lv_jine1 >= 100000
            AND  lv_jine2 < 100000 .
          ELSE .
            EXIT .
          ENDIF .
        ENDIF .
      ENDIF .
    ENDIF .
  ELSE .

    CLEAR ls_item  .

    LOOP AT t_item INTO ls_item WHERE loekz = '' .
      IF ls_head-bsart = 'ZJS' .
        CLEAR ls_a017 .
        READ TABLE t_a017 INTO ls_a017 WITH KEY matnr = ls_item-matnr .
        CLEAR ls_konp .
        READ TABLE t_konp INTO ls_konp WITH  KEY knumh = ls_a017-knumh .
        IF sy-subrc = 0 .
          lv_jine1 = lv_jine1 + ls_item-menge * ls_konp-kbetr / ls_konp-kpein * ls_head-wkurs .
        ENDIF .
      ELSE .
        lv_jine1 = lv_jine1 +  ls_item-kzwi1 * ls_head-wkurs .
        lv_jine3 = lv_jine3 + ls_item-effwr * ls_head-wkurs .
      ENDIF .
    ENDLOOP .

    CLEAR ls_item  .

    LOOP AT lt_item INTO ls_item WHERE loekz = '' .
      IF ls_head-bsart = 'ZJS' .
        CLEAR ls_a017 .
        READ TABLE lt_a017 INTO ls_a017 WITH KEY matnr = ls_item-matnr .
        CLEAR ls_konp .
        READ TABLE lt_konp INTO ls_konp WITH  KEY knumh = ls_a017-knumh .
        IF sy-subrc = 0 .
          lv_jine2 = lv_jine2 + ls_item-menge * ls_konp-kbetr / ls_konp-kpein * ls_head-wkurs .
        ENDIF .
      ELSE .
        lv_jine2 = lv_jine2 +  ls_item-kzwi1 * ls_head-wkurs .
        lv_jine4 = lv_jine4 + ls_item-effwr  * ls_head-wkurs .
      ENDIF .
    ENDLOOP .

    IF lv_jine1 + lv_jine3 = lv_jine2 + lv_jine4 ."金额不变 不需要下发
      EXIT .
    ENDIF .


  ENDIF .



*    LR_ITEMS = IM_HEADER->GET_ITEMS( ).
*
**行项目信息
*    REFRESH:LT_ITEM.
*    LOOP AT LR_ITEMS INTO LR_ITEM.
*      LS_ITEM = LR_ITEM-ITEM->GET_DATA( ).
*      APPEND LS_ITEM TO LT_ITEM.
*    ENDLOOP.


  CHECK ls_head-memory IS INITIAL.


************抬头
  MOVE-CORRESPONDING  ls_head TO lw_zpurchase_order-purchase_order .

  SELECT SINGLE ekotx FROM t024e
  INTO lw_zpurchase_order-purchase_order-ekotx
  WHERE ekorg EQ lw_zpurchase_order-purchase_order-ekorg.

  SELECT SINGLE eknam FROM t024
    INTO lw_zpurchase_order-purchase_order-eknam
    WHERE ekgrp EQ lw_zpurchase_order-purchase_order-ekgrp.

  SELECT SINGLE butxt FROM t001
    INTO lw_zpurchase_order-purchase_order-butxt
    WHERE bukrs EQ lw_zpurchase_order-purchase_order-bukrs.

  SELECT SINGLE name1 name2 name3 name4
    INTO ( lv_name1,lv_name2,lv_name3,lv_name4 )
    FROM lfa1
    WHERE lifnr EQ lw_zpurchase_order-purchase_order-lifnr.

  lw_zpurchase_order-purchase_order-lifnr_name = lv_name1 && lv_name2 &&
                                                   lv_name3 && lv_name4.

  SELECT SINGLE batxt FROM t161t
    INTO lw_zpurchase_order-purchase_order-batxt
    WHERE bsart EQ lw_zpurchase_order-purchase_order-bsart.
*************行项目
*    MOVE-CORRESPONDING IM_EKPO[] TO LW_ZPURCHASE_ORDER-PURCHASE_ORDER-LIST_OF_OTHERS[].
  IF lt_item[] IS NOT INITIAL.
*******物料描述
    DATA(lt_ekpo) = lt_item[].
*      SORT LT_EKPO BY MATNR.
*      DELETE ADJACENT DUPLICATES FROM LT_EKPO COMPARING MATNR.
*      SELECT MATNR
*             MAKTX
*        FROM MAKT INTO TABLE LT_MAKT
*      FOR ALL ENTRIES IN LT_EKPO
*      WHERE ( MATNR EQ LT_EKPO-MATNR
*      OR      MATNR EQ LT_EKPO-EMATN )
*      AND   SPRAS EQ SY-LANGU.

**********计划行
    SELECT ebeln
           ebelp
           eindt
           charg
     FROM eket INTO TABLE lt_eket
    FOR ALL ENTRIES IN lt_ekpo
    WHERE ebeln EQ lt_ekpo-ebeln
    AND   ebelp EQ lt_ekpo-ebelp.

    DELETE ADJACENT DUPLICATES FROM lt_ekpo COMPARING infnr matnr.
    SELECT * FROM eina INTO TABLE @DATA(lt_eina)
      FOR ALL ENTRIES IN @lt_ekpo
      WHERE lifnr EQ @ls_head-lifnr
      AND   matnr EQ @lt_ekpo-matnr.
    IF lt_eina[] IS NOT INITIAL.
      SELECT * FROM eine INTO TABLE @DATA(lt_eine)
        FOR ALL ENTRIES IN @lt_eina
       WHERE infnr EQ @lt_eina-infnr
       AND   ekorg EQ @ls_head-ekorg.
    ENDIF.

**********自定义制造商 20210322 top.
    LOOP AT lt_item INTO ls_item.
      IF ls_item-mfrnr = ''.

        ls_item-mfrnr = ls_item-zmfrnr.

        MODIFY lt_item FROM ls_item.

      ENDIF.

      CLEAR ls_item.
    ENDLOOP.
**********自定义制造商 20210322 end.

    lt_ekpo[] = lt_item[].
    SORT lt_ekpo BY mfrnr.
    DELETE ADJACENT DUPLICATES FROM lt_ekpo COMPARING mfrnr.

**********重新取值供应商描述 20210323 top.
*    SELECT MARA~MATNR
*           MARA~MFRNR
*           BUT000~NAME_ORG1
*    INTO TABLE LT_NAME_ORG1
*    FROM MARA AS MARA INNER JOIN BUT000 AS BUT000
*      ON MARA~MFRNR EQ BUT000~PARTNER
*     FOR ALL ENTRIES IN LT_EKPO
*    WHERE MFRNR EQ LT_EKPO-MFRNR.

    SELECT  but000~name_org1
            but000~partner AS mfrnr
            but000~name_org1
   INTO TABLE lt_name_org1
   FROM  but000 AS but000 FOR ALL ENTRIES IN lt_ekpo WHERE partner EQ lt_ekpo-mfrnr.
**********重新取值供应商描述 20210323 end.

  ENDIF.

  LOOP AT lt_item
    ASSIGNING FIELD-SYMBOL(<fs_im_ekpo>).
    MOVE-CORRESPONDING <fs_im_ekpo> TO lw_zdt_sap2oa_issue.
    DATA(lv_matnr) = <fs_im_ekpo>-matnr.
    lw_zdt_sap2oa_issue-waers = ls_head-waers .
    SELECT SINGLE groes
        INTO  lw_zdt_sap2oa_issue-size
        FROM mara
        WHERE matnr = lv_matnr .
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input        = lv_matnr
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    IF lv_matnr+0(2) EQ '10' AND lw_zpurchase_order-purchase_order-bsart <> 'ZJS'.
      lv_matnr = <fs_im_ekpo>-ematn.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input        = lv_matnr
        IMPORTING
          output       = lv_matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.
    ENDIF.

    lw_zdt_sap2oa_issue-matnr = lv_matnr.

*      READ TABLE LT_MAKT ASSIGNING FIELD-SYMBOL(<FS_MAKT>)
*       WITH KEY MATNR = LW_ZDT_SAP2OA_ISSUE-MATNR.
*      IF SY-SUBRC EQ 0.
*        LW_ZDT_SAP2OA_ISSUE-MAKTX = <FS_MAKT>-MAKTX.
*      ENDIF.
    lw_zdt_sap2oa_issue-maktx = <fs_im_ekpo>-txz01.
********含税单价 /含税总额
    IF <fs_im_ekpo>-pstyp NE '2'.
      lw_zdt_sap2oa_issue-price = <fs_im_ekpo>-brtwr / <fs_im_ekpo>-menge.
      lw_zdt_sap2oa_issue-brtwr = <fs_im_ekpo>-brtwr.
    ELSE.
*      LOOP AT lt_eina ASSIGNING FIELD-SYMBOL(<fs_eina>)
*        WHERE lifnr = ls_head-lifnr
*        AND   matnr = <fs_im_ekpo>-matnr.
*        READ TABLE lt_eine ASSIGNING FIELD-SYMBOL(<fs_eine>)
*         WITH KEY infnr = <fs_eina>-infnr
*                  ekorg = ls_head-ekorg
*                  werks = <fs_im_ekpo>-werks
*                  esokz = '2'.
*        IF sy-subrc EQ 0.
*          lw_zdt_sap2oa_issue-price = <fs_eine>-netpr / <fs_eine>-peinh.
*        ENDIF.
*      ENDLOOP.
*      lw_zdt_sap2oa_issue-brtwr = lw_zdt_sap2oa_issue-price * <fs_im_ekpo>-menge.

*20210415 寄售单价的取值 waymon top .
        CLEAR ls_a017 .
        READ TABLE lt_a017 INTO ls_a017 WITH KEY matnr = <fs_im_ekpo>-matnr.
        CLEAR ls_konp .
        READ TABLE lt_konp INTO ls_konp WITH  KEY knumh = ls_a017-knumh .

        IF sy-subrc EQ 0.
          lw_zdt_sap2oa_issue-price = ls_konp-kbetr / ls_konp-kpein.
        ENDIF.
*      ENDLOOP.
      lw_zdt_sap2oa_issue-brtwr = lw_zdt_sap2oa_issue-price * <fs_im_ekpo>-menge.
*20210415 寄售单价的取值 waymon end  .

    ENDIF.
*    komv-kschl为ZM01，取KOMV-kwert
    CLEAR lv_kawrt .
    lv_kposn = <fs_im_ekpo>-ebelp .
    SELECT SINGLE kwert
      INTO lv_kawrt
      FROM prcd_elements
      WHERE knumv = ls_head-knumv
        AND kposn = lv_kposn
        AND kschl = 'ZM01' .
    lw_zdt_sap2oa_issue-discount  =  lv_kawrt ."  折扣或附加费
    lw_zdt_sap2oa_issue-toamount = lw_zdt_sap2oa_issue-discount + lw_zdt_sap2oa_issue-brtwr	."含税总额（含附加费）

    READ TABLE lt_eket ASSIGNING FIELD-SYMBOL(<fs_eket>)
     WITH KEY ebeln = <fs_im_ekpo>-ebeln
              ebelp = <fs_im_ekpo>-ebelp.
    IF sy-subrc EQ 0.
      lw_zdt_sap2oa_issue-eindt = <fs_eket>-eindt.
      lw_zdt_sap2oa_issue-charg = <fs_eket>-charg.
    ENDIF.

    READ TABLE lt_name_org1 ASSIGNING FIELD-SYMBOL(<fs_name_org1>)
*     WITH KEY MATNR = <FS_IM_EKPO>-EMATN. 20210322 waymon.
      WITH KEY mfrnr = <fs_im_ekpo>-mfrnr.
    IF sy-subrc EQ 0.
      lw_zdt_sap2oa_issue-mfrnr_name = <fs_name_org1>-name_org1.
    ENDIF.

***********文本
    REFRESH:lt_flines.
    CLEAR:lv_tdname.
    lv_tdname = <fs_im_ekpo>-ebeln && <fs_im_ekpo>-ebelp.
    SELECT SINGLE * FROM stxl INTO @DATA(ls_stxl)
    WHERE tdobject = 'EKPO'
    AND   tdname = @lv_tdname
    AND   tdid  = 'F01'.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = ls_stxl-tdid
          language                = sy-langu
          name                    = ls_stxl-tdname
          object                  = ls_stxl-tdobject
        TABLES
          lines                   = lt_flines
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
        LOOP AT lt_flines ASSIGNING FIELD-SYMBOL(<fs_flines>).
          CONCATENATE lw_zdt_sap2oa_issue-itemnote  <fs_flines>-tdline INTO lw_zdt_sap2oa_issue-itemnote .
        ENDLOOP.
      ENDIF.
    ENDIF.

    REFRESH:lt_flines.
    CLEAR:ls_stxl.
    SELECT SINGLE * FROM stxl INTO ls_stxl
    WHERE tdobject = 'EKPO'
    AND   tdname = lv_tdname
    AND   tdid  = 'F03'.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = ls_stxl-tdid
          language                = sy-langu
          name                    = ls_stxl-tdname
          object                  = ls_stxl-tdobject
        TABLES
          lines                   = lt_flines
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
        LOOP AT lt_flines ASSIGNING <fs_flines>.
          CONCATENATE  lw_zdt_sap2oa_issue-ponote  <fs_flines>-tdline INTO lw_zdt_sap2oa_issue-ponote  .
        ENDLOOP.
      ENDIF.
      CLEAR:ls_stxl.
    ENDIF.
    IF <fs_im_ekpo>-repos EQ space
    AND <fs_im_ekpo>-pstyp <> '2'.
      lw_zdt_sap2oa_issue-umson = 'X'.
    ELSE.
      lw_zdt_sap2oa_issue-umson = space.
    ENDIF.
    IF lw_zdt_sap2oa_issue-loekz EQ 'L'.
      lw_zdt_sap2oa_issue-loekz = 'X'.
    ENDIF.

    IF <fs_im_ekpo>-loekz = 'L'.
      lw_zdt_sap2oa_issue-discount = '0'  .
      lw_zdt_sap2oa_issue-toamount = '0'.
      lw_zdt_sap2oa_issue-brtwr = '0' .
      lw_zdt_sap2oa_issue-menge = '0'.
    ENDIF .

    APPEND lw_zdt_sap2oa_issue TO lw_zpurchase_order-purchase_order-list_of_others-list_of_others.
    CLEAR:lw_zdt_sap2oa_issue.
  ENDLOOP.

*日志记录
  DATA:lv_guid       TYPE sxmsguid.
  lv_guid = zcl_bc_public=>get_guid( ).
  DATA(l_flag) = zcl_bc_public=>write_log( iv_logid = CONV #( lv_guid )
                                           iv_intid = 'MM048'    "OA传输SAP规格标准
                                           iv_ptype = 'I'
                                           is_data = lw_zpurchase_order ). "记录输入参数
***********SEND
  TRY.
      CALL METHOD lo_clientproxy->si_sap2oa_issue_out
        EXPORTING
          output = lw_zpurchase_order.
      COMMIT WORK.
    CATCH cx_ai_system_fault INTO lo_system_fault.
      lv_msg   = lo_system_fault->get_text( ).
      lv_error = abap_true.
      lv_msgty = gc_msgty_error.
  ENDTRY.


  TRY .
      lo_msg_id_protocol ?=  lo_clientproxy->get_protocol( if_wsprotocol=>message_id ).
      lv_msg_id = lo_msg_id_protocol->get_message_id( ).
    CATCH cx_ai_system_fault INTO lo_system_fault.
      EXIT.
  ENDTRY.
  IF lv_msgty EQ 'E'.
    MESSAGE e000(zmm) WITH  lv_msg.
  ELSE.
    DATA(lv_taskname) = sy-datum && sy-uzeit.
    IF ls_head-frgke EQ 'I'.
      CALL FUNCTION 'ZMM_PO_RELEASE1' STARTING NEW TASK lv_taskname
        EXPORTING
          iv_ebeln    = ls_head-ebeln
          iv_rel_code = 'Z1'.
    ELSEIF ls_head-frgke EQ 'R'
       OR ls_head-frgke EQ 'Z' .

      IF t_item[] <> lt_item .
        CALL FUNCTION 'ZMM_PO_RELEASE1' STARTING NEW TASK lv_taskname
          EXPORTING
            iv_ebeln    = ls_head-ebeln
            iv_rel_code = 'Z2'
            iv_flag     = ' '.
      ENDIF .
    ENDIF.
  ENDIF.

  DATA : zoutput TYPE zsmessage_out .
  zoutput-success = lv_msgty  .
  zoutput-message =  lv_msg  .
  zcl_bc_public=>write_log( iv_logid = CONV #( lv_guid )
                            iv_intid = 'MM048'
                            iv_ptype = 'E'
                            is_data = zoutput ).


ENDFUNCTION.