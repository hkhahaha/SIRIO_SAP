FUNCTION z_sd_sales_select2.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(INPUT) TYPE  ZMT_OA2SAP_SALESORDER2
*"     REFERENCE(MESSAGE_ID) TYPE  SXMSGUID
*"  EXPORTING
*"     REFERENCE(OUTPUT) TYPE  ZMT_OA2SAP_SALESORDER_RET2
*"----------------------------------------------------------------------
*手工备料单

  DATA: l_success.
  DATA:ls_input  TYPE zdt_oa2sap_salesorder2,
       lt_input2 TYPE TABLE OF zdt_oa2sap_salesorder2_header,
       ls_input2 TYPE zdt_oa2sap_salesorder2_header.
  ls_input = input-mt_oa2sap_salesorder2.
  lt_input2 = ls_input-header." 可以取到第一个值
  READ TABLE lt_input2 INTO ls_input2 INDEX 1.


  DATA:ls_output TYPE zdt_oa2sap_salesorder_ret2.

*&---------------------------------------------------------------------*
*& 程序名：LZSD001U04
*&作者：Seashell Huang
*&模块：
*&创建日期：04.11.2019 11:04:32
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：先判断订单的状态，避免重复审批
*&---------------------------------------------------------------------*
  DATA:lv_augru TYPE vbak-augru.
  PERFORM frm_check_input_status USING  ls_input2
                                  CHANGING lv_augru.
  IF lv_augru <> 'Z01'.
*检查输入条件是否都满足
    PERFORM frm_check_input_data2 USING    ls_input2
                                  CHANGING l_success.
*  CHECK l_success IS INITIAL.

*获取数据
    PERFORM frm_get_info_data2 USING    ls_input2
                               CHANGING ls_output.
    output-mt_oa2sap_salesorder_ret2 = ls_output.
  ELSE.
    output-mt_oa2sap_salesorder_ret2-type = 'E'.
    output-mt_oa2sap_salesorder_ret2-message = '订单已审批，勿重复提交'.
  ENDIF.
ENDFUNCTION.



FORM frm_check_input_status USING  u_input TYPE zdt_oa2sap_salesorder2_header
CHANGING lv_augru2.
  SELECT SINGLE
    augru
  FROM vbak
  INTO lv_augru2
  WHERE vbak~vbeln = u_input-vbeln.
ENDFORM.



FORM frm_check_input_data2  USING u_input TYPE zdt_oa2sap_salesorder2_header
CHANGING c_success.
  DATA:u_input_one  TYPE zdt_oa2sap_salesorder2_header,
       u_input_two  TYPE zdt_oa2sap_salesorder2_ite_tab,
       u_input_two2 TYPE zdt_oa2sap_salesorder2_item.
  u_input_two = u_input-item.
  READ TABLE u_input_two INTO u_input_two2 INDEX 1.
  IF  u_input-vbeln IS INITIAL
  OR  u_input_two2-posnr IS INITIAL.
    c_success = 'E'.
  ENDIF.
ENDFORM.

FORM frm_get_info_data2 USING    u_input TYPE zdt_oa2sap_salesorder2_header
CHANGING c_output TYPE zdt_oa2sap_salesorder_ret2.

  DATA: lt_vbak TYPE TABLE OF vbak,
        ls_vbak TYPE vbak,
        lt_vbap TYPE TABLE OF vbap,
        ls_vbap TYPE vbap,
        lt_kna1 TYPE TABLE OF kna1,
        ls_kna1 TYPE kna1,
        lt_vbkd TYPE TABLE OF vbkd,
        ls_vbkd TYPE vbkd,
        lt_makt TYPE TABLE OF makt,
        ls_makt TYPE makt.

  DATA: lt_so   TYPE zdt_oa2sap_salesorder_ret2_tab,
        ls_so   TYPE zdt_oa2sap_salesorder_ret2_so,

        lt_item TYPE zdt_oa2sap_salesorder_ret_tab3,
        ls_item TYPE zdt_oa2sap_salesorder_ret2_ite.



  DATA: lv_str  TYPE string,
        lv_str2 LIKE lv_str,
        i_posnr TYPE string.

  DATA:u_input_one  TYPE zdt_oa2sap_salesorder2,
       u_input_two  TYPE  zdt_oa2sap_salesorder2_ite_tab, "zdt_oa2sap_salesorder2_item,
       u_input_two2 TYPE zdt_oa2sap_salesorder2_item.
  u_input_two = u_input-item.
  READ TABLE u_input_two INTO u_input_two2 INDEX 1.
*  u_input_two2-posnr
  "获取查询的数据
  lv_str = '%' && u_input-vbeln && '%'.

*  修改开始，日期2019.9.3，修改人HK

*抬头字段
  SELECT
    *
  INTO CORRESPONDING FIELDS OF TABLE lt_vbak
  FROM vbak
  WHERE vbak~vbeln LIKE lv_str.

  IF lt_vbak IS NOT INITIAL.

    SELECT
    *
    INTO CORRESPONDING FIELDS OF TABLE lt_kna1
    FROM kna1
    FOR ALL ENTRIES IN lt_vbak
    WHERE kna1~kunnr = lt_vbak-kunnr.
    SELECT
      *
    INTO CORRESPONDING FIELDS OF TABLE lt_vbkd
    FROM vbkd
    FOR ALL ENTRIES IN lt_vbak
    WHERE vbkd~vbeln = lt_vbak-vbeln.





    LOOP AT u_input_two INTO u_input_two2.

*  抬头数据查询完毕，下面是行项目的数据
      IF u_input_two2-posnr IS NOT INITIAL.
        i_posnr = u_input_two2-posnr.
        SELECT
          *
        INTO CORRESPONDING FIELDS OF TABLE lt_vbap
        FROM vbap
        FOR ALL ENTRIES IN lt_vbak
        WHERE vbap~posnr = i_posnr
        AND   vbap~vbeln = lt_vbak-vbeln.
      ELSE.
        SELECT
        *
        INTO CORRESPONDING FIELDS OF TABLE lt_vbap
        FROM vbap
        FOR ALL ENTRIES IN lt_vbak
        WHERE vbap~vbeln = lt_vbak-vbeln.
      ENDIF.
      IF lt_vbap IS NOT INITIAL.
        SELECT
        *
        INTO CORRESPONDING FIELDS OF TABLE lt_makt
        FROM makt
        FOR ALL ENTRIES IN lt_vbap
        WHERE makt~matnr = lt_vbap-matnr.
      ENDIF.

      " 下面进行读内表插数据操作
      LOOP AT lt_vbak INTO ls_vbak.
        " 取出前导0
        ls_vbak-kunnr = zcl_bc_public=>conv_by_ddic( i_input = ls_vbak-kunnr i_out = 'X' ).
*      ls_vbak-vbeln = zcl_bc_public=>conv_by_ddic( i_input = ls_vbak-vbeln i_out = 'X' ).
        MOVE-CORRESPONDING ls_vbak TO ls_so.
        "9.29号，黄铠，修改自动匹配字段对应
*        LS_SO-erdat = ls_vbak-erdat."创建日期
        READ TABLE lt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_vbak-kunnr.
        IF sy-subrc = 0.
          ls_so-name = ls_kna1-name1 &&  ls_kna1-name2 && ls_kna1-name3 &&  ls_kna1-name4 .
          ls_so-kukla = ls_kna1-kukla."客户等级
        ENDIF.
        READ TABLE lt_vbkd INTO ls_vbkd WITH KEY vbeln = ls_vbak-vbeln.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_vbkd TO ls_so.
          MOVE ls_vbkd-bstkd_e TO ls_so-zkhpo.
        ENDIF.
        "下面需要往抬头注入item，否则的话是没有数据的。
        CLEAR ls_kna1.
        ls_so-item = lt_item.
        DATA: lt_lines  LIKE TABLE OF tline,
              ls_lines  LIKE tline,
              lt_vbpa   TYPE TABLE OF vbpa,
              ls_vbpa   TYPE vbpa,
              lt_knvv   TYPE TABLE OF knvv,
              ls_knvv   TYPE knvv,
              lt_but000 TYPE TABLE OF but000,
              ls_but000 TYPE but000.
        " 获取ZSCWB
        DATA: i_name LIKE thead-tdname.
        i_name = ls_vbak-vbeln.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
*           CLIENT                  = SY-MANDT
            id                      = 'Z001'
            language                = sy-langu
            name                    = i_name
            object                  = 'VBBK'
*           ARCHIVE_HANDLE          = 0
*           LOCAL_CAT               = ' '
*       IMPORTING
*           HEADER                  =
*           OLD_LINE_COUNTER        =
          TABLES
            lines                   = lt_lines
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
        READ TABLE lt_lines INTO ls_lines INDEX 1.
        ls_so-zscwb = ls_lines-tdline.
        CLEAR ls_lines.
        CLEAR  lt_lines.
        " INCO2_L 字段
        ls_so-inco2_l = ls_vbak-zckgj.
        ls_vbak-kunnr = zcl_bc_public=>conv_by_ddic( i_input = ls_vbak-kunnr ).
        " NAME 字段
        SELECT SINGLE
          *
        INTO CORRESPONDING FIELDS OF ls_kna1
        FROM kna1
        WHERE kna1~kunnr = ls_vbak-kunnr.
        ls_so-name = ls_kna1-name1 && ls_kna1-name2 && ls_kna1-name3 && ls_kna1-name4.
        ls_so-kukla = ls_kna1-kukla.
        " ZZSKKNO
        SELECT SINGLE
          kunnr
        FROM vbpa
        INTO ls_vbpa-kunnr
        WHERE vbpa~vbeln = ls_vbak-vbeln
        AND vbpa~parvw = 'Z1'.
        ls_so-zzskkno = ls_vbpa-kunnr.
        "ZZSKK 字段
        SELECT SINGLE
        *
        INTO CORRESPONDING FIELDS OF ls_but000
        FROM but000
        INNER JOIN vbpa
        ON vbpa~kunnr = but000~partner
        WHERE vbpa~parvw = 'Z1'
          AND but000~partner = ls_vbpa-kunnr.
        ls_so-zzskk = ls_but000-name_org1 && ls_but000-name_org2
                        && ls_but000-name_org3 && ls_but000-name_org4.
        " ZXSQD
        SELECT SINGLE
          *
        INTO CORRESPONDING FIELDS OF ls_knvv
        FROM knvv
        WHERE knvv~kunnr = ls_vbpa-kunnr
        AND   knvv~vkorg = ls_vbak-vkorg
        AND   knvv~vtweg = ls_vbak-vtweg
        AND   knvv~spart = ls_vbak-spart.
        ls_so-zxsqd = ls_vbak-kvgr1.
        SELECT SINGLE
          bezei
        INTO ls_so-zxsqd
        FROM tvv1t
        WHERE tvv1t~spras = '1'
        AND tvv1t~kvgr1 = ls_so-zxsqd.
        CLEAR ls_but000.
        " ZYWY
        SELECT SINGLE
        d~bu_sort1
        INTO CORRESPONDING FIELDS OF ls_but000
        FROM vbak AS a
        JOIN vbap AS b
        ON a~vbeln = b~vbeln
        JOIN vbpa AS c
        ON b~vbeln = c~vbeln  AND c~parvw = 'Z3'
        JOIN but000 AS d
        ON c~kunnr = d~partner
        JOIN kna1 AS e
        ON a~kunnr = e~kunnr
        WHERE a~vbeln = ls_vbak-vbeln.
        ls_so-zywy = ls_but000-bu_sort1.
*&---------------------------------------------------------------------*
*& 程序名：LZSD001U04
*&作者：Seashell Huang
*&模块：
*&创建日期：04.11.2019 13:10:10
*&功能描述：
*&---------------------------------------------------------------------*
*&修改记录：订单号前置0
*&
*&---------------------------------------------------------------------*
        ls_so-vbeln = zcl_bc_public=>conv_by_ddic( i_input = ls_vbak-vbeln ).


        "上面是抬头字段的信息
        "下面是行内容信息
        LOOP AT lt_vbap INTO ls_vbap WHERE vbeln = ls_vbak-vbeln.
          MOVE-CORRESPONDING ls_vbap TO ls_item.
          ls_item-zbhcl = ls_vbap-zbhcl."ZBHCL
          SELECT SINGLE
            *
          INTO CORRESPONDING FIELDS OF ls_vbkd
          FROM vbkd
          WHERE vbkd~posnr = ls_vbap-posnr
          AND vbkd~vbeln = ls_vbap-vbeln.
          MOVE-CORRESPONDING ls_vbkd TO ls_item.
          ls_item-posex_e = ls_vbkd-posex_e.
          MOVE ls_vbap-posnr TO ls_item-posnr.
          IF ls_vbap IS NOT INITIAL.
            READ TABLE lt_makt INTO ls_makt WITH KEY matnr = ls_vbap-matnr.
            MOVE ls_makt-maktx TO ls_item-zcpms.
          ENDIF.
          CLEAR i_name.
          i_name = ls_vbak-vbeln && ls_vbap-posnr.
          " 行项目ZSCHXMBZ 字段
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
*             CLIENT                  = SY-MANDT
              id                      = 'Z001'
              language                = sy-langu
              name                    = i_name
              object                  = 'VBBP'
*             ARCHIVE_HANDLE          = 0
*             LOCAL_CAT               = ' '
*         IMPORTING
*             HEADER                  =
*             OLD_LINE_COUNTER        =
            TABLES
              lines                   = lt_lines
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
          READ TABLE lt_lines INTO ls_lines INDEX 1.
          ls_item-zschxmbz = ls_lines-tdline.

          APPEND ls_item TO lt_item.


          CLEAR ls_item.
          CLEAR ls_vbkd.
          CLEAR ls_vbap.
          CLEAR ls_makt.
        ENDLOOP.
      ENDLOOP.
      ls_so-item = lt_item.
    ENDLOOP.
    IF lt_item IS NOT INITIAL.
      APPEND ls_so TO lt_so.
    ENDIF.

    CLEAR ls_so.

    c_output-so = lt_so.
    IF c_output-so IS NOT INITIAL.
      c_output-message = '成功'.
      c_output-type = 'Y'.
    ENDIF.

  ENDIF.


ENDFORM.