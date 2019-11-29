*&---------------------------------------------------------------------*
*& 包含               ZSDR0018_MAIN
*&---------------------------------------------------------------------*
DATA:lt_aufm     TYPE TABLE OF aufm WITH HEADER LINE,
     ls_aufm     TYPE aufm,
     lt_mseg     TYPE TABLE OF mseg,
     lt_mseg2    TYPE TABLE OF mseg,
     ls_mseg     TYPE mseg,
     lt_vbap     TYPE TABLE OF vbap,
     ls_vbap     TYPE vbap,
     lt_vbep     TYPE TABLE OF vbep,
     ls_vbep     TYPE vbep,
     lt_vbak     TYPE TABLE OF vbak,
     ls_vbak     TYPE vbak,
     lt_alv      TYPE TABLE OF ty_alv,
     lt_alv2     TYPE TABLE OF ty_alv,
     ls_alv      TYPE ty_alv,
     lt_vbapk    TYPE TABLE OF ty_vbapk,
     lt_vbapk2   TYPE TABLE OF ty_vbapk,
     ls_vbapk    TYPE ty_vbapk,
     lt_tvakt    TYPE TABLE OF tvakt,
     ls_tvakt    TYPE tvakt,
     lt_but000   TYPE TABLE OF but000,
     ls_but000   TYPE but000,
     lt_mssa     TYPE TABLE OF mssa,
     ls_mssa     TYPE mssa,
     lt_name     TYPE TABLE OF ty_name,
     ls_name     TYPE ty_name,
     lt_zsdr0018 TYPE TABLE OF zsdr0018,
     ls_zsdr0018 TYPE zsdr0018,
     lt_vbfa     TYPE TABLE OF vbfa,
     lt_vbfa2    TYPE TABLE OF vbfa,
     ls_vbfa     TYPE vbfa.

START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  IF sy-batch IS NOT INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_zsdr0018 FROM zsdr0018.
    READ TABLE lt_zsdr0018 INTO ls_zsdr0018 INDEX 1.
    IF ls_zsdr0018-active IS NOT INITIAL.
      PERFORM closedata.
    ELSE.
      EXIT.
    ENDIF.
  ELSE.
    PERFORM catalog.
    PERFORM alvshow.
  ENDIF.

END-OF-SELECTION.

FORM getdata.
  "查询AUFM表里面的数据
  SELECT
    a~budat
    a~bwart
    a~werks
    a~kdauf
    a~mblnr
    a~zeile
    a~kdpos
    b~posnr
    b~matnr
    b~arktx
    b~kwmeng
    b~zieme
    c~vkorg
    c~vtweg
    c~vbeln
    c~ernam
    c~kunnr
    c~auart
    b~umziz
    b~umzin
    INTO CORRESPONDING FIELDS OF TABLE lt_vbapk2
    FROM aufm AS a
    INNER JOIN vbap AS b
    ON b~vbeln = a~kdauf
    AND b~posnr = a~kdpos
    INNER JOIN vbak AS c
    ON c~vbeln = a~kdauf
    WHERE a~budat IN s_budat
    AND a~bwart = '101'
    AND a~werks IN s_werks
    AND a~kdauf <> ''
    AND a~kdauf IN s_vbeln
    AND a~kdpos IN s_posnr
    AND b~posnr IN s_posnr
    AND c~kunnr IN s_kunnr
    AND c~ernam IN s_ernam
    AND c~vkorg IN s_vkorg
    AND c~vtweg IN s_vtweg.

  "获取MSEG表里面的数据，此单结束的订单号和行项
  SELECT
    mblnr
    zeile
    wempf
    INTO CORRESPONDING FIELDS OF TABLE lt_mseg
    FROM mseg
    FOR ALL ENTRIES IN lt_vbapk2
    WHERE mseg~mblnr = lt_vbapk2-mblnr
    AND mseg~zeile = lt_vbapk2-zeile
    AND mseg~wempf <> '此单结束'.

  "获取MSEG表里面的数据，剔除行
  SELECT
    smbln
    smblp
    INTO CORRESPONDING FIELDS OF TABLE lt_mseg2
    FROM mseg.

  "已经关闭的销售订单，VBEP
  SELECT
    vbeln
    posnr
    INTO CORRESPONDING FIELDS OF TABLE lt_vbep
    FROM vbep
    WHERE vbep~ettyp LIKE '%ZB%'.

  "获取销售订单熟练郭小于的销售订单和行项目
  SELECT
    vbak~auart
    vbap~kwmeng
    vbap~vbeln
    vbap~posnr
    kunnr
    vbak~ernam
    vkorg
    vtweg
    vbap~matnr
    arktx
    zieme
    vbap~umziz
    vbap~umzin
    INTO CORRESPONDING FIELDS OF TABLE lt_vbapk
    FROM vbap
    INNER JOIN vbak
    ON vbak~vbeln = vbap~vbeln
*    INNER JOIN aufm
*    ON aufm~kdauf = vbap~vbeln
*    AND aufm~kdpos = vbap~posnr
    WHERE vbap~kwmeng <= '1'
    AND vbap~kwmeng > '0'
    AND vbak~auart LIKE 'ZA%'
    AND vbap~posnr IN s_posnr
    AND vbak~kunnr IN s_kunnr
    AND vbak~ernam IN s_ernam
    AND vbak~vbeln IN s_vbeln
    AND vbak~vkorg IN s_vkorg
    AND vbak~vtweg IN s_vtweg.
*    AND aufm~budat IN s_budat
*    AND aufm~werks IN s_werks.
  LOOP AT lt_vbapk INTO ls_vbapk.
    READ TABLE lt_vbep INTO ls_vbep WITH KEY vbeln = ls_vbapk-vbeln
                                             posnr = ls_vbapk-posnr.
    "如果有找到的话，需要关闭订单
    IF sy-subrc = 0.
      DELETE lt_vbapk WHERE vbeln = ls_vbapk-vbeln.
      CLEAR ls_vbep.
      CONTINUE.
    ENDIF.
  ENDLOOP.
  CLEAR ls_vbapk.
  SORT lt_mseg2 BY smbln smblp.
  SORT lt_mseg BY mblnr zeile.
  LOOP AT lt_vbapk2 INTO ls_vbapk.
    "剔除2个MSEG的表的数据
    READ TABLE lt_mseg2 INTO ls_mseg WITH KEY smbln = ls_vbapk-mblnr
                                              smblp = ls_vbapk-zeile BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE TABLE lt_vbapk2 FROM ls_vbapk.
      CLEAR ls_mseg.
      CONTINUE.
    ENDIF.

    READ TABLE lt_mseg INTO ls_mseg WITH KEY mblnr = ls_vbapk-mblnr
                                             zeile = ls_vbapk-zeile BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE TABLE lt_vbapk2 FROM ls_vbapk.
      CLEAR ls_mseg.
      CONTINUE.
    ENDIF.
  ENDLOOP.
  APPEND LINES OF lt_vbapk2 TO lt_vbapk.
  CLEAR ls_vbapk.
*  LOOP AT lt_vbapk2 INTO ls_vbapk.
*    MOVE-CORRESPONDING ls_vbapk TO ls_alv.
*    APPEND ls_alv TO lt_alv.
*  ENDLOOP.
  MOVE-CORRESPONDING lt_vbapk TO lt_alv.
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING ALL FIELDS.

  SELECT
    vbeln
    posnr
    etenr
    edatu
    ettyp
    INTO CORRESPONDING FIELDS OF TABLE lt_vbep
    FROM vbep
    FOR ALL ENTRIES IN lt_alv
    WHERE vbep~vbeln = lt_alv-vbeln
    AND vbep~posnr = lt_alv-posnr.

  SORT lt_vbep BY vbeln posnr edatu DESCENDING.
  MOVE-CORRESPONDING lt_alv TO lt_alv2.

  "取出全部的凭证类型描述
  SELECT
    auart
    bezei
    INTO CORRESPONDING FIELDS OF TABLE lt_tvakt
    FROM tvakt
    WHERE tvakt~spras = '1'.
  SORT lt_tvakt BY auart.

  "取出客户的描述
  SELECT
    partner
    name_org1
    name_org2
    name_org3
    name_org4
    INTO CORRESPONDING FIELDS OF TABLE lt_but000
    FROM but000
    FOR ALL ENTRIES IN lt_alv
    WHERE but000~partner = lt_alv-kunnr.
  SORT lt_but000 BY partner.

  SELECT
    salab
    vbeln
    posnr
    matnr
    INTO CORRESPONDING FIELDS OF TABLE lt_mssa
    FROM mssa
    FOR ALL ENTRIES IN lt_alv
    WHERE mssa~matnr = lt_alv-matnr
    AND mssa~vbeln = lt_alv-vbeln
    AND mssa~posnr = lt_alv-posnr.
  SORT lt_mssa BY vbeln posnr matnr.

  SELECT
    bname
    name_text
    INTO CORRESPONDING FIELDS OF TABLE lt_name
    FROM adrp
    INNER JOIN usr21
    ON usr21~persnumber = adrp~persnumber
    FOR ALL ENTRIES IN lt_alv
    WHERE usr21~bname = lt_alv-ernam.

  SORT lt_name BY bname.

  LOOP AT lt_alv INTO ls_alv.
    DATA(l_index) = sy-tabix.
    IF ls_alv-umzin IS NOT INITIAL.
      ls_alv-rate = ls_alv-umziz / ls_alv-umzin."转换率
    ELSE.
      ls_alv-rate = ls_alv-umziz / 1."转换率
    ENDIF.

    CLEAR lt_vbfa.
    SELECT
    vbelv
    posnv
    rfmng
    vbtyp_n
    INTO CORRESPONDING FIELDS OF TABLE lt_vbfa
    FROM vbfa
    WHERE vbfa~vbelv = ls_alv-vbeln
    AND vbfa~posnv = ls_alv-posnr.
*    AND vbfa~vbtyp_n IN ( 'R','h' ).
    SORT lt_vbfa BY vbelv posnv vbtyp_n.
    "获取过账数量
    IF lt_vbfa IS NOT INITIAL.
      LOOP AT lt_vbfa INTO ls_vbfa.
        COLLECT ls_vbfa INTO lt_vbfa2.
        CLEAR ls_vbfa.
      ENDLOOP.
      SORT lt_vbfa2 BY vbelv posnv vbtyp_n.
      READ TABLE lt_vbfa2 INTO ls_vbfa WITH KEY vbelv = ls_alv-vbeln
                                                posnv = ls_alv-posnr
                                                vbtyp_n = 'R' BINARY SEARCH.
      ls_alv-number1 = ls_vbfa-rfmng.
      CLEAR ls_vbfa.
      READ TABLE lt_vbfa2 INTO ls_vbfa WITH KEY vbelv = ls_alv-vbeln
                                                posnv = ls_alv-posnr
                                                vbtyp_n = 'h' BINARY SEARCH.
      ls_alv-number1 = ls_alv-number1 - ls_vbfa-rfmng.
    ENDIF.

*    MODIFY TABLE lt_alv2 FROM ls_alv.
    "读取第一条，获取其计划行类别ETTYP
    READ TABLE lt_vbep INTO ls_vbep WITH KEY vbeln = ls_alv-vbeln
                                             posnr = ls_alv-posnr BINARY SEARCH.
    IF sy-subrc = 0.
      ls_alv-ettyp = ls_vbep-ettyp.
      ls_alv-etenr = ls_vbep-etenr.
      CLEAR ls_vbep.
    ENDIF.

    "匹配凭证类型描述
    READ TABLE lt_tvakt INTO ls_tvakt WITH KEY auart = ls_alv-auart BINARY SEARCH.
    IF sy-subrc = 0.
      ls_alv-bezei = ls_tvakt-bezei.
      CLEAR ls_tvakt.
    ENDIF.

    "匹配客户描述
    READ TABLE lt_but000 INTO ls_but000 WITH KEY partner = ls_alv-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      ls_alv-name_org = ls_but000-name_org1 && ls_but000-name_org2 && ls_but000-name_org3 && ls_but000-name_org4.
      CLEAR ls_but000.
    ENDIF.

    "匹配非限制库存
    READ TABLE lt_mssa INTO ls_mssa WITH KEY vbeln = ls_alv-vbeln
                                             posnr = ls_alv-posnr
                                             matnr = ls_alv-matnr
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      ls_alv-number2 = ls_mssa-salab / ls_alv-rate.
      CLEAR ls_mssa.
    ENDIF.

    "匹配用户名
    READ TABLE lt_name INTO ls_name WITH KEY bname = ls_alv-ernam BINARY SEARCH.
    IF sy-subrc = 0.
      ls_alv-name_text = ls_name-name_text.
      CLEAR ls_name.
    ENDIF.

    "转换销售单位
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = ls_alv-zieme
*       LANGUAGE       = SY-LANGU
      IMPORTING
        long_text      = ls_alv-zieme
*       OUTPUT         =
*       SHORT_TEXT     =
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    ls_alv-number3 = ls_alv-number1 + ls_alv-number2.
    ls_alv-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_alv-matnr i_out = 'X' ).
    ls_alv-status = ''.
    MODIFY lt_alv2 FROM ls_alv INDEX l_index.
  ENDLOOP.
ENDFORM.

FORM catalog.
  CLEAR: fieldcat[], sy-tabix.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-coltext = &2."ALV列名
    fieldcat-col_pos = sy-tabix."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.


  fieldcatset 'VKORG' '销售组织'.
  fieldcatset 'VTWEG' '分销渠道'.
  fieldcatset 'VBELN' '销售订单'.
  fieldcatset 'ERNAM' '创建人'.
  fieldcatset 'NAME_TEXT' '创建名称'.
  fieldcatset 'KUNNR' '客户'.
  fieldcatset 'NAME_ORG' '客户描述'.
  fieldcatset 'ETENR' '计划行'.
  fieldcatset 'ETTYP' '计划行类别'.
  fieldcatset 'AUART' '凭证类型'.
  fieldcatset 'BEZEI' '凭证类型描述'.
  fieldcatset 'POSNR' '行项目'.
  fieldcatset 'MATNR' '物料'.
  fieldcatset 'ARKTX' '物料描述'.
  fieldcatset 'KWMENG' '数量'.
  fieldcatset 'NUMBER1' '已过账数量（销售单位'.
  fieldcatset 'NUMBER2' '当前库存数量（销售单位）'.
  fieldcatset 'NUMBER3' '已入库数量'.
*  fieldcatset 'WEMPF' '收货方'.
  fieldcatset 'ZIEME' '销售单位'.
  fieldcatset 'STATUS' '关闭状态'.
  fieldcatset 'STATUS2' '关闭操作详细信息'.
  layout-zebra = 'X'.
  layout-box_fname = 'sel'.
  layout-cwidth_opt = 'X'.
ENDFORM.

FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_USER_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = '关闭订单'
*     I_GRID_SETTINGS          =
      is_layout_lvc            = layout
      it_fieldcat_lvc          = fieldcat[]
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = lt_alv2
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.



FORM frm_user_status  USING i_it_extab TYPE slis_t_extab.
  SET PF-STATUS '0100' .
ENDFORM.                    " FRM_SET_PF_STATUS

FORM frm_user_command USING i_ucomm       TYPE sy-ucomm
      i_wa_selfield TYPE slis_selfield.
  DATA:
    ls_layout TYPE lvc_s_layo,
    lv_grid   TYPE REF TO cl_gui_alv_grid.
  DATA ls_stable TYPE lvc_s_stbl.



  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*   EXPORTING
*     IR_SALV_FULLSCREEN_ADAPTER       =
    IMPORTING
*     ET_EXCLUDING                     =
*     E_REPID                          =
*     E_CALLBACK_PROGRAM               =
*     E_CALLBACK_ROUTINE               =
      e_grid = lv_grid
*     ET_FIELDCAT_LVC                  =
*     ER_TRACE                         =
*     E_FLG_NO_HTML                    =
*     ES_LAYOUT_KKBLO                  =
*     ES_SEL_HIDE                      =
*     ET_EVENT_EXIT                    =
*     ER_FORM_TOL                      =
*     ER_FORM_EOL                      =
    .


*  CALL METHOD lv_grid->check_changed_data.
  CASE i_ucomm.
    WHEN '&POP'.  "click POP button in application toolbar
*      MESSAGE '退出程序' TYPE 'E'.
*      EXIT.
*      RETURN.
      "如果有选上的行，那么对应sel列标记为X，取出标记为X的字段行进行操作
      LOOP AT lt_alv2 INTO ls_alv WHERE sel = 'X'.
        DATA: salesdocument    LIKE  bapivbeln-vbeln,
              order_header_in  LIKE  bapisdh1,
              order_header_inx LIKE  bapisdh1x,


              order_item_in    LIKE  bapisditm  OCCURS 0 WITH HEADER LINE,
              order_item_inx   LIKE  bapisditmx OCCURS 0 WITH HEADER LINE,
              return1          LIKE  bapiret2  OCCURS 0 WITH HEADER LINE,
              ls_return1       LIKE  bapiret2,
              return           LIKE  bapiret2  OCCURS 0 WITH HEADER LINE,
              schedule_lines   LIKE  bapischdl  OCCURS 0 WITH HEADER LINE,
              schedule_linesx  LIKE  bapischdlx OCCURS 0 WITH HEADER LINE.

        CLEAR: salesdocument,order_header_in,order_header_inx,order_item_in,order_item_inx,return1,return,schedule_lines,schedule_linesx.
        CLEAR: order_item_in[],order_item_inx[],return1[],return[],schedule_lines[],schedule_linesx[].
        order_header_inx-updateflag = 'U'.


        order_item_in-itm_number     =  ls_alv-posnr.
        APPEND order_item_in.

        order_item_inx-itm_number     =  ls_alv-posnr.
        order_item_inx-updateflag     =  'U'.
        APPEND order_item_inx.
*
        schedule_lines-itm_number = ls_alv-posnr.
        schedule_lines-sched_line = ls_alv-etenr.
        schedule_lines-sched_type = 'ZB'.
*        schedule_lines-req_date = '20180808'.
*        schedule_lines-req_qty    = '35'. "4.  "数量
        APPEND schedule_lines.
*
        schedule_linesx-itm_number = ls_alv-posnr.
        schedule_linesx-sched_line = ls_alv-etenr.
        schedule_linesx-updateflag = 'U'.
        schedule_linesx-sched_type = 'X'.
*        schedule_linesx-req_qty    = 'X'.
*        schedule_linesx-req_date = 'X'.
*
*        APPEND schedule_linesx.
*        schedule_linesx-itm_number = '000010'.
*        schedule_linesx-updateflag = 'D'.
*        schedule_linesx-sched_type = 'X'.
*        schedule_linesx-req_qty    = 'X'.
*        schedule_linesx-req_date = 'X'.
*
        APPEND schedule_linesx.

**
        CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
          EXPORTING
            salesdocument    = ls_alv-vbeln
            order_header_inx = order_header_inx
          TABLES
            return           = return1
            order_item_in    = order_item_in
            order_item_inx   = order_item_inx
            schedule_lines   = schedule_lines
            schedule_linesx  = schedule_linesx.
        IF sy-subrc = '0'.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          MOVE return1 TO return.
        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        ENDIF.
        "拼接操作的详细信息
        LOOP AT  return1 INTO ls_return1.
          ls_alv-status2 = ls_alv-status2 && ls_return1-message.
          CLEAR ls_return1.
        ENDLOOP.


        IF return-type = 'S' OR return-type = 'W'.
          ls_alv-status = '成功'.
          MODIFY lt_alv2 FROM ls_alv.
        ELSE.
          ls_alv-status = '不成功'.
          MODIFY lt_alv2 FROM ls_alv.
        ENDIF.
      ENDLOOP.

    WHEN OTHERS.

  ENDCASE.
  i_wa_selfield-refresh = 'X'.
  i_wa_selfield-col_stable = 'X'.
  i_wa_selfield-row_stable = 'X'.
  CALL METHOD lv_grid->get_frontend_layout
    IMPORTING
      es_layout = ls_layout.

  ls_layout-cwidth_opt = 'x'.

  CALL METHOD lv_grid->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.
ENDFORM.

FORM closedata.
  LOOP AT lt_alv2 INTO ls_alv.
    DATA: salesdocument    LIKE  bapivbeln-vbeln,
          order_header_in  LIKE  bapisdh1,
          order_header_inx LIKE  bapisdh1x,


          order_item_in    LIKE  bapisditm  OCCURS 0 WITH HEADER LINE,
          order_item_inx   LIKE  bapisditmx OCCURS 0 WITH HEADER LINE,
          return1          LIKE  bapiret2  OCCURS 0 WITH HEADER LINE,
          ls_return1       LIKE  bapiret2,
          return           LIKE  bapiret2  OCCURS 0 WITH HEADER LINE,
          schedule_lines   LIKE  bapischdl  OCCURS 0 WITH HEADER LINE,
          schedule_linesx  LIKE  bapischdlx OCCURS 0 WITH HEADER LINE.

    CLEAR: salesdocument,order_header_in,order_header_inx,order_item_in,order_item_inx,return1,return,schedule_lines,schedule_linesx.
    CLEAR: order_item_in[],order_item_inx[],return1[],return[],schedule_lines[],schedule_linesx[].
    order_header_inx-updateflag = 'U'.


    order_item_in-itm_number     =  ls_alv-posnr.
    APPEND order_item_in.

    order_item_inx-itm_number     =  ls_alv-posnr.
    order_item_inx-updateflag     =  'U'.
    APPEND order_item_inx.
*
    schedule_lines-itm_number = ls_alv-posnr.
    schedule_lines-sched_line = ls_alv-etenr.
    schedule_lines-sched_type = 'ZB'.
*        schedule_lines-req_date = '20180808'.
*        schedule_lines-req_qty    = '35'. "4.  "数量
    APPEND schedule_lines.
*
    schedule_linesx-itm_number = ls_alv-posnr.
    schedule_linesx-sched_line = ls_alv-etenr.
    schedule_linesx-updateflag = 'U'.
    schedule_linesx-sched_type = 'X'.
*        schedule_linesx-req_qty    = 'X'.
*        schedule_linesx-req_date = 'X'.
*
*        APPEND schedule_linesx.
*        schedule_linesx-itm_number = '000010'.
*        schedule_linesx-updateflag = 'D'.
*        schedule_linesx-sched_type = 'X'.
*        schedule_linesx-req_qty    = 'X'.
*        schedule_linesx-req_date = 'X'.
*
    APPEND schedule_linesx.

**
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = ls_alv-vbeln
        order_header_inx = order_header_inx
      TABLES
        return           = return1
        order_item_in    = order_item_in
        order_item_inx   = order_item_inx
        schedule_lines   = schedule_lines
        schedule_linesx  = schedule_linesx.
    IF sy-subrc = '0'.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      MOVE return1 TO return.
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ENDIF.
  ENDLOOP.
ENDFORM.