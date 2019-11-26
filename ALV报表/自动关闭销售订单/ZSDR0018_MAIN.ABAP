*&---------------------------------------------------------------------*
*& 包含               ZSDR0018_MAIN
*&---------------------------------------------------------------------*
DATA:lt_aufm  TYPE TABLE OF aufm WITH HEADER LINE,
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

START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.

END-OF-SELECTION.

FORM getdata.
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
   INNER JOIN vbep AS a
   ON a~posnr = d~posnr
   AND a~vbeln = d~vbeln
   FOR ALL ENTRIES IN lt_aufm
   WHERE c~vbeln = lt_aufm-kdauf
   AND d~posnr = lt_aufm-kdpos
   AND A~ettyp NOT LIKE '%ZB%'
   AND c~auart LIKE 'ZA%'.

  CLEAR lt_vbapk.

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
    WHERE c~auart LIKE 'ZA%'
    AND c~kunnr IN s_kunnr
    AND c~ernam IN s_ernam
    AND c~vbeln IN s_vbeln
    AND c~vkorg IN s_vkorg
    AND d~kwmeng <= '1'
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
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-coltext = &2."ALV列名
    fieldcat-col_pos = sy-tabix."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.
  layout-zebra = 'X'.
  layout-box_fname = 'sel'.
  layout-cwidth_opt = 'X'.

  fieldcatset 'VKORG' '销售组织'.
  fieldcatset 'VTWEG' '分销渠道'.
  fieldcatset 'VBELN' '销售订单'.
  fieldcatset 'ERNAM' '创建人'.
  fieldcatset 'KUNNR' '客户'.
  fieldcatset 'NAME_ORG' '客户描述'.
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
ENDFORM.

FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_USER_STATUS'
*     i_callback_user_command  = 'FRM_USER_COMMAND'
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
*     I_SAVE                   = ' '
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
      t_outtab                 = lt_alv
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