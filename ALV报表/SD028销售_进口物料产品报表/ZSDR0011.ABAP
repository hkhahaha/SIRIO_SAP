*&---------------------------------------------------------------------*
*& Report ZSDR0011
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0011.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:mara,mast,stko.
TYPES:BEGIN OF lv_alv,
        matnr   TYPE mara-matnr, "成品物料号
        maktxf  TYPE makt-maktx, "物料类型(成品)
        bom     TYPE string, "子件
        maktx   TYPE makt-maktx, "物料描述（子件）
        zoriadd TYPE string, "原产地
      END OF lv_alv.
DATA:lt_alv  TYPE TABLE OF lv_alv,
     lt_alv2 TYPE TABLE OF lv_alv,
     ls_alv2 TYPE lv_alv,
     ls_alv  TYPE lv_alv.


SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_mtart FOR mara-mtart OBLIGATORY DEFAULT 'Z050',"条件输入框，一个for对应一个
"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
s_datuv FOR stko-datuv DEFAULT sy-datum,
s_matnr FOR mara-matnr.
PARAMETERS s_werks TYPE mast-werks OBLIGATORY.
*s_werks FOR mast-werks OBLIGATORY
SELECTION-SCREEN END OF BLOCK blk.

START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
*定义子程序

FORM getdata.
  DATA:lt_mara TYPE TABLE OF mara,
       ls_mara TYPE mara.
  "成品物料号、物料描述
  SELECT
    mara~matnr
    maktx AS maktxf
  INTO CORRESPONDING FIELDS OF TABLE lt_alv2
  FROM mara
  INNER JOIN makt
  ON mara~matnr = makt~matnr
  WHERE mara~mtart IN s_mtart
    AND mara~matnr IN s_matnr.

  "子件
  LOOP AT lt_alv2 INTO ls_alv2.
    "子件物料号
    DATA:lt_stb LIKE  TABLE OF stpox,
         ls_stb LIKE  stpox.
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
*       FTREL                 = ' '
*       ALEKZ                 = ' '
*       ALTVO                 = ' '
*       AUFSW                 = ' '
*       AUMGB                 = ' '
*       AUMNG                 = 0
*       AUSKZ                 = ' '
*       AMIND                 = ' '
*       BAGRP                 = ' '
*       BEIKZ                 = ' '
*       BESSL                 = ' '
*       BGIXO                 = ' '
*       BREMS                 = ' '
        capid                 = 'PP01'
*       CHLST                 = ' '
*       COSPR                 = ' '
*       CUOBJ                 = 000000000000000
*       CUOVS                 = 0
*       CUOLS                 = ' '
        datuv                 = sy-datum
*       DELNL                 = ' '
*       DRLDT                 = ' '
*       EHNDL                 = ' '
        emeng                 = 1000
*       ERSKZ                 = ' '
*       ERSSL                 = ' '
*       FBSTP                 = ' '
*       KNFBA                 = ' '
*       KSBVO                 = ' '
*       MBWLS                 = ' '
        mktls                 = 'X'
*       MDMPS                 = ' '
        mehrs                 = 'X'
*       MKMAT                 = ' '
*       MMAPS                 = ' '
*       SALWW                 = ' '
*       SPLWW                 = ' '
*       MMORY                 = ' '
        mtnrv                 = ls_alv2-matnr
*       NLINK                 = ' '
*       POSTP                 = ' '
*       RNDKZ                 = ' '
*       RVREL                 = ' '
*       SANFR                 = ' '
*       SANIN                 = ' '
*       SANKA                 = ' '
*       SANKO                 = ' '
*       SANVS                 = ' '
*       SCHGT                 = ' '
*       STKKZ                 = ' '
*       STLAL                 = ' '
*       STLAN                 = ' '
*       STPST                 = 0
        svwvo                 = 'X'
        werks                 = s_werks
*       NORVL                 = ' '
*       MDNOT                 = ' '
*       PANOT                 = ' '
*       QVERW                 = ' '
*       VERID                 = ' '
        vrsvo                 = 'X'
*       SGT_SCAT              =
*       SGT_REL               =
*       CALLER_APP            =
*       BOM_VERSN             =
*     IMPORTING
*       TOPMAT                =
*       DSTST                 =
      TABLES
        stb                   = lt_stb
*       MATCAT                =
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
    IF lt_stb IS NOT INITIAL.
      LOOP AT lt_stb INTO ls_stb.
        "子件物料号
        ls_alv-bom = ls_stb-idnrk.

        "物料描述（子件）
        SELECT SINGLE
        maktx
        INTO ls_alv-maktx
        FROM makt
        WHERE makt~matnr = ls_stb-idnrk
          AND makt~spras = '1'.
        ls_alv-matnr = ls_alv2-matnr.
*        ls_alv-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_alv-matnr i_out = 'X' ).
        ls_alv-maktxf = ls_alv2-maktxf.
        "子件对应的原产地
        SELECT SINGLE
          atwrt
        INTO ls_alv-zoriadd
        FROM ausp
        WHERE ausp~objek = ls_stb-idnrk
          AND ausp~atinn = '0000000027'.

*        IF ls_alv-matnr IS NOT INITIAL AND ls_alv-zoriadd IS NOT INITIAL.
        ls_alv-bom = zcl_bc_public=>conv_by_ddic( i_input = ls_stb-idnrk i_out = 'X' ).
        ls_alv-matnr = zcl_bc_public=>conv_by_ddic( i_input = ls_alv-matnr i_out = 'X' ).
        APPEND ls_alv TO lt_alv.
*        ENDIF.

        CLEAR ls_stb.
*        CLEAR LS_ALV.
      ENDLOOP.

*      CLEAR LS_ALV.
    ENDIF.
    CLEAR ls_alv.
  ENDLOOP.
  "将内表中的BOM重复项去重
  SORT lt_alv.
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING ALL FIELDS.
ENDFORM.


FORM catalog.
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'L'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
*    fieldcat-ref_tabname = 'BKPF'."调用透明表的数据结构
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.

  fieldcatset 'MATNR' '物料编码' sy-tabix.
  fieldcatset 'MAKTXF' '物料描述' sy-tabix.
  fieldcatset 'BOM' '子件物料编码' sy-tabix.
  fieldcatset 'MAKTX' '子件物料描述' sy-tabix.
  fieldcatset 'ZORIADD' '原产地' sy-tabix.
ENDFORM.


FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = w_repid "程序名称
*     I_CALLBACK_PF_STATUS_SET          = 'FRM_SET_PF_STATUS'
*     I_CALLBACK_USER_COMMAND           = 'ALV_USER_COMMAND'"对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     i_grid_title       = '会计凭证'"标题名
*     I_GRID_SETTINGS    =
      is_layout          = layout "程序所定义的layout名称
      it_fieldcat        = fieldcat[] "定义fieldcat数据
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
*     I_SAVE             = ' '
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab           = lt_alv
    EXCEPTIONS "下面都是默认的
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.