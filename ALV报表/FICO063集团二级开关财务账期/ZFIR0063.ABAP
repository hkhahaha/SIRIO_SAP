*&---------------------------------------------------------------------*
*& Report ZFIR0063
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0063.
TYPE-POOLS:slis."调用系统存在的类型池
DATA:fieldcat TYPE lvc_t_fcat WITH HEADER LINE, "调用系统存在的FIELDCAT类型。
     layout   TYPE lvc_s_layo, "调用系统存在的LAYOUT
     "FIELDCAT用于ALV的结构定义，LAYOUT用于定义ALV的输出格式，后者可以覆盖前者，后者是可选的
     w_repid  TYPE sy-repid. "记录系统当前的程序名
DATA:lv_guid TYPE sxmsguid,
     lv_grid TYPE REF TO cl_gui_alv_grid.
TABLES:zfit0063,t001b,zfit0063a.
TYPES:BEGIN OF ty_zfit0063,
        field_style TYPE lvc_t_styl,
        sel(1).

        INCLUDE TYPE zfit0063.
      TYPES END OF ty_zfit0063.

DATA:lt_zfit0063      TYPE TABLE OF ty_zfit0063, "用于ALV输出
     lt_zfit0063b     TYPE TABLE OF ty_zfit0063, "临时的中间表
     lt_zfit0063_top  TYPE TABLE OF ty_zfit0063, "集团数据对照表
*     lt_zfit0063_detail TYPE TABLE OF ty_zfit0063,
     lt_zfit0063_temp TYPE TABLE OF ty_zfit0063, "完整的数据库数据
     ls_zfit0063      LIKE LINE OF lt_zfit0063,
     ls_zfit0063_temp LIKE LINE OF lt_zfit0063,
     ls_zfit0063_top  LIKE LINE OF lt_zfit0063,
     lt_zfit0063a     TYPE TABLE OF zfit0063a,
     ls_zfit0063a     LIKE LINE OF lt_zfit0063a, "与zfit0063a表相同的结构
     lt_temp          TYPE TABLE OF zfit0063,
     ls_temp          LIKE LINE OF lt_temp, "与t001b表结构相同的工作区
     lt_t001b         LIKE TABLE OF t001b,
     lt_t001b2        TYPE TABLE OF t001b, "该内表已弃用
     ls_t001b         LIKE LINE OF lt_t001b.
DATA gv_sel_error TYPE flag.
DATA stylelin TYPE lvc_s_styl.

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
*SELECT-OPTIONS:s_bukrs FOR t001b-bukrs OBLIGATORY. "条件输入框，一个for对应一个
PARAMETERS:p_bukrs TYPE t001b-bukrs OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk.

START-OF-SELECTION.
*调用子程序
  PERFORM frm_authority_check."权限控制



  IF gv_sel_error <> 'X'.
    PERFORM getdata.
    PERFORM catalog.
    PERFORM alvshow.
  ENDIF.


*定义子程序
FORM getdata."查询语句
  DATA:varkey  TYPE  rstable-varkey,
       lv_name TYPE sy-uname.
  varkey = sy-sysid && '0' && p_bukrs.
  "锁对象，条件是根据当前系统的Id + 0 +账期公式作为拼接条件，存储在VARKEY参数中
  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
*     MODE_RSTABLE   = 'E'
      tabname        = 'T001B'
      varkey         = varkey
*     X_TABNAME      = ' '
*     X_VARKEY       = ' '
*     _SCOPE         = '2'
*     _WAIT          = ' '
*     _COLLECT       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
*  IF sy-subrc = 0.
** Implement suitable error handling here
*    lv_name = sy-uname.
*    SET PARAMETER ID 'YTEST' FIELD lv_name.
*  ELSE.
*    GET PARAMETER ID 'YTEST' FIELD lv_name.
*    MESSAGE '该账期变式账期被' && lv_name  && '锁定' TYPE 'E'.
*  ENDIF.

  CLEAR:lt_zfit0063,lt_zfit0063_temp,lt_zfit0063_top,lt_t001b.
  IF p_bukrs = '0000'.
    "集团的数据
    SELECT
         *
    FROM zfit0063 INTO CORRESPONDING FIELDS OF TABLE lt_zfit0063
    WHERE bukrs = p_bukrs.
  ELSEIF p_bukrs <> '0000'.

    "账期变式的数据，公司
    SELECT
       *
    FROM t001b INTO CORRESPONDING FIELDS OF TABLE lt_zfit0063
    WHERE bukrs = p_bukrs.
  ENDIF.

  "集团账期数据中间表
  SELECT
  *
  FROM zfit0063 INTO CORRESPONDING FIELDS OF TABLE lt_zfit0063_temp.
  "公司所在的表
  SELECT
   *
  FROM t001b INTO CORRESPONDING FIELDS OF TABLE lt_t001b.

  "修改编辑状态
  LOOP AT lt_zfit0063 INTO ls_zfit0063.
    stylelin-fieldname = 'MKOAR'.
    stylelin-style = cl_gui_alv_grid=>mc_style_disabled.
    APPEND stylelin TO ls_zfit0063-field_style.
    MODIFY lt_zfit0063 FROM ls_zfit0063.
    CLEAR ls_zfit0063.
  ENDLOOP.
  MOVE lt_zfit0063 TO lt_zfit0063b.
  SORT lt_zfit0063 BY mkoar.
ENDFORM.

FORM catalog."表头
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 12."自定义字段的长度
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-coltext = &2."ALV列名
    fieldcat-edit = &3."编辑状态
    fieldcat-col_pos = &4."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.
  fieldcatset 'BUKRS' '账期变式代码' '' sy-tabix.
  fieldcatset 'MKOAR' '帐户类型' 'X' sy-tabix.
  fieldcatset 'VKONT' '起始科目' '' sy-tabix.
  fieldcatset 'BKONT' '终止科目' '' sy-tabix.
  fieldcatset 'FRPE1' '起始期间1' 'X' sy-tabix.
  fieldcatset 'FRYE1' '起始年度1' 'X' sy-tabix.
  fieldcatset 'TOPE1' '终止期间1' 'X' sy-tabix.
  fieldcatset 'TOYE1' '终止年度1' 'X' sy-tabix.

  fieldcatset 'FRPE2' '起始期间2' 'X' sy-tabix.
  fieldcatset 'FRYE2' '起始年度2' 'X' sy-tabix.
  fieldcatset 'TOPE2' '终止期间2' 'X' sy-tabix.
  fieldcatset 'TOYE2' '终止年度2' 'X' sy-tabix.

  fieldcatset 'FRPE3' '起始期间3' 'X' sy-tabix.
  fieldcatset 'FRYE3' '起始年度3' 'X' sy-tabix.
  fieldcatset 'TOPE3' '终止期间3' 'X' sy-tabix.
  fieldcatset 'TOYE3' '终止年度3' 'X' sy-tabix.

  READ TABLE fieldcat INDEX 1.
*  fieldcat-ref_table = 'T001B'."调用透明表的数据结构
*  fieldcat-ref_field = 'BUKRS'."调用透明表的数据结构
*  fieldcat-edit = ' '.
  MODIFY fieldcat INDEX 1.

  READ TABLE fieldcat INDEX 2.
  fieldcat-ref_table = 'T001B'."调用透明表的数据结构
  fieldcat-ref_field = 'MKOAR'."调用透明表的数据结构
  MODIFY fieldcat INDEX 2.





*layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'."斑马线的样式
*  layout-edit = 'X'."可编辑
*  layout-edit_mode = 'A'.
  layout-box_fname = 'sel'.
*  layout-colwidth_optimize = 'X'.
  layout-stylefname = 'FIELD_STYLE'.

ENDFORM.
*响应鼠标点击时间的子程序
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
*  CASE r_ucomm.
*    WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
*      READ TABLE lbkpf INDEX rs_selfield-tabindex.
*      SET PARAMETER ID 'BLN' FIELD lbkpf-belnr.
*      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*  ENDCASE.
  PERFORM newdata USING rs_selfield.

ENDFORM.

FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.

  IF p_bukrs = '0000'.
    SET PF-STATUS 'ZHK'.
  ELSE.
    SET PF-STATUS 'ZHK2'.
  ENDIF.

ENDFORM.



FORM alvshow.
  IF p_bukrs = '0000'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
*       I_INTERFACE_CHECK        = ' '
*       I_BYPASSING_BUFFER       =
*       I_BUFFER_ACTIVE          =
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
        i_callback_user_command  = 'ALV_USER_COMMAND'
*       I_CALLBACK_TOP_OF_PAGE   = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME         =
*       I_BACKGROUND_ID          = ' '
        i_grid_title             = '集团账期'
*       I_GRID_SETTINGS          =
        is_layout_lvc            = layout
        it_fieldcat_lvc          = fieldcat[]
*       IT_EXCLUDING             =
*       IT_SPECIAL_GROUPS_LVC    =
*       IT_SORT_LVC              =
*       IT_FILTER_LVC            =
*       IT_HYPERLINK             =
*       IS_SEL_HIDE              =
        i_default                = 'X'
        i_save                   = 'A'
*       IS_VARIANT               =
*       IT_EVENTS                =
*       IT_EVENT_EXIT            =
*       IS_PRINT_LVC             =
*       IS_REPREP_ID_LVC         =
*       I_SCREEN_START_COLUMN    = 0
*       I_SCREEN_START_LINE      = 0
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE        = 0
*       I_HTML_HEIGHT_TOP        =
*       I_HTML_HEIGHT_END        =
*       IT_ALV_GRAPHICS          =
*       IT_EXCEPT_QINFO_LVC      =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER  =
*       ES_EXIT_CAUSED_BY_USER   =
      TABLES
        t_outtab                 = lt_zfit0063
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
*       I_INTERFACE_CHECK        = ' '
*       I_BYPASSING_BUFFER       =
*       I_BUFFER_ACTIVE          =
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
        i_callback_user_command  = 'ALV_USER_COMMAND'
*       I_CALLBACK_TOP_OF_PAGE   = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME         =
*       I_BACKGROUND_ID          = ' '
        i_grid_title             = '子公司账期'
*       I_GRID_SETTINGS          =
        is_layout_lvc            = layout
        it_fieldcat_lvc          = fieldcat[]
*       IT_EXCLUDING             =
*       IT_SPECIAL_GROUPS_LVC    =
*       IT_SORT_LVC              =
*       IT_FILTER_LVC            =
*       IT_HYPERLINK             =
*       IS_SEL_HIDE              =
        i_default                = 'X'
        i_save                   = 'A'
*       IS_VARIANT               =
*       IT_EVENTS                =
*       IT_EVENT_EXIT            =
*       IS_PRINT_LVC             =
*       IS_REPREP_ID_LVC         =
*       I_SCREEN_START_COLUMN    = 0
*       I_SCREEN_START_LINE      = 0
*       I_SCREEN_END_COLUMN      = 0
*       I_SCREEN_END_LINE        = 0
*       I_HTML_HEIGHT_TOP        =
*       I_HTML_HEIGHT_END        =
*       IT_ALV_GRAPHICS          =
*       IT_EXCEPT_QINFO_LVC      =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER  =
*       ES_EXIT_CAUSED_BY_USER   =
      TABLES
        t_outtab                 = lt_zfit0063
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.


ENDFORM.

FORM newdata USING is_selfield TYPE slis_selfield.
  DEFINE  message_set.
    MESSAGE '集团账户类型【' && ls_zfit0063_top-mkoar && '】'
            && '账期为【' && ls_zfit0063_top-frye1 && '】年'
            && '【' && ls_zfit0063_top-frpe1 && '】月 - '
            && '【' && ls_zfit0063_top-toye1 && '】年'
            && '【' && ls_zfit0063_top-tope1 && '】月' TYPE 'E'.
  END-OF-DEFINITION.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lv_grid.
  IF sy-ucomm = '&NEW'.

    CLEAR:ls_zfit0063,lt_zfit0063.
    ls_zfit0063-bkont = 'ZZZZZZZZZZ'.
    ls_zfit0063-bukrs = p_bukrs.
    ls_zfit0063-rrcty = '0'.
    APPEND ls_zfit0063 TO lt_zfit0063.

    is_selfield-refresh = 'X'.
    is_selfield-col_stable = 'X'.
    is_selfield-row_stable = 'X'.
    CALL METHOD lv_grid->get_frontend_layout
      IMPORTING
        es_layout = layout.

    CALL METHOD lv_grid->set_frontend_layout
      EXPORTING
        is_layout = layout.

  ELSEIF sy-ucomm = '&SAV' OR sy-ucomm = '&SAVE'.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = lv_grid.
    CALL METHOD lv_grid->check_changed_data.
    "首先先检查数据是否正确
    CLEAR lt_zfit0063_top.
    "判断是否有按回车
    LOOP AT lt_zfit0063 INTO ls_zfit0063 WHERE bukrs IS INITIAL OR mkoar IS INITIAL.
      MESSAGE '亲，可能没有在保存前按下回车键对吧？' TYPE 'E'.
    ENDLOOP.

    "取出集团数据
    LOOP AT lt_zfit0063_temp INTO ls_zfit0063_temp WHERE bukrs = '0000'.
      APPEND ls_zfit0063_temp TO lt_zfit0063_top.
      CLEAR ls_zfit0063_temp.
    ENDLOOP.
    IF lt_zfit0063_top IS INITIAL.
      READ TABLE lt_zfit0063 INTO ls_zfit0063 WITH KEY bukrs = '0000'.
      IF sy-subrc <> 0.
        MESSAGE '请先维护集团数据' TYPE 'E'.
      ENDIF.
    ENDIF.
    "对新增数据进行初步格式判断
    LOOP AT lt_zfit0063 INTO ls_zfit0063.
      IF ls_zfit0063-frye1 > ls_zfit0063-toye1.
        MESSAGE '终止账期1不允许小于起始账期1（年）' TYPE 'E'.
      ELSEIF ls_zfit0063-frye1 = ls_zfit0063-toye1.
        IF ls_zfit0063-frpe1 > ls_zfit0063-tope1.
          MESSAGE '终止账期1不允许小于起始账期1（月）' TYPE 'E'.
        ENDIF.
      ENDIF.

      IF ls_zfit0063-frye2 > ls_zfit0063-toye2.
        MESSAGE '终止账期2不允许小于起始账期2（年）' TYPE 'E'.
      ELSEIF ls_zfit0063-frye2 = ls_zfit0063-toye2.
        IF ls_zfit0063-frpe2 > ls_zfit0063-tope2.
          MESSAGE '终止账期2不允许小于起始账期2（月）' TYPE 'E'.
        ENDIF.
      ENDIF.

      IF ls_zfit0063-frye3 > ls_zfit0063-toye3.
        MESSAGE '终止账期3不允许小于起始账期3（年）' TYPE 'E'.
      ELSEIF ls_zfit0063-frye3 = ls_zfit0063-toye3.
        IF ls_zfit0063-frpe3 > ls_zfit0063-tope3.
          MESSAGE '终止账期3不允许小于起始账期3（月）' TYPE 'E'.
        ENDIF.
      ENDIF.
      CLEAR ls_zfit0063.
    ENDLOOP.



    "判断账期变式的数据是否在集团内
    LOOP AT lt_zfit0063 INTO ls_zfit0063 WHERE bukrs <> '0000'."循环遍历账期变式的数据
      READ TABLE lt_zfit0063_top INTO ls_zfit0063_top WITH KEY mkoar = ls_zfit0063-mkoar."读取集团数据
      IF sy-subrc <> 0.
        MESSAGE '对应账户类型下没有先维护好集团的数据，请先维护集团数据喔！异常类型：' && ls_zfit0063-mkoar  TYPE 'E'.
      ELSEIF sy-subrc = 0.
        "判断账期变式的数据是否在集团的区间下面
        "(1)判断时间区间是否正确
        "第一个
        IF ls_zfit0063-toye1 >= ls_zfit0063-frye1.
          IF ls_zfit0063-toye1 = ls_zfit0063-frye1 AND ls_zfit0063-frpe1 > ls_zfit0063-tope1.
            MESSAGE '同个会计年度，终止期间1 < 起始期间1' TYPE 'E'.
          ENDIF.

          "起始的时间
          IF ls_zfit0063-frye1 < ls_zfit0063_top-frye1.
            message_set.
          ELSEIF ls_zfit0063-frye1 = ls_zfit0063_top-frye1.
            IF ls_zfit0063-frpe1 < ls_zfit0063_top-frpe1.
              message_set.
            ENDIF.
          ENDIF.
          "终止的时间
          IF ls_zfit0063-toye1 > ls_zfit0063_top-toye1.
            message_set.
          ELSEIF ls_zfit0063-toye1 = ls_zfit0063_top-toye1.
            IF ls_zfit0063-tope1 > ls_zfit0063_top-tope1.
              message_set.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE '终止会计年度1 > 起始会计年度1' TYPE 'E'.
        ENDIF.
        "第二个
        IF ls_zfit0063-toye2 >= ls_zfit0063-frye2.
          IF ls_zfit0063-toye2 = ls_zfit0063-frye2 AND ls_zfit0063-frpe2 > ls_zfit0063-tope2.
            MESSAGE '同个会计年度，终止期间2 < 起始期间2' TYPE 'E'.
          ENDIF.

          "起始的时间
          IF ls_zfit0063-frye2 < ls_zfit0063_top-frye2.
            message_set.
          ELSEIF ls_zfit0063-frye2 = ls_zfit0063_top-frye2.
            IF ls_zfit0063-frpe2 < ls_zfit0063_top-frpe2.
              message_set.
            ENDIF.
          ENDIF.
          "终止的时间
          IF ls_zfit0063-toye2 > ls_zfit0063_top-toye2.
            message_set.
          ELSEIF ls_zfit0063-toye2 = ls_zfit0063_top-toye2.
            IF ls_zfit0063-tope2 > ls_zfit0063_top-tope2.
              message_set.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE '终止会计年度2 > 起始会计年度2' TYPE 'E'.
        ENDIF.

        "第三个
        IF ls_zfit0063-toye3 >= ls_zfit0063-frye3.
          IF ls_zfit0063-toye3 = ls_zfit0063-frye3 AND ls_zfit0063-frpe3 > ls_zfit0063-tope3.
            MESSAGE '同个会计年度，终止期间3 < 起始期间3' TYPE 'E'.
          ENDIF.

          "起始的时间
          IF ls_zfit0063-frye3 < ls_zfit0063_top-frye3.
            message_set.
          ELSEIF ls_zfit0063-frye3 = ls_zfit0063_top-frye3.
            IF ls_zfit0063-frpe3 < ls_zfit0063_top-frpe3.
              message_set.
            ENDIF.
          ENDIF.
          "终止的时间
          IF ls_zfit0063-toye3 > ls_zfit0063_top-toye3.
            message_set.
          ELSEIF ls_zfit0063-toye3 = ls_zfit0063_top-toye3.
            IF ls_zfit0063-tope3 > ls_zfit0063_top-tope3.
              message_set.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE '终止会计年度3 > 起始会计年度3' TYPE 'E'.
        ENDIF.
      ENDIF.
      CLEAR:ls_zfit0063,ls_zfit0063_top.
    ENDLOOP.

    "当维护的是集团数据的时候，修改集团的账期顺带修改对应类型的子公司账期





    "对数据库进行写入操作
    "账期变式写入T001B表
    LOOP AT lt_zfit0063 INTO ls_zfit0063 WHERE bukrs <> '0000'.
      IF ls_zfit0063-guid IS INITIAL OR ls_zfit0063-guid = '0'.
        CLEAR lv_guid.
        lv_guid = zcl_bc_public=>get_guid( ).
      ENDIF.
      MOVE-CORRESPONDING ls_zfit0063 TO ls_t001b.
      ls_zfit0063a-ztype = '新增'.
      INSERT t001b FROM ls_t001b.
      IF sy-subrc = 0.
        COMMIT WORK.
        MODIFY lt_t001b FROM ls_t001b TRANSPORTING rrcty bukrs mkoar bkont
                                      WHERE bukrs = ls_t001b-bukrs
                                      AND   rrcty = ls_t001b-rrcty
                                      AND   mkoar = ls_t001b-mkoar
                                      AND   bkont = ls_t001b-bkont.
      ELSE.
        ls_zfit0063a-ztype = '修改'.
        UPDATE t001b FROM ls_t001b.
        COMMIT WORK.
        MODIFY lt_t001b FROM ls_t001b TRANSPORTING rrcty bukrs mkoar bkont
                                      WHERE bukrs = ls_t001b-bukrs
                                      AND   rrcty = ls_t001b-rrcty
                                      AND   mkoar = ls_t001b-mkoar
                                      AND   bkont = ls_t001b-bkont.
      ENDIF.
      "将数据写入日志表
      MOVE-CORRESPONDING ls_zfit0063 TO ls_zfit0063a.
      ls_zfit0063a-usnam = sy-uname.
      ls_zfit0063a-cpudt = sy-datum.
      ls_zfit0063a-cputm = sy-uzeit.
      ls_zfit0063a-seqno = lv_guid.
      MODIFY zfit0063a FROM ls_zfit0063a.
      CLEAR:ls_zfit0063,lv_guid,ls_zfit0063a.
    ENDLOOP.
    IF sy-subrc = 0.
      MESSAGE '保存成功' TYPE 'S'.
    ELSE.
      "集团写入自定义表
      CLEAR lt_zfit0063b.
      LOOP AT lt_zfit0063 INTO ls_zfit0063 WHERE bukrs = '0000'.
        IF ls_zfit0063-guid IS INITIAL OR ls_zfit0063-guid = '0'.
          CLEAR lv_guid.
          lv_guid = zcl_bc_public=>get_guid( ).
        ENDIF.
        MOVE-CORRESPONDING ls_zfit0063 TO ls_temp.
        ls_temp-guid = lv_guid.
        ls_temp-mandt = sy-mandt.
        ls_zfit0063a-ztype = '新增'.
        INSERT zfit0063 FROM ls_temp.
        IF sy-subrc = 0.
          COMMIT WORK.
*          IF lt_zfit0063_temp IS INITIAL.
*            APPEND ls_zfit0063  TO lt_zfit0063_temp.
*          ENDIF.

          "判断公司的起始账期是否在集团账期内，如果没有的话就修改成集团账期
          LOOP AT lt_t001b INTO ls_t001b WHERE mkoar = ls_zfit0063-mkoar.
*          READ TABLE lt_t001b INTO ls_t001b WITH KEY mkoar = ls_zfit0063-mkoar.
            IF sy-subrc = 0.
              "第一个
              "判断全部账期
              IF ls_t001b-frye1 < ls_zfit0063-frye1 AND ls_t001b-toye1 < ls_zfit0063-toye1 OR ls_t001b-frye1 > ls_zfit0063-frye1 AND ls_t001b-toye1 > ls_zfit0063-toye1.
                ls_t001b-frye1 = ls_zfit0063-frye1.
                ls_t001b-frpe1 = ls_zfit0063-frpe1.
                ls_t001b-toye1 = ls_zfit0063-toye1.
                ls_t001b-tope1 = ls_zfit0063-tope1.
              ENDIF.
              "判断起始账期
              IF ls_zfit0063-frye1 > ls_t001b-frye1.
                ls_t001b-frye1 = ls_zfit0063-frye1.
                ls_t001b-frpe1 = ls_zfit0063-frpe1.
              ELSEIF ls_zfit0063-frye1 = ls_t001b-frye1.
                IF ls_zfit0063-frye1 > ls_t001b-frye1.
                  ls_t001b-frye1 = ls_zfit0063-frye1.
                ENDIF.
              ENDIF.

              "判断终止账期
              IF ls_zfit0063-toye1 < ls_t001b-toye1.
                ls_t001b-toye1 = ls_zfit0063-toye1.
                ls_t001b-tope1 = ls_zfit0063-tope1.
                IF ls_t001b-tope1 < ls_t001b-frpe1.
                  ls_t001b-tope1 = ls_zfit0063-tope1.
                ENDIF.
              ELSEIF ls_zfit0063-toye1 = ls_t001b-toye1.
                IF ls_zfit0063-tope1 < ls_t001b-tope1.
                  ls_t001b-tope1 = ls_zfit0063-tope1.
                ENDIF.
              ENDIF.
              IF ls_t001b-frye1 = ls_t001b-toye1.
                IF ls_t001b-tope1 < ls_t001b-frpe1.
                  ls_t001b-tope1 = ls_zfit0063-tope1.
                ENDIF.
              ENDIF.

              "第二个
              "判断全部账期
              IF ls_t001b-frye2 < ls_zfit0063-frye2 AND ls_t001b-toye2 < ls_zfit0063-toye2 OR ls_t001b-frye2 > ls_zfit0063-frye2 AND ls_t001b-toye2 > ls_zfit0063-toye2.
                ls_t001b-frye2 = ls_zfit0063-frye2.
                ls_t001b-frpe2 = ls_zfit0063-frpe2.
                ls_t001b-toye2 = ls_zfit0063-toye2.
                ls_t001b-tope2 = ls_zfit0063-tope2.
              ENDIF.
              "判断起始账期
              IF ls_zfit0063-frye2 > ls_t001b-frye2.
                ls_t001b-frye2 = ls_zfit0063-frye2.
                ls_t001b-frpe2 = ls_zfit0063-frpe2.
              ELSEIF ls_zfit0063-frye2 = ls_t001b-frye2.
                IF ls_zfit0063-frye2 > ls_t001b-frye2.
                  ls_t001b-frye2 = ls_zfit0063-frye2.
                ENDIF.
              ENDIF.

              "判断终止账期
              IF ls_zfit0063-toye2 < ls_t001b-toye2.
                ls_t001b-toye2 = ls_zfit0063-toye2.
                ls_t001b-tope2 = ls_zfit0063-tope2.
                IF ls_t001b-tope2 < ls_t001b-frpe2.
                  ls_t001b-tope2 = ls_zfit0063-tope2.
                ENDIF.
              ELSEIF ls_zfit0063-toye2 = ls_t001b-toye2.
                IF ls_zfit0063-tope2 < ls_t001b-tope2.
                  ls_t001b-tope2 = ls_zfit0063-tope2.
                ENDIF.
              ENDIF.
              IF ls_t001b-frye2 = ls_t001b-toye2.
                IF ls_t001b-tope2 < ls_t001b-frpe2.
                  ls_t001b-tope2 = ls_zfit0063-tope2.
                ENDIF.
              ENDIF.

              "第三个
              "判断全部账期
              IF ls_t001b-frye3 < ls_zfit0063-frye3 AND ls_t001b-toye3 < ls_zfit0063-toye3 OR ls_t001b-frye3 > ls_zfit0063-frye3 AND ls_t001b-toye3 > ls_zfit0063-toye3.
                ls_t001b-frye3 = ls_zfit0063-frye3.
                ls_t001b-frpe3 = ls_zfit0063-frpe3.
                ls_t001b-toye3 = ls_zfit0063-toye3.
                ls_t001b-tope3 = ls_zfit0063-tope3.
              ENDIF.
              "判断起始账期
              IF ls_zfit0063-frye3 > ls_t001b-frye3.
                ls_t001b-frye3 = ls_zfit0063-frye3.
                ls_t001b-frpe3 = ls_zfit0063-frpe3.
              ELSEIF ls_zfit0063-frye3 = ls_t001b-frye3.
                IF ls_zfit0063-frye3 > ls_t001b-frye3.
                  ls_t001b-frye3 = ls_zfit0063-frye3.
                ENDIF.
              ENDIF.

              "判断终止账期
              IF ls_zfit0063-toye3 < ls_t001b-toye3.
                ls_t001b-toye3 = ls_zfit0063-toye3.
                ls_t001b-tope3 = ls_zfit0063-tope3.
                IF ls_t001b-tope3 < ls_t001b-frpe3.
                  ls_t001b-tope3 = ls_zfit0063-tope3.
                ENDIF.
              ELSEIF ls_zfit0063-toye3 = ls_t001b-toye3.
                IF ls_zfit0063-tope3 < ls_t001b-tope3.
                  ls_t001b-tope3 = ls_zfit0063-tope3.
                ENDIF.
              ENDIF.
              IF ls_t001b-frye3 = ls_t001b-toye3.
                IF ls_t001b-tope3 < ls_t001b-frpe3.
                  ls_t001b-tope3 = ls_zfit0063-tope3.
                ENDIF.
              ENDIF.



              UPDATE t001b FROM ls_t001b.
              IF sy-subrc = 0.
                COMMIT WORK.
                MODIFY lt_t001b FROM ls_t001b TRANSPORTING rrcty bukrs mkoar bkont
                                      WHERE bukrs = ls_t001b-bukrs
                                      AND   rrcty = ls_t001b-rrcty
                                      AND   mkoar = ls_t001b-mkoar
                                      AND   bkont = ls_t001b-bkont.
              ELSE.
                MESSAGE '数据插入T001异常' TYPE 'E'.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          ls_zfit0063a-ztype = '修改'.
          UPDATE zfit0063 FROM ls_temp.
          COMMIT WORK.
          "判断公司的起始账期是否在集团账期内，如果没有的话就修改成集团账期
*          MOVE-CORRESPONDING lt_t001b TO lt_t001b2.
          LOOP AT lt_t001b INTO ls_t001b WHERE mkoar = ls_zfit0063-mkoar.
*          READ TABLE lt_t001b INTO ls_t001b WITH KEY mkoar = ls_zfit0063-mkoar.
            IF sy-subrc = 0.
              "第一个
              "判断全部账期
              IF ls_t001b-frye1 < ls_zfit0063-frye1 AND ls_t001b-toye1 < ls_zfit0063-toye1 OR ls_t001b-frye1 > ls_zfit0063-frye1 AND ls_t001b-toye1 > ls_zfit0063-toye1.
                ls_t001b-frye1 = ls_zfit0063-frye1.
                ls_t001b-frpe1 = ls_zfit0063-frpe1.
                ls_t001b-toye1 = ls_zfit0063-toye1.
                ls_t001b-tope1 = ls_zfit0063-tope1.
              ENDIF.
              "判断起始账期
              IF ls_zfit0063-frye1 > ls_t001b-frye1.
                ls_t001b-frye1 = ls_zfit0063-frye1.
                ls_t001b-frpe1 = ls_zfit0063-frpe1.
              ELSEIF ls_zfit0063-frye1 = ls_t001b-frye1.
                IF ls_zfit0063-frpe1 > ls_t001b-frpe1.
                  ls_t001b-frpe1 = ls_zfit0063-frpe1.
                ENDIF.
              ENDIF.
              "判断终止账期
              IF ls_zfit0063-toye1 < ls_t001b-toye1.
                ls_t001b-toye1 = ls_zfit0063-toye1.
                ls_t001b-tope1 = ls_zfit0063-tope1.
                IF ls_t001b-toye1 < ls_t001b-frye1.
                  ls_t001b-toye1 = ls_zfit0063-toye1.
                ENDIF.
              ELSEIF ls_zfit0063-toye1 = ls_t001b-toye1.
                IF ls_zfit0063-tope1 < ls_t001b-tope1.
                  ls_t001b-tope1 = ls_zfit0063-tope1.
                ENDIF.
              ENDIF.
              IF ls_t001b-frye1 = ls_t001b-toye1.
                IF ls_t001b-tope1 < ls_t001b-frpe1.
                  ls_t001b-tope1 = ls_zfit0063-tope1.
                ENDIF.
              ENDIF.

              "第2个
              "判断全部账期
              IF ls_t001b-frye2 < ls_zfit0063-frye2 AND ls_t001b-toye2 < ls_zfit0063-toye2 OR ls_t001b-frye2 > ls_zfit0063-frye2 AND ls_t001b-toye2 > ls_zfit0063-toye2.
                ls_t001b-frye2 = ls_zfit0063-frye2.
                ls_t001b-frpe2 = ls_zfit0063-frpe2.
                ls_t001b-toye2 = ls_zfit0063-toye2.
                ls_t001b-tope2 = ls_zfit0063-tope2.
              ENDIF.
              "判断起始账期
              IF ls_zfit0063-frye2 > ls_t001b-frye2.
                ls_t001b-frye2 = ls_zfit0063-frye2.
                ls_t001b-frpe2 = ls_zfit0063-frpe2.
              ELSEIF ls_zfit0063-frye2 = ls_t001b-frye2.
                IF ls_zfit0063-frpe2 > ls_t001b-frpe2.
                  ls_t001b-frpe2 = ls_zfit0063-frpe2.
                ENDIF.
              ENDIF.
              "判断终止账期
              IF ls_zfit0063-toye2 < ls_t001b-toye2.
                ls_t001b-toye2 = ls_zfit0063-toye2.
                ls_t001b-tope2 = ls_zfit0063-tope2.
                IF ls_t001b-toye2 < ls_t001b-frye2.
                  ls_t001b-toye2 = ls_zfit0063-toye2.
                ENDIF.
              ELSEIF ls_zfit0063-toye2 = ls_t001b-toye2.
                IF ls_zfit0063-tope2 < ls_t001b-tope2.
                  ls_t001b-tope2 = ls_zfit0063-tope2.
                ENDIF.
              ENDIF.
              IF ls_t001b-frye2 = ls_t001b-toye2.
                IF ls_t001b-tope2 < ls_t001b-frpe2.
                  ls_t001b-tope2 = ls_zfit0063-tope2.
                ENDIF.
              ENDIF.

              "第3个
              "判断全部账期
              IF ls_t001b-frye3 < ls_zfit0063-frye3 AND ls_t001b-toye3 < ls_zfit0063-toye3 OR ls_t001b-frye3 > ls_zfit0063-frye3 AND ls_t001b-toye3 > ls_zfit0063-toye3.
                ls_t001b-frye3 = ls_zfit0063-frye3.
                ls_t001b-frpe3 = ls_zfit0063-frpe3.
                ls_t001b-toye3 = ls_zfit0063-toye3.
                ls_t001b-tope3 = ls_zfit0063-tope3.
              ENDIF.
              "判断起始账期
              IF ls_zfit0063-frye3 > ls_t001b-frye3.
                ls_t001b-frye3 = ls_zfit0063-frye3.
                ls_t001b-frpe3 = ls_zfit0063-frpe3.
              ELSEIF ls_zfit0063-frye3 = ls_t001b-frye3.
                IF ls_zfit0063-frpe3 > ls_t001b-frpe3.
                  ls_t001b-frpe3 = ls_zfit0063-frpe3.
                ENDIF.
              ENDIF.
              "判断终止账期
              IF ls_zfit0063-toye3 < ls_t001b-toye3.
                ls_t001b-toye3 = ls_zfit0063-toye3.
                ls_t001b-tope3 = ls_zfit0063-tope3.
                IF ls_t001b-toye3 < ls_t001b-frye3.
                  ls_t001b-toye3 = ls_zfit0063-toye3.
                ENDIF.
              ELSEIF ls_zfit0063-toye3 = ls_t001b-toye3.
                IF ls_zfit0063-tope3 < ls_t001b-tope3.
                  ls_t001b-tope3 = ls_zfit0063-tope3.
                ENDIF.
              ENDIF.
              IF ls_t001b-frye3 = ls_t001b-toye3.
                IF ls_t001b-tope3 < ls_t001b-frpe3.
                  ls_t001b-tope3 = ls_zfit0063-tope3.
                ENDIF.
              ENDIF.


              UPDATE t001b FROM ls_t001b.
              IF sy-subrc = 0.
                COMMIT WORK.
                MODIFY lt_t001b FROM ls_t001b TRANSPORTING rrcty bukrs mkoar bkont
                                      WHERE bukrs = ls_t001b-bukrs
                                      AND   rrcty = ls_t001b-rrcty
                                      AND   mkoar = ls_t001b-mkoar
                                      AND   bkont = ls_t001b-bkont.
*                MODIFY lt_t001b FROM ls_t001b TRANSPORTING RRCTY BUKRS MKOAR BKONT.
                "将需要修改的数据存入GT_T001B表。
              ELSE.
                MESSAGE '数据插入T001异常' TYPE 'E'.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
*        CLEAR lt_t001b.
*        MOVE-CORRESPONDING lt_t001b2 TO lt_t001b.
        "将数据写入日志表
        IF lv_guid IS INITIAL OR lv_guid = '0'.
          lv_guid = zcl_bc_public=>get_guid( ).
        ENDIF.
        MOVE-CORRESPONDING ls_zfit0063 TO ls_zfit0063a.
        ls_zfit0063a-usnam = sy-uname.
        ls_zfit0063a-cpudt = sy-datum.
        ls_zfit0063a-cputm = sy-uzeit.
        ls_zfit0063a-seqno = lv_guid.
        MODIFY zfit0063a FROM ls_zfit0063a.
        MOVE-CORRESPONDING ls_temp TO ls_zfit0063.
        APPEND ls_zfit0063 TO lt_zfit0063b.
        CLEAR:ls_zfit0063,lv_guid,ls_temp,ls_zfit0063a.
      ENDLOOP.
      IF sy-subrc = 0.
        MESSAGE '保存成功' TYPE 'S'.
        CLEAR lt_zfit0063.
        CLEAR lt_zfit0063.
        SELECT
          *
          FROM zfit0063 INTO CORRESPONDING FIELDS OF TABLE lt_zfit0063_temp.

        MOVE lt_zfit0063b TO lt_zfit0063.
        LOOP AT lt_zfit0063_temp INTO ls_zfit0063_temp WHERE bukrs = '0000'.
          APPEND ls_zfit0063_temp TO lt_zfit0063.
          CLEAR ls_zfit0063_temp.
        ENDLOOP.
        SORT lt_zfit0063 BY rrcty bukrs mkoar bkont.
        DELETE ADJACENT DUPLICATES FROM lt_zfit0063 COMPARING rrcty bukrs mkoar bkont.
        CALL METHOD lv_grid->refresh_table_display.
      ELSE.
        MESSAGE '数据有误' TYPE 'E' DISPLAY LIKE 'S'.
      ENDIF.


    ENDIF.

*    LEAVE TO SCREEN 0.





  ELSEIF sy-ucomm = '&DELETE' OR sy-ucomm = '&DEL'.
    LOOP AT lt_zfit0063 INTO ls_zfit0063 WHERE sel = 'X'.
      "将数据写入日志表
      IF ls_zfit0063-guid IS INITIAL OR ls_zfit0063-guid = '0'.
        CLEAR lv_guid.
        lv_guid = zcl_bc_public=>get_guid( ).
      ENDIF.
      MOVE-CORRESPONDING ls_zfit0063 TO ls_zfit0063a.
      ls_zfit0063a-usnam = sy-uname.
      ls_zfit0063a-cpudt = sy-datum.
      ls_zfit0063a-cputm = sy-uzeit.
      ls_zfit0063a-seqno = lv_guid.
      ls_zfit0063a-ztype = '删除'.

      IF ls_zfit0063-bukrs = '0000'.
        READ TABLE lt_t001b INTO ls_t001b WITH KEY mkoar = ls_zfit0063-mkoar.
        IF sy-subrc = 0.
          MESSAGE '子账期变式存在对应账户类型账期，不允许删除。异常账期变式：'&& ls_t001b-bukrs TYPE 'E'.
        ENDIF.
      ENDIF.

*      DELETE FROM zfit0063 WHERE guid = ls_zfit0063-guid."删除的是集团数据
      DELETE FROM zfit0063 WHERE bukrs = ls_zfit0063-bukrs AND rrcty = ls_zfit0063-rrcty AND mkoar = ls_zfit0063-mkoar AND bkont = ls_zfit0063-bkont.
      DELETE lt_zfit0063_temp WHERE bukrs = ls_zfit0063-bukrs AND rrcty = ls_zfit0063-rrcty AND mkoar = ls_zfit0063-mkoar AND bkont = ls_zfit0063-bkont.
      IF sy-subrc = 0.
        MESSAGE '集团数据删除成功' TYPE 'S'.
        DELETE lt_zfit0063 WHERE bukrs = ls_zfit0063-bukrs AND rrcty = ls_zfit0063-rrcty AND mkoar = ls_zfit0063-mkoar AND bkont = ls_zfit0063-bkont.
        MOVE-CORRESPONDING ls_zfit0063 TO ls_zfit0063a.
        MODIFY zfit0063a FROM ls_zfit0063a.
      ELSEIF sy-subrc <> 0.
        DELETE FROM t001b WHERE bukrs = ls_zfit0063-bukrs
                            AND rrcty = ls_zfit0063-rrcty
                            AND mkoar = ls_zfit0063-mkoar
                            AND bkont = ls_zfit0063-bkont.
        MESSAGE '账期变式数据删除成功' TYPE 'S'.
        DELETE lt_zfit0063 WHERE bukrs = ls_zfit0063-bukrs AND rrcty = ls_zfit0063-rrcty AND mkoar = ls_zfit0063-mkoar AND bkont = ls_zfit0063-bkont.
        MOVE-CORRESPONDING ls_zfit0063 TO ls_zfit0063a.
        MODIFY zfit0063a FROM ls_zfit0063a.
      ENDIF.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lv_grid.
      CALL METHOD lv_grid->check_changed_data.
      CALL METHOD lv_grid->refresh_table_display.
      CLEAR:ls_zfit0063,ls_zfit0063a.
    ENDLOOP.
  ENDIF.
  SORT lt_zfit0063 BY mkoar.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lv_grid.
  is_selfield-refresh = 'X'.
  is_selfield-col_stable = 'X'.
  is_selfield-row_stable = 'X'.
  CALL METHOD lv_grid->get_frontend_layout
    IMPORTING
      es_layout = layout.


  CALL METHOD lv_grid->set_frontend_layout
    EXPORTING
      is_layout = layout.
  CLEAR lv_guid.
ENDFORM.



"权限控制
FORM frm_authority_check.
  CLEAR gv_sel_error.
  AUTHORITY-CHECK OBJECT 'ZFI_ACC_PE'
*   ID 'ACTVT' DUMMY
  ID 'ZBUKRS' FIELD p_bukrs.
  IF sy-subrc <> 0.
* Implement a suitable exception handling here
    MESSAGE '权限检查失败' TYPE 'E'.
    gv_sel_error = 'X'.
  ELSE.
    MESSAGE '有权限' TYPE 'S'.
    gv_sel_error = ''.
  ENDIF.

ENDFORM.