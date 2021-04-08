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
        sel(1).
        INCLUDE TYPE zfit0063.
      TYPES END OF ty_zfit0063.

DATA:lt_zfit0063        TYPE TABLE OF ty_zfit0063,
     lt_zfit0063_top    TYPE TABLE OF ty_zfit0063,
     lt_zfit0063_detail TYPE TABLE OF ty_zfit0063,
     lt_zfit0063_temp   TYPE TABLE OF ty_zfit0063,
     ls_zfit0063        LIKE LINE OF lt_zfit0063,
     ls_zfit0063_temp   LIKE LINE OF lt_zfit0063,
     ls_zfit0063_top    LIKE LINE OF lt_zfit0063,
     lt_zfit0063a       TYPE TABLE OF zfit0063a,
     ls_zfit0063a       LIKE LINE OF lt_zfit0063a,
     lt_temp            TYPE TABLE OF zfit0063,
     ls_temp            LIKE LINE OF lt_temp,
     lt_t001b           TYPE TABLE OF t001b,
     ls_t001b           LIKE LINE OF lt_t001b.
DATA gv_sel_error TYPE flag.

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

  IF p_bukrs = '0000'.
    "集团的数据
    SELECT
         *
      FROM zfit0063 INTO CORRESPONDING FIELDS OF TABLE lt_zfit0063
      WHERE bukrs = p_bukrs.
  ELSEIF p_bukrs <> '0000'.
    "公司的数据
    SELECT
       *
    FROM t001b INTO CORRESPONDING FIELDS OF TABLE lt_zfit0063
    WHERE bukrs = p_bukrs.
  ENDIF.


  SELECT
    *
  FROM zfit0063 INTO CORRESPONDING FIELDS OF TABLE lt_zfit0063_temp.

  SELECT
    *
  FROM t001b INTO CORRESPONDING FIELDS OF TABLE lt_t001b.
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
  fieldcatset 'BUKRS' '公司代码' '' sy-tabix.
  fieldcatset 'MKOAR' '帐户类型' 'X' sy-tabix.
  fieldcatset 'VKONT' '起始科目' 'X' sy-tabix.
  fieldcatset 'BKONT' '终止科目' 'X' sy-tabix.
  fieldcatset 'FRPE1' '起始期间' 'X' sy-tabix.
  fieldcatset 'FRYE1' '起始年度' 'X' sy-tabix.
  fieldcatset 'TOPE1' '终止期间' 'X' sy-tabix.
  fieldcatset 'TOYE1' '终止年度' 'X' sy-tabix.

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
  SET PF-STATUS 'ZHK'.
ENDFORM.



FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
      i_callback_user_command  = 'ALV_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = '集团二级开关财务账期'
*     I_GRID_SETTINGS          =
      is_layout_lvc            = layout
      it_fieldcat_lvc          = fieldcat[]
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
      i_default                = 'X'
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
      t_outtab                 = lt_zfit0063
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
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


  IF sy-ucomm = '&NEW'.
    CLEAR:ls_zfit0063,lt_zfit0063.
    ls_zfit0063-bkont = 'ZZZZZZZZZZ'.
    ls_zfit0063-bukrs = p_bukrs.
    APPEND ls_zfit0063 TO lt_zfit0063.
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

    "判断公司的数据是否在集团内
    LOOP AT lt_zfit0063 INTO ls_zfit0063 WHERE bukrs <> '0000'."循环遍历公司的数据
      READ TABLE lt_zfit0063_top INTO ls_zfit0063_top WITH KEY mkoar = ls_zfit0063-mkoar."读取集团数据
      IF sy-subrc <> 0.
        MESSAGE '对应账户类型下没有先维护好集团的数据，请先维护集团数据喔！' TYPE 'E'.
      ELSEIF sy-subrc = 0.
        "判断公司的数据是否在集团的区间下面
        "(1)判断时间区间是否正确
        IF ls_zfit0063-toye1 >= ls_zfit0063-frye1.
          IF ls_zfit0063-toye1 = ls_zfit0063-frye1 AND ls_zfit0063-frpe1 > ls_zfit0063-tope1.
            MESSAGE '同个会计年度，终止期间 < 起始期间' TYPE 'E'.
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
          MESSAGE '终止会计年度 > 起始会计年度' TYPE 'E'.
        ENDIF.
      ENDIF.
      CLEAR:ls_zfit0063,ls_zfit0063_top.
    ENDLOOP.




    "对数据库进行写入操作
    "公司写入T001B表
    LOOP AT lt_zfit0063 INTO ls_zfit0063 WHERE bukrs <> '0000'.
      IF ls_zfit0063-guid IS INITIAL OR ls_zfit0063 = '0'.
        CLEAR lv_guid.
        lv_guid = zcl_bc_public=>get_guid( ).
      ENDIF.
      MOVE-CORRESPONDING ls_zfit0063 TO ls_t001b.
      ls_zfit0063a-ztype = '新增'.
      INSERT t001b FROM ls_t001b.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ls_zfit0063a-ztype = '修改'.
        UPDATE t001b FROM ls_t001b.
        COMMIT WORK.
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
      LOOP AT lt_zfit0063 INTO ls_zfit0063 WHERE bukrs = '0000'.
        IF ls_zfit0063-guid IS INITIAL OR ls_zfit0063 = '0'.
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
        ELSE.
          ls_zfit0063a-ztype = '修改'.
          UPDATE zfit0063 FROM ls_temp.
          COMMIT WORK.
        ENDIF.

        "将数据写入日志表
        MOVE-CORRESPONDING ls_zfit0063 TO ls_zfit0063a.
        ls_zfit0063a-usnam = sy-uname.
        ls_zfit0063a-cpudt = sy-datum.
        ls_zfit0063a-cputm = sy-uzeit.
        ls_zfit0063a-seqno = lv_guid.
        MODIFY zfit0063a FROM ls_zfit0063a.
        CLEAR:ls_zfit0063,lv_guid,ls_temp,ls_zfit0063a.
      ENDLOOP.
      IF sy-subrc = 0.
        MESSAGE '保存成功' TYPE 'S'.
      ELSE.
        MESSAGE '数据有误' TYPE 'E' DISPLAY LIKE 'S'.
      ENDIF.


    ENDIF.

    LEAVE TO SCREEN 0.
  ELSEIF sy-ucomm = '&DELETE'.
    LOOP AT lt_zfit0063 INTO ls_zfit0063 WHERE sel = 'X'.
      "将数据写入日志表
      IF ls_zfit0063-guid IS INITIAL OR ls_zfit0063 = '0'.
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
          MESSAGE '请先删除公司数据后再删除集团数据！异常公司：'&& ls_t001b-bukrs TYPE 'E'.
        ENDIF.
      ENDIF.

      DELETE FROM zfit0063 WHERE guid = ls_zfit0063-guid."删除的是集团数据
      IF sy-subrc = 0.
        MESSAGE '集团数据删除成功' TYPE 'S'.
        DELETE lt_zfit0063 WHERE guid = ls_zfit0063-guid.
        MOVE-CORRESPONDING ls_zfit0063 TO ls_zfit0063a.
        MODIFY zfit0063a FROM ls_zfit0063a.
      ELSEIF sy-subrc <> 0.
        DELETE FROM t001b WHERE bukrs = ls_zfit0063-bukrs
                            AND rrcty = ls_zfit0063-rrcty
                            AND mkoar = ls_zfit0063-mkoar
                            AND bkont = ls_zfit0063-bkont.
        MESSAGE '公司数据删除成功' TYPE 'S'.
        MOVE-CORRESPONDING ls_zfit0063 TO ls_zfit0063a.
        MODIFY zfit0063a FROM ls_zfit0063a.
      ENDIF.
      CLEAR:ls_zfit0063,ls_zfit0063a.
    ENDLOOP.
  ENDIF.
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




FORM frm_authority_check.
  CLEAR gv_sel_error.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
*   ID 'ACTVT' DUMMY
  ID 'BUKRS' FIELD p_bukrs.
  IF sy-subrc <> 0.
* Implement a suitable exception handling here
    MESSAGE '权限检查失败' TYPE 'E'.
    gv_sel_error = 'X'.
  ELSE.
    MESSAGE '有权限' TYPE 'S'.
    gv_sel_error = ''.
  ENDIF.

ENDFORM.