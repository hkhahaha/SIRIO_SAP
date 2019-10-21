*&---------------------------------------------------------------------*
*& Report ZHKALV1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhkalv1."报表程序声明
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid."记录系统当前的程序名
TABLES:bkpf.
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE t0."定义屏幕
SELECT-OPTIONS:s_bukrs FOR bkpf-bukrs OBLIGATORY,"条件输入框，一个for对应一个
    s_belnr FOR bkpf-belnr,"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
    s_gjahr FOR bkpf-gjahr.
SELECTION-SCREEN END OF BLOCK blk.

START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
*定义子程序
FORM getdata."查询语句
  SELECT *
     FROM bkpf INTO CORRESPONDING FIELDS OF TABLE lbkpf
    WHERE bukrs IN s_bukrs
    AND belnr IN s_belnr
    AND gjahr IN s_gjahr.
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


  fieldcatset 'BUKRS' '公司代码' sy-tabix.
  fieldcatset 'gjahr' '会计年度' sy-tabix.
  fieldcatset 'BUKRS' '会计期间' sy-tabix.
  fieldcatset 'BELNR' '凭证编号' sy-tabix.
  fieldcatset 'BLART' '凭证类型' sy-tabix.
  fieldcatset 'BLDAT' '凭证日期' sy-tabix.
  fieldcatset 'BUDAT' '过账日期' sy-tabix.
  fieldcatset 'XSTOV' '冲销标识' sy-tabix.
  fieldcatset 'STBLG' '冲销凭证' sy-tabix.
  fieldcatset 'PPNAM' '制单人' sy-tabix.
  fieldcatset 'USNAM' '过账人' sy-tabix.

  READ TABLE fieldcat INDEX 3."读取报表第三列
  fieldcat-hotspot = 'X'."鼠标热点事件
  fieldcat-key = 'X'."定义为主键（颜色改变）
  MODIFY fieldcat INDEX 3."修改样式

  READ TABLE fieldcat INDEX 4.
  fieldcat-HOTSPOT = 'X'.
  fieldcat-emphasize = 'C500'.
  MODIFY fieldcat INDEX 4.

  READ TABLE fieldcat INDEX 1.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 1.

  READ TABLE fieldcat INDEX 2.
  fieldcat-emphasize = 'C300'.
  MODIFY fieldcat INDEX 2.

  READ TABLE fieldcat INDEX 5.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 5.

  READ TABLE fieldcat INDEX 6.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 6.

  READ TABLE fieldcat INDEX 7.
  fieldcat-emphasize = 'C100'.
  MODIFY fieldcat INDEX 7.

  READ TABLE fieldcat INDEX 8.
  fieldcat-emphasize = 'C200'.
  MODIFY fieldcat INDEX 8.
  READ TABLE fieldcat index 9.
  fieldcat-checkbox = 'X'.
  MODIFY fieldcat index 9.

*layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'."斑马线的样式

ENDFORM.
*响应鼠标点击时间的子程序
FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-ucomm
      RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE R_UCOMM.
  WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
    READ TABLE lbkpf INDEX RS_SELFIELD-tabindex.
    SET PARAMETER ID 'BLN' FIELD lbkpf-belnr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.

  ENDFORM.

FORM FRM_SET_PF_STATUS USING PT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZHKALV1'.
ENDFORM.



FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = w_repid"程序名称
     I_CALLBACK_PF_STATUS_SET          = 'FRM_SET_PF_STATUS'
     I_CALLBACK_USER_COMMAND           = 'ALV_USER_COMMAND'"对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
      i_grid_title       = '会计凭证'"标题名
*     I_GRID_SETTINGS    =
      is_layout          = layout"程序所定义的layout名称
      it_fieldcat        = fieldcat[]"定义fieldcat数据
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
      t_outtab           = lbkpf
    EXCEPTIONS"下面都是默认的
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.