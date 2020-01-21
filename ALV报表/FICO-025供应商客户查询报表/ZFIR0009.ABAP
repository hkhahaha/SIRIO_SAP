*&---------------------------------------------------------------------*
*& Report ZFIR0009
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0009."报表程序声明
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:bkpf,knb1,lfb1,but000,tvtw,knvv,lfa1,lfbk,knbk.

TYPES:BEGIN OF lv_alv,
        bukrs      TYPE knb1-bukrs, "公司代码
        partner    TYPE but000-partner, "业务伙伴编号
        name(100)  TYPE c, "业务伙伴名称
        type(1),"类型
        vtweg(30)  TYPE c, "分销渠道
        vtext(100) TYPE c, "分销渠道描述
        nodel      TYPE knb1-nodel, "删除标识
        date       TYPE but000-crdat, "创建、更新日期
      END OF lv_alv.
DATA:ls_alv    TYPE lv_alv,
     lt_alv    TYPE TABLE OF lv_alv,
     lt_alv2   TYPE TABLE OF lv_alv,
     lt_but000 TYPE TABLE OF but000,
     ls_but000 TYPE but000,
     lt_laf1   TYPE TABLE OF lfa1,
     ls_laf1   TYPE lfa1,
     lt_lfbk   TYPE TABLE OF lfbk,
     ls_lfbk   TYPE lfbk,
     lt_kna1   TYPE TABLE OF kna1,
     ls_kna1   TYPE kna1,
     lt_knbk   TYPE TABLE OF knbk,
     ls_knbk   TYPE knbk,
     lt_tvtw   TYPE TABLE OF tvtw,
     ls_tvtw   TYPE tvtw,
     lt_knvv   TYPE TABLE OF knvv,
     ls_knvv   TYPE knvv,
     lt_knb1   TYPE TABLE OF knb1,
     ls_knb1   TYPE knb1,
     lt_lfb1   TYPE TABLE OF lfb1,
     ls_lfb1   TYPE lfb1.
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf

SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_bukrs FOR knb1-bukrs,"条件输入框，一个for对应一个
               s_date FOR but000-crdat."for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) TEXT-002.
PARAMETERS: p_x1 RADIOBUTTON GROUP gp1 DEFAULT 'X'. "操作类型，创建的话是空的，更新的话打上X
SELECTION-SCREEN COMMENT 26(6) TEXT-003 FOR FIELD p_x1.
PARAMETERS: p_x2 RADIOBUTTON GROUP gp1.
SELECTION-SCREEN COMMENT 38(6) TEXT-004 FOR FIELD p_x2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk.

START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
*定义子程序
FORM getdata."查询语句
  IF p_x1 = 'X'."查的是创建日期
    "客户
    SELECT
      partner,
      crdat AS date,
      bukrs,
      ' ' AS type,
      nodel,
    ( name_org1 &&
    name_org2 &&
    name_org3 &&
    name_org4 ) AS name
    INTO CORRESPONDING FIELDS OF TABLE @lt_alv
    FROM but000
    INNER JOIN knb1
    ON knb1~kunnr = but000~partner
    WHERE but000~crdat IN @s_date
      AND knb1~bukrs IN @s_bukrs.
    "因为需要整合客户和供应商的数据，存在重复项，所以分开查询，下面同理
    "供应商
    SELECT
    partner,
    crdat AS date,
    bukrs,
    ' ' AS type,
    lfb1~nodel,
    name1 AS name
    APPENDING CORRESPONDING FIELDS OF TABLE @lt_alv
    FROM but000
    INNER JOIN lfb1
    ON lfb1~lifnr = but000~partner
    INNER JOIN lfa1
    ON lfa1~lifnr = but000~partner
    WHERE but000~crdat IN @s_date
      AND lfb1~bukrs IN @s_bukrs.
  ELSEIF p_x2 = 'X'."查的是更新日期
    "客户
    SELECT
    partner,
    chdat AS date,"这个是更新日期
    bukrs,
    'X' AS type,
    nodel,
    ( name_org1 &&
    name_org2 &&
    name_org3 &&
    name_org4 ) AS name
    INTO CORRESPONDING FIELDS OF TABLE @lt_alv
    FROM but000
    INNER JOIN knb1
    ON knb1~kunnr = but000~partner
    WHERE but000~chdat IN @s_date
      AND knb1~bukrs IN @s_bukrs.
    "供应商
    SELECT
    partner,
    chdat AS date,"这个是更新日期
    bukrs,
    'X' AS type,
    lfb1~nodel,
    name1 AS name
    APPENDING CORRESPONDING FIELDS OF TABLE @lt_alv
    FROM but000
    INNER JOIN lfb1
    ON lfb1~lifnr = but000~partner
    INNER JOIN lfa1
    ON lfa1~lifnr = but000~partner
    WHERE but000~chdat IN @s_date
    AND lfb1~bukrs IN @s_bukrs.
  ENDIF.

  LOOP AT lt_alv INTO ls_alv.
    SELECT
      knvv~vtweg,
      vtext
    INTO TABLE @DATA(lt_vtweg)
    FROM knvv
    INNER JOIN tvtwt
    ON tvtwt~vtweg = knvv~vtweg
    WHERE knvv~vkorg = @ls_alv-bukrs
      AND knvv~kunnr = @ls_alv-partner
      AND tvtwt~spras = '1'.
    DATA ls_vtweg LIKE LINE OF lt_vtweg.
    IF lt_vtweg IS NOT INITIAL.
      LOOP AT lt_vtweg INTO ls_vtweg.
        CONCATENATE ls_alv-vtweg ls_vtweg-vtweg INTO ls_alv-vtweg SEPARATED BY ','.
        CONCATENATE ls_alv-vtext ls_vtweg-vtext INTO ls_alv-vtext SEPARATED BY ','.
        CLEAR ls_vtweg.
      ENDLOOP.
    ENDIF.
    ls_alv-vtweg = ls_alv-vtweg+1(28).
    ls_alv-vtext = ls_alv-vtext+1(98).
    APPEND ls_alv TO lt_alv2.
  ENDLOOP.
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
*  bukrs     TYPE knb1-bukrs, "公司代码
*  partner   TYPE but000-partner, "业务伙伴编号
*  name(100) TYPE C, "业务伙伴名称
*  TYPE(1),"类型
*  vtweg     TYPE knvv-vtweg, "分销渠道
*  vtext     TYPE tvtw-vtweg, "分销渠道描述
*  nodel     TYPE knb1-nodel, "删除标识
*  DATE      TYPE but000-crdat, "创建、更新日期

  fieldcatset 'BUKRS' '公司代码' sy-tabix.
  fieldcatset 'PARTNER' '业务伙伴编号' sy-tabix.
  fieldcatset 'NAME' '业务伙伴名称' sy-tabix.
  fieldcatset 'TYPE' '类型' sy-tabix.
  fieldcatset 'VTWEG' '分销渠道' sy-tabix.
  fieldcatset 'VTEXT' '分销渠道描述' sy-tabix.
  fieldcatset 'NODEL' '删除标识' sy-tabix.
  fieldcatset 'DATE' '创建/更新日期' sy-tabix.


  READ TABLE fieldcat INDEX 3."读取报表第三列
*  fieldcat-hotspot = 'X'."鼠标热点事件
  fieldcat-key = 'X'."定义为主键（颜色改变）
  MODIFY fieldcat INDEX 3."修改样式

  READ TABLE fieldcat INDEX 4.
*  fieldcat-hotspot = 'X'.
  fieldcat-emphasize = 'C500'.
  MODIFY fieldcat INDEX 4.

  READ TABLE fieldcat INDEX 1.
  fieldcat-emphasize = 'C600'.
  MODIFY fieldcat INDEX 1.

  READ TABLE fieldcat INDEX 2.
  fieldcat-emphasize = 'C300'.
  MODIFY fieldcat INDEX 2.


*layout-colwidth_optimize = 'X'.
  layout-zebra = 'X'."斑马线的样式

ENDFORM.
*响应鼠标点击时间的子程序
FORM alv_user_command USING r_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
      READ TABLE lbkpf INDEX rs_selfield-tabindex.
      SET PARAMETER ID 'BLN' FIELD lbkpf-belnr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.

FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZHKALV1'.
ENDFORM.



FORM alvshow.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = w_repid "程序名称
      i_callback_pf_status_set = 'FRM_SET_PF_STATUS'
*     i_callback_user_command  = 'ALV_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
      i_grid_title             = '会计凭证' "标题名
*     I_GRID_SETTINGS          =
      is_layout                = layout "程序所定义的layout名称
      it_fieldcat              = fieldcat[] "定义fieldcat数据
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
*     I_SAVE                   = ' '
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = lt_alv2
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.