*&---------------------------------------------------------------------*
*& Report ZQMR0017
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zqmr0017.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:eord,mara,adrc,lfa1,but0id,bkpf,ekko,makt,zqmt011.
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf
TYPES:BEGIN OF ty_alv,
        matnr           TYPE eord-matnr, "物料编码
        werks           TYPE eord-werks, "工厂
        matkx           TYPE makt-maktx, "物料描述（短描述）
        matkl           TYPE mara-matkl, "物料类别
        lifnr           TYPE eord-lifnr, "经销商编号
        lifnr_name(100),"经销商名称
        stras           TYPE lfa1-stras, "经销商地址
        regiogroup      TYPE adrc-regiogroup, "经销商等级
        date1           TYPE but0id-valid_date_from, "经销商评估合格日期
        date2           TYPE but0id-valid_date_to, "经销商计划评估日期
        ematn           TYPE eord-ematn, "MPN料号
        mfrnr           TYPE mara-mfrnr, "生产商编号

        pro_name(100),"生厂商名称
        stras2          TYPE lfa1-stras, "生产厂家地址
        pro_regiogroup  TYPE adrc-regiogroup, "生厂商等级
        date3           TYPE but0id-valid_date_from, "生厂商评估合格日期
        date4           TYPE but0id-valid_date_to, "生产商计划评估日期
        zallergen(80),"过敏原
        ztrsgen(80),"转基因
        zexpmat(80),"贵重物料
        zwhp(80),"危险化学品
        zoriadd(80),"原产地
        zqmsamp(80),"开箱量计算标识
        cctj            TYPE zqmt011-cctj, "存储条件
        zmatdet(80),"材质明细
        zsamping(80),"QA取样量
        time            TYPE ekko-aedat, "最近下单时间
      END OF ty_alv.
TYPES:BEGIN OF ty_all.
        INCLUDE TYPE ty_alv.
      TYPES:END OF ty_all.

DATA:gt_alv TYPE TABLE OF ty_alv,
     gs_alv LIKE LINE OF gt_alv.




SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_werks FOR eord-werks,"条件输入框，一个for对应一个\工厂
               s_matnr FOR eord-matnr,"for前面的是字段显示的名称，可以修改，但是注意不要超过8个字符，否则会报错、物料编码
               s_lifnr FOR eord-lifnr,"制造商编码
               s_mfrnr FOR mara-mfrnr,"制造商编码
               s_group1 FOR adrc-regiogroup,"经销商等级
               s_group2 FOR adrc-regiogroup,"制造商等级
               s_date1 FOR but0id-valid_date_from,"经销商评估合格日期
               s_date2 FOR but0id-valid_date_to,"经销商计划评估日期
               s_date3 FOR but0id-valid_date_from,"生产商计划评估日期
               s_date4 FOR but0id-valid_date_to,"生厂商计划评估日期
               s_time FOR ekko-aedat,"最近下单日期
               s_matkl FOR mara-matkl."物料组
SELECTION-SCREEN END OF BLOCK blk.

START-OF-SELECTION.
*调用子程序
  PERFORM getdata.
  PERFORM catalog.
  PERFORM alvshow.
*定义子程序
FORM getdata."查询语句

ENDFORM.

FORM catalog."表头
  w_repid = sy-repid.
  CLEAR fieldcat.

  DEFINE fieldcatset."宏定义
    fieldcat-just = 'C'."字段居中显示
    fieldcat-outputlen = 10."自定义字段的长度
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
  fieldcat-hotspot = 'X'.
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
  READ TABLE fieldcat INDEX 9.
  fieldcat-checkbox = 'X'.
  MODIFY fieldcat INDEX 9.

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
      i_callback_user_command  = 'ALV_USER_COMMAND' "对ALV操作的时候触发所定义的子程序
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
      t_outtab                 = lbkpf
    EXCEPTIONS "下面都是默认的
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.