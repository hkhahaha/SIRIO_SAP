*&---------------------------------------------------------------------*
*& Report ZMDMR0003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmdmr0003.

TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
TABLES:bkpf,mara.
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf

TYPES:BEGIN OF ty_out,
        matnr_main        TYPE mara-matnr, "检索物料号
        matnr_bom         TYPE mara-matnr, "展BOM物料号
        maktx             TYPE makt-maktx, "物料描述
        detail(250),"基本信息
        lgort_detail(100),"库存信息
        po_detail(100),"采购信息
        sales_detail(100),"销售信息
      END OF ty_out.
TYPES:BEGIN OF ty_all,
        matnr          TYPE mara-matnr,  "物料号
        mtart          TYPE mara-mtart,  "物料类型
        maktx          TYPE makt-maktx,  "物料长描述
        werks          TYPE marc-werks,  "工厂
        mmsta          TYPE marc-mmsta,  "特定工厂的物料状态
        mstdv          TYPE mara-mstdv,  "有效启用期
        mstav          TYPE mara-mstav,  "指定分销链状态
        lgort          TYPE lips-lgort,  "库存地点
        lgort_num      TYPE mska-kains,  "库存总数量
        ebeln          TYPE ekpo-ebeln,  "采购订单单据编号
        aedat          TYPE ekko-aedat,  "采购最后下单
        lifnr          TYPE ekko-lifnr,  "采购订单供应商
        ekko_noadd_num TYPE ekpo-menge,  "采购订单未付数量
        ekko_noend_num TYPE ekbe-menge,  "采购订单未结算数量
        vbeln          TYPE vbak-vbeln,  "销售订单编号
        erdat          TYPE vbak-erdat,  "销售订单最后下单日期
        kunnr          TYPE vbak-kunnr,  "销售订单客户
        sale_noadd_num TYPE vbep-bmeng, "销售订单未交付数量
        sale_noend_num TYPE lips-lfimg, "销售订单未结算数量
      END OF ty_all.
DATA:lt_out TYPE TABLE OF ty_out,
     ls_out LIKE LINE OF lt_out.


SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE TEXT-001."定义屏幕
SELECT-OPTIONS:s_matnr FOR mara-matnr."条件输入框，一个for对应一个
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
    fieldcat-ref_tabname = 'BKPF'."调用透明表的数据结构
    fieldcat-fieldname = &1."透明表字段名
    fieldcat-seltext_l = &2."ALV列名
    fieldcat-col_pos = &3."列位置
    APPEND fieldcat.
  END-OF-DEFINITION.


  fieldcatset 'BUKRS' '公司代码' sy-tabix.
*  READ TABLE fieldcat INDEX 3."读取报表第三列
**  fieldcat-hotspot = 'X'."鼠标热点事件
**  fieldcat-key = 'X'."定义为主键（颜色改变）
*  MODIFY fieldcat INDEX 3."修改样式

  layout-zebra = 'X'."斑马线的样式

ENDFORM.
*响应鼠标点击时间的子程序
*FORM alv_user_command USING r_ucomm LIKE sy-ucomm
*      rs_selfield TYPE slis_selfield.
*  CASE r_ucomm.
*    WHEN '&IC1'."默认的值就是这个，网上查资料据说能修改~
*      READ TABLE lbkpf INDEX rs_selfield-tabindex.
*      SET PARAMETER ID 'BLN' FIELD lbkpf-belnr.
*      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*  ENDCASE.
*
*ENDFORM.

FORM frm_set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZHKALV'.
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
      i_grid_title             = '物料冻结前信息查询报表' "标题名
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