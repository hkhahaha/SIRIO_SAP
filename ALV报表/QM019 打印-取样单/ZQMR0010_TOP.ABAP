

*&---------------------------------------------------------------------*
*& Include ZQMROO10_TOP                             - Report ZQMR0010
*&---------------------------------------------------------------------*
REPORT zqmr0010.
TABLES: qals.

TYPES:BEGIN OF typ_out,"选择屏幕读取
        matnr         LIKE qals-matnr, "物料编号
        maktx         LIKE makt-maktx, " 物料描述
        charg         LIKE qals-charg, " 批次号
        zpack_sta(40) TYPE c, " 规格
        lmengeist     TYPE string, "批量
        zchanx        TYPE string, " 取样区域
        lifnr         TYPE string, "供应商
        mfrnr(16)     TYPE c, "制造商
        num           TYPE string, "  抽样规则（开箱数）
        station       TYPE string, " 检验项/岗位
        gesstichpr    TYPE string, "检验量
        menge2        TYPE string, "每批日常留样
        verwmerkm(40) TYPE c, " 标样
        zcqwdxly      TYPE string, "长期稳定性考察
        zjswdxly      TYPE string, "加速稳定性考察
        zdqwdxly      TYPE string, "短期稳定性考察
        zsfxbz        TYPE string, "留样观察
        zndwjly       TYPE string, " 年度外检
        sel           TYPE c, " 用来标识选择的字段
      END OF typ_out.
TYPES:BEGIN OF typ_out2,
        number TYPE string,
      END OF typ_out2.


DATA: gt_out     TYPE STANDARD TABLE OF typ_out, "存储根据选择条件抓取的值
      gt_out2    TYPE  STANDARD TABLE OF typ_out,
      gv_ok_code TYPE sy-ucomm,
      gs_out     LIKE LINE OF gt_out,
      lv_out     TYPE typ_out2.

"DOI 标准变量
DATA:grf_container     TYPE REF TO cl_gui_custom_container,
     grf_control       TYPE REF TO i_oi_container_control,
     grf_document      TYPE REF TO i_oi_document_proxy,
     grf_spreadsheet   TYPE REF TO i_oi_spreadsheet,
     gt_excel_input    TYPE soi_generic_table,
     gs_excel_input    TYPE soi_generic_item,
     gs_error          TYPE REF TO i_oi_error,
     gt_error          TYPE REF TO i_oi_error OCCURS 0 WITH HEADER LINE,
     gs_range          TYPE soi_range_item,
     gt_range          TYPE soi_range_item OCCURS 0,
     gv_retcode        TYPE soi_ret_string,
     gv_initialized(1),
     gv_item_url(256).
DATA:gs_qals TYPE qals.
SELECTION-SCREEN: BEGIN OF BLOCK blk1.
SELECT-OPTIONS :p_matnr FOR gs_qals-matnr, "检验批号
              p_charg FOR gs_qals-charg, "检验批号
              p_pruef FOR gs_qals-prueflos, "检验批号
              p_werks FOR gs_qals-werk OBLIGATORY." 工厂
SELECTION-SCREEN:END OF BLOCK blk1.