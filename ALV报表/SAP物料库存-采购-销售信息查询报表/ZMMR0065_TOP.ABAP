*&---------------------------------------------------------------------*
*& 包含               ZMMR0065_TOP.
*&---------------------------------------------------------------------*
TABLES: mara,marc,makt,ekpo,ekko,ekbe,mard,mseg,rkpf,afko,aufk,mbew.


TYPES: BEGIN OF ty_alv,
         werks    TYPE  marc-werks,
         matnr    TYPE  marc-matnr, "  物料编码
         maktx    TYPE  makt-maktx , "  物料描述
         mtart    TYPE  mara-mtart , "  物料类型
         lvorm1   TYPE  mara-lvorm , "  删除标记（集团）
         lvorm2   TYPE  marc-lvorm  , " 删除标记（工厂）
         mmsta    TYPE  marc-mmsta, " 特定工厂的物料状态
         mmstd    TYPE  marc-mmstd  , " 有效起始期
         zcuscode TYPE  cabn-atnam , "  客户检索项（客户代码）
         ersda    TYPE  mara-ersda , "  物料创建时间
         cgdd     TYPE  char200, " 采购订单
         xsdd     TYPE  char200 , " 销售订单
         kcqk     TYPE  char200 , " 库存情况
         lcdd     TYPE  char200 , " 流程订单
         zjll     TYPE  char200 , " 最近领料

       END OF ty_alv.

DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.
DATA:lt_alv TYPE TABLE OF ty_alv,
     ls_alv TYPE ty_alv.
DATA:gv_sel_error TYPE c.


*----------------------------------------------------------------------*
*       ALV层级关系定义
*----------------------------------------------------------------------*
DATA: gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat,
      gt_events   TYPE slis_t_event,   "事件存储内表
      gs_events   TYPE slis_alv_event.
DATA gs_layout TYPE lvc_s_layo.


*&---------------------------------------------------------------------*
*&      Define marco
*&---------------------------------------------------------------------*
DEFINE  macro_fill_fcat.
  CLEAR gs_fieldcat.
  &1 = &1 + 1.
  gs_fieldcat-col_pos       = &1.
  gs_fieldcat-fieldname     = &2.
  gs_fieldcat-coltext     = &3.
  gs_fieldcat-hotspot       = &4.
  gs_fieldcat-no_zero       = &5.
  gs_fieldcat-key           = &6.
  gs_fieldcat-ref_table   = &7.
  gs_fieldcat-ref_field = &8.   " 内表中数量参照字段
*  gs_fieldcat-edit = &9.
 gs_fieldcat-outputlen  = &9.

  APPEND gs_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.