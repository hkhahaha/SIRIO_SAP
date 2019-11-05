*&---------------------------------------------------------------------*
*& 包含               ZQMR008_TOP
*&---------------------------------------------------------------------*

TABLES: mara,mseg,qals,makt,qmat,qmtt,zppt0001,plmk,qprs,afpo,qpmk,tq11t,kna1,t006a.


TYPES: BEGIN OF ty_alv,
         aendt       TYPE qprs-aendt, "接检日期
*         pastrterm   TYPE  qals-pastrterm, "  接检日期
         paendterm   TYPE  qals-paendterm , "  预计完成日期
         mpdau       TYPE  qmat-mpdau , "  平均检验期
         kdauf       TYPE afpo-kdauf, "  销售订购单号
*         kdauf       TYPE  zppt0001-kdauf, "  销售订购单号
         kunnr       TYPE  vbak-kunnr, "  客户检索码
         sortl       TYPE kna1-sortl, "  客户检索码
         prueflos    TYPE  qals-prueflos  , "  样品编号
         matnr       TYPE  qals-matnr , "  物料编码
         maktx       TYPE  makt-maktx , "  物料名称
         charg       TYPE  qals-charg , " 成品批号
         zpack_charg TYPE  cabn-atnam  , "打印包装批号
*         charg       TYPE  qals-charg , "SAP批次号
         groes       TYPE  mara-groes , "规格型号
         zpack_sta   TYPE  cabn-atnam, "包装规格
         prfql       TYPE  qpmk-prfql  , "检验岗位
         dummy40     TYPE  qpmk-dummy40  , "检验项目
         msehi       TYPE t006a-mseh6 , "计量单位
         ltextkz1    TYPE  qpmt-kurztext , "规定标准
         ltextkz5    TYPE  qpmt-kurztext , "规定标准(EN )
         ltextkz2    TYPE  qpmt-kurztext , "内控标准
         ltextkz3    TYPE  qmtt-kurztext , "检验方法
         lagort      TYPE  qals-lagortchrg, "库存地
         lagortchrg  TYPE  qals-lagortchrg, "库存地
*         gesstichpr  TYPE  qals-gesstichpr, " 样本数量
         menge       TYPE qprs-menge, " 样本数量
         meinh       TYPE qprs-meinh, "样本单位
*         einhprobe   TYPE  qals-einhprobe, "样本单位
         zlwedt      TYPE  mcha-lwedt, "90天内同一供应商批号非首次到货
         werk        TYPE  qals-werk,
         lgobe       TYPE  t001l-lgobe,
         verwmerkm   TYPE  qamv-verwmerkm,
         mkversion   TYPE  qamv-mkversion,
         mkmnr       TYPE  qpmk-mkmnr,
         version     TYPE  qpmk-version,
         pmethode    TYPE  qamv-pmethode,
         pmtversion  TYPE  qamv-pmtversion,
         zaehler     TYPE  qpmt-zaehler,
         sellifnr    TYPE  qals-sellifnr,
         ltextkz4    TYPE  qmtt-kurztext,
         art         TYPE  qals-art, "检验类型
         masseinhsw  TYPE qpmk-masseinhsw,
         steuerkz    TYPE qpmk-steuerkz,
         print       TYPE  qmtt-kurztext,
         mtart       TYPE mara-mtart,









       END OF ty_alv.

DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.


*----------------------------------------------------------------------*
*       ALV层级关系定义
*----------------------------------------------------------------------*
DATA: gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat,
      gt_events   TYPE slis_t_event,   "事件存储内表
      gs_events   TYPE slis_alv_event.
DATA gs_layout TYPE lvc_s_layo.
DATA:gv_sel_error TYPE c.


*&---------------------------------------------------------------------*
*&      Define marco
*&---------------------------------------------------------------------*
DEFINE  macro_fill_fcat.
  CLEAR gs_fieldcat.
  &1 = &1 + 1.
  gs_fieldcat-col_pos       = &1.
  gs_fieldcat-fieldname     = &2.
  gs_fieldcat-coltext     = &3.
  gs_fieldcat-icon       = &4.
  gs_fieldcat-no_zero       = &5.
  gs_fieldcat-key           = &6.
  gs_fieldcat-ref_table   = &7.
  gs_fieldcat-ref_field = &8.   " 内表中数量参照字段
  gs_fieldcat-edit = &9.   "
  APPEND gs_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.