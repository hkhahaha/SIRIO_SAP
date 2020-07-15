*&---------------------------------------------------------------------*
*& 包含               ZQMR006_TOP
*&---------------------------------------------------------------------*



TABLES: mara,ausp,qals,makt.


TYPES: BEGIN OF ty_alv,
         prueflos    TYPE  qals-prueflos, "  样品编号
         matnr       TYPE  qals-matnr , "  物料编码
         maktx       TYPE  makt-maktx , "  物料品名
         charg       TYPE  qals-charg , "  SAP批次号
         zchargl(70) TYPE  c , "  长批号 批次属性
         zpack_sta   TYPE  cabn-atnam  , "  包装规格
         lmengeist   TYPE  qals-lmengeist , "  批量
         zallergen   TYPE  cabn-atnam , "  过敏源
         zexpmat     TYPE  cabn-atnam , "贵重物料
         lagortchrg  TYPE  qals-lagortchrg  , "库存地
         zbox_num    TYPE  cabn-atnam , "开箱数
         zlwedt      TYPE  mcha-lwedt , "到货日期
*        sellifnr   TYPE  but000-name_org1, "供应商
         lifnr       TYPE  but000-name_org1, "供应商
         hersteller  TYPE  but000-name_org1  , "制造商
         zoriadd     TYPE  cabn-atnam , "产地
         zhsdat	     TYPE  mcha-hsdat, "生产日期
         zvfdat	     TYPE  mcha-vfdat, "有效日期
         raube       TYPE  mara-raube, "储存条件
         rbtxt       TYPE  t142t-rbtxt, "存储条件描述
         werk        TYPE  qals-werk,
         obtyp       TYPE j_obtyp,
         mfrnr       TYPE mara-mfrnr,
         lgobe       TYPE t001l-lgobe,
         atwrt       TYPE ausp-atwrt, "材质明细
         phynr       TYPE qprs-phynr, "主样本号
         pn_nr       TYPE qprs-pn_nr, "取样编号
         ftmat       TYPE c, "首进物料
         znrmode(6),"新零售
       END OF ty_alv.




DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.

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
  gs_fieldcat-icon       = &4.
  gs_fieldcat-no_zero       = &5.
  gs_fieldcat-key           = &6.
  gs_fieldcat-ref_table   = &7.
  gs_fieldcat-ref_field = &8.   " 内表中数量参照字段
  gs_fieldcat-edit = &9.   "
  APPEND gs_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.