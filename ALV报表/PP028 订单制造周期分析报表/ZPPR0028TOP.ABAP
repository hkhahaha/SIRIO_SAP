*&---------------------------------------------------------------------*
*& 包含               ZPPR0028TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*  Tables
*&---------------------------------------------------------------------*
TABLES: vbak,vbap,mara,tvakt,sscrfields.
CONSTANTS cns_bom_bmeng TYPE basmn VALUE 10000.

*&---------------------------------------------------------------------*
*  Types
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_detail,
         werks      LIKE vbap-werks, "工厂
         matnr      LIKE vbap-matnr, "订单产品编号
         matnr_m    LIKE vbap-matnr, "m物料编码
         maktx_m    LIKE makt-maktx, "m描述
         sl_m       TYPE kmpmg,  "m数量
         meins_m    LIKE mara-meins, "m单位
         matnr_s    LIKE vbap-matnr, "对应S码
         maktx_s    LIKE makt-maktx, "S描述
         matkl_s    LIKE vbap-matkl,  "半成品
         matkl_s4   LIKE vbap-matkl,  "半成品
         bcpxl_dec  LIKE t023t-wgbez, "半成品小类 描述
         sjx_dec    LIKE t023t-wgbez, "半成品中类
         sl_s       TYPE p LENGTH 15 DECIMALS 3,  "S数量
         meins_s    LIKE mara-meins,
         zdzeit     LIKE zppt0006-zdzeit,    "生产时间-通过配置表读取时间
         zmatscat_s LIKE cawn-atwrt,    "压制时间-物料分类ZMATSCAT特征值
         matnr_yfl  LIKE vbap-matnr, "原辅料物料编码
         maktx_yfl  LIKE makt-maktx, "原辅料物料描述
         ruezt_yfl  LIKE marc-ruezt, "原辅料准备时间-从配置表读取
         plifz_yfl  LIKE marc-plifz, "原辅料计划交货时间（天）
         webaz_yfl  LIKE marc-webaz, "原辅料以天计的收货处理时间
         matnr_nb   LIKE vbap-matnr, "内包物料编码
         maktx_nb   LIKE makt-maktx, "内包物料描述
         ruezt_nb   LIKE marc-ruezt, "内包物料准备时间-从配置表读取
         plifz_nb   LIKE marc-plifz, "内包物料计划交货时间（天）
         webaz_nb   LIKE marc-webaz, "内包物料以天计的收货处理时间
         matnr_wb   LIKE vbap-matnr, "外包物料编码
         maktx_wb   LIKE makt-maktx, "外包物料描述
         ruezt_wb   LIKE marc-ruezt,    "外包物料准备时间-从配置表读取
         plifz_wb   LIKE marc-plifz,    "外包物料计划交货时间（天）
         webaz_wb   LIKE marc-webaz,    "外包物料以天计的收货处理时间
       END OF ty_detail.

TYPES: BEGIN OF ty_alv,
         vbeln        LIKE vbap-vbeln, "销售订单号
         posnr        LIKE vbap-posnr, "销售订单行项目
         werks        LIKE vbap-werks, "工厂
         kunnr        LIKE vbak-kunnr, "客户编号
         kunnr_dec    TYPE char200,       "客户名称
         bu_sort1(20),   "检索项
         kukla        LIKE kna1-kukla,    "客户级别
         kuklatxt     TYPE tkukt-vtext, "客户分类描述
         vtweg        LIKE vbak-vtweg,    "部门(分销渠道)
         vtext        LIKE tvtwt-vtext,   "部门名称
         vkbur        LIKE vbak-vkbur,    "二级部门（销售办事处）
         bezei        LIKE tvkbt-bezei,   "二级部门名称
         auart        LIKE vbak-auart,   "订单类型
         auartxt      TYPE tvakt-bezei, "订单类型描述
         ywy          LIKE vbak-kunnr,    "业务员编号
         ywy_dec      TYPE char200, "业务员名称
         ernam        LIKE vbak-ernam,    "销售订单 创建人
         ernamt       LIKE adrp-name_text, "创建人名称
         objnr        LIKE vbak-objnr, "销售订单状态
         txt30        LIKE tj30t-txt30,   "订单审核状态
         udate        TYPE jcds-udate, "二审日期
         utime        TYPE jcds-utime, "二审时间
         ettyp        LIKE vbep-ettyp,    "计划行类别
         zbhcl        LIKE vbap-zbhcl,    "备货策略
         ddtext       LIKE dd07t-ddtext,  "备货策略描述
         zckgj        LIKE vbak-zckgj,    "出口国家
         landx        LIKE t005t-landx,   "出口国家 名称
         matnr        LIKE vbap-matnr, "订单产品编号
         maktx        LIKE makt-maktx, "订单产品描述
         zbzgg        LIKE vbap-zbzgg,    "包装规格
         zsfxcp       LIKE vbap-zsfxcp,   "是否新产品
         zsfxbz       LIKE vbap-zsfxbz,   "是否新包装
         zpro         TYPE char70,   "是否益生菌-物料分类ZPRO特征值
         zatpdfrq     LIKE vbap-zatpdfrq, "ATP答复时间
         kwmeng       LIKE vbap-kwmeng, "以销售单位表示的累计订购数量
         vrkme        LIKE vbap-vrkme, "销售单位
         bmeng        LIKE vbap-kwmeng, "以基本单位表示的累计订购数量
         meins        LIKE vbap-meins, "基本计量单位
         umvkz        LIKE vbap-umvkz,    "销售数量转换成SKU的分子(因子)
         umvkn        LIKE vbap-umvkn,    "销售数量转换为 SKU 的值（除数）
         zchej        TYPE aufk-zchej,  "车间
         budat_zao    LIKE mkpf-budat,    "最早入库日期
         budat_wan    LIKE mkpf-budat,    "最晚入库日期
         greuname     TYPE zppt0003-uname, "入库申请单的输入者
         budat_b      TYPE matdoc-budat, "销售订单的流程订单首次收货过账日期
         cputm_b      TYPE matdoc-cputm, "销售订单的流程订单首次收货过账时间
         budat_e      TYPE matdoc-budat, "销售订单的流程订单末次收货过账日期
         cputm_e      TYPE matdoc-cputm, "销售订单的流程订单末次收货过账时间
         erdat        LIKE vbak-erdat,  "订单创建日期
         znbjhq       LIKE vbap-znbjhq,   "要求交货日期
         zysjhq       LIKE vbap-zysjhq,   "客户要求交货时间
         wadat_ist    LIKE likp-wadat_ist, "实际发货日期

         wemng        TYPE afpo-wemng, "销售订单对应的流程订单累计入库量，需换算成销售单位
         yfhsl        LIKE lips-lfimg,  "已发货数量
         wfhsl        LIKE lips-lfimg,  "未发货数量
         matkl        LIKE vbap-matkl, "销售订单产品物料组
         matkl_4      LIKE vbap-matkl,
         cpxl_dec     LIKE t023t-wgbez, "成品小类 描述
         mjx_dec      LIKE t023t-wgbez, "剂型
         sjzq         TYPE i,           "M实际周期
         matnr_m      LIKE vbap-matnr, "m物料编码
         maktx_m      LIKE makt-maktx, "m描述
         sl_m         TYPE kmpmg,  "m数量
         meins_m      LIKE mara-meins, "m单位
         matnr_s      LIKE vbap-matnr, "对应S码
         maktx_s      LIKE makt-maktx, "S描述
         matkl_s      LIKE vbap-matkl,  "半成品
         matkl_s4     LIKE vbap-matkl,  "半成品
         bcpxl_dec    LIKE t023t-wgbez, "半成品小类 描述
         sjx_dec      LIKE t023t-wgbez, "半成品中类
         sl_s         TYPE p LENGTH 15 DECIMALS 3,  "S数量
         meins_s      LIKE mara-meins,
         zdzeit       LIKE zppt0006-zdzeit,    "生产时间-通过配置表读取时间
         zmatscat_s   LIKE cawn-atwrt,    "压制时间-物料分类ZMATSCAT特征值
         matnr_yfl    LIKE vbap-matnr, "原辅料物料编码
         maktx_yfl    LIKE makt-maktx, "原辅料物料描述
         ruezt_yfl    LIKE marc-ruezt, "原辅料准备时间-从配置表读取
         plifz_yfl    LIKE marc-plifz, "原辅料计划交货时间（天）
         webaz_yfl    LIKE marc-webaz, "原辅料以天计的收货处理时间
         matnr_nb     LIKE vbap-matnr, "内包物料编码
         maktx_nb     LIKE makt-maktx, "内包物料描述
         ruezt_nb     LIKE marc-ruezt, "内包物料准备时间-从配置表读取
         plifz_nb     LIKE marc-plifz, "内包物料计划交货时间（天）
         webaz_nb     LIKE marc-webaz, "内包物料以天计的收货处理时间
         matnr_wb     LIKE vbap-matnr, "外包物料编码
         maktx_wb     LIKE makt-maktx, "外包物料描述
         ruezt_wb     LIKE marc-ruezt,    "外包物料准备时间-从配置表读取
         plifz_wb     LIKE marc-plifz,    "外包物料计划交货时间（天）
         webaz_wb     LIKE marc-webaz,    "外包物料以天计的收货处理时间
*         cpghzq     TYPE i,               "成品供货周期
         zscbldh      LIKE vbap-zscbldh,  "是否有手工备料
         ltxts        LIKE zpps_zppr0014_head-ltxts, "销售订单行项目长文本
         sjgz         TYPE tvk1t-vtext, "保健品送检规则
         "20220530 Seashell 新增字段
         pmatn        TYPE mvke-pmatn, "定价参考物料
         pmatnX        TYPE MAKT-maktx, "定价参考物料
         zappsou(70),"批文来源
         plifz(4),"供货周期
         vkorg        TYPE mvke-vkorg, "销售组织
       END OF ty_alv.

DATA:BEGIN OF gs_kunnr,
       kunnr TYPE vbak-kunnr,
     END OF gs_kunnr.
DATA:BEGIN OF gs_auart,
       auart TYPE vbak-auart,
     END OF gs_auart.
DATA:BEGIN OF gs_vtweg,
       vtweg TYPE vbak-vtweg,
     END OF gs_vtweg.
DATA:BEGIN OF gs_vkbur ,
       vkbur TYPE vbak-vkbur,
     END OF gs_vkbur.
DATA:BEGIN OF gs_vbeln,
       vbeln LIKE vbap-vbeln, "销售订单号
       posnr LIKE vbap-posnr, "销售订单行项目
     END OF gs_vbeln.
DATA:BEGIN OF gs_ernam,
       ernam TYPE vbak-ernam,    "销售订单 创建人
     END OF gs_ernam.
DATA:BEGIN OF gs_objnr,
       objnr LIKE vbak-objnr, "销售订单状态
     END OF gs_objnr.
DATA:BEGIN OF gs_zckgj,
       zckgj LIKE vbak-zckgj,    "出口国家
     END OF gs_zckgj.
DATA:BEGIN OF gs_matnr,
       matnr TYPE mara-matnr,
     END OF gs_matnr.
DATA:BEGIN OF gs_objek,
       objek TYPE ausp-objek,
     END OF gs_objek.
DATA:BEGIN OF gs_zppt0003,
       kdauf TYPE zppt0003-kdauf,
       kdpos TYPE zppt0003-kdpos,
       erdat TYPE zppt0003-erdat,
       uname TYPE zppt0003-uname,
     END OF gs_zppt0003.
DATA:BEGIN OF gs_matdoc,
       mat_kdauf TYPE matdoc-mat_kdauf,
       mat_kdpos TYPE matdoc-mat_kdpos,
       budat     TYPE matdoc-budat, "销售订单的流程订单收货过账日期
       cputm     TYPE matdoc-cputm, "销售订单的流程订单收货过账时间
     END OF gs_matdoc.
DATA:BEGIN OF gs_matkl,
       matkl TYPE mara-matkl,
     END OF gs_matkl.
DATA:BEGIN OF gs_bom_matnr,
       matnr TYPE mara-matnr,
       werks TYPE marc-werks,
       matkl TYPE mara-matkl,
     END OF   gs_bom_matnr.
DATA:BEGIN OF gs_bom_head,
       matnr TYPE mast-matnr,
       werks TYPE mast-werks,
       matkl TYPE mara-matkl,
       stlnr TYPE mast-stlnr, "物料清单
       stlal TYPE mast-stlal, "备选物料清单
       bmeng TYPE stko-bmeng,
       bmein TYPE stko-bmein,
*       stktx TYPE stko-stktx,"可选 BOM 文本
*       stlst TYPE stko-stlst,"物料清单状态
     END OF gs_bom_head.
DATA:BEGIN OF gs_raw_cycle,
       stufe      TYPE stpox-stufe,
       ttidx      TYPE stpox-ttidx,
       matnr_head TYPE stpox-idnrk,
       mtart_head TYPE mara-mtart,
       idnrk      TYPE stpox-idnrk,
       werks      TYPE stpox-werks,
       ojtxp      TYPE stpox-ojtxp,
       mtart      TYPE stpox-mtart,
       plifz      TYPE marc-plifz,
       webaz      TYPE marc-webaz,
       zpretime   TYPE zppt0007-zpretime,
       totaltime  TYPE marc-plifz,
     END OF gs_raw_cycle.
*&---------------------------------------------------------------------*
*  Intertables
*&---------------------------------------------------------------------*
DATA: gt_alv          TYPE TABLE OF ty_alv,
      gs_alv          TYPE ty_alv,
      gt_alv_head     TYPE TABLE OF ty_alv,
      gt_matkl        LIKE SORTED TABLE OF gs_matkl WITH UNIQUE KEY matkl,
      gt_bom_matnr    LIKE SORTED TABLE OF gs_bom_matnr WITH UNIQUE KEY matnr werks matkl,
      gt_objek        LIKE SORTED TABLE OF gs_objek WITH UNIQUE KEY objek,
      gt_alv_detail   TYPE TABLE OF ty_detail WITH EMPTY KEY WITH NON-UNIQUE SORTED KEY skeys COMPONENTS matnr werks,
      gt_alv_output   TYPE TABLE OF ty_alv,
      gt_raw_cycle    LIKE TABLE OF gs_raw_cycle,
      go_salv_table   TYPE REF TO cl_salv_table,
      go_salv_columns TYPE REF TO cl_salv_columns_table,
      gs_zppt0006     TYPE zsppt0006,
      gt_zppt0006     TYPE TABLE OF zsppt0006,
      gs_zppt0007     TYPE zsppt0007,
      gt_zppt0007     TYPE TABLE OF zsppt0007,
      gs_layout       TYPE lvc_s_layo,
      gs_stable       TYPE lvc_s_stbl,
      go_docking      TYPE REF TO cl_gui_docking_container,
      go_alv_grid     TYPE REF TO cl_gui_alv_grid,
      gt_alv_fidcat   TYPE lvc_t_fcat,
      ok_code         TYPE sy-tcode,
      save_code       TYPE sy-tcode,
      gs_rsdswhere    TYPE  rsdswhere,
      gt_rsdswhere    TYPE TABLE OF rsdswhere.

TYPES:ty_tab_stpox     TYPE TABLE OF stpox,
      ty_tab_cscmat    TYPE TABLE OF cscmat,
      ty_tab_alv       TYPE TABLE OF ty_alv,
      ty_tab_detial    TYPE TABLE OF ty_detail,
      ty_tab_raw_cycle LIKE TABLE OF gs_raw_cycle.

FIELD-SYMBOLS: <fs_alv>    TYPE ty_alv,
               <fs_detail> TYPE ty_detail.