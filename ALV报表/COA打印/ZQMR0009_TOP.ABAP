*&---------------------------------------------------------------------*
*& Include ZQMROOO9_TOP                             - Report ZQMR0009
*&---------------------------------------------------------------------*
REPORT zqmr0009.
TABLES:qave,
       qals,
       qmtt,
       qpmt,
       qpmk,
       qamv,
       qamr,
       qprn,
       qprs,
       plko,
       plpo,
       tq11t,
       t142t,
       lfa1,
       mara,
       ausp,
       mcha,
       mseg,
       t006a,
       tq42t.

TYPES: BEGIN OF typ_plos, "检验批_检验计划数据
         prueflos   LIKE qals-prueflos, "检验批号
         charg      LIKE qals-charg, "批次号
         lmengeist  LIKE qals-lmengeist, "实际批量
         mseht      LIKE t006a-msehl, "单位文本
         vbewertung LIKE qave-vbewertung, "使用决策
         matnr      LIKE qals-matnr, "物料编码
         sellifnr   LIKE qals-sellifnr, "供应商编号
         name1      LIKE lfa1-name1, "供应商名称
         kdauf      LIKE qals-kdauf, "销售订单
         ktext(300) TYPE c, "检验计划描述
         werks      LIKE qals-werk, "工厂
       END OF typ_plos,

       BEGIN OF typ_sample,"样本信息
         frgdt          LIKE qprn-frgdt, "样本确认时间
         menge          LIKE qprs-menge, "样本数量
         mseht          LIKE t006a-msehl, "单位文本
         tq42t_kurztext LIKE tq42t-kurztext, "样本容器文本
         pn_nr          LIKE qprs-pn_nr, "采样图形信息采样编号
       END OF typ_sample,

       BEGIN OF typ_test,"检验特性-已检
         verwmerkm             LIKE qamv-verwmerkm, "主检验特征编码
         sortfeld              LIKE qpmk-sortfeld, "检验特性名称
         qamv_kurztext(256)    TYPE c, "规定标准
         original_input        LIKE qasr-original_input, "初始值
         mseht                 LIKE t006a-msehl, "检验单位文本
         qpmt_kurztext_z1(256) TYPE c, "内控标准
         mbewertg              LIKE qamr-mbewertg, "评估结果
         qmtt_kurztext_z1(256) TYPE c, "检验方法
         pmtversion            LIKE qamv-pmtversion, "检验方法版本
         steuerkz              TYPE c, "取最后一位 空为打印、X为不打印、*为跳过时不打印。记录检验结果时临时添加的特性如何处理
         pqtxt                 LIKE tq11t-pqtxt, "岗位
         arbpl                 LIKE crhd-arbpl, "工作中心
         sign_name_index2(60)  TYPE c, "审核人
         sign_name_index3(60)  TYPE c, "批准人
         tstamp(30)            TYPE c, "批准时间
         pruefbemkt(256)       TYPE c, "定性检验结果
         dummy40(256)          TYPE c, "英文名称
         qpmt_kurztext_en(256) TYPE c, "英文规格标准
         qmtt_kurztext_en(256) TYPE c, "英文检验方法
       END OF typ_test,

       BEGIN OF typ_test_s,"S码-已检
         verwmerkm             LIKE qamv-verwmerkm, "主检验特征编码
         sortfeld              LIKE qpmk-sortfeld, "检验特性名称
         qamv_kurztext(256)    TYPE c, "规定标准
         original_input        LIKE qasr-original_input, "初始值
         mseht                 LIKE t006a-msehl, "检验单位文本
         qpmt_kurztext_z1(256) TYPE c, "内控标准
         mbewertg              LIKE qamr-mbewertg, "评估结果
         qmtt_kurztext_z1(256) TYPE c, "检验方法
         pmtversion            LIKE qamv-pmtversion, "检验方法版本
         steuerkz              TYPE c, "取最后一位 空为打印、X为不打印、*为跳过时不打印。记录检验结果时临时添加的特性如何处理
         pqtxt                 LIKE tq11t-pqtxt, "岗位
         arbpl                 LIKE crhd-arbpl, "工作中心
         pruefbemkt(256)       TYPE c, "定性检验结果
         dummy40(256)          TYPE c, "英文名称
         qpmt_kurztext_en(256) TYPE c, "英文规格标准
         qmtt_kurztext_en(256) TYPE c, "英文检验方法
       END OF typ_test_s,

       BEGIN OF typ_untest_s,"S码未检
         verwmerkm             LIKE qamv-verwmerkm, "主检验特征编码
         sortfeld              LIKE qpmk-sortfeld, "检验特性名称
         qamv_kurztext(256)    TYPE c, "规定标准
         qpmt_kurztext_z1(256) TYPE c, "内控标准
         qmtt_kurztext_z1(256) TYPE c, "检验方法
         pmtversion            LIKE qamv-pmtversion, "检验方法版本
         steuerkz              TYPE c, "取最后一位 空为打印、X为不打印、*为跳过时不打印。记录检验结果时临时添加的特性如何处理
         pqtxt                 LIKE tq11t-pqtxt, "岗位
         qmtt_kurztext_en(256) TYPE c, "英文检验方法
         mseht                 LIKE t006a-msehl, "检验单位文本
         dummy40(256)          TYPE c, "英文名称
         qpmt_kurztext_en(256) TYPE c, "英文规格标准
       END OF typ_untest_s,

       BEGIN OF typ_s_info,
         charg    LIKE mseg-charg,   "S码批号
         aufnr    LIKE qals-aufnr,   "流程订单
         prueflos LIKE qals-prueflos, "检验批号
         matnr    LIKE qals-matnr,   "S码物料编码
       END OF typ_s_info,

       BEGIN OF typ_gelatin,"明胶检验结果
         charg              TYPE charg_d,       "批号
         verwmerkm          LIKE qamv-verwmerkm, "主检验特征编码
         sortfeld           LIKE qpmk-sortfeld, "检验特性名称
         qamv_kurztext(256) TYPE c, "规定标准
         original_input     LIKE qasr-original_input, "初始值
         mseht              LIKE t006a-msehl, "检验单位文本
         mbewertg           LIKE qamr-mbewertg, "评估结果
         qmtt_kurztext(256) TYPE c, "检验方法
         pruefbemkt(256)    TYPE c, "定性检验结果
         prueflos           LIKE qamv-prueflos , "检验批号
       END OF typ_gelatin,

       BEGIN OF typ_material,"物料主数据
         zstdname  LIKE ausp-atwrt, "标准名称
         zpspe     LIKE ausp-atwrt, "包装规格
         zpack_sta LIKE ausp-atwrt, "规格
         mhdhb     LIKE mara-mhdhb, "有效期
         iprkz     LIKE mara-iprkz, "有效期单位
         zcuscode  LIKE ausp-atwrt, "客户检索项
         zcumcode  LIKE ausp-atwrt, "客户物料代码
         rbtxt     LIKE t142t-rbtxt, "存储条件-文本
         mtype     TYPE c,           "物料类型
         maktx     LIKE makt-maktx, "物料描述
         zwgspe    LIKE ausp-atwrt, "内容物规格
       END OF typ_material,

       BEGIN OF typ_charg,"批次数据
         hsdat        LIKE mcha-hsdat,
         z_batch_dypc LIKE ausp-atwrt, "成品打印批号（批次特性）
         zchargl      LIKE ausp-atwrt, "原辅包长批号（批次特性）
       END OF typ_charg,

       BEGIN OF range_item,
         name(128)  TYPE c,
         rows(4)    TYPE c,
         columns(4) TYPE c,
         code(1)    TYPE c,
       END OF range_item,
*********************HK 添加销售订单信息、S码检验计划数据***************
       BEGIN OF range_sales,
         kunnr    LIKE vbak-kunnr, "售达方
         bu_sort1 LIKE but000-bu_sort1, "客户检索项
       END OF range_sales,
       BEGIN OF range_plandata_s,
         detail(300) TYPE c,
       END OF range_plandata_s.
************************END**************************

DATA: gt_plos             TYPE TABLE OF typ_plos, "检验批数据
      gs_plos             LIKE LINE OF gt_plos,
      gt_material         TYPE TABLE OF typ_material, "物料主数据
      gs_material         LIKE LINE OF gt_material,
      gs_s_sample         TYPE typ_sample, "S码样本信息
      gs_sample           TYPE typ_sample, "样本信息
      gt_test             TYPE TABLE OF typ_test, "检验特性-已检
      gs_test             LIKE LINE OF gt_test,
      gt_test_s           TYPE TABLE OF typ_test, "S码-已检
      gt_untest_s         TYPE TABLE OF typ_untest_s, "S码未检
      gs_untest_s         LIKE LINE OF gt_untest_s,
      gt_s_info           TYPE TABLE OF typ_s_info, "S码信息
      gs_s_info           LIKE LINE OF gt_s_info,
      gt_charg            TYPE TABLE OF typ_charg, "批次数据
      gs_charg            LIKE LINE OF gt_charg,
      gt_gelatin          TYPE TABLE OF typ_gelatin, "明胶检验结果
      gs_gelatin          LIKE LINE OF gt_gelatin,
      gv_ok_code          TYPE sy-ucomm,
      gr_matnr_mj         TYPE RANGE OF matnr, "明胶物料代码
      gs_matnr_mj         LIKE LINE OF gr_matnr_mj,
      gs_range_sales      TYPE range_sales, "销售订单信息
      gs_range_plandata_s TYPE range_plandata_s. "S码检验计划数据

"DOI 标准变量
DATA:grf_container     TYPE REF TO cl_gui_container,
     grf_splitter      TYPE REF TO cl_gui_splitter_container,
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

CONSTANTS: cns_zstdname       TYPE atnam            VALUE 'ZSTDNAME', "标准名称-物料特性
           cns_zpspe          TYPE atnam            VALUE 'ZPSPE', "包装规格-物料特性
           cns_zwgspe         TYPE atnam            VALUE 'ZWGSPE', "型号规格-物料特性
           cns_zcuscode       TYPE atnam            VALUE 'ZCUSCODE', "客户检索项-物料特性
           cns_zcumcode       TYPE atnam            VALUE 'ZCUMCODE', "客户物料代码-物料特性
           cns_zchargl        TYPE atnam            VALUE 'ZCHARGL', "原辅包长批号-批次特性
           cns_z_batch_dypc   TYPE atnam            VALUE 'Z_BATCH_DYPC', "成品打印批号-批次特性
           cns_zpack_sta      TYPE atnam            VALUE 'ZPACK_STA', "包装规格-批次特性
           cns_mtart_yfl      TYPE mtart            VALUE 'Z010', "原辅料
           cns_mtart_bcp      TYPE mtart            VALUE 'Z030', "半成品
           cns_mtart_cp       TYPE mtart            VALUE 'Z050', "成品
           cns_bwart          TYPE bwart            VALUE '261',  "移动类型
           cns_doc_classname  TYPE sbdst_classname  VALUE 'HRFPM_EXCEL_STANDARD',
           cns_doc_classtype  TYPE sbdst_classtype  VALUE 'OT',
           cns_doc_object_key TYPE sbdst_object_key VALUE 'ZQMR0009',
           cns_prop_name(25)  TYPE c                VALUE 'BDS_KEYWORD',
           cns_sheetname(50)  TYPE c                VALUE '数据源',   "填充数据的excel-sheet页名
           cns_sheetname2(50) TYPE c                VALUE '数据修正'. "填充数据的第二页页名

SELECTION-SCREEN:BEGIN OF BLOCK blk1.
PARAMETERS p_pruef LIKE qals-prueflos. "检验批号
SELECTION-SCREEN:END OF BLOCK blk1.