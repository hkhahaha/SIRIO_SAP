*&---------------------------------------------------------------------*
*& 包含               ZMMR0004_TOP
*&---------------------------------------------------------------------*
"ALV报表中需要展示的字段
TYPES:BEGIN OF ty_alv,
        light         TYPE char1,            "指示灯
        dlv_planno    TYPE zmmwms_pol-dlv_planno,   "送货计划号
        dlv_itemno    TYPE zmmwms_pol-dlv_itemno,   "送货计划号
        guid          TYPE char32,                  "唯一标识
        ebeln         TYPE zmmwms_pol-ebeln,        "采购订单号
        ebelp         TYPE zmmwms_pol-ebelp,        "采购订单行
        lifnr         TYPE ekko-lifnr,              "供应商编号
        name1_gp      TYPE lfa1-name1,              "供应商名称
        pdlv_date     TYPE zmmwms_pol-pdlv_date,    "答复送货日期
        delivery_days TYPE i,                       "预计送货天数
        matnr         TYPE zmmwms_pol-matnr,        "物料编号
        maktx         TYPE maktx,                   "物料描述
        matkl         TYPE matkl,                   "物料组
        producer      TYPE zmmwms_pol-producer,     "制造商编号
        producer_name TYPE lfa1-name1,              "制造商名称
        aps_req_date  type ZREQDATE,                "APS需求日期
        pdlv_date_ext TYPE zmmwms_pol-pdlv_date_ext, "最新调整送货日期
        meins         TYPE zmmwms_pol-meins,        "基本计量单位
        mseht         TYPE t006a-mseht,             "基本计量单位文本
        pdlv_menge    TYPE zmmwms_pol-pdlv_menge,   "确认送货数量
        recv_menge    TYPE zmmwms_pol-pdlv_menge,   "确认日期净收货量
        undlv_mende   TYPE zmmwms_pol-pdlv_menge,   "剩余未到货量
        delay         TYPE i,                       "延期天数
        count_year    TYPE i,                       "当前交货次数
        min_dlv       TYPE marc-bstmi,              "最小订购批量
        origin        TYPE zmmwms_pol-origin,       "计划来源
        origin_txt    TYPE char5,               "计划来源文本
        responsible   TYPE char70,                  "采购部门负责人
        made_in_china TYPE char70,                  "原产地
        xprecious     TYPE char70,                  "贵重标识
        xclosed       TYPE zmmwms_pol-xclosed,      "关闭标识
      END OF ty_alv.


TYPES: BEGIN OF ty_bwart_merge,
         dlv_planno TYPE zmmwms_pol-dlv_planno,
         dlv_itemno TYPE zmmwms_pol-dlv_itemno,
         num        TYPE i,
       END OF ty_bwart_merge.

DATA: gr_matnr TYPE RANGE OF mara-matnr.


DATA:gt_alv TYPE TABLE OF ty_alv,
     gs_alv TYPE ty_alv.


DATA: gs_program TYPE salv_s_layout_key, "该结构包含了布局变式所属程序名
      gr_table   TYPE REF TO cl_salv_table,
      gr_columns TYPE REF TO cl_salv_columns_table,
      gr_column  TYPE REF TO cl_salv_column_table,
      gr_layout  TYPE REF TO cl_salv_layout.

DATA: BEGIN OF gs_aps,
        dlv_planno TYPE zmmwms_pol-dlv_planno,   "送货计划号
        dlv_itemno TYPE zmmwms_pol-dlv_itemno,   "送货计划号
        guid       TYPE char32,                           "唯一标识
        reqdate    TYPE dats,                          "APS需求日期
      END OF gs_aps.


DATA gt_aps LIKE TABLE OF gs_aps.