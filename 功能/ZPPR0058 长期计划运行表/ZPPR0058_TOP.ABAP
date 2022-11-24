*&---------------------------------------------------------------------*
*& 包含               ZPPR0058_TOP
*&---------------------------------------------------------------------*
TABLES:pbim,pbed.
* TAB_TRIPE的tab function
CONSTANTS:BEGIN OF gc_tab_funs,
            zfc_pir_gross     LIKE sy-ucomm  VALUE 'ZFC_PIR_GROSS', "PIR毛需求的TAB FUNCTION
            zfc_pir_raw_gross LIKE sy-ucomm VALUE 'ZFC_PIR_RAW_GROSS', "PIR原辅料毛需求的TAB FUNCTION
            zfc_pir_net       LIKE sy-ucomm VALUE 'ZFC_PIR_NET', "PIR净需求的TAB FUNCTION
            zfc_pir_raw_net   LIKE sy-ucomm VALUE 'ZFC_PIR_RAW_NET', "PIR净需求的TAB FUNCTION
            zfc_warehouse     LIKE sy-ucomm VALUE 'ZFC_WAREHOUSE', "PIR原辅料净需求的TAB FUNCTION
          END OF gc_tab_funs.
CONTROLS ztab_strip TYPE TABSTRIP. "与屏幕9000中的tabstrip的名称一致
DATA:BEGIN OF gs_tab_appl,
       subscreen   LIKE sy-dynnr,
       prog        LIKE sy-repid VALUE 'ZPPR0058',
       pressed_tab LIKE sy-ucomm VALUE gc_tab_funs-zfc_pir_gross, "默认激活PIR毛需求的TAB
     END OF gs_tab_appl.
DATA:BEGIN OF gs_pir_gross,
       werks     TYPE pbim-werks, "工厂
       matnr     TYPE pbim-matnr, "物料编号
       maktx     TYPE makt-maktx, "物料描述
       zpspe     TYPE zpspe, "包装规格
       versb     TYPE pbim-versb, "独立需求的版本号
       pbdnr     TYPE pbim-pbdnr, "需求计划
       meins     TYPE pbed-meins, "基本计量单位
       bztxt     TYPE t171t-bztxt, "二级部门
       name_org1 TYPE but000-name_org1, "业务员
       kunnr     TYPE kunnr, "客户编号
       bu_sort1  TYPE but000-bu_sort1, "客户简称
       ym1       TYPE plnmg, "期间1-计划数量
       ym2       TYPE plnmg, "期间2-计划数量
       ym3       TYPE plnmg, "期间3-计划数量
       ym4       TYPE plnmg, "期间4-计划数量
       ym5       TYPE plnmg, "期间5-计划数量
       ym6       TYPE plnmg, "期间6-计划数量
       ym7       TYPE plnmg, "期间7-计划数量
       ym8       TYPE plnmg, "期间8-计划数量
       ym9       TYPE plnmg, "期间9-计划数量
       ym10      TYPE plnmg, "期间10-计划数量
       ym11      TYPE plnmg, "期间11-计划数量
       ym12      TYPE plnmg, "期间12-计划数量
     END OF gs_pir_gross.

DATA:BEGIN OF gs_ymfield_desc,
       ym1  TYPE char10, "期间1-计划数量
       ym2  TYPE char10, "期间2-计划数量
       ym3  TYPE char10, "期间3-计划数量
       ym4  TYPE char10, "期间4-计划数量
       ym5  TYPE char10, "期间5-计划数量
       ym6  TYPE char10, "期间6-计划数量
       ym7  TYPE char10, "期间7-计划数量
       ym8  TYPE char10, "期间8-计划数量
       ym9  TYPE char10, "期间9-计划数量
       ym10 TYPE char10, "期间10-计划数量
       ym11 TYPE char10, "期间11-计划数量
       ym12 TYPE char10, "期间12-计划数量
     END OF     gs_ymfield_desc.
DATA:BEGIN OF gs_pir_period,
       perxx      TYPE perxx,
       field      TYPE char6,
       field_desc TYPE char15,
     END OF gs_pir_period.
DATA:BEGIN OF gs_pir_raw_gross,
       zid   TYPE syst_tabix,
       idnrk TYPE matnr, "组件物料编号
       ojtxp TYPE makt-maktx, "组件物料描述
       mmein TYPE meins, "组件物料基本单位
       plifz TYPE marc-plifz, "计划交货时间
       webaz TYPE marc-webaz. "以天计的收货处理时间
       INCLUDE STRUCTURE gs_pir_gross.
     DATA:END OF gs_pir_raw_gross.
DATA:BEGIN OF gs_pir_net,
       werks     TYPE pbim-werks, "工厂
       matnr     TYPE pbim-matnr, "物料编号
       maktx     TYPE makt-maktx, "物料描述
       meins     TYPE pbed-meins, "基本计量单位
       kalab     TYPE mska-kalab, "销售订单库存
       menge     TYPE matdoc-menge, "销售订单已发货数量(基本数量)
       menge_wip TYPE menge_d, "WIP还原数量
       wip_flag  TYPE char1, "仅WIP还原
       ym1       TYPE plnmg, "期间1-计划数量
       ym2       TYPE plnmg, "期间2-计划数量
       ym3       TYPE plnmg, "期间3-计划数量
       ym4       TYPE plnmg, "期间4-计划数量
       ym5       TYPE plnmg, "期间5-计划数量
       ym6       TYPE plnmg, "期间6-计划数量
       ym7       TYPE plnmg, "期间7-计划数量
       ym8       TYPE plnmg, "期间8-计划数量
       ym9       TYPE plnmg, "期间9-计划数量
       ym10      TYPE plnmg, "期间10-计划数量
       ym11      TYPE plnmg, "期间11-计划数量
       ym12      TYPE plnmg, "期间12-计划数量
     END OF gs_pir_net.
DATA:BEGIN OF gs_pir_raw_net,
       zid       TYPE syst_tabix,
       idnrk     TYPE matnr, "组件物料编号
       ojtxp     TYPE makt-maktx, "组件物料描述
       mmein     TYPE meins, "组件物料基本单位
       plifz     TYPE marc-plifz, "计划交货时间
       webaz     TYPE marc-webaz, "以天计的收货处理时间
       raw_stock TYPE labst. "在制品还原成的原辅包材库存
       INCLUDE STRUCTURE gs_pir_net.
     DATA:END OF gs_pir_raw_net.
DATA:BEGIN OF gs_matnr,
       matnr TYPE matnr,
       objek TYPE ausp-objek,
     END OF gs_matnr.

DATA:BEGIN OF gs_pir_delivery,
       werks   TYPE pbim-werks, "工厂
       matnr   TYPE pbim-matnr, "物料编号
       menge_s TYPE matdoc-menge, "销售订单已发货数量(基本数量)
       menge_h TYPE matdoc-menge, "冲销销售订单已发货数量(基本数量)
     END OF gs_pir_delivery.
DATA:BEGIN OF gs_pir_so_inventory,
       werks TYPE pbim-werks, "工厂
       matnr TYPE pbim-matnr, "物料编号
       kalab TYPE mska-kalab, "非限制数量
       kains TYPE mska-kains, "质检数量
       kaspe TYPE mska-kaspe, "冻结数量
     END OF gs_pir_so_inventory.
DATA:BEGIN OF gs_pir_wip_inventory,
       werks TYPE pbim-werks, "工厂
       matnr TYPE pbim-matnr, "物料编号
       kalab TYPE mska-kalab, "非限制数量
       kains TYPE mska-kains, "质检数量
       kaspe TYPE mska-kaspe, "冻结数量
       mtart TYPE mara-mtart,
       meins TYPE mara-meins,
       maktx TYPE makt-maktx,
     END OF gs_pir_wip_inventory.
DATA: gt_pir_gross                LIKE TABLE OF gs_pir_gross,
      gt_pir_raw_gross            LIKE TABLE OF gs_pir_raw_gross,
      gt_pir_raw_net              LIKE TABLE OF gs_pir_raw_net,
      gt_pir_raw_net_bak          LIKE TABLE OF gs_pir_raw_net,
      gt_pir_raw_net_sum          LIKE TABLE OF gs_pir_raw_net,
      gt_pir_net                  LIKE TABLE OF gs_pir_net,
      gt_pir_delivery             LIKE TABLE OF gs_pir_delivery,
      gt_pir_so_inventory         LIKE TABLE OF gs_pir_so_inventory,
      gt_raw_inventory            LIKE TABLE OF gs_pir_so_inventory,
      gt_pir_wip_inventory        LIKE TABLE OF gs_pir_wip_inventory,
      gs_zppt0011                 TYPE zppt0011,
      gt_zppt0011                 TYPE TABLE OF zppt0011,
      gr_pdatu                    TYPE RANGE OF pbed-pdatu,
      gv_okcode                   LIKE sy-ucomm,
      gv_savecode                 LIKE sy-ucomm,
      gv_calc_pir_raw_gross_flag  TYPE flag,
      gv_calc_pir_raw_net_flag    TYPE flag,
      gv_calc_pir_net_flag        TYPE flag,
      gv_calc_raw_net_flag        TYPE flag,
      gv_detail_2_sum_flag        TYPE flag,
      gv_action_by_zbn_switch     TYPE flag,
      gv_switch                   TYPE char1, "显示明细/汇总标识，1-明细，2-汇总
      gt_pir_period               LIKE TABLE OF gs_pir_period,
      go_con_pir_gross            TYPE REF TO cl_gui_custom_container,
      go_con_pir_raw_gross        TYPE REF TO cl_gui_custom_container,
      go_con_pir_net              TYPE REF TO cl_gui_custom_container,
      go_con_pir_raw_net          TYPE REF TO cl_gui_custom_container,
      go_con_warehouse            TYPE REF TO cl_gui_custom_container,
      go_salv_columns             TYPE REF TO cl_salv_columns_table,
      go_salv_table_pir_gross     TYPE REF TO  cl_salv_table,
      go_salv_table_pir_raw_gross TYPE REF TO  cl_salv_table,
      go_salv_table_pir_net       TYPE REF TO  cl_salv_table,
      go_salv_table_pir_raw_net   TYPE REF TO  cl_salv_table,
      go_alv_grid_wh              TYPE REF TO cl_gui_alv_grid,
      gt_alv_fidcat_wh            TYPE lvc_t_fcat,
      gs_layout                   TYPE lvc_s_layo,
      gs_stable                   TYPE lvc_s_stbl,
      gt_req                      TYPE TABLE OF zpps_bom_req,
      gt_resp_header              TYPE TABLE OF zpps_bom_resp_header WITH NON-UNIQUE SORTED KEY bom_zid COMPONENTS zid,
      gt_resp_item                TYPE TABLE OF zpps_bom_resp_item WITH NON-UNIQUE SORTED KEY bom_zid COMPONENTS zid,
      gs_bapiret                  TYPE bapiret2.

FIELD-SYMBOLS <fs_pir_gross> LIKE gs_pir_gross.
FIELD-SYMBOLS <fs_zppt0011> LIKE zppt0011.
TYPES ty_tab_pir_delivery LIKE TABLE OF gs_pir_delivery.