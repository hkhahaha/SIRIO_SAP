*&---------------------------------------------------------------------*
*& 包含               ZCOR0001_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 包含               ZPPR0003_TOP
*&---------------------------------------------------------------------*
TABLES:sscrfields,keko,ckis,tck06,t001w,makt,zppt0010,zcot0001.
DATA:go_docking             TYPE REF TO cl_gui_docking_container,
     go_alv_grid_main       TYPE REF TO cl_gui_alv_grid,
     go_alv_grid_cancel     TYPE REF TO cl_gui_alv_grid,
     go_dialogbox_container TYPE REF TO cl_gui_dialogbox_container,
     go_alv_grid_detail     TYPE REF TO cl_gui_alv_grid,
     gs_disvariant          TYPE disvariant,
     gs_layout              TYPE lvc_s_layo,
     gs_layout_detail       TYPE lvc_s_layo,
     gt_fieldcatalog        TYPE lvc_t_fcat,
     gt_fldcat_detail       TYPE lvc_t_fcat,
     gv_grid_title          TYPE  lvc_title.
DATA: gv_code TYPE sy-ucomm.
DATA(gs_stable) = VALUE lvc_s_stbl( row = abap_true col = abap_true ).

DATA: gt_output TYPE TABLE OF zscor001 WITH NON-UNIQUE KEY matnr werks kadky bwvar kalnr zz001  WITH NON-UNIQUE SORTED KEY zindex_checked COMPONENTS  zchecked, "alv输出内表
      ls_alv    TYPE zscor001,
      gt_help   TYPE TABLE OF zppt0010 WITH NON-UNIQUE KEY matnr werks zz001 WITH NON-UNIQUE SORTED KEY matnr_werks_zz001 COMPONENTS matnr werks zz001.  "报价单号搜索帮助

DATA:lt_celltab TYPE lvc_t_styl,  "ALV 控制: 单元格的类型表
     ls_celltab TYPE lvc_s_styl.   "ALV 控制: 单元格的类型表

DATA: BEGIN OF t_alv,
        select(1)   TYPE c, "选择
        matnr       LIKE keko-matnr, "物料编号
        maktx       LIKE makt-maktx, "物料描述
        werks       LIKE keko-werks, "工厂编号
        name1       LIKE t001w-name1, "工厂描述
        kadky       LIKE keko-kadky, "成本核算日期
        kalnr       LIKE keko-kalnr, "成本估算编号
        bwvar       LIKE keko-bwvar, "估算变式编号
        txbwv       LIKE tck06-txbwv, "估算变式描述
        losgr       LIKE keko-losgr,   "成本核算批量
        zz001       LIKE zppt0010-zz001, "需求报价单号
        gpreis1     LIKE ckis-gpreis,  "成本(原料/包材)
        gpreis2     LIKE ckis-gpreis,  "直接人工
        gpreis3     LIKE ckis-gpreis,  "折旧
        gpreis4     LIKE ckis-gpreis,  "能耗
        gpreis5     LIKE ckis-gpreis,  "制造费用
        flag(1)     TYPE c,  "发送标识
        message(30) TYPE c,        "消息
        uname(12)   TYPE c,
        cpudt       TYPE cpudt,
      END OF t_alv.