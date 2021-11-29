*&---------------------------------------------------------------------*
*& 包含               ZMMR0032_TOP
*&---------------------------------------------------------------------*


TYPE-POOLS :slis.
" 使用ALV显示
DATA: i_fieldcat_alv TYPE slis_t_fieldcat_alv,
      wa_alv_field   TYPE slis_fieldcat_alv.
DATA: it_layout TYPE slis_layout_alv,
      wa_layout TYPE lvc_s_layo,
      lt_event  TYPE slis_t_event,
      ls_event  TYPE slis_alv_event.

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

TABLES: resb,ekko.

"打印
DATA: wa_control_parameters TYPE ssfctrlop,
      wa_out_option         TYPE ssfcompop,
      i_lastobjky(30)       TYPE c,  "最后一行的凭证号
      i_firstobjky(30)      TYPE c  "第一行的凭证号
      .

*定义输出内表

TYPES: BEGIN OF ls_ebeln,
         ebeln LIKE ekko-ebeln, "凭证编号
         lifnr LIKE ekko-lifnr, "供应商
         name1 LIKE lfa1-name1, "供应商名称
         ekgrp LIKE ekko-ekgrp, "采购组
         bedat LIKE ekko-bedat, "凭证日期
         ernam LIKE ekko-ernam, "制单人
         ebelp LIKE ekpo-ebelp, "行项目
         matnr LIKE ekpo-matnr,
         maktx LIKE makt-maktx,
         ekorg LIKE ekko-ekorg,
         sel   TYPE c, "选择
       END OF ls_ebeln.

DATA:zucomm TYPE sy-ucomm.

"生产订单
DATA lt_ebeln TYPE STANDARD TABLE OF ls_ebeln .
*DATA lt_ebeln_tmp LIKE TABLE OF ls_ebeln WITH HEADER LINE .

DATA: gt_head   TYPE TABLE OF zmms_zmmr0017_head,
      gs_head   LIKE zmms_zmmr0017_head,
      gt_detail TYPE TABLE OF zmms_zmmr0017_detail,
      gs_detail LIKE zmms_zmmr0017_detail,
      gs_line   TYPE i.
DATA : gv_tdsfname TYPE tdsfname .


DATA:ls_zmmt0032 TYPE zmmt0032,
     lt_zmmt0032 TYPE TABLE OF zmmt0032.


DATA: BEGIN OF ls_ebeln2,
        light      TYPE icon_d,
        msg(50),
        ebeln      LIKE ekko-ebeln, "凭证编号
        lifnr      LIKE ekko-lifnr, "供应商
        name1      LIKE lfa1-name1, "供应商名称
        ekgrp      LIKE ekko-ekgrp, "采购组
        bedat      LIKE ekko-bedat, "凭证日期
        ernam      LIKE ekko-ernam, "制单人
        ebelp      LIKE ekpo-ebelp, "行项目
        matnr      LIKE ekpo-matnr,
        maktx      LIKE makt-maktx,
        ekorg      LIKE ekko-ekorg,
        version(4),
        sign(15),"合同状态
        sel        TYPE char4, "选择
      END OF ls_ebeln2.


DATA lt_ebeln2 LIKE TABLE OF ls_ebeln2.

DATA: go_alv_grid TYPE REF TO cl_gui_alv_grid.