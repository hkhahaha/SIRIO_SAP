*&---------------------------------------------------------------------*
*& 包含               ZMMR0004_SRC
*&---------------------------------------------------------------------*

TABLES: ekko, zmmwms_pol, mara.

SELECTION-SCREEN BEGIN OF BLOCK blk_scr1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_ekorg TYPE ekorg.                          "采购组织
SELECT-OPTIONS: s_werks FOR zmmwms_pol-werks,            "工厂
                s_ekgrp FOR ekko-ekgrp,                  "采购组
                s_lifnr FOR ekko-lifnr,                  "供应商
                s_ebeln FOR ekko-ebeln,                  "采购凭证
            s_matkl FOR mara-matkl,                      "物料组
            s_matnr FOR mara-matnr,                      "物料
            s_dat FOR zmmwms_pol-pdlv_date,              "确定送货日期
            s_aram FOR ekko-ernam.                       "采购订单制单人
PARAMETERS: p_code TYPE char2.                           "指定标准
PARAMETERS: p_closed(1) AS LISTBOX VISIBLE LENGTH 8.     "关闭标识


SELECTION-SCREEN END OF BLOCK blk_scr1.