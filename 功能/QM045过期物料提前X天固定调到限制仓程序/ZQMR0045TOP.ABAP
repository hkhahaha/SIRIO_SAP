*&---------------------------------------------------------------------*
*& 包含               ZQMR0045TOP
*&---------------------------------------------------------------------*
TABLES:makt,mara,marc,qals,aufm,mard,zmm_changeno,mseg.
TYPE-POOLS:slis."调用系统存在的类型池
*在调用ALV之前，需要先定义Layout和Fieldcat，他们属于slis类型池
DATA:fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     layout   TYPE slis_layout_alv,
     w_repid  TYPE sy-repid. "记录系统当前的程序名
DATA:lt_alv TYPE TABLE OF zmm_changeno.
DATA wms_custid TYPE char10.
TABLES:bkpf.
DATA:lbkpf LIKE STANDARD TABLE OF bkpf WITH HEADER LINE."标准表格式的内表lbkpf
DATA: gt_wms_custid_help TYPE TABLE OF zmmwms_sys.
DATA t_link LIKE TABLE OF tline WITH HEADER LINE .
"进行类型定义
"创建通知单抬头字段
TYPES:BEGIN OF ty_head,
        zsapnote_no TYPE zmm_changeno-zsapnote_no,
        wms_custid  TYPE zmm_changeno-wms_custid,
        werks       TYPE mard-werks,
        bwart       TYPE mseg-bwart,
        note        TYPE zmm_changeno-note,
      END OF ty_head.
"创建通知单行项目信息
TYPES:BEGIN OF ty_item.
        INCLUDE TYPE zmm_changeno.
      TYPES:
              light TYPE icon_d, " 状态灯
              maktx TYPE makt-maktx, "物料描述
            END OF ty_item.




DATA: gv_name  TYPE vrm_id,
      gv_name2 TYPE vrm_id,
      gv_name3 TYPE vrm_id,
      gt_list  TYPE vrm_values,
      gt_list2 TYPE vrm_values, "ITAB
      lv_num   TYPE zesapnote_no,
      lv_item  TYPE zesapnote_line.


DATA: gt_ext_st   TYPE TABLE OF zmmwms_ext_st,
      gs_ext_st   LIKE LINE OF gt_ext_st,
      gt_head     TYPE TABLE OF ty_head,
      gs_head     LIKE LINE OF gt_head,
      gt_item     TYPE TABLE OF ty_item,
      gs_item     LIKE LINE OF gt_item,
      t001w-werks LIKE t001w-werks.