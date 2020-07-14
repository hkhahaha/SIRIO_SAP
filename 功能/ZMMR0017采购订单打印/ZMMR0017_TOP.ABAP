*&---------------------------------------------------------------------*
*& 包含               ZMMR0017_TOP
*&---------------------------------------------------------------------*


DATA: gt_head   TYPE TABLE OF zmms_zmmr0017_head,
      gs_head   LIKE zmms_zmmr0017_head,
      gt_detail TYPE TABLE OF zmms_zmmr0017_detail,
      gs_detail LIKE zmms_zmmr0017_detail,
      gs_line   TYPE i.
DATA p_page TYPE p DECIMALS 1.
DATA: p_ltext   TYPE tdline.    