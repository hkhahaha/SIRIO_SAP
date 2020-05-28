*&---------------------------------------------------------------------*
*& 包含               ZSDR0023_TOP
*&---------------------------------------------------------------------*

TABLES:sscrfields.


TYPES: BEGIN OF ty_exclude,
         fcode LIKE sy-ucomm,
       END OF ty_exclude.

TYPES:BEGIN OF ty_upload,
        matnr  TYPE matnr,
        zpwsx  TYPE zpwsx,
        zjyrq  TYPE zjyrq,
        zpjno  TYPE zpjno,
        zqbno  TYPE zqbno,
        zxkzno TYPE zxkzno,
        uname  TYPE uname,
        erdat  TYPE datum,
        erzet  TYPE uzeit,
        maktx  TYPE maktx,
      END OF ty_upload.



TYPES: BEGIN OF ty_alv,
         sel(1),
         error(1),
         light    TYPE icon-id,
         msg      TYPE char100.
         INCLUDE TYPE ty_upload.
       TYPES:  END OF ty_alv.

CONSTANTS: gc_green  LIKE dv70a-statusicon VALUE '@08@',
           gc_yellow LIKE dv70a-statusicon VALUE '@09@',
           gc_red    LIKE dv70a-statusicon VALUE '@0A@'.

DATA: gs_alv TYPE ty_alv,
      gt_alv TYPE TABLE OF ty_alv.


*工具栏按钮
DATA: gv_functxt TYPE smp_dyntxt,
      gt_exclude TYPE TABLE OF ty_exclude.

DATA: gt_fieldcat    TYPE slis_t_fieldcat_alv.
DATA: gs_layout      TYPE slis_layout_alv.

TYPES:BEGIN OF ty_makt,
        matnr TYPE matnr,
        maktx TYPE maktx,
      END OF ty_makt.
DATA: lt_makt TYPE TABLE OF ty_makt.
DATA: ls_makt TYPE ty_makt.


DATA: gt_upload TYPE TABLE OF ty_upload.