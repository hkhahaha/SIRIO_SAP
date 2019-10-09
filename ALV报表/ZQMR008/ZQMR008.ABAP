*&---------------------------------------------------------------------*
*& Report ZQMR008
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZQMR008.

INCLUDE: ZQMR008_top,
         ZQMR008_scr,
         ZQMR008_f01.



INITIALIZATION.

AT SELECTION-SCREEN.


START-OF-SELECTION.

  PERFORM frm_get_data.

  PERFORM frm_dis_alv.