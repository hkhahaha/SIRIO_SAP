*&---------------------------------------------------------------------*
*& Report ZQMR008
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zqmr008.

INCLUDE: zqmr008_top,
         zqmr008_scr,
         zqmr008_f01.



INITIALIZATION.

AT SELECTION-SCREEN.


START-OF-SELECTION.
  PERFORM frm_authority_check.

  PERFORM frm_get_data.

  PERFORM frm_dis_alv.

END-OF-SELECTION.