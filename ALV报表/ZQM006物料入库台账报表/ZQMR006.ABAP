*&---------------------------------------------------------------------*
*& Report ZQMR006
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZQMR006.

INCLUDE: ZQMR006_top,
         ZQMR006_scr,
         ZQMR006_f01.



INITIALIZATION.

AT SELECTION-SCREEN.


START-OF-SELECTION.
  PERFORM frm_authority_check.

  PERFORM frm_get_data.

  PERFORM frm_dis_alv.

    END-OF-SELECTION.