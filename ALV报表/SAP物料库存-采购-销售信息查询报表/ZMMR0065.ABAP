*&---------------------------------------------------------------------*
*& Report ZMMR0065
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZMMR0065.
INCLUDE: zmmr0065_top,
         zmmr0065_scr,
         zmmr0065_f01.



INITIALIZATION.

AT SELECTION-SCREEN.


START-OF-SELECTION.
  PERFORM frm_authority_check.

  PERFORM frm_get_data.

  PERFORM frm_dis_alv.

END-OF-SELECTION.