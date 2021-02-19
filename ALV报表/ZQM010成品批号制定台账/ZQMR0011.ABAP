*&---------------------------------------------------------------------*
*& Report ZQMR0011
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZQMR0011.

INCLUDE ZQMR0011_TOP.

INCLUDE ZQMR0011_S01.

INCLUDE ZQMR0011_F01.

START-OF-SELECTION.
  PERFORM FRM_AUTHORITY_CHECK.

  PERFORM FRM_GET_DATA.

  PERFORM FRM_DISPLAY_ALV.