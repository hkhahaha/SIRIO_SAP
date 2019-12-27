*&---------------------------------------------------------------------*
*& Report ZFIR0021
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFIR0021.

INCLUDE: ZFIR0021_TOP.

INCLUDE: ZFIR0021_S01.

INCLUDE: ZFIR0021_F01.


START-OF-SELECTION.

  PERFORM FRM_AUTHORITY_CHECK.

  PERFORM FRM_GET_DATA.

  PERFORM FRM_DISPLAY_ALV.