*&---------------------------------------------------------------------*
*& 包含               ZPPR0005_SRC
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) TEXT-001.
SELECTION-SCREEN POSITION 19.

PARAMETERS:p_create   RADIOBUTTON GROUP rg1 USER-COMMAND crt DEFAULT 'X'.
SELECTION-SCREEN COMMENT 24(8) TEXT-002 FOR FIELD p_create.
SELECTION-SCREEN POSITION 40.

PARAMETERS p_query RADIOBUTTON GROUP rg1 .
SELECTION-SCREEN COMMENT 48(8) TEXT-003 FOR FIELD p_query.

PARAMETERS p_rqr RADIOBUTTON GROUP rg1 .
SELECTION-SCREEN COMMENT 59(10) TEXT-005 FOR FIELD p_query.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-004.
PARAMETERS p_dwerk TYPE afpo-dwerk OBLIGATORY DEFAULT '8100'.
SELECT-OPTIONS:s_reqno FOR zppt0003-zsapnote_no MODIF ID m2 ,
               s_aufnr FOR afpo-aufnr ,
               s_kdauf FOR afpo-kdauf MODIF ID m1,
               s_kdpos FOR afpo-kdpos MODIF ID m1,
               s_gstrp FOR afko-gstrp MODIF ID m1,
               s_matnr FOR afpo-matnr ,
               s_charg FOR afpo-charg ,
               s_verid FOR afpo-verid MODIF ID m1,
               s_fevor FOR afko-fevor MODIF ID m1,
               s_lgort FOR afpo-lgort MODIF ID m1 NO-DISPLAY,
               s_budat FOR zppt0003-budat MODIF ID m2,
               s_uname FOR zppt0003-uname DEFAULT sy-uname MODIF ID m2.
PARAMETERS: p_sttxt TYPE c AS CHECKBOX DEFAULT 'X' MODIF ID m1,  "排除TECO
            p_del   TYPE char1 AS CHECKBOX MODIF ID m2,
            p_cqr   TYPE char1 AS CHECKBOX MODIF ID m3 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blk1.