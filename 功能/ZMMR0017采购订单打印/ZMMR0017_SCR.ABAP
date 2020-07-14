*&---------------------------------------------------------------------*
*& 包含               ZMMR0017_SCR
*&---------------------------------------------------------------------*



SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-t01.

* PARAMETERS s_aufnr TYPE afko-aufnr.
* SELECT-OPTIONS s_aufnr FOR afko-aufnr.
PARAMETERS: s_EBELN type ekko-EBELN OBLIGATORY.
PARAMETERS: p_werks TYPE ekpo-werks OBLIGATORY.

SELECTION-SCREEN SKIP 1.

*PARAMETERS:  s_ret RADIOBUTTON GROUP g,
*             s_pro RADIOBUTTON GROUP g.
*


SELECTION-SCREEN END OF BLOCK blk1.