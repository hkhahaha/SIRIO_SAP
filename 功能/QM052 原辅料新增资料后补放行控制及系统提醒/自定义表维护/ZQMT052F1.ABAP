*----------------------------------------------------------------------*
***INCLUDE LZQMT052F04.
*----------------------------------------------------------------------*
FORM ZQMT052F1.
    DATA:F_INDEX LIKE SY-TABIX.
    LOOP AT TOTAL.
      READ TABLE EXTRACT WITH KEY <vim_xtotal_key>.
      IF SY-SUBRC = 0.
        F_INDEX = SY-TABIX.
      ELSE.
        CLEAR F_INDEX.
      ENDIF.
  
      MODIFY total.
      CHECK F_INDEX GT 0 .
      EXTRACT = total.
      MODIFY EXTRACT INDEX F_INDEX.
    ENDLOOP.
  ENDFORM.