FUNCTION EXIT_SAPMQPDA_001.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_OLD_CHAR) LIKE  PLMK-VERWMERKM
*"             VALUE(I_OLD_CHAR_PLANT) LIKE  PLMK-QPMK_ZAEHL
*"             VALUE(I_OLD_CHAR_VERSION) LIKE  PLMK-MKVERSION
*"             VALUE(I_OLD_CTRL_INDICATOR) LIKE  PLMK-STEUERKZ
*"             VALUE(I_NEW_CHAR) LIKE  PLMK-VERWMERKM
*"             VALUE(I_NEW_CHAR_PLANT) LIKE  PLMK-QPMK_ZAEHL
*"             VALUE(I_NEW_CHAR_VERSION) LIKE  PLMK-MKVERSION
*"             VALUE(I_PLNTY) LIKE  PLMK-PLNTY
*"             VALUE(I_PLNNR) LIKE  PLMK-PLNNR
*"             VALUE(I_WERKS) LIKE  PLKO-WERKS
*"             VALUE(I_PLNAL) LIKE  PLAS-PLNAL
*"             VALUE(I_PLNKN) LIKE  PLMK-PLNKN
*"             VALUE(I_PROFIDNETZ) LIKE  PLKO-PROFIDNETZ
*"             VALUE(I_PLNFL) LIKE  PLAS-PLNFL
*"             VALUE(I_STICHPRVER) LIKE  PLMK-STICHPRVER
*"             VALUE(I_VERWE) LIKE  PLKO-VERWE
*"             VALUE(I_GUELTIGAB) LIKE  PLMK-GUELTIGAB
*"             VALUE(I_LOCK_INDICATOR) LIKE  PLMK-QPMK_REF
*"             VALUE(I_AENDERGNR) LIKE  PLMK-AENDERGNR
*"             VALUE(I_SPCKRIT) LIKE  PLMK-SPCKRIT
*"             VALUE(I_QKZRASTER) LIKE  PLKO-QKZRASTER
*"             VALUE(I_DYNMERKREF) LIKE  PLMK-DYNMERKREF
*"             VALUE(I_QDYNREGEL) LIKE  PLMK-QDYNREGEL
*"       EXPORTING
*"             VALUE(E_LOCK_INDICATOR) LIKE  PLMK-QPMK_REF
*"----------------------------------------------------------------------


  INCLUDE ZXQPAU06.


ENDFUNCTION.