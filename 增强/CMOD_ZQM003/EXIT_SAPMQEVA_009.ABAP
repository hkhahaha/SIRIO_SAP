FUNCTION EXIT_SAPMQEVA_009.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_QALS) LIKE  QALS STRUCTURE  QALS
*"             VALUE(I_QAVE) LIKE  QAVE STRUCTURE  QAVE
*"       EXPORTING
*"             VALUE(E_EXIT_ACTIV) LIKE  QM00-QKZ
*"             VALUE(E_SAMPLE_QUANTITY) LIKE  RQEVA-VMENGE03
*"       TABLES
*"              T_CHARACTERISTIC_RESULTS STRUCTURE  QAMKR
*"----------------------------------------------------------------------


  INCLUDE ZXQEVU11.


ENDFUNCTION.