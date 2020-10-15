FUNCTION EXIT_SAPLCQBM_002.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_PLMKB) LIKE  PLMKB STRUCTURE  PLMKB
*"             VALUE(I_QPMK) LIKE  QPMK STRUCTURE  QPMK
*"             VALUE(I_QPMT) LIKE  QPMT STRUCTURE  QPMT
*"             VALUE(I_QPMZ) LIKE  QPMZ STRUCTURE  QPMZ
*"             VALUE(I_RQPAS) LIKE  RQPAS STRUCTURE  RQPAS
*"             VALUE(I_NO_COPY_TXT) LIKE  PLMKB-LTEXTKZ
*"       EXPORTING
*"             VALUE(E_QPMK) LIKE  QPMK STRUCTURE  QPMK
*"             VALUE(E_QPMT) LIKE  QPMT STRUCTURE  QPMT
*"             VALUE(E_QPMZ) LIKE  QPMZ STRUCTURE  QPMZ
*"             VALUE(E_UNLOCK_QPMK) LIKE  QM00-QKZ
*"----------------------------------------------------------------------


  INCLUDE ZXCQBU02.


ENDFUNCTION.