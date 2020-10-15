FUNCTION EXIT_SAPLQPL1_001.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_QALS) LIKE  QALS STRUCTURE  QALS
*"             VALUE(I_HERSTELLER) LIKE  RMQEA-HERSTELLER
*"             VALUE(I_KUNDE) LIKE  RMQEA-KUNNR
*"             VALUE(I_MATERIAL) LIKE  RMQEA-MATNR
*"             VALUE(I_REVLV) LIKE  RMQEA-REVLV
*"             VALUE(I_STICHTAG) LIKE  RMQEA-STICHTAG
*"             VALUE(I_VERWENDUNG) LIKE  RMQEA-PPLVERW
*"             VALUE(I_WERKS) LIKE  RMQEA-WERKS
*"             VALUE(I_LIEFERANT) LIKE  RMQEA-LIFNR
*"       EXPORTING
*"             VALUE(E_HERSTELLER) LIKE  RMQEA-HERSTELLER
*"             VALUE(E_LIEFERANT) LIKE  RMQEA-LIFNR
*"             VALUE(E_KUNDE) LIKE  RMQEA-KUNNR
*"             VALUE(E_MATERIAL) LIKE  RMQEA-MATNR
*"             VALUE(E_SUCHFELD) LIKE  RMQEA-SUCHFELD
*"             VALUE(E_VERWENDUNG) LIKE  RMQEA-PPLVERW
*"             VALUE(E_ACTIVE) LIKE  QM00-QKZ
*"             VALUE(E_REVLV) LIKE  RMQEA-REVLV
*"             VALUE(E_WERKS) LIKE  RMQEA-WERKS
*"             VALUE(E_STICHTAG) LIKE  RMQEA-STICHTAG
*"----------------------------------------------------------------------


  INCLUDE ZXQPLU01.


ENDFUNCTION.