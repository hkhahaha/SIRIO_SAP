function exit_saplvbmd_001.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_BNCOM) LIKE  BNCOM STRUCTURE  BNCOM OPTIONAL
*"  CHANGING
*"     VALUE(C_BEZDT) LIKE  MCHA-VFDAT OPTIONAL
*"     VALUE(C_IPRKZ_HBD) LIKE  MARA-IPRKZ OPTIONAL
*"     VALUE(C_IPRKZ_RLZ) LIKE  MARA-IPRKZ OPTIONAL
*"     VALUE(C_RDMHD) LIKE  MARA-RDMHD OPTIONAL
*"     VALUE(C_GESHB) LIKE  MARA-MHDHB OPTIONAL
*"     VALUE(C_RSTLZ) LIKE  MARA-MHDRZ OPTIONAL
*"     VALUE(C_MHDAT) LIKE  MCHA-VFDAT OPTIONAL
*"     VALUE(C_HSDAT) LIKE  MCHA-VFDAT OPTIONAL
*"     VALUE(C_CALC) OPTIONAL
*"     VALUE(C_CHECK) OPTIONAL
*"----------------------------------------------------------------------


include zxmhdu01 .


endfunction.