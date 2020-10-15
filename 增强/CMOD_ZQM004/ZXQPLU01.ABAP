*&---------------------------------------------------------------------*
*& 包含               ZXQPLU01
*&---------------------------------------------------------------------*
****读取检验批相关数据，在增强ZQM04中的 EXIT_SAPLQPAP_002出口中调用

e_hersteller = i_hersteller.
e_kunde = i_kunde.
e_material = i_material.
e_verwendung = i_verwendung.
e_lieferant = i_lieferant.
e_werks = i_werks.


FREE MEMORY ID 'ZQM01_QALS'.

EXPORT  a = i_qals TO MEMORY ID 'ZQM01_QALS'.