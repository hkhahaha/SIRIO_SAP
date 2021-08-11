*& Report  ：ZMMR0050
*&---------------------------------------------------------------------*
*& Tcode    ：
*& 功能     ：采购对账单
*&---------------------------------------------------------------------*
*& Author:  YANGHUIBO
*& Date:    2020-03-25
*&---------------------------------------------------------------------*
*& Modification Log:
*& Version   Date        Author       DESCRIPTION     CHANGE REQUEST
*& -------- ----------  -----------  -------------  -----------------
*&---------------------------------------------------------------------*
REPORT ZMMR0050.



INCLUDE  ZMMR0050_TOP.

INCLUDE  ZMMR0050_CLASS.

INCLUDE  ZMMR0050_MODULE.

INCLUDE  ZMMR0050_F01.


AT SELECTION-SCREEN OUTPUT .

 PERFORM FRM_GET_SCREEN.



START-OF-SELECTION.


  PERFORM SUB_GET_DATA.             "获取数据

 " PERFORM SUB_DEAL_DATA.            "处理数据



END-OF-SELECTION.

  PERFORM SUB_SHOW_ALV.