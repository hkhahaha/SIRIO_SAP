*&------------------------------------------------------------------
*& PROGRAM NAME: ZQMR0009
*& T-CODE:  ZQM009
*& PORGRAM TYPE: REPORT
*& DESCRIPTION: COA打印
*& DS NUMBER: QM_029
*&------------------------------------------------------------------
*& AUTHOR:  朱尤陆
*& DATE:    2019.09.23
*&------------------------------------------------------------------
*& MODIFICATION LOG:
*& VERSION   DATE        AUTHOR       DESCRIPTION     CHANGE REQUEST
*& -------- ----------  -----------  -------------  -----------------
*& 1.0      2019.09.23  朱尤陆
*&-------------------------------------------------------------------

INCLUDE ZQMR0009_TOP                            .  " Global Data
INCLUDE ZQMR0009_O01                            .  " PBO-Modules
INCLUDE ZQMR0009_I01                            .  " PAI-Modules
INCLUDE ZQMR0009_F01                            .  " FORM-Routines

START-OF-SELECTION.
  PERFORM frm_get_data.

END-OF-SELECTION.
  CALL SCREEN 0100.