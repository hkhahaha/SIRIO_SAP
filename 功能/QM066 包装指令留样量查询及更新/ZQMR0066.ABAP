**&********************************************************************
*& PROGRAM NAME        : 包装指令留样量查询及更新                      *
*& Module Name         : QM                                           *
*& Apply Author        :                                              *
*& Author              : ZHANGWANG                                    *
*& Started on          : 20230317                                     *
*& Transaction         : ZQM066                                       *
*& Program type        :                                              *
*& SAP Release         :                                              *
*& Program ID          : ZQMR0066                                     *
*& Program Description : 包装指令留样量查询及更新                      *
*&*&*******************************************************************

REPORT ZQMR0066.

INCLUDE ZQMR0066_TOP.

INCLUDE ZQMR0066_FRM.

*&---------------------------------------------------------------------*
*&      INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.

  PERFORM FRM_INIT.

*----------------------------------------------------------------------*
*        AT SELECTION-SCREEN ON VALUE-REQUEST                          *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&       AT SELECTION-SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM FRM_PBO.

*----------------------------------------------------------------------*
*        AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*&---------------------------------------------------------------------*
*&      START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  CASE 'X'.
    WHEN  R1.
      PERFORM FRM_CHECK_INPUT_R1.

      PERFORM FRM_GET_DATA_R1.
    WHEN  R2.
      PERFORM FRM_CHECK_INPUT_R2.

      PERFORM FRM_GET_DATA_R2.
    WHEN OTHERS.
  ENDCASE .

*&---------------------------------------------------------------------*
*&      END-OF-SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      frm_alv_show.
*&---------------------------------------------------------------------*

  CASE 'X'.
    WHEN  R1.
      PERFORM F_PROCESS_DATA_R1.

      PERFORM F_SHOW_ALV_R1.
    WHEN  R2.
      PERFORM F_PROCESS_DATA_R2.

      PERFORM F_SHOW_ALV_R2.
    WHEN OTHERS.
  ENDCASE .