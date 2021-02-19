*&---------------------------------------------------------------------*
*& 包含               ZQMR0011_S01
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.
  PARAMETERS: P_WERKS TYPE AFPO-PWERK OBLIGATORY.
  SELECT-OPTIONS: S_ERSDA FOR MCHA-ERSDA.               " 日期
  SELECT-OPTIONS: S_KDAUF FOR AFPO-KDAUF.               " 销售订单号
  SELECT-OPTIONS: S_SORT1 FOR BUT000-BU_SORT1.          " 客户检索号
  SELECT-OPTIONS: S_MATNR FOR AFPO-MATNR.               " 客户检索号
  SELECT-OPTIONS: S_CHARG FOR MSEG-CHARG.               " 工艺批号
  SELECT-OPTIONS: S_SMATN FOR MSEG-MATNR.               " S产品代码
SELECTION-SCREEN:END OF BLOCK BLK1.