*&---------------------------------------------------------------------*
*& 包含               ZQMR008_SCR
*&---------------------------------------------------------------------*



SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE t0.
SELECT-OPTIONS:
s_aendt FOR qprs-aendt, "接检日期
s_kdauf FOR afpo-kdauf,"销售订单号
s_prfql FOR qpmk-prfql,"检验岗位
s_lagort FOR qals-lagortchrg,"库存地
s_art FOR qals-art,"检验类型
s_matnr FOR  qals-matnr, " 物料编码
s_werk FOR   qals-werk," 工厂
s_flos FOR  qals-prueflos, "检验批
s_charg FOR   qals-charg."批次




SELECTION-SCREEN END OF BLOCK blk.