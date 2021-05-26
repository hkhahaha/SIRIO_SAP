*&---------------------------------------------------------------------*
*& 包含               ZMMR0065_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE t0.
SELECT-OPTIONS: s_werks FOR marc-werks, " 工厂
                s_mtart FOR mara-mtart, "物料类型
                s_matnr FOR marc-matnr, "物料
                s_lvorm1 FOR mara-lvorm, "删除标记（集团）
                s_lvorm2 FOR marc-lvorm, "删除标记（工厂）
                s_mmsta  FOR marc-mmsta, "特定工厂的物料状态
                s_mmstd FOR marc-mmstd, "有效起始期
                s_cxrq  FOR mara-ersda. "查询日期

SELECTION-SCREEN END OF BLOCK blk.