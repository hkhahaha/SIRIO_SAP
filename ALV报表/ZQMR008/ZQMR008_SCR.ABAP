*&---------------------------------------------------------------------*
*& 包含               ZQMR008_SCR
*&---------------------------------------------------------------------*

*初始化时下拉框赋值
DATA: vid   TYPE vrm_id , "屏幕字段(可以是单个的I/O空间或者是Table Control中的一个单元格)
      list  TYPE vrm_values,
      value LIKE LINE OF list.


SELECTION-SCREEN BEGIN OF BLOCK blk WITH FRAME TITLE t0.
SELECT-OPTIONS:
s_aendt FOR qprs-aendt, "接检日期
s_kdauf FOR afpo-kdauf,"销售订单号
s_prfql FOR qpmk-prfql,"检验岗位
s_lagort FOR qals-lagortchrg,"库存地
s_art FOR qals-art,"检验类型
s_matnr FOR  qals-matnr, " 物料编码
*s_werk FOR   qals-werk," 工厂
s_flos FOR  qals-prueflos, "检验批
s_charg FOR   qals-charg."批次
PARAMETERS s_werk TYPE werks_d OBLIGATORY. " 工厂
PARAMETERS s_lsbox(2) TYPE c  AS LISTBOX VISIBLE LENGTH 20 DEFAULT ''. " 取样状态




SELECTION-SCREEN END OF BLOCK blk.

*初始化时下拉框赋值
INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

  REFRESH list .

  value-key = '10' . "这个就是变量P_LIST的值
  value-text = '已取样' . "这个是text
  APPEND value TO list .

  value-key = '20' .
  value-text = '未取样' .
  APPEND value TO list .

**调用下拉框赋值函数
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'S_LSBOX'
      values = list.





START-OF-SELECTION.

  ......

END-OF-SELECTION.