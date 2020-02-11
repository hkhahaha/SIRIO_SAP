*&---------------------------------------------------------------------*
*& 包含               ZPPR0028TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*  Tables
*&---------------------------------------------------------------------*
TABLES: qals, afpo.

*&---------------------------------------------------------------------*
*  Types
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_alv,
         prueflos     LIKE qals-prueflos,         "检验批号
         objnr        LIKE qals-objnr,            "对象号
         werks        LIKE qals-werk,             "工厂
         pastrterm    LIKE qals-pastrterm,        "检验开始时间
         paendterm    LIKE qals-paendterm,        "检验结束时间
         aufnr        LIKE afpo-aufnr,            "订单号
         kdauf        LIKE afpo-kdauf,            "销售订单
         zckgj        LIKE vbak-zckgj,            "出口国家
         landx        LIKE t005t-landx,           "出口国家名称
         kunnr        LIKE vbak-kunnr,            "客户
         name1        LIKE kna1-name1,            "客户名称
         matnr        LIKE qals-matnr,            "产品编号
         maktx        LIKE makt-maktx,            "产品名称
         zpspe        LIKE ausp-atwrt,            "规格型号
         lmengeist    LIKE qals-lmengeist,        "数量
         mengeneinh   LIKE qals-mengeneinh,       "单位
         charg        LIKE qals-charg,            "批次
         matnr_bcp    LIKE mseg-matnr,            "半成品编码
         maktx_bcp    LIKE makt-maktx,            "半成品名称
         prueflos_bcp LIKE qals-prueflos,         "半成品检验批号
         charg_bcp    LIKE mseg-charg,            "工艺批号
         sortfeld     LIKE qpmk-sortfeld,         "检验项目
         mittelwert   LIKE qamr-mittelwert,       "结果 - 定量
         pruefbemkt   LIKE qamr-pruefbemkt,       "结果 - 定性
         jg           LIKE qamr-pruefbemkt,       "结果
         mbewertg     LIKE qamr-mbewertg,         "评价
         pjzw         TYPE qamr-pruefbemkt,
       END OF ty_alv.

*&---------------------------------------------------------------------*
*  Intertables
*&---------------------------------------------------------------------*
DATA: gt_alv TYPE TABLE OF ty_alv,
      gs_alv TYPE ty_alv.

*&---------------------------------------------------------------------*
*  ALV变量
*&---------------------------------------------------------------------*
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gv_status   TYPE slis_formname VALUE 'FRM_STANDARD_ALV',
      gv_comand   TYPE slis_formname VALUE 'FRM_USER_COMMAND'.

*ALV菜单内表
DATA: gt_status_tab LIKE TABLE OF rsmpe-func,
      gs_status_tab LIKE rsmpe-func.

*&---------------------------------------------------------------------*
*  Macro
*&---------------------------------------------------------------------*
DEFINE append_range.
  IF &3 = ''.
    &1-sign = 'I'.
    &1-option = 'EQ'.
    &1-low = &2.
    APPEND &1.
    CLEAR  &1.
  ELSEIF &2 <> '' AND &3 <> ''.
    &1-sign = 'I'.
    &1-option = 'BT'.
    &1-low = &2.
    &1-high = &3.
    APPEND &1.
    CLEAR  &1.
  ENDIF.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*  选择屏幕
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_pruef  FOR qals-prueflos,
                s_kdauf  FOR afpo-kdauf,
                s_matnr  FOR qals-matnr,
                s_pastr  FOR qals-pastrterm,
                s_paend  FOR qals-paendterm,
                s_charg  FOR qals-charg,
                s_werk   FOR qals-werk NO-EXTENSION NO INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl.

*======================================================================*
*  Selection Screen Events
*======================================================================*
*  maintain selection screen output
AT SELECTION-SCREEN OUTPUT.
*修改屏幕
  PERFORM frm_change_screen.