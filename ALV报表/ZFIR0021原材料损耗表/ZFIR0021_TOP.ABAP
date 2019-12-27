*&---------------------------------------------------------------------*
*& 包含               ZFIR0021_TOP
*&---------------------------------------------------------------------*
TABLES: AUFK, MATDOC, ACDOCA.
TYPE-POOLS: SLIS.

TYPES: BEGIN OF GTY_OUT,
         AUFNR  TYPE AUFK-AUFNR,            " 订单号
         AUART  TYPE AUFK-AUART,            " 订单类型
         WERKS  TYPE AUFK-WERKS,            " 工厂
         IDAT2  TYPE AUFK-IDAT2,            " 日期
         PLNBEZ TYPE AFKO-PLNBEZ,           " 物料号
         TXT30  TYPE TJ02T-TXT30,           " 订单状态
         AUSSS  TYPE MARC-AUSSS,            " 装配报废
         SAUSSS TYPE CHAR10,                " 装配报废(%)
         MAKTX  TYPE MAKT-MAKTX,            " 产品名称
         CHARG  TYPE MATDOC-CHARG,          " 批次
         MENGE  TYPE MATDOC-MENGE,          " 数量
         MEINS  TYPE MATDOC-MEINS,          " 单位
         SMATNR TYPE MATDOC-MATNR,          " 物料号
         SMAKTX TYPE MAKT-MAKTX,            " 产品名称
         SCHARG TYPE MATDOC-CHARG_SID,      " 产品批次
         SMENGE TYPE MATDOC-MENGE,          " 数量(实际)
         SMEINS TYPE MATDOC-MEINS,          " 单位(实际)
         BMENGE TYPE RESB-BDMNG,            " 数量(标准)
         BMEINS TYPE RESB-MEINS,            " 单位(标准)
         HMENGE TYPE RESB-BDMNG,            " 固定损耗量
         AUSCH  TYPE RESB-AUSCH,            " 组件报废百分比
         SAUSCH TYPE CHAR10,                " 组件报废百分比(%)
         ZLC    TYPE RESB-BDMNG,            " 量差(数量(实际)-数量(BOM))
         ZLCL   TYPE P LENGTH 10 DECIMALS 2," 量差/标准使用量
         ZSLCL  TYPE CHAR10,                " 量差率(百分比)
       END OF GTY_OUT.

DATA: GT_OUT TYPE STANDARD TABLE OF GTY_OUT,
      GS_OUT TYPE GTY_OUT.

DATA: GR_DATE  TYPE RANGE OF AUFIDAT2,
      GS_DATE  LIKE LINE OF GR_DATE,
      GR_AUFNR TYPE RANGE OF AUFNR,
      GS_AUFNR LIKE LINE OF GR_AUFNR.

DATA: GS_LAYOUT   TYPE  SLIS_LAYOUT_ALV,
      GT_FIELDCAT TYPE  SLIS_T_FIELDCAT_ALV.